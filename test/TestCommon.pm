# Written by Zack Weinberg <zackw at panix.com> in 2020.
# To the extent possible under law, Zack Weinberg has waived all
# copyright and related or neighboring rights to this work.
#
# See https://creativecommons.org/publicdomain/zero/1.0/ for further
# details.

# Code shared among all of the Perl-language tests in this directory.

package TestCommon;

use v5.14;    # implicit use strict, use feature ':5.14'
use warnings FATAL => 'all';
use utf8;
use open qw(:utf8);

no  if $] >= 5.022, warnings => 'experimental::re_strict';
use if $] >= 5.022, re       => 'strict';

use Cwd qw(realpath);
use File::Spec::Functions qw(
    catfile
    catdir
    catpath
    file_name_is_absolute
    path
    splitpath
);
use FindBin ();
use POSIX   ();

our @EXPORT_OK;
use Exporter qw(import);

BEGIN {
    @EXPORT_OK = qw(
        clean_PATH
        compare_symbol_lists
        ensure_C_locale
        error
        fail
        find_real_library
        get_symbols
        invocation_error
        popen
        sh_quote
        sh_split
        skip
        subprocess_error
        which
    );
}

## All of the functions in this group take a list of strings as arguments
## and process them as a whole.
## no critic (Subroutines::RequireArgUnpacking)

# Diagnostics: report that the test has failed.
sub fail {
    print {*STDERR} $FindBin::Script, ': FAIL: ', join(q{ }, @_), "\n";
    exit 1;
}

# Diagnostics: report that the test should be 'skipped' because
# some piece of infrastructure we need is missing.
sub skip {
    print {*STDERR} $FindBin::Script, ': skipping test: ', join(q{ }, @_), "\n";
    exit 77;
}

# Diagnostics: report that the test has encountered some kind of
# catastrophic internal error.
sub error {
    print {*STDERR} $FindBin::Script, ': ERROR: ', join(q{ }, @_), "\n";
    exit 99;
}

# Like 'error', but the problem was with a subprocess, detected upon
# trying to start the program named as @_.
sub invocation_error {
    my $err = "$!";
    my $cmd = join q{ }, @_;
    error("failed to invoke $cmd: $err");
}

# Like 'error', but the problem was with a subprocess, detected upon
# termination of the program named as @_; interpret both $! and $?
# appropriately.
sub subprocess_error {
    my $syserr = $!;
    my $status = $?;
    my $cmd    = join q{ }, @_;
    if ($syserr) {
        error("system error with pipe to $cmd: $syserr");

    } elsif ($status == 0) {
        return;

    } elsif (($status & 0xFF) == 0) {
        # we wouldn't be here if the exit status was zero
        error("$cmd: exit " . ($status >> 8));

    } else {
        my $sig = ($status & 0x7F);
        # Neither Perl core nor the POSIX module exposes strsignal.
        # This is the least terrible kludge I can presently find;
        # it decodes the numbers to their <signal.h> constant names
        # (e.g. "SIGKILL" instead of "Killed" for signal 9).
        # Linear search through POSIX's hundreds of symbols is
        # acceptable because this function terminates the test,
        # so it can only ever be called once per run.
        my $signame;
        while (my ($name, $glob) = each %{POSIX::}) {
            if ($name =~ /^SIG(?!_|RT)/ && (${$glob} // -1) == $sig) {
                $signame = $name;
                last;
            }
        }
        $signame //= "signal $sig";
        error("$cmd: killed by $signame");
    }
}

# Split a string into words, exactly the way the Bourne shell would do
# it, with the default setting of IFS, when the string is the result
# of a variable expansion.  If any of the resulting words would be
# changed by filename expansion, throw an exception, otherwise return
# a list of the words.
#
# Note: the word splitting process does *not* look for nested
# quotation, substitutions, or operators.  For instance, if a
# shell variable was set with
#    var='"ab cd"'
# then './a.out $var' would pass two arguments to a.out:
# '"ab' and 'cd"'.
sub sh_split {
    my @words = split /[ \t\n]+/, shift;
    for my $w (@words) {
        die "sh_split: '$w' could be changed by filename expansion"
            if $w =~ /(?a)(?<!\\)[\[?*]/;
    }
    return @words;
}

# Quote a string, or list of strings, so that they will pass
# unmolested through the shell.  Avoids adding quotation whenever
# possible.  Algorithm copied from Python's shlex.quote.
sub sh_quote {
    my @quoted;
    for my $w (@_) {
        if ($w =~ m{[^\w@%+=:,./-]}a) {
            my $q = $w;
            $q =~ s/'/'\\q{}/g;
            $q =~ s/^/'/;
            $q =~ s/$/'/;
            push @quoted, $q;
        } else {
            push @quoted, $w;
        }
    }
    return wantarray ? @quoted : $quoted[0];
}

# Emit a logging message for the execution of a subprocess whose
# argument vector is @_.
sub log_execution {
    print {*STDERR} '+ ', join(q{ }, sh_quote(@_)), "\n";
    return;
}

## use critic

# Run, and log execution of, a subprocess.  @_ should be one of the
# open modes that creates a pipe, followed by an argument vector.
# An anonymous filehandle for the pipe is returned.
# Calls invocation_error() if open() fails.
# Does *not* call which(); do that yourself if you need it.
sub popen {
    my ($mode, @args) = @_;
    die "popen: inappropriate mode argument '$mode'"
        unless $mode eq '-|' || $mode eq '|-';
    die 'popen: no command to execute'
        if scalar(@args) == 0;

    log_execution(@args);
    open my $fh, $mode, @args
        or invocation_error($args[0]);
    return $fh;
}

# Force use of the C locale for this process and all subprocesses.
# This is necessary because subprocesses' output may be locale-
# dependent.  If the C.UTF-8 locale is available, it is used,
# otherwise the plain C locale.  Note that we do *not*
# 'use locale' here or anywhere else!
sub ensure_C_locale {
    use POSIX qw(setlocale LC_ALL);

    for my $k (keys %ENV) {
        if ($k eq 'LANG' || $k eq 'LANGUAGE' || $k =~ /^LC_/) {
            delete $ENV{$k};
        }
    }
    if (defined(setlocale(LC_ALL, 'C.UTF-8'))) {
        $ENV{LC_ALL} = 'C.UTF-8'; ## no critic (RequireLocalizedPunctuationVars)
    } elsif (defined(setlocale(LC_ALL, 'C'))) {
        $ENV{LC_ALL} = 'C';       ## no critic (RequireLocalizedPunctuationVars)
    } else {
        error("could not set 'C' locale: $!");
    }
    return;
}

# Clean up $ENV{PATH}, and return the cleaned path as a list.
sub clean_PATH {
    state @path;
    if (!@path) {
        for my $d (path()) {
            # Discard all entries that are not absolute paths.
            next unless file_name_is_absolute($d);
            # Discard all entries that are not directories, or don't
            # exist.  (This is not just for tidiness; realpath()
            # behaves unpredictably if called on a nonexistent
            # pathname.)
            next unless -d $d;
            # Resolve symlinks in all remaining entries.
            $d = realpath($d);
            # Discard duplicates.
            push @path, $d unless grep { $_ eq $d } @path;
        }
        error('nothing left after cleaning PATH')
            unless @path;

        # File::Spec knows internally whether $PATH is colon-separated
        # or semicolon-separated, but it won't tell us.  Assume it's
        # colon-separated unless the first element of $PATH has a
        # colon in it (and is therefore probably a DOS-style absolute
        # path, with a drive letter).
        my $newpath;
        if ($path[0] =~ /:/) {
            $newpath = join ';', @path;
        } else {
            $newpath = join ':', @path;
        }
        $ENV{PATH} = $newpath;    ## no critic (RequireLocalizedPunctuationVars)
    }
    return @path;
}

# Locate a program that we need in order to run this test.
# $_[0] is the name of the program along with any options that are
# required to use it correctly.  Split this into an argument list,
# exactly as /bin/sh would do it, and then search $PATH for the
# executable.  If we find it, return a list whose first element is
# the absolute pathname of the executable, followed by any options.
# Otherwise return an empty list.
sub which {
    my ($command) = @_;
    my @PATH = clean_PATH();

    # Split the command name from any options attached to it.
    my ($cmd, @options) = sh_split($command);
    my ($vol, $path, $file) = splitpath($cmd);

    if ($file eq 'false') {
        # Special case: the command 'false' is never considered to be
        # available.  Autoconf sets config variables like $CC and $NM to
        # 'false' if it can't find the requested tool.
        return ();

    } elsif ($file ne $cmd) {
        # $cmd was not a bare filename.  Do not do path search, but do
        # verify that $cmd exists and is executable, then convert it
        # to a canonical absolute path.
        #
        # Note: the result of realpath() is unspecified if its
        # argument does not exist, so we must test its existence
        # first.
        #
        # Note: if $file is a symlink, we must *not* resolve that
        # symlink, because that may change the name of the program,
        # which in turn may change what the program does.
        # For instance, suppose $CC is /usr/lib/ccache/cc, and this
        # 'cc' is a symlink to /usr/bin/ccache.  Resolving the symlink
        # will cause ccache to be invoked as 'ccache' instead of 'cc'
        # and it will error out because it's no longer being told
        # it's supposed to run the compiler.
        if (-f -x $cmd) {
            return (catfile(realpath(catpath($vol, $path, q{})), $file),
                @options);
        } else {
            return ();
        }

    } else {
        for my $d (@PATH) {
            my $cand = catfile($d, $cmd);
            if (-f -x $cand) {
                # @PATH came from clean_PATH, so all of the directories
                # have already been canonicalized.  If the last element
                # of $cand is a symlink, we should *not* resolve it (see
                # above).  Therefore, we do not call realpath here.
                return ($cand, @options);
            }
        }
        return ();

    }
}

# Parse a .la file (arg 1) and determine the name of the actual .a or
# .so file it refers to (arg 2: 'static' for .a, 'shared' for .so)
sub find_real_library {
    my ($lib_la, $type) = @_;

    state @SH;
    if (!@SH) {
        @SH = which($ENV{SHELL} || $ENV{CONFIG_SHELL} || '/bin/sh');
        error('no shell available???') if !@SH;
    }

    my $param;
    if ($type eq 'shared') {
        $param = 'dlname';
    } elsif ($type eq 'static') {
        $param = 'old_library';
    } else {
        error("unknown library type: '$type'");
    }

    # We're going to interpolate $lib_la into a shell command.
    # Save the unmangled directory part first, then quote it.
    my ($vol, $dir, undef) = splitpath($lib_la);
    $lib_la = sh_quote($lib_la);

    # .la files are shell script fragments.  The easiest way to learn
    # the name of the actual library is to ask a shell to parse the
    # fragment for us.
    my $fh = popen('-|', @SH, '-c', ". $lib_la; printf %s \"\$$param\"");
    my $real_library;
    {
        local $/ = undef;    # slurp
        $real_library = <$fh>;
    }
    close $fh or subprocess_error($SH[0]);

    chomp $real_library;
    $real_library = catpath($vol, catdir($dir, '.libs'), $real_library);
    error("'$real_library' does not exist") unless -f $real_library;
    return realpath($real_library);
}

# In some object file formats, all symbols defined in C have an
# underscore prepended to their names.  The configure script detects
# this and the Makefiles set this environment variable appropriately.
my $symbol_prefix = $ENV{SYMBOL_PREFIX} || q{};

# Return a hashset of symbols exported by the library $_[0], using readelf.
# If it is a dynamic library, annotate each symbol with its version tag.
sub get_symbols_readelf {
    my $lib    = shift;
    my $filter = shift // sub { 1 };

    state $readelf_works = 1;
    die "readelf doesn't work\n" unless $readelf_works;

    state @READELF;
    if (!@READELF) {
        @READELF = which($ENV{READELF} || 'readelf');
        die "readelf not available\n" unless @READELF;
    }

    my @opts              = ('--wide');
    my $want_version_tags = 0;
    if ($lib =~ /\.(?:a|lib)$/) {
        push @opts, '--syms';
    } else {
        push @opts, '--dyn-syms';
        $want_version_tags = 1;
    }

    my $fh = popen('-|', @READELF, @opts, $lib);

    local $_;
    my %symbols;
    my $saw_version_tags = 0;
    while (<$fh>) {
        chomp;
        s/\s+$//;
        next if /^(?:$|File:|Symbol table)/;
        next if /^\s*Num:\s+Value\s+Size\s+Type\s+Bind\s+Vis\s+Ndx\s+Name$/;

        my ($num, $value, $size, $type, $bind, $vis, $ndx, $name) = split;

        # We are only interested in globally visible, defined,
        # non-absolute symbols.
        next
            if $ndx eq 'UND'
            || $ndx eq 'ABS'
            || $bind eq 'LOCAL';

        # Strip the symbol prefix, if any, from each symbol.
        $name =~ s/^$symbol_prefix// if $symbol_prefix ne q{};

        $saw_version_tags = 1 if $name =~ /@[A-Z_]+[0-9]/;

        if (&{$filter}($name)) {
            print {*STDERR} "|+ $name\n";
            $symbols{$name} = 1;
        } else {
            print {*STDERR} "|- $name\n";
        }
    }
    if (!close $fh) {
        # If it ran but exited 1 or 2, don't give up yet, we still
        # have nm to try.
        if ($! == 0 && ($? == 256 || $? == 512)) {
            $readelf_works = 0;
            die "$READELF[0] exited " . ($? >> 2) . "\n";
        }
        subprocess_error($READELF[0]);
    }
    if ($want_version_tags && !$saw_version_tags) {
        $readelf_works = 0;
        die "$READELF[0] did not print version tags\n";
    }
    return \%symbols;
}

# Return a hashset of symbols exported by the library $_[0], using nm.
# If it is a dynamic library, annotate each symbol with its version tag.
sub get_symbols_nm {
    my $lib    = shift;
    my $filter = shift // sub { 1 };

    state $nm_works = 1;
    die "nm doesn't work\n" unless $nm_works;

    state @NM;
    if (!@NM) {
        @NM = which($ENV{NM} || 'nm');
        die "nm not available\n" unless @NM;
    }

    my @opts              = qw(--format=bsd --extern-only --defined-only);
    my $want_version_tags = 0;
    if ($lib !~ /\.(?:a|lib)$/) {
        push @opts, qw(--dynamic --with-symbol-versions);
        $want_version_tags = 1;
    }

    my $fh = popen('-|', @NM, @opts, $lib);
    local $_;
    my %symbols;
    my $saw_version_tags = 0;
    while (<$fh>) {
        chomp;
        s/\s+$//;
        next unless $_;

        # BSD-format nm output, when restricted to external, defined
        # symbols, has three fields per line: address type name.
        # We shouldn't ever see symbols with the address field blank,
        # but just in case, discard them.
        next unless /^([0-9a-fA-F]+)\s+([A-Za-z])\s+(\S+)$/;
        my $addr = $1;
        my $type = $2;
        my $name = $3;

        # Symbols whose address is 0 and type is A are uninteresting;
        # they define the set of symbol version tags.
        next if $addr =~ /^0+$/ && $type eq 'A';

        # Strip the symbol prefix, if any, from each symbol.
        $name =~ s/^$symbol_prefix// if $symbol_prefix;

        # Compensate for a bug in some versions of GNU nm
        # where the symbol version is printed twice.
        $name =~ s/(@+[A-Z0-9_.]+)\1$/$1/;

        $saw_version_tags = 1 if $name =~ /@[A-Z_]+[0-9]/;

        if (&{$filter}($name)) {
            print {*STDERR} "|+ $name\n";
            $symbols{$name} = 1;
        } else {
            print {*STDERR} "|- $name\n";
        }
    }
    if (!close $fh) {
        # If it ran but exited 1 or 2, don't give up yet, we still
        # have readelf to try.
        if ($! == 0 && ($? == 256 || $? == 512)) {
            $nm_works = 0;
            die "$NM[0] exited " . ($? >> 8) . "\n";
        }
        subprocess_error($NM[0]);
    }
    if ($want_version_tags && !$saw_version_tags) {
        $nm_works = 0;
        die "$NM[0] did not print version tags\n";
    }
    return \%symbols;
}

# Return a hashset of symbols exported by the library $_[0], using
# readelf or nm, whichever works on this system.  If it is a dynamic
# library, annotate each symbol with its version tag.  If $_[1] is
# defined, it is a filter procedure; only symbols for which the filter
# returns true are included in the hashset.
## no critic (Subroutines::RequireArgUnpacking)
sub get_symbols {
    my $result;

    $result = eval { get_symbols_nm(@_); };
    return $result if $result;
    print {*STDERR} "get_symbols_nm: $@";

    $result = eval { get_symbols_readelf(@_); };
    return $result if $result;
    print {*STDERR} "get_symbols_readelf: $@";

    skip('cannot get symbols using either readelf or nm');
}
## use critic

sub compare_symbol_lists {
    my ($found, $expected, $tag, $extra_allowed) = @_;
    my @extra;
    my @missing;
    local $_;
    for (keys %{$expected}) {
        push @missing, $_ unless exists $found->{$_};
    }
    for (keys %{$found}) {
        push @extra, $_ unless exists $expected->{$_};
    }

    my $error = 0;
    if (@extra) {
        $error = 1 unless $extra_allowed;
        print {*STDERR} "*** Extra $tag:\n";
        for (sort @extra) {
            s/^_crypt_//;
            print {*STDERR} "  $_\n";
        }
    }
    if (@missing) {
        $error = 1;
        print {*STDERR} "*** Missing $tag:\n";
        for (sort @missing) {
            s/^_crypt_//;
            print {*STDERR} "  $_\n";
        }
    }
    return $error;
}

1;
