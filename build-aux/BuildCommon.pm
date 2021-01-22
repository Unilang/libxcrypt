# Written by Zack Weinberg <zackw at panix.com> in 2017 and 2020.
# To the extent possible under law, Zack Weinberg has waived all
# copyright and related or neighboring rights to this work.
#
# See https://creativecommons.org/publicdomain/zero/1.0/ for further
# details.

package BuildCommon;

use v5.14;    # implicit use strict, use feature ':5.14'
use warnings FATAL => 'all';
use utf8;
use open qw(:utf8);
no warnings 'experimental::smartmatch';
no  if $] >= 5.022, warnings => 'experimental::re_strict';
use if $] >= 5.022, re       => 'strict';

use File::Spec::Functions qw(splitpath);
use FindBin;

our @EXPORT_OK;
use Exporter qw(import);

BEGIN {
    @EXPORT_OK = qw(
        enabled_set
        parse_hashes_conf
        parse_version_map_in
    );
}

#
# Code shared among scripts that work from hashes.conf
#

use Class::Struct HashSpec => [
    name    => '$',
    prefix  => '$',
    nrbytes => '$',
];
use Class::Struct HashesConfData => [
    hashes             => '*%',
    groups             => '*%',
    max_namelen        => '$',
    max_prefixlen      => '$',
    default_candidates => '*@',
];

# The canonical list of flags that can appear in the fourth field
# of a hashes.conf entry.  Alphabetical, except for STRONG and
# DEFAULT.
my %VALID_FLAGS = (
    STRONG  => 1,
    DEFAULT => 1,
    ALT     => 1,
    FEDORA  => 1,
    FREEBSD => 1,
    GLIBC   => 1,
    NETBSD  => 1,
    OPENBSD => 1,
    OSX     => 1,
    OWL     => 1,
    SOLARIS => 1,
    SUSE    => 1,
);

sub parse_hashes_conf {
    my $fname = shift;
    my $error = 0;

    my $err = sub {
        my ($line, $msg) = @_;
        if (!defined $msg) {
            $msg  = $line;
            $line = $.;
        }
        print {*STDERR} "$fname:$line: error: $msg\n";
        $error = 1;
    };
    my $note = sub {
        my ($line, $msg) = @_;
        if (!defined $msg) {
            $msg  = $line;
            $line = $.;
        }
        print {*STDERR} "$fname:$line: note: $msg\n";
    };

    open my $fh, '<', $fname
        or die "$fname: $!\n";

    my %line_of;
    my %hashes;
    my %groups;
    my $max_namelen   = 0;
    my $max_prefixlen = 0;
    my @default_candidates;
    local $_;
    while (<$fh>) {
        next if /^#/;
        chomp;
        s/\s+$//;
        next if $_ eq q{};

        my @fields = split;
        if (scalar(@fields) != 4) {
            $err->('wrong number of fields');
            next;
        }
        my ($name, $h_prefix, $nrbytes, $flags) = @fields;
        my $default_cand = 0;
        my $is_strong    = 0;
        my @groups;

        if ($name eq ':') {
            $err->('method name cannot be blank');
            $name = "_missing_$.";
        }

        # No two hashing method names can be the same.
        if (exists $line_of{$name}) {
            $err->("method name '$name' reused");
            $note->($line_of{$name}, 'previous use was here');
        } else {
            $line_of{$name} = $.;
            if ($max_namelen < length $name) {
                $max_namelen = length $name;
            }
        }

        $h_prefix = q{} if $h_prefix eq ':';
        if ($max_prefixlen < length $h_prefix) {
            $max_prefixlen = length $h_prefix;
        }

        if ($nrbytes !~ /^[0-9]+$/ || $nrbytes == 0) {
            $err->('nrbytes must be a positive integer');
            $nrbytes = 1;
        }

        $flags = q{} if $flags eq ':';
        for (split /,/, $flags) {
            if (!exists $VALID_FLAGS{$_}) {
                $err->("unrecognized flag $_");
            } elsif ($_ eq 'DEFAULT') {
                $default_cand = 1;
            } else {
                push @groups, lc;
                if ($_ eq 'STRONG') {
                    $is_strong = 1;
                }
            }
        }
        if ($default_cand && !$is_strong) {
            $err->('weak hash marked as default candidate');
        }

        next if $error;

        my $entry = HashSpec->new(
            name    => $name,
            prefix  => $h_prefix,
            nrbytes => $nrbytes
        );
        $hashes{$name} = $entry;
        for my $g (@groups) {
            $groups{$g} = [] unless exists $groups{$g};
            push @{$groups{$g}}, $entry;
        }
        if ($default_cand) {
            push @default_candidates, $entry;
        }
    }

    # No hash prefix can be a prefix of any other hash prefix, except
    # for the empty prefix.
    for my $a (values %hashes) {
        my $pa = $a->prefix;
        next if $pa eq q{};
        my $mpa = qr/^\Q$pa\E/;
        for my $b (values %hashes) {
            next if $a->name eq $b->name;
            my $pb = $b->prefix;
            next if $pb eq q{};
            if ($pb =~ $mpa) {
                $err->(
                    $line_of{$b->name},
                    "prefix collision: '$pb' begins with '$pa'"
                );
                $note->(
                    $line_of{$a->name},
                    "'$pa' used for hash '" . $a->name . q{'}
                );
            }
        }
    }

    die "errors while parsing '$fname'\n" if $error;
    return HashesConfData->new(
        hashes             => \%hashes,
        groups             => \%groups,
        max_namelen        => $max_namelen,
        max_prefixlen      => $max_prefixlen,
        default_candidates => \@default_candidates,
    );
}

sub enabled_set {
    return map { $_ => 1 }
        grep   { $_ ne q{} }
        split /,/,
        shift;
}

#
# Code shared among scripts that work from libcrypt.map.in
#

use Class::Struct VersionedSymbol => [
    name        => '$',
    included    => '$',
    compat_only => '$',
    versions    => '*@',
];

use Class::Struct SymbolVersionMap => [
    symbols    => '*@',
    versions   => '*@',
    basemap    => '$',
    max_symlen => '$',
];

# Process command-line arguments to a program that works from a
# .map.in file.  These are the name of the .map.in file plus var=value
# settings for SYMVER_MIN, SYMVER_FLOOR, and COMPAT_ABI, in any order.
sub parse_symver_args {
    my (@args) = @_;
    my $usage_error = sub {
        print {*STDERR}
            "${FindBin::Script}: usage: ",
            'SYMVER_MIN=value SYMVER_FLOOR=value ',
            'COMPAT_ABI=value libcrypt.map.in',
            "\n";
        exit 1;
    };
    $usage_error->() if scalar(@args) != 4;

    my $map_in;
    my $SYMVER_MIN;
    my $SYMVER_FLOOR;
    my $COMPAT_ABI;
    local $_;
    for (@args) {
        when (/^SYMVER_MIN=(.+)$/) {
            $usage_error->() if defined $SYMVER_MIN;
            $SYMVER_MIN = $1;
        }
        when (/^SYMVER_FLOOR=(.+)$/) {
            $usage_error->() if defined $SYMVER_FLOOR;
            $SYMVER_FLOOR = $1;
        }
        when (/^COMPAT_ABI=(.+)$/) {
            $usage_error->() if defined $COMPAT_ABI;
            $COMPAT_ABI = $1;
        }
        default {
            $usage_error->() if defined $map_in;
            $map_in = $_;
        }
    }
    return $map_in, $SYMVER_MIN, $SYMVER_FLOOR, $COMPAT_ABI;
}

# Read a .map.in file and compute the set of symbol versions to be
# included in this build of the library.
#
# All compat symbol versions that do not match COMPAT_ABI are ignored.
# All symbol versions lower than SYMVER_MIN are discarded from the output.
# All symbol versions lower than SYMVER_FLOOR are replaced with SYMVER_FLOOR.
# SYMVER_FLOOR must be greater than or equal to SYMVER_MIN.
#
# The ordering of symbol versions is entirely controlled by the %chain
# directive, which must therefore list both all of the versions
# actually used for symbols, and all of the versions that might be
# used as SYMVER_MIN or SYMVER_FLOOR.
## no critic (Subroutines::RequireArgUnpacking)
sub parse_version_map_in {
    my ($map_in, $SYMVER_MIN, $SYMVER_FLOOR, $COMPAT_ABI) =
        parse_symver_args(@_);

    my %symbols;
    my %vorder;
    my $vmax = 0;
    my $error;
    my $max_symlen = 0;
    open my $fh, '<', $map_in
        or die "$map_in: $!\n";

    local $_;
    while (<$fh>) {
        next if /^#/;
        chomp;
        s/\s+$//;
        next if $_ eq q{};

        my @vers = split;
        my $sym  = shift @vers;
        if ($sym eq '%chain') {
            for my $v (@vers) {
                if (exists $vorder{$v}) {
                    print {*STDERR}
                        "$map_in:$.: error: '$v' used twice in %chain\n";
                    $error = 1;
                    next;
                }
                $vorder{$v} = $vmax;
                $vmax++;
            }
            next;
        }
        if (exists $symbols{$sym}) {
            print {*STDERR}
                "$map_in:$.: error: more than one entry for '$sym'\n";
            $error = 1;
            next;
        }
        if ($max_symlen < length $sym) {
            $max_symlen = length $sym;
        }

        # Dash in the second field means there is no default version
        # for this symbol.
        my $compat_only = 0;
        if ($vers[0] eq '-') {
            $compat_only = 1;
            shift @vers;
        }

        my @enabled_vers;
        for my $v (@vers) {
            # Each $v is a symbol version name followed by zero
            # or more compatibility tags, separated by colons.
            # If there are no tags, the symbol version is available
            # unconditionally; if there are any tags, the symbol
            # version is available if COMPAT_ABI is equal to 'yes'
            # or equal to one of the tags.
            my @tags = split /:/, $v;
            $v = shift @tags;
            my $enabled = 1;
            if (@tags && $COMPAT_ABI ne 'yes') {
                $enabled = 0;
                for my $t (@tags) {
                    if ($t eq $COMPAT_ABI) {
                        $enabled = 1;
                        last;
                    }
                }
            }
            push @enabled_vers, $v if $enabled;
        }
        $symbols{$sym} = VersionedSymbol->new(
            name        => $sym,
            included    => 1,
            compat_only => $compat_only,
            versions    => \@enabled_vers,
        );
    }

    my $symver_min_idx;
    my $symver_floor_idx;
    if (!%vorder) {
        print {*STDERR} "$map_in: error: missing %chain directive\n";
        $error = 1;
    } else {
        $symver_min_idx   = $vorder{$SYMVER_MIN}   // -2;
        $symver_floor_idx = $vorder{$SYMVER_FLOOR} // -1;
        if ($symver_min_idx < 0) {
            print {*STDERR}
                "$map_in: error: SYMVER_MIN ($SYMVER_MIN) ",
                "not found in \%chain directives\n";
            $error = 1;
        }
        if ($symver_floor_idx < 0) {
            print {*STDERR}
                "$map_in: error: SYMVER_FLOOR ($SYMVER_FLOOR) ",
                "not found in \%chain directives\n";
            $error = 1;
        }
        if ($symver_floor_idx < $symver_min_idx) {
            print {*STDERR}
                "$map_in: error: SYMVER_FLOOR ($SYMVER_FLOOR) ",
                "is lower than SYMVER_MIN ($SYMVER_MIN)\n";
            $error = 1;
        }
    }
    die "errors processing '$map_in'\n" if $error;

    # For each symbol, remove all of its versions below SYMVER_MIN,
    # and replace all of its versions below SYMVER_FLOOR with a single
    # instance of SYMVER_FLOOR.  If none are left, mark the symbol as
    # not included.  Otherwise, sort its 'versions' array in
    # _descending_ order of symbol version.  As we do this, keep track
    # of all the symbol versions that are actually used.
    my %used_versions;
    for my $sym (values %symbols) {
        my %pruned_versions;
        for my $v (@{$sym->versions}) {
            if (!exists $vorder{$v}) {
                print {*STDERR}
                    "$map_in: error: version '$v' for symbol '",
                    $sym->name, "' not found in %chain\n";
                $error = 1;
                next;
            }
            if ($vorder{$v} < $symver_min_idx) {
                next;
            } elsif ($vorder{$v} < $symver_floor_idx) {
                $pruned_versions{$SYMVER_FLOOR} = 1;
                $used_versions{$SYMVER_FLOOR}   = 1;
            } else {
                $pruned_versions{$v} = 1;
                $used_versions{$v}   = 1;
            }
        }
        if (%pruned_versions) {
            @{$sym->versions} =
                sort { -($vorder{$a} <=> $vorder{$b}) }
                keys %pruned_versions;
        } else {
            $sym->included(0);
            @{$sym->versions} = ();
        }
    }

    # Sort the set of used symbol versions in _ascending_ order.
    my @vchain = sort { $vorder{$a} <=> $vorder{$b} } keys %used_versions;

    my (undef, undef, $basemap) = splitpath($map_in);
    return SymbolVersionMap->new(
        symbols    => [values %symbols],
        versions   => \@vchain,
        basemap    => $basemap,
        max_symlen => $max_symlen
    );
}
## use critic

1;
