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
no  if $] >= 5.022, warnings => 'experimental::re_strict';
use if $] >= 5.022, re       => 'strict';

our @EXPORT_OK;
use Exporter qw(import);

BEGIN {
    @EXPORT_OK = qw(
        enabled_set
        parse_hashes_conf
    );
}

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
            &{$err}('wrong number of fields');
            next;
        }
        my ($name, $h_prefix, $nrbytes, $flags) = @fields;
        my $default_cand = 0;
        my $is_strong    = 0;
        my @groups;

        if ($name eq ':') {
            &{$err}('method name cannot be blank');
            $name = "_missing_$.";
        }

        # No two hashing method names can be the same.
        if (exists $line_of{$name}) {
            &{$err}("method name '$name' reused");
            &{$note}($line_of{$name}, 'previous use was here');
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
            &{$err}('nrbytes must be a positive integer');
            $nrbytes = 1;
        }

        $flags = q{} if $flags eq ':';
        for (split /,/, $flags) {
            if (!exists $VALID_FLAGS{$_}) {
                &{$err}("unrecognized flag $_");
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
            &{$err}('weak hash marked as default candidate');
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
                &{$err}(
                    $line_of{$b->name},
                    "prefix collision: '$pb' begins with '$pa'"
                );
                &{$note}(
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

1;
