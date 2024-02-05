#!/usr/bin/env perl
#
# Lists all "distinct" solutions to the 24 puzzle.
#
# Usage:
# perl solve.pl [number to make=24] [min number to use=0] [max number to use=13]
#
# License: Artistic-2.0 OR GPL-2.0-or-later
#
use v5.10;
use strict;
use warnings;
use autodie;

my $number_to_make    = 0+(shift // 24);
my $min_number_to_use = 0+(shift // 0);
my $max_number_to_use = 0+(shift // 13);
my @numbers_to_use    = ($min_number_to_use .. $max_number_to_use);

# Loads the `normalize` and `negate` subroutines.
require "./normalize.pl";

# Loads the possible 733 expressions.  Cf. https://oeis.org/A247982
open my $fh, "<", "expressions.txt";
my @expressions = map { s/\s+//g; $_ } <$fh>;
close $fh;

# Iterates over the possible 4-combinations with repetition of numbers.  If 13
# numbers are used, there are C(13+4-1, 4) = 1820 ways.  Cf.
# https://mathworld.wolfram.com/Multichoose.html
for my $numbers (combinations_with_repetition(\@numbers_to_use, 4)) {
    my ($a, $b, $c, $d) = @$numbers;

    my @solutions;
    my %seen;

    # Iterates over the expressions.
    for my $expr (@expressions) {
        my $value = eval $expr;

        # Skips on a division by zero.
        if ($@) {
            next;
        }

        # Checks if the value is equal to the target number, ignoring the sign.
        if (abs($value) eq $number_to_make) {
            # The expression with variables substituted with numbers (but not
            # evaluated).
            my $subst_expr = eval qq("$expr");

            if ($value < 0) {
                $subst_expr = negate($subst_expr);
                $subst_expr =~ s{ \s+ | ^\( | \)$ }{}gx;
            }

            my $normal_form = normalize($subst_expr);

            if (not exists $seen{$normal_form}) {
                push @solutions, $normal_form;
                $seen{$normal_form} = 1;
            }
        }
    }

    if (@solutions) {
        local $, = "\t";
        say join(" ", @$numbers), scalar(@solutions), @solutions;
    }
}

# Generates the k-combinations with repetition of an array, like Python's
# `itertools.combinations_with_replacement` or Ruby's
# `Array#repeated_combination`.
sub combinations_with_repetition {
    my $array = shift;
    my $k     = shift;

    if ($k == 0) {
        return ([]);
    }

    if (@$array == 0) {
        return ();
    }

    my ($first, @rest) = @$array;
    return (
        (map { [$first, @$_] } combinations_with_repetition($array, $k - 1)),
        combinations_with_repetition(\@rest, $k),
    );
}
