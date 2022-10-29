use strict;
use warnings;
use Algorithm::Combinatorics ('combinations_with_repetition');
use ntheory ('formultiperm', 'forsetproduct');

require './normalize.pl';

my $NUMBER_TO_MAKE = shift(@ARGV);
if (not defined($NUMBER_TO_MAKE)) {
    $NUMBER_TO_MAKE = 10;
}

my @numbers = (0 .. 9);
my @operators = ('+', '-', '*', '/');
my $number_of_numbers = 4;
my @patterns = (
    '((%d %%s %d) %%s %d) %%s %d',
    '(%d %%s (%d %%s %d)) %%s %d',
    '(%d %%s %d) %%s (%d %%s %d)',
    '%d %%s ((%d %%s %d) %%s %d)',
    '%d %%s (%d %%s (%d %%s %d))',
);

# DEBUG
# my $flag;

# Generates 10 multichoose 4 = 715 possible puzzles from 0000 to 9999.
my $iterator = combinations_with_repetition(\@numbers, $number_of_numbers);
while (my $number_combination = $iterator->next()) {
    my $problem = join('', @$number_combination);

# DEBUG
# if (not $flag and $problem ne '0011') {
#     next;
# }
# $flag = 1;

    my %seen;
    my @solutions;

    # Generates multiset permutations of the numbers.
    formultiperm {
        my @number_permutation = @_;

        # Generates 3-permutations with repetition of the operators.
        forsetproduct {
            my @operator_permutation = @_;

            for my $pattern (@patterns) {
                my $expression = sprintf(
                    sprintf($pattern, @number_permutation),
                    @operator_permutation,
                );
                my $value = eval($expression);

                # Division by zero
                if ($@) {
                    next;
                }

                if ($value eq $NUMBER_TO_MAKE) {
# DEBUG
# warn("$expression\n");

                    my $normal_form = normalize($expression);
                    if (not exists($seen{$normal_form})) {
                        push(@solutions, $normal_form);
                        $seen{$normal_form} = 1;
                    }
                }
            }
        } (\@operators) x ($number_of_numbers - 1);
    } $number_combination;

    if (@solutions) {
        local $| = 1;
        local $\ = "\n";
        local $, = "\t";
        print($problem, scalar(@solutions), @solutions);
    }
}
