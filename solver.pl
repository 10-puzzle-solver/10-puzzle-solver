use strict;
use warnings;
use Algorithm::Combinatorics ('combinations_with_repetition');
use ntheory ('formultiperm', 'forsetproduct');

require './normalize.pl';

my @numbers_to_make = @ARGV || reverse(2 .. 24);
my $output_directory = 'solutions';

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

for my $number_to_make (@numbers_to_make) {
    my $path = sprintf("$output_directory/%03d.txt", $number_to_make);
    open(my $fh, '>', $path) or die("$path: $!");
    $fh->autoflush(1);

# DEBUG
# my $flag;

    # Generates 10 multichoose 4 = 715 possible puzzles from 0000 to 9999.
    my $iterator = combinations_with_repetition(\@numbers, $number_of_numbers);
    while (my $number_combination = $iterator->next()) {
        my $problem = join(' ', @$number_combination);

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

                    if ($value eq $number_to_make) {
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
            local $\ = "\n";
            local $, = "\t";
            print $fh ($problem, scalar(@solutions), @solutions);
        }
    }

    close($fh);
}
