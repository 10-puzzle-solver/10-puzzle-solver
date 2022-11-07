use strict;
use warnings;
use Algorithm::Combinatorics ('combinations_with_repetition');

require './normalize.pl';

my $VERBOSE = 1;

my $expression_list_file = 'expressions.txt';
my $output_directory = 'solutions';

my @numbers_to_make = @ARGV;
if (not @numbers_to_make) {
    @numbers_to_make = reverse(0 .. 24);
}

my @expressions;
open(my $fh, '<', $expression_list_file) or die("$expression_list_file: $!");
while (<$fh>) {
    chomp($_);
    push(@expressions, $_);
}
close($fh);

my @numbers = (0 .. 9);
my $number_of_numbers = 4;

for my $number_to_make (@numbers_to_make) {
    $VERBOSE and warn("$number_to_make\n");
    my $path = sprintf("$output_directory/%03d.txt", $number_to_make);
    open(my $fh, '>', $path) or die("$path: $!");
    $fh->autoflush(1);

    my $iterator = combinations_with_repetition(\@numbers, $number_of_numbers);
    while (my $number_combination = $iterator->next()) {
        my ($a, $b, $c, $d) = @$number_combination;
        my %seen;
        my @solutions;

        for my $expression (@expressions) {
            my $value = eval($expression);

            # Division by zero
            if ($@) {
                next;
            }

            if ($value eq $number_to_make or $value eq -$number_to_make) {
                my $substituted_expression = eval(qq{"$expression"});
                if ($value < 0) {
                    $substituted_expression = negate($substituted_expression);
                    $substituted_expression =~ s{ \A \( | \) \z }{}gx;
                }
                my $normal_form = normalize($substituted_expression);
                if (not exists($seen{$normal_form})) {
                    push(@solutions, $normal_form);
                    $seen{$normal_form} = 1;
                }
            }
        }

        if (@solutions) {
            local $\ = "\n";
            local $, = "\t";
            print $fh (
                join(' ', @$number_combination),
                scalar(@solutions),
                @solutions,
            );
        }
    }

    close($fh);
}
