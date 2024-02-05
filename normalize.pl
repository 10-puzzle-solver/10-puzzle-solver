#!/usr/bin/env perl
#
# Normalizes an expression that represents a solution to the 24 puzzle.  The
# expression must be in the form ((1*2)/3)+4.
#
# Usage:
# perl normalize.pl
# [Reads expressions line by line from STDIN...]
#
# License: Artistic-2.0 OR GPL-2.0-or-later
#
use v5.10;
use strict;
use warnings;
my $VERBOSE = 0;

# For perl prior to v5.18.  Cf. https://perldoc.perl.org/perl5180delta
use re "eval";

#
# Regular expressions.  Cf. https://perldoc.perl.org/perlre
#

# A number.
my $NUMBER   = qr{ (?<! \d ) \d+ (?! \d ) }x;

# An operator.
my $OPERATOR = qr{ [+\-*/] }x;
my $OP       = qr{ (?<OP> $OPERATOR ) }x;

# An expression.
my $EXPR = qr{
    (
        $NUMBER
        |
        # Recurses to the innermost group (i.e., whole pattern).
        \( (?-1) $OPERATOR (?-1) \)
    )
}x;
my $A = qr{ (?<A> $EXPR ) }x;
my $B = qr{ (?<B> $EXPR ) }x;
my $C = qr{ (?<C> $EXPR ) }x;

my $X = qr{ (?<X> $EXPR ) }x;

# An expression whose value is equal to X.
my $X2 = qr{
    (?<X2> $EXPR )
    (?(?{ eval("($+{X2}) - ($+{X})") eq "0" }) | (*FAIL) )
}x;

# An expression whose value is 0.
my $ZERO_EXPR = qr{
    ( $EXPR )
    (?(?{
        # `$^N` is the same as `$1` but works even if embedded in other
        # patterns.
        eval($^N) eq "0"
    }) | (*FAIL) )
}x;
my $ZERO  = qr{ (?<ZERO> $ZERO_EXPR ) }x;
my $ZERO2 = qr{ (?<ZERO2> $ZERO_EXPR ) }x;

# An expression whose value is 1.
my $ONE_EXPR = qr{
    ( $EXPR )
    (?(?{ eval($^N) eq "1" }) | (*FAIL) )
}x;
my $ONE  = qr{ (?<ONE> $ONE_EXPR ) }x;
my $ONE2 = qr{ (?<ONE2> $ONE_EXPR ) }x;

my $A_PLUS_ZERO  = qr{ $A \+ $ZERO | $ZERO \+ $A }x;
my $B_PLUS_ZERO  = qr{ $B \+ $ZERO | $ZERO \+ $B }x;

my $A_TIMES_ZERO = qr{ $A \* $ZERO | $ZERO \* $A }x;

my $A_TIMES_ONE  = qr{ $A \* $ONE | $ONE \* $A }x;
my $B_TIMES_ONE  = qr{ $B \* $ONE | $ONE \* $B }x;

my $A_PLUS_X     = qr{ $A \+ $X | $X \+ $A }x;
my $B_PLUS_X     = qr{ $B \+ $X | $X \+ $B }x;

my $A_TIMES_X    = qr{ $A \* $X | $X \* $A }x;
my $B_TIMES_X    = qr{ $B \* $X | $X \* $B }x;
my $B_TIMES_X2   = qr{ $B \* $X2 | $X2 \* $B }x;

#
# Rewrite rules.
#

my @rules = (
    # Subtraction by zero to addition.
    "A-0=>A+0" => [ qr{ $A - $ZERO }x => sub { "$+{A} + $+{ZERO}" } ],

    # Multiplication of zeros to addition.
    "0*0=>0+0" => [ qr{ $ZERO \* $ZERO2 }x => sub { "$+{ZERO} + $+{ZERO2}" } ],

    # Division by one to multiplication.
    "A/1=>A*1" => [ qr{ $A / $ONE }x => sub { "$+{A} * $+{ONE}" } ],

    # Division where the dividend is zero to multiplication.
    "0/A=>0*A" => [ qr{ $ZERO / $A }x => sub { "$+{ZERO} * $+{A}" } ],

    # All operators in the other factor of multiplication by zero to addition.
    # E.g., ((1-2)/3)*0 => 0*((1+2)+3).
    #
    # A * 0 => 0 * addify(A) if A involves operators other than addition
    "A*0=>0*f(A)" => [
        qr{
            $A_TIMES_ZERO
            (?(?{ $+{A} =~ m{ (?! \+ ) $OPERATOR }x }) | (*FAIL) )
        }x
        => sub { "$+{ZERO} * " . addify($+{A}) }
    ],
    # (A * 0) * B => 0 * addify(A + B)
    "(A*0)*B=>0*f(A+B)" => [
        qr{
            \( $A_TIMES_ZERO \) \* $B
            |
            $B \* \( $A_TIMES_ZERO \)
        }x
        => sub { "$+{ZERO} * " . addify("($+{A} + $+{B})") }
    ],

    # Addition by non-literal zero to the factor of multiplication by zero.
    #
    # (A * 0) + 0' => 0 * addify(0' + A) if 0' != "0"
    "(A*0)+0'=>0*f(0'+A)" => [
        qr{
            \( $A_TIMES_ZERO \) \+ (?! 0 ) $ZERO2
            |
            (?! 0 ) $ZERO2 \+ \( $A_TIMES_ZERO \)
        }x
        => sub { "$+{ZERO} * " . addify("($+{ZERO2} + $+{A})") }
    ],

    # Multiplication by one to the factor of multiplication by zero.
    #
    # (A * 0) + (B * 1) => (0 * addify(1 + A)) + B
    "(A*0)+(B*1)=>(0*f(1+A))+B" => [
        qr{ \( $A_TIMES_ZERO \) \+ \( $B_TIMES_ONE \) }x
        => sub {
            "($+{ZERO} * "
            . addify("($+{ONE} + $+{A})")
            . ") + $+{B}"
        }
    ],

    # Multiplication by (1 * 1) to addition by (1 - 1).
    "(A*1)*1=>(A+1)-1" => [
        qr{
            \( $A_TIMES_ONE \) \* $ONE2
            |
            $ONE2 \* \( $A_TIMES_ONE \)
        }x
        => sub { "($+{A} + $+{ONE}) - $+{ONE2}" }
    ],

    # Multiplication by (X / X) to addition by (X - X).
    #
    # (A * X) / X => (A + X) - X
    # A * (X / X) => (A + X) - X
    "(A*X)/X=>(A+X)-X" => [
        qr{
            \( $A_TIMES_X \) / $X2
            |
            $A \* \( $X / $X2 \)
            |
            \( $X / $X2 \) \* $A
        }x
        => sub { "($+{A} + $+{X}) - $+{X2}" }
    ],
    # (A * (B * X)) / X => ((A * B) + X) - X
    "(A*(B*X))/X=>((A*B)+X)-X" => [
        qr{ \( $A \* \( $B_TIMES_X \) \) / $X2 }x
        => sub { "(($+{A} * $+{B}) + $+{X}) - $+{X2}" }
    ],
    # (A * X) / (B * X) => ((A / B) + X) - X
    # (A / X) * (X / B) => ((A / B) + X) - X
    "(A*X)/(B*X)=>((A/B)+X)-X" => [
        qr{
            \( $A_TIMES_X \) / \( $B_TIMES_X2 \)
            |
            \( $A / $X \) \* \( $X2 / $B \)
            |
            \( $X / $B \) \* \( $A / $X2 \)
        }x
        => sub { "(($+{A} / $+{B}) + $+{X}) - $+{X2}" }
    ],

    # Separation of addition by zero.
    #
    # (A + 0) . B => 0 + (A . B) if not (B = 0 and . = +)
    # A . (B + 0) => 0 + (A . B) if not (A = 0 and . = +)
    "(A+0).B=>0+(A.B)" => [
        qr{
            \( $A_PLUS_ZERO \)
            (?! \+ $ZERO_EXPR )
            $OP $B
            |
            (?! $ZERO_EXPR \+ )
            $A $OP
            \( $B_PLUS_ZERO \)
        }x
        => sub { "$+{ZERO} + ($+{A} $+{OP} $+{B})" }
    ],

    # Separation of multiplication by one.
    #
    # (A * 1) . B => 1 * (A . B)
    # if not ((B = 0 and . = +) or (B = 1 and . = *))
    #
    # A . (B * 1) => 1 * (A . B)
    # if not ((A = 0 and . = +) or (A = 1 and . = *))
    "(A*1).B=>1*(A.B)" => [
        qr{
            \( $A_TIMES_ONE \)
            (?! \+ $ZERO_EXPR | \* $ONE_EXPR )
            $OP $B
            |
            (?! $ZERO_EXPR \+ | $ONE_EXPR \* )
            $A $OP
            \( $B_TIMES_ONE \)
        }x
        => sub { "$+{ONE} * ($+{A} $+{OP} $+{B})" }
    ],

    # Commutativity.
    "BA=>AB" => [
        qr{
            $B (?<OP> [+*] ) $A
            (?(?{ compare($+{B}, $+{A}) == 1 }) | (*FAIL) )
        }x
        => sub { "$+{A} $+{OP} $+{B}" }
    ],
    "B(AC)=>A(BC)" => [
        qr{
            $B (?<OP> [+*] ) \( $A \g{OP} $C \)
            (?(?{ compare($+{B}, $+{A}) == 1 }) | (*FAIL) )
        }x
        => sub { "$+{A} $+{OP} ($+{B} $+{OP} $+{C})" }
    ],

    # Associativity.
    #
    # (A + B) + C => A + (B + C) if C != 0
    # (A * B) * C => A * (B * C) if C != 1
    "(AB)C=>A(BC)" => [
        qr{
            \( $A (?<OP> [+*] ) $B \)
            (?! \+ $ZERO_EXPR | \* $ONE_EXPR )
            \g{OP} $C
        }x
        => sub { "$+{A} $+{OP} ($+{B} $+{OP} $+{C})" }
    ],

    # Associativity of mixed addition and subtraction.
    #
    # A + (B - C) => (A + B) - C if A != 0
    # A - (C - B) => (A + B) - C
    "A+(B-C)=>(A+B)-C" => [
        qr{
            (?! $ZERO_EXPR ) $A \+ \( $B - $C \)
            |
            \( $B - $C \) \+ (?! $ZERO_EXPR ) $A
            |
            $A - \( $C - $B \)
        }x
        => sub { "($+{A} + $+{B}) - $+{C}" }
    ],
    # (A - B) - C => A - (B + C)
    "(A-B)-C=>A-(B+C)" => [
        qr{ \( $A - $B \) - $C }x => sub { "$+{A} - ($+{B} + $+{C})" }
    ],

    # Associativity of mixed multiplication and division.
    #
    # A * (B / C) => (A * B) / C if A != 1
    # A / (C / B) => (A * B) / C
    "A*(B/C)=>(A*B)/C" => [
        qr{
            (?! $ONE_EXPR ) $A \* \( $B / $C \)
            |
            \( $B / $C \) \* (?! $ONE_EXPR ) $A
            |
            $A / \( $C / $B \)
        }x
        => sub { "($+{A} * $+{B}) / $+{C}" }
    ],
    # (A / B) / C => A / (B * C)
    "(A/B)/C=>A/(B*C)" => [
        qr{ \( $A / $B \) / $C }x => sub { "$+{A} / ($+{B} * $+{C})" }
    ],

    # Separation of addition by (X - X) from the operands of multiplication and
    # division.
    #
    # ((A + X) - X) . B => ((A . B) + X) - X if B != 1
    # A . ((B + X) - X) => ((A . B) + X) - X if A != 1
    # if . = * or /
    "((A+X)-X).B=>((A.B)+X)-X" => [
        qr{
            \( \( $A_PLUS_X \) - $X2 \)
            (?<OP> [*/] )
            (?! $ONE_EXPR ) $B
            |
            (?! $ONE_EXPR ) $A
            (?<OP> [*/] )
            \( \( $B_PLUS_X \) - $X2 \)
        }x
        => sub { "(($+{A} $+{OP} $+{B}) + $+{X}) - $+{X2}" }
    ],

    # Negative addition to subtraction.
    #
    # A + B => B - negate(A) if A < 0
    "A+B=>B-(-A)" => [
        qr{
            $A \+ $B
            (?(?{ eval($+{A}) < 0 }) | (*FAIL) )
        }x
        => sub { "$+{B} - " . negate($+{A}) }
    ],
    # A + B => A - negate(B) if B < 0
    "A+B=>A-(-B)" => [
        qr{
            $A \+ $B
            (?(?{ eval($+{B}) < 0 }) | (*FAIL) )
        }x
        => sub { "$+{A} - " . negate($+{B}) }
    ],

    # Negative subtraction to addition.
    #
    # A - B => A + negate(B) if B < 0
    "A-B=>A+(-B)" => [
        qr{
            $A - $B
            (?(?{ eval($+{B}) < 0 }) | (*FAIL) )
        }x
        => sub { "$+{A} + " . negate($+{B}) }
    ],

    # Negative multiplication and division to positive.
    #
    # A * B => negate(A) * negate(B)
    # A / B => negate(A) / negate(B)
    # if (A <= 0 and B < 0) or (A < 0 and B <= 0)
    "AB=>(-A)(-B)" => [
        qr{
            $A (?<OP> [*/] ) $B
            (?(?{
                my $a = eval $+{A};
                my $b = eval $+{B};
                ($a <= 0 and $b < 0) or ($a < 0 and $b <= 0)
            }) | (*FAIL) )
        }x
        => sub { negate($+{A}) . $+{OP} . negate($+{B}) }
    ],
);

#
# Subroutines.
#

# Compares the order of operands for commutativity.
sub compare {
    my $a = shift;
    my $b = shift;

    # Removes the leading open parentheses.
    $a =~ s/^\(+//;
    $b =~ s/^\(+//;

    return $a cmp $b;
}

# Replaces all operators in an expression with addition.
sub addify {
    my $expr = shift;
    $expr =~ s{ (?! \+ ) $OPERATOR }{+}gx;
    return $expr;
}

# Negates a negative expression.  The result does not contain a unary minus.
sub negate {
    my $expr = shift;
    $expr =~ s/\s+//g;
    $expr =~ m{ $A $OP $B }x or return $expr;
    my ($a, $op, $b) = ($+{A}, $+{OP}, $+{B});

    # a - b => (b - a)
    if ($op eq "-") {
        return "($b $op $a)";
    }

    # a + b =>
    #     ((-a) - b) if a < 0
    #     ((-b) - a) if b < 0
    if ($op eq "+") {
        return eval($a) < 0
            ? "(" . negate($a) . "- $b)"
            : "(" . negate($b) . "- $a)";
    }

    # a * b =>
    #     ((-a) * b) if a < 0
    #     (a * (-b)) if b < 0
    #
    # a / b =>
    #     ((-a) / b) if a < 0
    #     (a / (-b)) if b < 0
    return eval($a) < 0
        ? "(" . negate($a) . "$op $b)"
        : "($a $op" . negate($b) . ")";
}

# Normalizes an expression.
sub normalize {
    my $expr  = shift;
    my $depth = shift // 0;
    if ($depth == 0) {
        $expr =~ s/\s+//g;
    }
    elsif ($VERBOSE) {
        warn "depth $depth\n";
    }

    my $should_recurse;

    for (my $i = 0; $i < @rules; $i += 2) {
        my ($pattern, $replace) = @{$rules[$i + 1]};

        while ($expr =~ s/$pattern/$replace->()/eg) {
            $expr =~ s/\s+//g;
            if ($VERBOSE) { warn "=> $expr\t$rules[$i]\n" }
            $should_recurse ||= 1;
        }
    }

    if ($should_recurse) {
        return normalize($expr, $depth + 1);
    }
    return $expr;
}

if (not caller) {
    $VERBOSE = 1;
    while (my $expr = <>) {
        chomp $expr;
        warn "$expr\n";
        say normalize($expr);
        warn "\n";
    }
}

1;
