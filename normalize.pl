use strict;
use warnings;

my $VERBOSE = 0;

my $NUMBER = qr{ (?!0\d) \d+ }x;
my $OPERATOR = qr{ [+\-*/] }x;
my $OPERAND = qr{
    (
        $NUMBER
        |
        \( (?-1) $OPERATOR (?-1) \)
    )
}x;

my $OP = qr{ (?<OP> $OPERATOR ) }x;

my $A = qr{ (?<A> $OPERAND ) }x;
my $B = qr{ (?<B> $OPERAND ) }x;
my $B2 = qr{
    (?<B2> $OPERAND ) (?(?{ eval("$+{B2} - $+{B}") eq '0' }) | (*FAIL) )
}x;
my $C = qr{ (?<C> $OPERAND ) }x;
my $C2 = qr{
    (?<C2> $OPERAND ) (?(?{ eval("$+{C2} - $+{C}") eq '0' }) | (*FAIL) )
}x;

my $ZERO = qr{
    (?<ZERO> $OPERAND ) (?(?{ eval($+{ZERO}) eq '0' }) | (*FAIL) )
}x;
my $ZERO2 = qr{
    (?<ZERO2> $OPERAND ) (?(?{ eval($+{ZERO2}) eq '0' }) | (*FAIL) )
}x;
my $ONE = qr{ (?<ONE> $OPERAND ) (?(?{ eval($+{ONE}) eq '1' }) | (*FAIL) ) }x;

sub normalize_for_comparison {
    my ($expression) = @_;

    # Removes the leading open parentheses.
    $expression =~ s{ \A \(+ }{}x;

    return $expression;
}

# Defines the order of operands for commutativity.
sub compare {
    my ($a, $b) = @_;
    $a = normalize_for_comparison($a);
    $b = normalize_for_comparison($b);
    return $a cmp $b;
}

# Takes a negative expression and returns its additive inverse in a form that
# does not have a unary minus operator.
sub negate {
    my ($expression) = @_;
    $expression =~ m{ \A \( $A $OP $B \) \z }x
        or die "Not an expression: $expression";
    my ($a, $op, $b) = @+{'A', 'OP', 'B'};

    # -(a - b) => (b - a)
    if ($op eq '-') {
        return '(' . $b . $op . $a . ')';
    }

    # -(a + b) => ((-a) - b) if a < 0
    #             ((-b) - a) if b < 0
    if ($op eq '+') {
        return eval($a) < 0
            ? ('(' . negate($a) . '-' . $b . ')')
            : ('(' . negate($b) . '-' . $a . ')');
    }

    # -(a * b) => ((-a) * b) if a < 0
    #             (a * (-b)) if b < 0
    #
    # -(a / b) => ((-a) / b) if a < 0
    #             (a / (-b)) if b < 0
    return eval($a) < 0
        ? ('(' . negate($a) . $op . $b . ')')
        : ('(' . $a . $op . negate($b) . ')');
}

my @rewrite_rules = (
    # General
    # -------

    # Commutativity
    # B + A => A + B
    # B * A => A * B
    #
    # B - A => A - B
    # B / A => A / B
    # if B == A
    #
    # if B > A lexicographically
    'BA=>AB' => [
        qr{
            (?:
                $B (?<OP> [+*] ) $A
                |
                $B (?<OP> [-/] ) $A
                (?(?{ eval("$+{B} - $+{A}") eq '0' }) | (*FAIL) )
            )
            (?(?{ compare($+{B}, $+{A}) == 1 }) | (*FAIL) )
        }x
        => sub { $+{A} . $+{OP} . $+{B} }
    ],

    # Associativity
    # (A + B) + C => A + (B + C) if C != 0
    # (A * B) * C => A * (B * C) if C != 1
    '(AB)C=>A(BC)' => [
        qr{
            \( $A (?<OP> [+*] ) $B \) \g{OP} $C
            (?(?{ eval($+{C}) ne ($+{OP} eq '+' ? '0' : '1') }) | (*FAIL) )
        }x
        => sub { $+{A} . $+{OP} . '(' . $+{B} . $+{OP} . $+{C} . ')' }
    ],

    # Mixed addition and subtraction
    # A + (B - C) => (A + B) - C if A != 0 and B != C
    # (A - C) + B => (A + B) - C if B != 0 and A != C
    # A - (C - B) => (A + B) - C
    'A+(B-C)|(A-C)+B|A-(C-B)=>(A+B)-C' => [
        qr{
            $A \+ \( $B - $C \)
            (?(?{
                eval($+{A}) ne '0' and eval("$+{B} - $+{C}") ne '0'
            }) | (*FAIL) )
            |
            \( $A - $C \) \+ $B
            (?(?{
                eval($+{B}) ne '0' and eval("$+{A} - $+{C}") ne '0'
            }) | (*FAIL) )
            |
            $A - \( $C - $B \)
        }x
        => sub { '(' . $+{A} . '+' . $+{B} . ')' . '-' . $+{C} }
    ],
    # (A - B) - C => A - (B + C)
    '(A-B)-C=>A-(B+C)' => [
        qr{ \( $A - $B \) - $C }x
        => sub { $+{A} . '-' . '(' . $+{B} . '+' . $+{C} . ')' }
    ],

    # Mixed multiplication and division
    # A * (B / C) => (A * B) / C if A != 1
    # (A / C) * B => (A * B) / C if B != 1
    # A / (C / B) => (A * B) / C
    'A*(B/C)|(A/C)*B|A/(C/B)=>(A*B)/C' => [
        qr{
            $A \* \( $B / $C \) (?(?{ eval($+{A}) ne '1' }) | (*FAIL) )
            |
            \( $A / $C \) \* $B (?(?{ eval($+{B}) ne '1' }) | (*FAIL) )
            |
            $A / \( $C / $B \)
        }x
        => sub { '(' . $+{A} . '*' . $+{B} . ')' . '/' . $+{C} }
    ],
    # (A / B) / C => A / (B * C)
    '(A/B)/C=>A/(B*C)' => [
        qr{ \( $A / $B \) / $C }x
        => sub { $+{A} . '/' . '(' . $+{B} . '*' . $+{C} . ')' }
    ],

    # Addition by zero
    # ----------------

    # Subtraction by zero to addition
    # A - 0 => A + 0
    'A-0=>A+0' => [ qr{ $A - $ZERO }x => sub { $+{A} . '+' . $+{ZERO} } ],

    # Zero times zero to addition
    # 0 * 0 => 0 + 0
    '0*0=>0+0' => [
        qr{ $ZERO \* $ZERO2 }x => sub { $+{ZERO} . '+' . $+{ZERO2} }
    ],

    # Separation of addition by zero
    # (0 + A) . B => 0 + (A . B)
    # (A + 0) . B => 0 + (A . B)
    #
    # A . (0 + B) => 0 + (A . B)
    # A . (B + 0) => 0 + (A . B)
    # if not (A == 0 and . == +)
    '(0+A).B|(A+0).B|A.(0+B)|A.(B+0)=>0+(A.B)' => [
        qr{
            \( $ZERO \+ $A \) $OP $B
            |
            \( $A \+ $ZERO \) $OP $B
            |
            (?:
                $A $OP \( $ZERO \+ $B \)
                |
                $A $OP \( $B \+ $ZERO \)
            )
            (?(?{ not (eval($+{A}) eq '0' and $+{OP} eq '+') }) | (*FAIL) )
        }x
        => sub { $+{ZERO} . '+' . '(' . $+{A} . $+{OP} . $+{B} . ')' }
    ],

    # Separation of addition by (X - X)
    # (A + B) - B => A + (B - B)
    # (B + A) - B => A + (B - B)
    '(A+B)-B|(B+A)-B=>A+(B-B)' => [
        qr{
            \( $A \+ $B \) - $B2
            |
            \( $B \+ $A \) - $B2
        }x
        => sub { $+{A} . '+' . '(' . $+{B} . '-' . $+{B2} . ')' }
    ],
    # (A + (B + C)) - C => (A + B) + (C - C)
    # (A + (C + B)) - C => (A + B) + (C - C)
    '(A+(B+C))-C|(A+(C+B))-C=>(A+B)+(C-C)' => [
        qr{
            \( $A \+ \( $B \+ $C \) \) - $C2
            |
            \( $A \+ \( $C \+ $B \) \) - $C2
        }x
        => sub {
            '(' . $+{A} . '+' . $+{B} . ')'
            . '+'
            . '(' . $+{C} . '-' . $+{C2} . ')'
        }
    ],

    # Multiplication by (X / X) to addition by (X - X)
    # (A * B) / B => A + (B - B)
    # (B * A) / B => A + (B - B)
    '(A*B)/B|(B*A)/B=>A+(B-B)' => [
        qr{
            \( $A \* $B \) / $B2
            |
            \( $B \* $A \) / $B2
        }x
        => sub { $+{A} . '+' . '(' . $+{B} . '-' . $+{B2} . ')' }
    ],
    # (A * (B * C)) / C => (A * B) + (C - C)
    # (A * (C * B)) / C => (A * B) + (C - C)
    '(A*(B*C))/C|(A*(C*B))/C=>(A*B)+(C-C)' => [
        qr{
            \( $A \* \( $B \* $C \) \) / $C2
            |
            \( $A \* \( $C \* $B \) \) / $C2
        }x
        => sub {
            '(' . $+{A} . '*' . $+{B} . ')'
            . '+'
            . '(' . $+{C} . '-' . $+{C2} . ')'
        }
    ],

    # Multiplication by one
    # ---------------------

    # Division by one to multiplication
    # A / 1 => A * 1
    'A/1=>A*1' => [ qr{ $A / $ONE }x => sub { $+{A} . '*' . $+{ONE} } ],

    # Separation of multiplication by one
    # (1 * A) . B => 1 * (A . B)
    # (A * 1) . B => 1 * (A . B)
    # if not (B == 0 and . == +) and not (B == 1 and . == *)
    #
    # A . (1 * B) => 1 * (A . B)
    # A . (B * 1) => 1 * (A . B)
    # if not (A == 0 and . == +) and not (A == 1 and . == *)
    '(1*A).B|(A*1).B|A.(1*B)|A.(B*1)=>1*(A.B)' => [
        qr{
            (?:
                \( $ONE \* $A \) $OP $B
                |
                \( $A \* $ONE \) $OP $B
            )
            (?(?{
                not (eval($+{B}) eq '0' and $+{OP} eq '+')
                and not (eval($+{B}) eq '1' and $+{OP} eq '*')
            }) | (*FAIL) )
            |
            (?:
                $A $OP \( $ONE \* $B \)
                |
                $A $OP \( $B \* $ONE \)
            )
            (?(?{
                not (eval($+{A}) eq '0' and $+{OP} eq '+')
                and not (eval($+{A}) eq '1' and $+{OP} eq '*')
            }) | (*FAIL) )
        }x
        => sub { $+{ONE} . '*' . '(' . $+{A} . $+{OP} . $+{B} . ')' }
    ],

    # Multiplication by zero
    # ----------------------

    # Division where the dividend is zero to multiplication
    # 0 / A => 0 * A
    '0/A=>0*A' => [ qr{ $ZERO / $A }x => sub { $+{ZERO} . '*' . $+{A} } ],

    # Sign
    # ----

    # Negative addition to subtraction
    # A + B => B - (-A) if A < 0
    'A+B=>B-(-A)' => [
        qr{ $A \+ $B (?(?{ eval($+{A}) < 0 }) | (*FAIL) ) }x
        => sub { $+{B} . '-' . negate($+{A}) }
    ],
    # A + B => A - (-B) if B < 0
    'A+B=>A-(-B)' => [
        qr{ $A \+ $B (?(?{ eval($+{B}) < 0 }) | (*FAIL) ) }x
        => sub { $+{A} . '-' . negate($+{B}) }
    ],

    # Negative subtraction to addition
    # A - B => A + (-B) if B < 0
    'A-B=>A+(-B)' => [
        qr{ $A - $B (?(?{ eval($+{B}) < 0 }) | (*FAIL) ) }x
        => sub { $+{A} . '+' . negate($+{B}) }
    ],

    # Negative multiplication and division to positive
    # A * B => (-A) * (-B)
    # A / B => (-A) / (-B)
    # if A < 0 and B < 0
    'AB=>(-A)(-B)' => [
        qr{
            $A (?<OP> [*/] ) $B
            (?(?{ eval($+{A}) < 0 and eval($+{B}) < 0 }) | (*FAIL) )
        }x
        => sub { negate($+{A}) . $+{OP} . negate($+{B}) }
    ],
);

sub warn_step {
    my ($expression, $rule_name) = @_;
    $expression =~ s{$OP}{ $+{OP} }g;
    warn("=> $expression\t$rule_name\n");
}

sub normalize {
    my ($expression, $depth) = @_;
    $depth ||= 0;

    if ($depth == 0) {
        # Removes whitespace.
        $expression =~ s{ \s+ }{}gx;
    }
    else {
        $VERBOSE and warn("depth $depth\n");
    }

    my $should_recur;

    for (my $i = 0; $i < @rewrite_rules; $i += 2) {
        my ($rule_name, $pair) = @rewrite_rules[$i, $i + 1];
        my ($pattern, $replacement) = @$pair;

        while ($expression =~ s{$pattern}{$replacement->()}eg) {
            $VERBOSE and warn_step($expression, $rule_name);
            $should_recur = 1;
        }
    }

    if ($should_recur) {
        return normalize($expression, $depth + 1);
    }
    return $expression;
}

if (not caller()) {
    $VERBOSE = 1;

    my @expressions = (
        '((0 + 0) * 1) * 1',
    );
    for my $expression (@expressions) {
        $VERBOSE and warn("$expression\n");
        print(normalize($expression), "\n");
        $VERBOSE and warn("\n");
    }
}

1;
