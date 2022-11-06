use strict;
use warnings;

my $VERBOSE = 0;

my $NUMBER = qr{ (?!0\d) \d+ }x;
my $OPERATOR = qr{ [+\-*/] }x;
my $OPERAND = qr{ ( $NUMBER | \( (?-1) $OPERATOR (?-1) \) ) }x;

my $OP = qr{ (?<OP> $OPERATOR ) }x;

my $A = qr{ (?<A> $OPERAND ) }x;
my $B = qr{ (?<B> $OPERAND ) }x;
my $C = qr{ (?<C> $OPERAND ) }x;
my $X = qr{ (?<X> $OPERAND ) }x;
my $X2 = qr{
    (?<X2> $OPERAND ) (?(?{ eval("$+{X2} - $+{X}") eq '0' }) | (*FAIL) )
}x;

my $ZERO = qr{
    (?<ZERO> $OPERAND ) (?(?{ eval($+{ZERO}) eq '0' }) | (*FAIL) )
}x;
my $ZERO2 = qr{
    (?<ZERO2> $OPERAND ) (?(?{ eval($+{ZERO2}) eq '0' }) | (*FAIL) )
}x;
my $ONE = qr{ (?<ONE> $OPERAND ) (?(?{ eval($+{ONE}) eq '1' }) | (*FAIL) ) }x;

my $A_PLUS_ZERO = qr{ $A \+ $ZERO | $ZERO \+ $A }x;
my $B_PLUS_ZERO = qr{ $B \+ $ZERO | $ZERO \+ $B }x;

my $A_TIMES_ONE = qr{ $A \* $ONE | $ONE \* $A }x;
my $B_TIMES_ONE = qr{ $B \* $ONE | $ONE \* $B }x;

my $A_TIMES_ZERO = qr{ $A \* $ZERO | $ZERO \* $A }x;
my $B_TIMES_ZERO = qr{ $B \* $ZERO | $ZERO \* $B }x;

my $A_PLUS_X = qr{ $A \+ $X | $X \+ $A }x;
my $B_PLUS_X = qr{ $B \+ $X | $X \+ $B }x;
my $B_PLUS_X2 = qr{ $B \+ $X2 | $X2 \+ $B }x;

my $A_TIMES_X = qr{ $A \* $X | $X \* $A }x;
my $B_TIMES_X = qr{ $B \* $X | $X \* $B }x;
my $B_TIMES_X2 = qr{ $B \* $X2 | $X2 \* $B }x;

# Defines the order of operands for commutativity.
sub compare {
    my ($a, $b) = @_;

    # Removes the leading open parentheses.
    $a =~ s{ \A \(+ }{}x;
    $b =~ s{ \A \(+ }{}x;

    return $a cmp $b;
}

# Converts all operators in an expression to addition.
sub convert_to_addition {
    my ($expression) = @_;
    $expression =~ s{ (?!\+) $OPERATOR }{+}gx;
    return $expression;
}

# Takes a negative expression and returns its additive inverse in a form that
# does not have a unary minus operator.
sub negate {
    my ($expression) = @_;
    $expression =~ m{ $A $OP $B }x or return $expression;
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
    # B + (A + C) => A + (B + C)
    # B * (A * C) => A * (B * C)
    # if B > A lexicographically
    'B(AC)=>A(BC)' => [
        qr{
            $B (?<OP> [+*] ) \( $A \g{OP} $C \)
            (?(?{ compare($+{B}, $+{A}) == 1 }) | (*FAIL) )
        }x
        => sub { $+{A} . $+{OP} . '(' . $+{B} . $+{OP} . $+{C} . ')' }
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
    # (A + 0) . B => 0 + (A . B)
    # A . (B + 0) => 0 + (A . B) if not (A == 0 and . == +)
    '(A+0).B|A.(B+0)=>0+(A.B)' => [
        qr{
            \( $A_PLUS_ZERO \) $OP $B
            |
            $A $OP \( $B_PLUS_ZERO \)
            (?(?{ not (eval($+{A}) eq '0' and $+{OP} eq '+') }) | (*FAIL) )
        }x
        => sub { $+{ZERO} . '+' . '(' . $+{A} . $+{OP} . $+{B} . ')' }
    ],

    # Separation of addition by (X - X)
    # (A + X) - X => (X - X) + A
    '(A+X)-X=>(X-X)+A' => [
        qr{ \( $A_PLUS_X \) - $X2 }x
        => sub { '(' . $+{X} . '-' . $+{X2} . ')' . '+' . $+{A} }
    ],
    # (A + (B + X)) - X => (X - X) + (A + B)
    '(A+(B+X))-X=>(X-X)+(A+B)' => [
        qr{ \( $A \+ \( $B_PLUS_X \) \) - $X2 }x
        => sub {
            '(' . $+{X} . '-' . $+{X2} . ')'
            . '+'
            . '(' . $+{A} . '+' . $+{B} . ')'
        }
    ],
    # (A + X) - (B + X) => (X - X) + (A - B)
    '(A+X)-(B+X)=>(X-X)+(A-B)' => [
        qr{ \( $A_PLUS_X \) - \( $B_PLUS_X2 \) }x
        => sub {
            '(' . $+{X} . '-' . $+{X2} . ')'
            . '+'
            . '(' . $+{A} . '-' . $+{B} . ')'
        }
    ],

    # Multiplication by (X / X) to addition by (X - X)
    # (A * X) / X => (X - X) + A
    '(A*X)/X=>(X-X)+A' => [
        qr{ \( $A_TIMES_X \) / $X2 }x
        => sub { '(' . $+{X} . '-' . $+{X2} . ')' . '+' . $+{A} }
    ],
    # (A * (B * X)) / X => (X - X) + (A * B)
    '(A*(B*X))/X=>(X-X)+(A*B)' => [
        qr{ \( $A \* \( $B_TIMES_X \) \) / $X2 }x
        => sub {
            '(' . $+{X} . '-' . $+{X2} . ')'
            . '+'
            . '(' . $+{A} . '*' . $+{B} . ')'
        }
    ],
    # (A * X) / (B * X) => (X - X) + (A / B)
    '(A*X)/(B*X)=>(X-X)+(A/B)' => [
        qr{ \( $A_TIMES_X \) / \( $B_TIMES_X2 \) }x
        => sub {
            '(' . $+{X} . '-' . $+{X2} . ')'
            . '+'
            . '(' . $+{A} . '/' . $+{B} . ')'
        }
    ],

    # Multiplication by one
    # ---------------------

    # Division by one to multiplication
    # A / 1 => A * 1
    'A/1=>A*1' => [ qr{ $A / $ONE }x => sub { $+{A} . '*' . $+{ONE} } ],

    # Separation of multiplication by one
    # (A * 1) . B => 1 * (A . B)
    # if not (B == 0 and . == +) and not (B == 1 and . == *)
    #
    # A . (B * 1) => 1 * (A . B)
    # if not (A == 0 and . == +) and not (A == 1 and . == *)
    '(A*1).B|A.(B*1)=>1*(A.B)' => [
        qr{
            \( $A_TIMES_ONE \) $OP $B
            (?(?{
                not (eval($+{B}) eq '0' and $+{OP} eq '+')
                and not (eval($+{B}) eq '1' and $+{OP} eq '*')
            }) | (*FAIL) )
            |
            $A $OP \( $B_TIMES_ONE \)
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

    # All operators in the factor of multiplication by zero to addition
    # A * 0 => 0 * convert_to_addition(A)
    # if A has an operator other than addition
    'A*0=>0*f(A)' => [
        qr{
            $A_TIMES_ZERO (?(?{ $+{A} =~ m{ (?!\+) $OPERATOR }x }) | (*FAIL) )
        }x
        => sub { $+{ZERO} . '*' . convert_to_addition($+{A}) }
    ],
    # (A * 0) * B => 0 * convert_to_addition(A + B)
    # A * (B * 0) => 0 * convert_to_addition(A + B)
    '(A*0)*B|A*(B*0)=>0*f(A+B)' => [
        qr{ \( $A_TIMES_ZERO \) \* $B | $A \* \( $B_TIMES_ZERO \) }x
        => sub {
            $+{ZERO}
            . '*'
            . convert_to_addition('(' . $+{A} . '+' . $+{B} . ')')
        }
    ],

    # Addition by zero to a factor of multiplication by zero
    # (A * 0) + 0' => 0 * convert_to_addition(0' + A) if 0' != "0"
    "(A*0)+0'=>0*f(0'+A)" => [
        qr{
            (?: \( $A_TIMES_ZERO \) \+ $ZERO2 | $ZERO2 \+ \( $A_TIMES_ZERO \) )
            (?(?{ $+{ZERO2} ne '0' }) | (*FAIL) )
        }x
        => sub {
            $+{ZERO}
            . '*'
            . convert_to_addition('(' . $+{ZERO2} . '+' . $+{A} . ')')
        }
    ],

    # Multiplication by one to a factor of multiplication by zero
    # (A * 0) + (B * 1) => (0 * convert_to_addition(1 + A)) + B
    "(A*0)+(B*1)=>(0*f(1+A))+B" => [
        qr{ \( $A_TIMES_ZERO \) \+ \( $B_TIMES_ONE \) }x
        => sub {
            '('
            . $+{ZERO}
            . '*'
            . convert_to_addition('(' . $+{ONE} . '+' . $+{A} . ')')
            . ')'
            . '+'
            . $+{B}
        }
    ],

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
    # if (A <= 0 and B < 0) or (A < 0 and B <= 0)
    'AB=>(-A)(-B)' => [
        qr{
            $A (?<OP> [*/] ) $B
            (?(?{
                my $a = eval($+{A});
                my $b = eval($+{B});
                ($a <= 0 and $b < 0) or ($a < 0 and $b <= 0)
            }) | (*FAIL) )
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
        '(9+9)-(9+9)',
    );
    for my $expression (@expressions) {
        $VERBOSE and warn("$expression\n");
        print(normalize($expression), "\n");
        $VERBOSE and warn("\n");
    }
}

1;
