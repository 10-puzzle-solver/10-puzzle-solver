use strict;
use warnings;

my $VERBOSE = 0;

my $NUMBER = qr{ (?! 0 \d ) \d+ }x;
my $OPERATOR = qr{ [+\-*/] }x;
my $EXPRESSION = qr{ ( $NUMBER | \( (?-1) $OPERATOR (?-1) \) ) }x;

my $ZERO_EXPRESSION = qr{
    ( $EXPRESSION ) (?(?{ eval($^N) eq '0' }) | (*FAIL) )
}x;
my $ONE_EXPRESSION = qr{
    ( $EXPRESSION ) (?(?{ eval($^N) eq '1' }) | (*FAIL) )
}x;

my $OP = qr{ (?<OP> $OPERATOR ) }x;

my $A = qr{ (?<A> $EXPRESSION ) }x;
my $B = qr{ (?<B> $EXPRESSION ) }x;
my $C = qr{ (?<C> $EXPRESSION ) }x;
my $X = qr{ (?<X> $EXPRESSION ) }x;
my $X2 = qr{
    (?<X2> $EXPRESSION ) (?(?{ eval("($+{X2}) - ($+{X})") eq '0' }) | (*FAIL) )
}x;

my $ZERO = qr{ (?<ZERO> $ZERO_EXPRESSION ) }x;
my $ZERO2 = qr{ (?<ZERO2> $ZERO_EXPRESSION ) }x;

my $ONE = qr{ (?<ONE> $ONE_EXPRESSION ) }x;
my $ONE2 = qr{ (?<ONE2> $ONE_EXPRESSION ) }x;

my $A_PLUS_ZERO = qr{ $A \+ $ZERO | $ZERO \+ $A }x;
my $B_PLUS_ZERO = qr{ $B \+ $ZERO | $ZERO \+ $B }x;

my $A_TIMES_ONE = qr{ $A \* $ONE | $ONE \* $A }x;
my $B_TIMES_ONE = qr{ $B \* $ONE | $ONE \* $B }x;

my $A_TIMES_ZERO = qr{ $A \* $ZERO | $ZERO \* $A }x;
my $B_TIMES_ZERO = qr{ $B \* $ZERO | $ZERO \* $B }x;

my $A_PLUS_X = qr{ $A \+ $X | $X \+ $A }x;
my $B_PLUS_X = qr{ $B \+ $X | $X \+ $B }x;

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

# Replaces all operators in the expression with addition.
sub convert_to_addition {
    my ($expression) = @_;
    $expression =~ s{ (?! \+ ) $OPERATOR }{+}gx;
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
    # Subtraction by zero to addition
    # A - 0 => A + 0
    'A-0=>A+0' => [ qr{ $A - $ZERO }x => sub { $+{A} . '+' . $+{ZERO} } ],

    # Multiplication of zeros to addition
    # 0 * 0 => 0 + 0
    '0*0=>0+0' => [
        qr{ $ZERO \* $ZERO2 }x => sub { $+{ZERO} . '+' . $+{ZERO2} }
    ],

    # Division by one to multiplication
    # A / 1 => A * 1
    'A/1=>A*1' => [ qr{ $A / $ONE }x => sub { $+{A} . '*' . $+{ONE} } ],

    # Division where the dividend is zero to multiplication
    # 0 / A => 0 * A
    '0/A=>0*A' => [ qr{ $ZERO / $A }x => sub { $+{ZERO} . '*' . $+{A} } ],

    # All operators in the factor of multiplication by zero to addition
    # A * 0 => 0 * convert_to_addition(A)
    # if A has an operator other than addition
    'A*0=>0*f(A)' => [
        qr{
            $A_TIMES_ZERO
            (?(?{ $+{A} =~ m{ (?! \+ ) $OPERATOR }x }) | (*FAIL) )
        }x
        => sub { $+{ZERO} . '*' . convert_to_addition($+{A}) }
    ],
    # (A * 0) * B => 0 * convert_to_addition(A + B)
    '(A*0)*B=>0*f(A+B)' => [
        qr{ \( $A_TIMES_ZERO \) \* $B | $B \* \( $A_TIMES_ZERO \) }x
        => sub {
            $+{ZERO}
            . '*'
            . convert_to_addition('(' . $+{A} . '+' . $+{B} . ')')
        }
    ],

    # Addition by zero to the factor of multiplication by zero
    # (A * 0) + 0' => 0 * convert_to_addition(0' + A) if 0' != "0"
    "(A*0)+0'=>0*f(0'+A)" => [
        qr{
            \( $A_TIMES_ZERO \) \+ (?! 0 ) $ZERO2
            |
            (?! 0 ) $ZERO2 \+ \( $A_TIMES_ZERO \)
        }x
        => sub {
            $+{ZERO}
            . '*'
            . convert_to_addition('(' . $+{ZERO2} . '+' . $+{A} . ')')
        }
    ],

    # Multiplication by one to the factor of multiplication by zero
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

    # Multiplication by (1 * 1) to addition by (1 - 1)
    # (A * 1) * 1 => (A + 1) - 1
    '(A*1)*1=>(A+1)-1' => [
        qr{ \( $A_TIMES_ONE \) \* $ONE2 | $ONE2 \* \( $A_TIMES_ONE \) }x
        => sub { '(' . $+{A} . '+' . $+{ONE} . ')' . '-' . $+{ONE2} }
    ],

    # Multiplication by (X / X) to addition by (X - X)
    # (A * X) / X => (A + X) - X
    # A * (X / X) => (A + X) - X
    '(A*X)/X|A*(X/X)=>(A+X)-X' => [
        qr{
            \( $A_TIMES_X \) / $X2
            |
            $A \* \( $X / $X2 \) | \( $X / $X2 \) \* $A
        }x
        => sub { '(' . $+{A} . '+' . $+{X} . ')' . '-' . $+{X2} }
    ],
    # (A * (B * X)) / X => ((A * B) + X) - X
    '(A*(B*X))/X=>((A*B)+X)-X' => [
        qr{ \( $A \* \( $B_TIMES_X \) \) / $X2 }x
        => sub {
            '(' . '(' . $+{A} . '*' . $+{B} . ')' . '+' . $+{X} . ')'
            . '-'
            . $+{X2}
        }
    ],
    # (A * X) / (B * X) => ((A / B) + X) - X
    # (A / X) * (X / B) => ((A / B) + X) - X
    '(A*X)/(B*X)|(A/X)*(X/B)=>((A/B)+X)-X' => [
        qr{
            \( $A_TIMES_X \) / \( $B_TIMES_X2 \)
            |
            \( $A / $X \) \* \( $X2 / $B \) | \( $X / $B \) \* \( $A / $X2 \)
        }x
        => sub {
            '(' . '(' . $+{A} . '/' . $+{B} . ')' . '+' . $+{X} . ')'
            . '-'
            . $+{X2}
        }
    ],

    # Separation of addition by zero
    # (A + 0) . B => 0 + (A . B) if not (B == 0 and . == +)
    # A . (B + 0) => 0 + (A . B) if not (A == 0 and . == +)
    '(A+0).B|A.(B+0)=>0+(A.B)' => [
        qr{
            \( $A_PLUS_ZERO \) (?! \+ $ZERO_EXPRESSION ) $OP $B
            |
            (?! $ZERO_EXPRESSION \+ ) $A $OP \( $B_PLUS_ZERO \)
        }x
        => sub { $+{ZERO} . '+' . '(' . $+{A} . $+{OP} . $+{B} . ')' }
    ],

    # Separation of multiplication by one
    # (A * 1) . B => 1 * (A . B)
    # if not (B == 0 and . == +) and not (B == 1 and . == *)
    #
    # A . (B * 1) => 1 * (A . B)
    # if not (A == 0 and . == +) and not (A == 1 and . == *)
    '(A*1).B|A.(B*1)=>1*(A.B)' => [
        qr{
            \( $A_TIMES_ONE \)
            (?! \+ $ZERO_EXPRESSION | \* $ONE_EXPRESSION ) $OP $B
            |
            (?! $ZERO_EXPRESSION \+ | $ONE_EXPRESSION \* ) $A $OP
            \( $B_TIMES_ONE \)
        }x
        => sub { $+{ONE} . '*' . '(' . $+{A} . $+{OP} . $+{B} . ')' }
    ],

    # Commutativity
    # B + A => A + B
    # B * A => A * B
    # if B > A lexicographically
    'BA=>AB' => [
        qr{
            $B (?<OP> [+*] ) $A (?(?{ compare($+{B}, $+{A}) == 1 }) | (*FAIL) )
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

    # Associativity of addition and multiplication
    # (A + B) + C => A + (B + C) if C != 0
    # (A * B) * C => A * (B * C) if C != 1
    '(AB)C=>A(BC)' => [
        qr{
            \( $A (?<OP> [+*] ) $B \)
            (?! \+ $ZERO_EXPRESSION | \* $ONE_EXPRESSION ) \g{OP} $C
        }x
        => sub { $+{A} . $+{OP} . '(' . $+{B} . $+{OP} . $+{C} . ')' }
    ],

    # Associativity of mixed addition and subtraction
    # A + (B - C) => (A + B) - C if A != 0
    # A - (C - B) => (A + B) - C
    'A+(B-C)|A-(C-B)=>(A+B)-C' => [
        qr{
            (?! $ZERO_EXPRESSION ) $A \+ \( $B - $C \)
            |
            \( $B - $C \) \+ (?! $ZERO_EXPRESSION ) $A
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

    # Associativity of mixed multiplication and division
    # A * (B / C) => (A * B) / C if A != 1
    # A / (C / B) => (A * B) / C
    'A*(B/C)|A/(C/B)=>(A*B)/C' => [
        qr{
            (?! $ONE_EXPRESSION ) $A \* \( $B / $C \)
            |
            \( $B / $C \) \* (?! $ONE_EXPRESSION ) $A
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

    # Separation of addition by (X - X) from the operands of multiplication and
    # division
    # ((A + X) - X) . B => ((A . B) + X) - X if B != 1
    # A . ((B + X) - X) => ((A . B) + X) - X if A != 1
    # if . == * or /
    '((A+X)-X).B|A.((B+X)-X)=>((A.B)+X)-X' => [
        qr{
            \( \( $A_PLUS_X \) - $X2 \) (?<OP> [*/] ) (?! $ONE_EXPRESSION ) $B
            |
            (?! $ONE_EXPRESSION ) $A (?<OP> [*/] ) \( \( $B_PLUS_X \) - $X2 \)
        }x
        => sub {
            '(' . '(' . $+{A} . $+{OP} . $+{B} . ')' . '+' . $+{X} . ')'
            . '-'
            . $+{X2}
        }
    ],

    # Negative addition to subtraction
    # A + B => B - (-A) if A < 0 and A is multiplicative
    'A+B=>B-(-A)' => [
        qr{
            $A \+ $B
            (?(?{ eval($+{A}) < 0 and $+{A} =~ m{ [*/] }x }) | (*FAIL) )
        }x
        => sub { $+{B} . '-' . negate($+{A}) }
    ],
    # A + B => A - (-B) if B < 0 and B is multiplicative
    'A+B=>A-(-B)' => [
        qr{
            $A \+ $B
            (?(?{ eval($+{B}) < 0 and $+{B} =~ m{ [*/] }x }) | (*FAIL) )
        }x
        => sub { $+{A} . '-' . negate($+{B}) }
    ],

    # Negative subtraction to addition
    # A - B => A + (-B) if B < 0 and B is multiplicative
    'A-B=>A+(-B)' => [
        qr{
            $A - $B
            (?(?{ eval($+{B}) < 0 and $+{B} =~ m{ [*/] }x }) | (*FAIL) )
        }x
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
        '((1+1)-1)*9',
    );
    for my $expression (@expressions) {
        $VERBOSE and warn("$expression\n");
        print(normalize($expression), "\n");
        $VERBOSE and warn("\n");
    }
}

1;
