

## Grammar for rational numbers

      <rational> ::= <whole> "/" <positive> | <decimal>
      <decimal> ::= <integer> "." <whole>
      <integer> ::= <whole> | "~" <whole>
      <whole> ::= <digit>+
      <positive> ::= <zero>* <non-zero-digit> <digit>*
      <zero> ::= "0"
      <non-zero-digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
      <digit> ::= <zero> | <non-zero-digit>

## Grammar for rational number expressions

      <expression> ::= <term> | <term> "+" <expression> | <term> "-" <expression>
      <term> ::= <factor> | <factor> "*" <term> | <factor> "/" <term>
      <expression> ::= <term> | <expression> "+" <term> | <expression> "-" <term>
      <term> ::= <factor> | <term> "*" <factor> | <term> " / " <factor>
      <factor> ::= <number> | "(" <expression> ")" | <variable>
      <number> ::= <integer> | <rational>
      <rational> ::= <whole> "/" <positive> | <decimal>
      <decimal> ::= <integer> "." <whole>
      <integer> ::= <whole> | "~" <whole>
      <whole> ::= <digit>+
      <positive> ::= <zero>* <non-zero-digit> <digit>*
      <zero> ::= "0"
      <non-zero-digit> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
      <digit> ::= <zero> | <non-zero-digit>
      <variable> ::= <letter> <letter-or-digit>*
      <letter> ::= [a-zA-Z]
      <letter-or-digit> ::= <letter> | <digit>

Note: To separate division from rational numbers, we can use the token " / " instead of "/".

Codebase:
bigint.sml - Big integer arithmetic
rational.sml - Rational number arithmetic
(I have not yet implemented showDecimal/toDecimal)

CLI:
run loader.sml to load the codebase
to start using cli type `cli();` in the REPL

Allowed operations:
"+ - * / ( ) ~"

number format:
integer: 123
rational: 123/456
decimal: 123.456(789)
Note:
- it is compulsory to add recurring digits in decimal
- eg: 0.3 must be written as 0.3(0)


