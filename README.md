

## Grammar for rational numbers

      <rational> ::= <whole> "/" <positive> | <decimal> | LB <rational> RB
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

