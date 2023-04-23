

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


## Grammar for program
      Program ::= Block .
      Block ::= DeclarationSeq CommandSeq .
      DeclarationSeq ::= [VarDecls] [ProcDecls] .
      VarDecls ::= [RatVarDecls] [IntVarDecls] [BoolVarDecls] .
      RatVarDecls ::= rational Ident {, Ident}; .
      IntVarDecls ::= integer Ident {, Ident}; .
      BoolVarDecls ::= boolean Ident {, Ident}; .
      ProcDecls ::= [P rocDef {;P rocDecls};] .
      ProcDef ::= procedure Ident Block .
      CommandSeq ::= {{Command;}} .
      Command ::= AssignmentCmd | CallCmd | ReadCmd | P rintCmd |
      ConditionalCmd | W hileCmd .
      AssignmentCmd ::= Ident := Expression .
      CallCmd ::= call Ident .
      ReadCmd ::= read( Ident ) .
      PrintCmd ::= print( Expression ) .
      Expression ::= RatExpression | IntExpression | BoolExpression .
      ConditionalCmd ::= if BoolExpression then CommandSeq else CommandSeq fi .
      WhileCmd ::= while BoolExpression do CommandSeq od .


