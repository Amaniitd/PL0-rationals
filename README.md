

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


Program ::= Block .

Block ::= DeclarationSeq CommandSeq .

DeclarationSeq ::= VarDecls ProcDecls | ProcDecls VarDecls | VarDecls | ProcDecls | ε .

VarDecls ::= RatVarDecls IntVarDecls BoolVarDecls | RatVarDecls IntVarDecls | RatVarDecls BoolVarDecls | IntVarDecls BoolVarDecls | RatVarDecls | IntVarDecls | BoolVarDecls | ε .

RatVarDecls ::= rational Ident RatVarDeclsTail ;

RatVarDeclsTail ::= , Ident RatVarDeclsTail | ε .

IntVarDecls ::= integer Ident IntVarDeclsTail ;

IntVarDeclsTail ::= , Ident IntVarDeclsTail | ε .

BoolVarDecls ::= boolean Ident BoolVarDeclsTail ;

BoolVarDeclsTail ::= , Ident BoolVarDeclsTail | ε .

ProcDecls ::= ProcDef ; ProcDecls | ProcDef ; | ε .

ProcDef ::= procedure Ident Block .

CommandSeq ::= { CommandSeqTail } .

CommandSeqTail ::= Command ; CommandSeqTail | ε .

Command ::= AssignmentCmd | CallCmd | ReadCmd | PrintCmd | ConditionalCmd | WhileCmd .

AssignmentCmd ::= Ident := Expression .

CallCmd ::= call Ident .

ReadCmd ::= read ( Ident ) .

PrintCmd ::= print ( Expression ) .

Expression ::= RatExpression | IntExpression | BoolExpression .

RatExpression ::= RatTerm RatExpressionTail .

RatExpressionTail ::= + RatTerm RatExpressionTail | - RatTerm RatExpressionTail | ε .

RatTerm ::= RatFactor RatTermTail .

RatTermTail ::= * RatFactor RatTermTail | / RatFactor RatTermTail | ε .

RatFactor ::= ( RatExpression ) | Ident | RatNumber .

RatNumber ::= Integer | Rational .

BoolExpression ::= BoolTerm BoolExpressionTail .

BoolExpressionTail ::= or BoolTerm BoolExpressionTail | ε .

BoolTerm ::= BoolFactor BoolTermTail .

BoolTermTail ::= and BoolFactor BoolTermTail | ε .

BoolFactor ::= ( BoolExpression ) | not BoolFactor | BoolValue | RelationalExpression .

RelationalExpression ::= RatExpression RelationalOperator RatExpression .

RelationalOperator ::= < | > | <= | >= | = | <> .

IntExpression ::= IntTerm IntExpressionTail .

IntExpressionTail ::= + IntTerm IntExpressionTail | - IntTerm IntExpressionTail | ε .

IntTerm ::= IntFactor IntTermTail .

IntTermTail ::= * IntFactor IntTermTail | / IntFactor IntTermTail | ε .

IntFactor ::= ( IntExpression ) | Ident | IntNumber .

IntNumber ::= Integer .

Boolean ::= tt | ff .


