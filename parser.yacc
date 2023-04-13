

%%

%name Parser

%term INT of R.rational
   | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF 

%nonterm Start of AST.exp 
   | Exp of AST.exp
   |Term of AST.exp
   | Unit of AST.exp

%pos int
%eop EOF 
%noshift EOF

%left ADD SUB 
%left MUL DIV



%start Start
%keyword
%verbose

%%

Start : Exp (Exp)

Exp : Exp ADD Term (AST.Binopr(AST.Add, Exp , Term))
   | Exp SUB Term (AST.Binopr(AST.Sub, Exp , Term))
   | Term (Term)

Term : Term MUL Unit (AST.Binopr(AST.Mul, Term , Unit))
    | Term DIV Unit (AST.Binopr(AST.Div, Term , Unit))
    | Unit (Unit)
   
Unit : INT (AST.Int (INT))
     | LPAREN Exp RPAREN (Exp)

