

%%

%name Parser

%term INT of R.rational
   | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF | IF | THEN | ELSE | FI | WHILE | DO | OD | RATIONAL | INTEGER | BOOLEAN | TRUE | FALSE | VAR | PROCEDURE | PRINT | READ | CALL | INVERSE | MAKERAT | RAT | SHOWRAT | SHOWDECIMAL | FROMDECIMAL | TODECIMAL | NOT | AND | OR | EQ | NE | LT | LE | GT | GE | ASSIGN | SEMICOLON | COMMA | LBRACE | RBRACE

%nonterm PROGRAM | 



%pos int
%eop EOF 
%noshift EOF

%left ADD SUB 
%left MUL DIV



%start PROGRAM
%keyword
%verbose

%%

PROGRAM : BLOCK (AST.program(BLOCK))
BLOCK : DECLS CMDS (AST.block(DECLS, CMDS))
DECLS : DECLS DECL (AST.decls(DECLS, DECL))
      | (AST.decls())
DECL : VAR VARLIST SEMICOLON (AST.decl(VARLIST))



Conditionalcmd : IF EXP THEN CMDS ELSE CMDS FI (AST.conditionalcmd(EXP, CMDS, CMDS))
WHILECMD : WHILE EXP DO CMDS OD (AST.whilecmd(EXP, CMDS))

EXP : EXP ADD EXP (AST.Binopr(AST.Add, EXP, EXP))
    | EXP SUB EXP (AST.Binopr(AST.Sub, EXP, EXP))
    | EXP MUL EXP (AST.Binopr(AST.Mul, EXP, EXP))
    | EXP DIV EXP (AST.Binopr(AST.Div, EXP, EXP))
    | EXP EQ EXP (AST.Binopr(AST.Eq, EXP, EXP))
    | EXP NE EXP (AST.Binopr(AST.Neq, EXP, EXP))
    | EXP LT EXP (AST.Binopr(AST.Lt, EXP, EXP))
    | EXP LE EXP (AST.Binopr(AST.Leq, EXP, EXP))
    | EXP GT EXP (AST.Binopr(AST.Gt, EXP, EXP))
    | EXP GE EXP (AST.Binopr(AST.Geq, EXP, EXP))
    | EXP AND EXP (AST.Binopr(AST.And, EXP, EXP))
    | EXP OR EXP (AST.Binopr(AST.Or, EXP, EXP))
    | SUB EXP (AST.Unopr(AST.Neg, EXP))
    | NOT EXP (AST.Unopr(AST.Not, EXP))
    | INT (AST.int(INT))
    | TRUE (AST.Boolean(true))
    | FALSE (AST.Boolean(false))
    | LPAREN EXP RPAREN (EXP)
    | VAR (AST.var(VAR))
    | CALL VAR (AST.call(VAR))
    | INVERSE EXP (AST.inverse(EXP))
    | MAKERAT EXP EXP (AST.makerat(EXP, EXP))
    | RAT EXP (AST.rat(EXP))
    | SHOWRAT EXP (AST.showrat(EXP))
    | SHOWDECIMAL EXP (AST.showdecimal(EXP))
    | FROMDECIMAL EXP (AST.fromdecimal(EXP))
    | TODECIMAL EXP (AST.todecimal(EXP))
    | VAR ASSIGN EXP (AST.assign(VAR, EXP))
    | VAR LBRACE EXP COMMA EXP RBRACE (AST.arrayassign(VAR, EXP, EXP))
    | VAR LBRACE EXP RBRACE (AST.arrayref(VAR, EXP))
    | VAR LBRACE EXP COMMA EXP RBRACE ASSIGN EXP (AST.arrayassign(VAR, EXP, EXP))
    | VAR LBRACE EXP RBRACE ASSIGN EXP (AST.arrayassign(VAR, EXP, EXP))
    | VAR LBRACE EXP COMMA EXP RBRACE ASSIGN EXP (AST.arrayassign(V