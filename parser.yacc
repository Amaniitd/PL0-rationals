

%%

%name Parser

%term INT of R.rational
   | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF | TT | FF | 
   OR | AND | NOT | EQ | NEQ | LT | GT | LEQ | GEQ | ASSIGN | ID of string | CALL
   | READ | PRINT | IF | THEN | ELSE | FI | LBRACE | RBRACE | SEMICOLON
   | WHILE | DO | OD | RATIONAL | COMMA

%nonterm Start of AST.Block
   | Block of AST.Block
   | DeclSeq of AST.decls
   | varDecls of AST.varDecls
   | RatVars of AST.RatVars
   | CmdSeq of AST.cmds
   | Cmds of AST.cmds
   | Cmd of AST.command
   | Exp of AST.exp
   | BoolExp of AST.BoolExp
   | RatExp of AST.RatExp
   |Term of AST.RatExp
   | Unit of AST.RatExp

%pos int
%eop EOF 
%noshift EOF

%left ADD SUB 
%left MUL DIV

%left OR AND
%left EQ NEQ LT GT LEQ GEQ

%left NOT





%start Start
%keyword
%verbose

%%

Start : Block (Block)
Block : DeclSeq CmdSeq (AST.Block(DeclSeq, CmdSeq))

DeclSeq : varDecls (AST.varDecls(varDecls))
        | (AST.emptyDecls)

varDecls : RATIONAL RatVars SEMICOLON (AST.ratDecls(RatVars))

RatVars : ID COMMA RatVars (AST.RatVars(ID, RatVars))
        | ID (AST.RatVar(ID))


CmdSeq : LBRACE Cmds RBRACE (Cmds)
   | LBRACE RBRACE (AST.emptyCmds)

Cmds : Cmd SEMICOLON Cmds (AST.cmds(Cmd, Cmds))
     | Cmd SEMICOLON (AST.command(Cmd))

Cmd : ID ASSIGN Exp (AST.assignCmd(AST.Assign(ID, Exp)))
   | CALL ID (AST.callCmd(AST.Call(ID)))
   | READ LPAREN ID RPAREN (AST.readCmd(AST.Read(ID)))
   | PRINT LPAREN Exp RPAREN (AST.printCmd(AST.Print(Exp)))
   | IF BoolExp THEN Cmds ELSE Cmds FI (AST.ifCmd(AST.If(BoolExp, Cmds, Cmds)))
   | WHILE BoolExp DO Cmds OD (AST.whileCmd(AST.While(BoolExp, Cmds)))

Exp : BoolExp (AST.BoolExp(BoolExp))
   | RatExp (AST.RatExp(RatExp))

BoolExp : TT (AST.TT)
        | FF (AST.FF)
        | BoolExp OR BoolExp (AST.Binopr_bool(AST.Or, BoolExp , BoolExp))
        | BoolExp AND BoolExp (AST.Binopr_bool(AST.And, BoolExp , BoolExp))
        | NOT BoolExp (AST.Unopr_bool(AST.Not, BoolExp))
        | RatExp EQ RatExp (AST.relationalOpr(AST.Eq, RatExp , RatExp))
        | RatExp NEQ RatExp (AST.relationalOpr(AST.Neq, RatExp , RatExp))
        | RatExp LT RatExp (AST.relationalOpr(AST.Lt, RatExp , RatExp))
        | RatExp GT RatExp (AST.relationalOpr(AST.Gt, RatExp , RatExp))
        | RatExp LEQ RatExp (AST.relationalOpr(AST.Leq, RatExp , RatExp))
        | RatExp GEQ RatExp (AST.relationalOpr(AST.Geq, RatExp , RatExp))


RatExp : RatExp ADD Term (AST.Binopr(AST.Add, RatExp , Term))
   | RatExp SUB Term (AST.Binopr(AST.Sub, RatExp , Term))
   | Term (Term)

Term : Term MUL Unit (AST.Binopr(AST.Mul, Term , Unit))
    | Term DIV Unit (AST.Binopr(AST.Div, Term , Unit))
    | Unit (Unit)
   
Unit : INT (AST.Int (INT))


