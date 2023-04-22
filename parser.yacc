

%%

%name Parser

%term INT of R.rational
   | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF | TT | FF | 
   OR | AND | NOT | EQ | NEQ | LT | GT | LEQ | GEQ | ASSIGN | ID of string | CALL
   | READ | PRINT | IF | THEN | ELSE | FI | LBRACE | RBRACE | SEMICOLON
   | WHILE | DO | OD | RATIONAL | COMMA | INTEGER | BOOLEAN | PROCEDURE

%nonterm Start of AST.Block
   | Block of AST.Block
   | DeclSeq of AST.decls
   | procDecls of AST.procDef
   | varDecls of AST.varDecls
   | RatVars of AST.RatVars
   | BoolVars of AST.BoolVars
   | IntVars of AST.IntVars
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

DeclSeq : varDecls DeclSeq (AST.varDecls(varDecls, DeclSeq))
        | procDecls DeclSeq (AST.procDecls(procDecls, DeclSeq))
        | (AST.emptyDecls)

procDecls : PROCEDURE ID Block SEMICOLON (AST.ProcDef(ID, Block))

varDecls : RATIONAL RatVars SEMICOLON (AST.ratDecls(RatVars))
         | INTEGER IntVars SEMICOLON (AST.intDecls(IntVars))
         | BOOLEAN BoolVars SEMICOLON (AST.boolDecls(BoolVars))

RatVars : ID COMMA RatVars (AST.RatVars(ID, RatVars))
        | ID (AST.RatVar(ID))

BoolVars : ID COMMA BoolVars (AST.BoolVars(ID, BoolVars))
         | ID (AST.BoolVar(ID))

IntVars : ID COMMA IntVars (AST.IntVars(ID, IntVars))
         | ID (AST.IntVar(ID))


CmdSeq : LBRACE Cmds RBRACE (Cmds)
   | LBRACE RBRACE (AST.emptyCmds)

Cmds : Cmd SEMICOLON Cmds (AST.cmds(Cmd, Cmds))
     | Cmd SEMICOLON (AST.command(Cmd))

Cmd : ID ASSIGN Exp (AST.assignCmd(AST.Assign(ID, Exp)))
   | CALL ID (AST.callCmd(AST.Call(ID)))
   | READ LPAREN ID RPAREN (AST.readCmd(AST.Read(ID)))
   | PRINT LPAREN Exp RPAREN (AST.printCmd(AST.Print(Exp)))
   | IF BoolExp THEN Cmds ELSE Cmds FI (AST.ifCmd(AST.If(BoolExp, Cmds1, Cmds2)))
   | WHILE BoolExp DO Cmds OD (AST.whileCmd(AST.While(BoolExp, Cmds)))

Exp : BoolExp (AST.BoolExp(BoolExp))
   | RatExp (AST.RatExp(RatExp))
   | ID (AST.IDExp(ID))

BoolExp : TT (AST.TT)
        | FF (AST.FF)
        | BoolExp OR BoolExp (AST.Binopr_bool(AST.Or, BoolExp1 , BoolExp2))
        | BoolExp AND BoolExp (AST.Binopr_bool(AST.And, BoolExp1 , BoolExp2))
        | NOT BoolExp (AST.Unopr_bool(AST.Not, BoolExp))
        | RatExp EQ RatExp (AST.relationalOpr(AST.Eq, RatExp1, RatExp2))
        | RatExp NEQ RatExp (AST.relationalOpr(AST.Neq, RatExp1, RatExp2))
         | RatExp LT RatExp (AST.relationalOpr(AST.Lt, RatExp1, RatExp2))
         | RatExp GT RatExp (AST.relationalOpr(AST.Gt, RatExp1, RatExp2))
         | RatExp LEQ RatExp (AST.relationalOpr(AST.Leq, RatExp1, RatExp2))
         | RatExp GEQ RatExp (AST.relationalOpr(AST.Geq, RatExp1, RatExp2))


RatExp : RatExp ADD Term (AST.Binopr(AST.Add, RatExp , Term))
   | RatExp SUB Term (AST.Binopr(AST.Sub, RatExp , Term))
   | Term (Term)

Term : Term MUL Unit (AST.Binopr(AST.Mul, Term , Unit))
    | Term DIV Unit (AST.Binopr(AST.Div, Term , Unit))
    | Unit (Unit)
   
Unit : INT (AST.Int (INT))


