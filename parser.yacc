

%%

%name Parser

%term INT of R.rational
   | ADD | SUB | MUL | DIV | LPAREN | RPAREN | EOF | TT | FF | 
   OR | AND | NOT | EQ | NEQ | LT | GT | LEQ | GEQ | ASSIGN | ID of string | CALL
   | READ | PRINT | IF | THEN | ELSE | FI | LBRACE | RBRACE | SEMICOLON
   | WHILE | DO | OD | RATIONAL | COMMA | INTEGER | BOOLEAN | PROCEDURE | INVERSE
   | MOD

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
   |Term of AST.exp
   | Unit of AST.exp

%pos int
%eop EOF 
%noshift EOF

%left ADD SUB 
%left MUL DIV

%left OR AND
%left EQ NEQ LT GT LEQ GEQ
%left INVERSE

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
   | IF Exp THEN CmdSeq ELSE CmdSeq FI (AST.ifCmd(AST.If(Exp, CmdSeq1, CmdSeq1)))
   | WHILE Exp DO CmdSeq OD (AST.whileCmd(AST.While(Exp, CmdSeq)))

Exp : TT (AST.TT)
        | FF (AST.FF)
        | Exp OR Exp (AST.Binopr_bool(AST.Or, Exp1 , Exp2))
        | Exp AND Exp (AST.Binopr_bool(AST.And, Exp1 , Exp2))
        | NOT Exp (AST.Unopr_bool(AST.Not, Exp))
        | Exp EQ Exp (AST.relationalOpr(AST.Eq, Exp1, Exp2))
        | Exp NEQ Exp (AST.relationalOpr(AST.Neq, Exp1, Exp2))
         | Exp LT Exp (AST.relationalOpr(AST.Lt, Exp1, Exp2))
         | Exp GT Exp (AST.relationalOpr(AST.Gt, Exp1, Exp2))
         | Exp LEQ Exp (AST.relationalOpr(AST.Leq, Exp1, Exp2))
         | Exp GEQ Exp (AST.relationalOpr(AST.Geq, Exp1, Exp2))
         | Exp ADD Term (AST.Binopr(AST.Add, Exp , Term))
         | Exp SUB Term (AST.Binopr(AST.Sub, Exp , Term))
         | Term (Term)
         | LPAREN Exp RPAREN (Exp)


Term : Term MUL Unit (AST.Binopr(AST.Mul, Term , Unit))
    | Term DIV Unit (AST.Binopr(AST.Div, Term , Unit))
    | Term MOD Unit (AST.Binopr(AST.Mod, Term , Unit))
    | Unit (Unit)
   
Unit : INT (AST.Int (INT))
   | ID (AST.Var (ID))



