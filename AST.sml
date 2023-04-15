structure R = Rational(Bigint)

structure AST = 
struct 

datatype Block = Block of decls * cmds

and decls = varDecls of varDecls * decls | procDecls of procDef * decls 
   | varDecl of varDecls | procDecl of procDef | emptyDecls

and varDecls = ratDecls of RatVars 
| boolDecls of BoolVars 
| intDecls of IntVars 
| ratDecl of RatVars
| boolDecl of BoolVars
| intDecl of IntVars

and RatVars = RatVars of string * RatVars | RatVar of string 
and BoolVars = BoolVars of string * BoolVars | BoolVar of string
and IntVars = IntVars of string * IntVars | IntVar of string


and procDef = ProcDef of string * Block


and cmds = cmds of command * cmds | command of command | emptyCmds


and command = assignCmd of assignCmd
| callCmd of callCmd
| readCmd of readCmd
| printCmd of printCmd
| ifCmd of ifCmd
| whileCmd of whileCmd

and assignCmd = Assign of string * exp
and callCmd = Call of string
and readCmd = Read of string
and printCmd = Print of exp
and ifCmd = If of BoolExp * cmds * cmds
and whileCmd = While of BoolExp * cmds

and exp = RatExp of RatExp | BoolExp of BoolExp

and BoolExp = TT | FF | Unopr_bool of unop_bool * BoolExp
| Binopr_bool of binop_bool * BoolExp * BoolExp
| relationalOpr of relational_op * RatExp * RatExp

and binop_bool = And | Or 
and unop_bool = Not
and relational_op = Eq | Neq | Lt | Gt | Leq | Geq

and RatExp = Int of R.rational
| Binopr of binop * RatExp * RatExp

and binop = Add | Sub | Mul | Div 



end
