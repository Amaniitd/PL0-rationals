structure R = Rational(Bigint)

structure AST = 
struct 

(* 
P rogram ::= Block .
Block ::= DeclarationSeq CommandSeq .
DeclarationSeq ::= [VarDecls] [ProcDecls] .
VarDecls ::= [RatV arDecls] [IntVarDecls] [BoolVarDecls] .
RatV arDecls ::= rational Ident {, Ident}; .
IntV arDecls ::= integer Ident {, Ident}; .
BoolV arDecls ::= boolean Ident {, Ident}; .
ProcDecls ::= [ProcDef {;P rocDecls};] .
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
*)

datatype program = Program of block
and block = Block of declseq * cmdseq
and declseq = Declseq of decl * declseq | Emptydeclseq of unit
and decl = Vardecls of vardecls
| Procdecls of procdecls
and vardecls = Ratvardecls of ratvardecls
| Intvardecls of intvardecls
| Boolvardecls of boolvardecls
and ratvardecls = Ratvardecls of ident list
and intvardecls = Intvardecls of ident list
and boolvardecls = Boolvardecls of ident list
and procdecls = Procdecls of procdef list
and procdef = Procdef of ident * block
and cmdseq = Cmdseq of command list
and command = Assignmentcmd of ident * expression
| Callcmd of ident
| Readcmd of ident
| Printcmd of expression
| Conditionalcmd of expression * cmdseq * cmdseq
| Whilecmd of expression * cmdseq
and expression = Ratexpression of ratexpression
| Intexpression of intexpression
| Boolexpression of boolexpression
and ratexpression = Ratexp of exp
and intexpression = Intexp of exp
and boolexpression = Boolexp of exp
and exp = Int of int
| Rat of R.rational
| Boolean of bool
| Ident of ident
| Binopr of binop * exp * exp
| Unopr of unop * exp
and ident = Ident of string
and unop = Neg | Not
and binop = Add | Sub | Mul | Div | Eq | Neq | Lt | Gt | Leq | Geq | And | Or

fun eval(program) = "hi"

end
