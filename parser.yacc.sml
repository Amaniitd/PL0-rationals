functor ParserLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Parser_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct



end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\047\000\000\000\
\\001\000\001\000\047\000\009\000\046\000\010\000\045\000\013\000\044\000\000\000\
\\001\000\002\000\067\000\003\000\066\000\014\000\065\000\015\000\064\000\
\\016\000\063\000\017\000\062\000\018\000\061\000\019\000\060\000\000\000\
\\001\000\006\000\049\000\000\000\
\\001\000\006\000\050\000\000\000\
\\001\000\007\000\092\000\000\000\
\\001\000\007\000\093\000\000\000\
\\001\000\008\000\000\000\000\000\
\\001\000\011\000\070\000\012\000\069\000\026\000\072\000\000\000\
\\001\000\011\000\070\000\012\000\069\000\033\000\068\000\000\000\
\\001\000\020\000\052\000\000\000\
\\001\000\021\000\015\000\000\000\
\\001\000\021\000\017\000\000\000\
\\001\000\021\000\019\000\000\000\
\\001\000\021\000\021\000\000\000\
\\001\000\021\000\030\000\022\000\029\000\023\000\028\000\024\000\027\000\
\\025\000\026\000\030\000\025\000\032\000\024\000\000\000\
\\001\000\021\000\030\000\022\000\029\000\023\000\028\000\024\000\027\000\
\\025\000\026\000\032\000\024\000\000\000\
\\001\000\021\000\051\000\000\000\
\\001\000\021\000\076\000\000\000\
\\001\000\027\000\095\000\000\000\
\\001\000\028\000\097\000\000\000\
\\001\000\029\000\014\000\000\000\
\\001\000\030\000\039\000\000\000\
\\001\000\031\000\032\000\000\000\
\\001\000\031\000\034\000\000\000\
\\001\000\031\000\036\000\000\000\
\\001\000\031\000\038\000\000\000\
\\001\000\031\000\053\000\000\000\
\\001\000\034\000\094\000\000\000\
\\099\000\000\000\
\\100\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\035\000\010\000\037\000\009\000\038\000\008\000\039\000\007\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\036\000\037\000\000\000\
\\110\000\000\000\
\\111\000\036\000\033\000\000\000\
\\112\000\000\000\
\\113\000\036\000\035\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\021\000\030\000\022\000\029\000\023\000\028\000\024\000\027\000\
\\025\000\026\000\032\000\024\000\000\000\
\\118\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\000\000\
\\124\000\011\000\070\000\012\000\069\000\000\000\
\\125\000\002\000\067\000\003\000\066\000\014\000\065\000\015\000\064\000\
\\016\000\063\000\017\000\062\000\018\000\061\000\019\000\060\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\002\000\067\000\003\000\066\000\000\000\
\\132\000\002\000\067\000\003\000\066\000\000\000\
\\133\000\002\000\067\000\003\000\066\000\000\000\
\\134\000\002\000\067\000\003\000\066\000\000\000\
\\135\000\002\000\067\000\003\000\066\000\000\000\
\\136\000\002\000\067\000\003\000\066\000\000\000\
\\137\000\004\000\059\000\005\000\058\000\000\000\
\\138\000\004\000\059\000\005\000\058\000\000\000\
\\139\000\004\000\059\000\005\000\058\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\"
val actionRowNumbers =
"\033\000\033\000\033\000\021\000\
\\029\000\011\000\012\000\013\000\
\\014\000\031\000\032\000\030\000\
\\015\000\033\000\023\000\041\000\
\\024\000\043\000\025\000\039\000\
\\026\000\022\000\001\000\045\000\
\\001\000\003\000\004\000\017\000\
\\010\000\027\000\037\000\012\000\
\\036\000\013\000\035\000\014\000\
\\047\000\044\000\072\000\069\000\
\\002\000\009\000\001\000\057\000\
\\056\000\073\000\008\000\001\000\
\\018\000\049\000\001\000\034\000\
\\040\000\042\000\038\000\046\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\016\000\001\000\
\\001\000\060\000\016\000\055\000\
\\054\000\005\000\006\000\048\000\
\\071\000\070\000\066\000\065\000\
\\064\000\063\000\062\000\061\000\
\\068\000\067\000\028\000\059\000\
\\058\000\019\000\051\000\050\000\
\\053\000\016\000\020\000\052\000\
\\007\000"
val gotoT =
"\
\\001\000\096\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\005\000\001\000\000\000\
\\003\000\009\000\004\000\002\000\005\000\001\000\000\000\
\\003\000\010\000\004\000\002\000\005\000\001\000\000\000\
\\009\000\011\000\000\000\
\\000\000\
\\000\000\
\\007\000\014\000\000\000\
\\008\000\016\000\000\000\
\\006\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\021\000\011\000\020\000\000\000\
\\002\000\029\000\003\000\003\000\004\000\002\000\005\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\041\000\014\000\040\000\015\000\039\000\016\000\038\000\000\000\
\\000\000\
\\013\000\046\000\014\000\040\000\015\000\039\000\016\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\052\000\000\000\
\\000\000\
\\008\000\053\000\000\000\
\\000\000\
\\006\000\054\000\000\000\
\\010\000\055\000\011\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\069\000\014\000\040\000\015\000\039\000\016\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\073\000\013\000\072\000\014\000\071\000\015\000\039\000\
\\016\000\038\000\000\000\
\\000\000\
\\000\000\
\\012\000\075\000\013\000\072\000\014\000\071\000\015\000\039\000\
\\016\000\038\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\076\000\000\000\
\\016\000\077\000\000\000\
\\014\000\078\000\015\000\039\000\016\000\038\000\000\000\
\\014\000\079\000\015\000\039\000\016\000\038\000\000\000\
\\014\000\080\000\015\000\039\000\016\000\038\000\000\000\
\\014\000\081\000\015\000\039\000\016\000\038\000\000\000\
\\014\000\082\000\015\000\039\000\016\000\038\000\000\000\
\\014\000\083\000\015\000\039\000\016\000\038\000\000\000\
\\015\000\084\000\016\000\038\000\000\000\
\\015\000\085\000\016\000\038\000\000\000\
\\010\000\086\000\011\000\020\000\000\000\
\\013\000\087\000\014\000\040\000\015\000\039\000\016\000\038\000\000\000\
\\013\000\088\000\014\000\040\000\015\000\039\000\016\000\038\000\000\000\
\\000\000\
\\010\000\089\000\011\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\094\000\011\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 97
val numrules = 45
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | INT of unit ->  (R.rational)
 | Unit of unit ->  (AST.RatExp) | Term of unit ->  (AST.RatExp)
 | RatExp of unit ->  (AST.RatExp) | BoolExp of unit ->  (AST.BoolExp)
 | Exp of unit ->  (AST.exp) | Cmd of unit ->  (AST.command)
 | Cmds of unit ->  (AST.cmds) | CmdSeq of unit ->  (AST.cmds)
 | IntVars of unit ->  (AST.IntVars)
 | BoolVars of unit ->  (AST.BoolVars)
 | RatVars of unit ->  (AST.RatVars)
 | varDecls of unit ->  (AST.varDecls)
 | procDecls of unit ->  (AST.procDef)
 | DeclSeq of unit ->  (AST.decls) | Block of unit ->  (AST.Block)
 | Start of unit ->  (AST.Block)
end
type svalue = MlyValue.svalue
type result = AST.Block
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "INT"
  | (T 1) => "ADD"
  | (T 2) => "SUB"
  | (T 3) => "MUL"
  | (T 4) => "DIV"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "EOF"
  | (T 8) => "TT"
  | (T 9) => "FF"
  | (T 10) => "OR"
  | (T 11) => "AND"
  | (T 12) => "NOT"
  | (T 13) => "EQ"
  | (T 14) => "NEQ"
  | (T 15) => "LT"
  | (T 16) => "GT"
  | (T 17) => "LEQ"
  | (T 18) => "GEQ"
  | (T 19) => "ASSIGN"
  | (T 20) => "ID"
  | (T 21) => "CALL"
  | (T 22) => "READ"
  | (T 23) => "PRINT"
  | (T 24) => "IF"
  | (T 25) => "THEN"
  | (T 26) => "ELSE"
  | (T 27) => "FI"
  | (T 28) => "LBRACE"
  | (T 29) => "RBRACE"
  | (T 30) => "SEMICOLON"
  | (T 31) => "WHILE"
  | (T 32) => "DO"
  | (T 33) => "OD"
  | (T 34) => "RATIONAL"
  | (T 35) => "COMMA"
  | (T 36) => "INTEGER"
  | (T 37) => "BOOLEAN"
  | (T 38) => "PROCEDURE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32)
 $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 
2) $$ (T 1)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Block Block1, Block1left, Block1right)) :: 
rest671)) => let val  result = MlyValue.Start (fn _ => let val  (Block
 as Block1) = Block1 ()
 in (Block)
end)
 in ( LrTable.NT 0, ( result, Block1left, Block1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.CmdSeq CmdSeq1, _, CmdSeq1right)) :: ( _, ( 
MlyValue.DeclSeq DeclSeq1, DeclSeq1left, _)) :: rest671)) => let val  
result = MlyValue.Block (fn _ => let val  (DeclSeq as DeclSeq1) = 
DeclSeq1 ()
 val  (CmdSeq as CmdSeq1) = CmdSeq1 ()
 in (AST.Block(DeclSeq, CmdSeq))
end)
 in ( LrTable.NT 1, ( result, DeclSeq1left, CmdSeq1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.DeclSeq DeclSeq1, _, DeclSeq1right)) :: ( _,
 ( MlyValue.varDecls varDecls1, varDecls1left, _)) :: rest671)) => let
 val  result = MlyValue.DeclSeq (fn _ => let val  (varDecls as 
varDecls1) = varDecls1 ()
 val  (DeclSeq as DeclSeq1) = DeclSeq1 ()
 in (AST.varDecls(varDecls, DeclSeq))
end)
 in ( LrTable.NT 2, ( result, varDecls1left, DeclSeq1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.DeclSeq DeclSeq1, _, DeclSeq1right)) :: ( _,
 ( MlyValue.procDecls procDecls1, procDecls1left, _)) :: rest671)) =>
 let val  result = MlyValue.DeclSeq (fn _ => let val  (procDecls as 
procDecls1) = procDecls1 ()
 val  (DeclSeq as DeclSeq1) = DeclSeq1 ()
 in (AST.procDecls(procDecls, DeclSeq))
end)
 in ( LrTable.NT 2, ( result, procDecls1left, DeclSeq1right), rest671)

end
|  ( 4, ( rest671)) => let val  result = MlyValue.DeclSeq (fn _ => (
AST.emptyDecls))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Block 
Block1, _, _)) :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
PROCEDURE1left, _)) :: rest671)) => let val  result = 
MlyValue.procDecls (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Block as Block1) = Block1 ()
 in (AST.ProcDef(ID, Block))
end)
 in ( LrTable.NT 3, ( result, PROCEDURE1left, SEMICOLON1right), 
rest671)
end
|  ( 6, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.RatVars 
RatVars1, _, _)) :: ( _, ( _, RATIONAL1left, _)) :: rest671)) => let
 val  result = MlyValue.varDecls (fn _ => let val  (RatVars as 
RatVars1) = RatVars1 ()
 in (AST.ratDecls(RatVars))
end)
 in ( LrTable.NT 4, ( result, RATIONAL1left, SEMICOLON1right), rest671
)
end
|  ( 7, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.IntVars 
IntVars1, _, _)) :: ( _, ( _, INTEGER1left, _)) :: rest671)) => let
 val  result = MlyValue.varDecls (fn _ => let val  (IntVars as 
IntVars1) = IntVars1 ()
 in (AST.intDecls(IntVars))
end)
 in ( LrTable.NT 4, ( result, INTEGER1left, SEMICOLON1right), rest671)

end
|  ( 8, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.BoolVars 
BoolVars1, _, _)) :: ( _, ( _, BOOLEAN1left, _)) :: rest671)) => let
 val  result = MlyValue.varDecls (fn _ => let val  (BoolVars as 
BoolVars1) = BoolVars1 ()
 in (AST.boolDecls(BoolVars))
end)
 in ( LrTable.NT 4, ( result, BOOLEAN1left, SEMICOLON1right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.RatVars RatVars1, _, RatVars1right)) :: _ ::
 ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result
 = MlyValue.RatVars (fn _ => let val  (ID as ID1) = ID1 ()
 val  (RatVars as RatVars1) = RatVars1 ()
 in (AST.RatVars(ID, RatVars))
end)
 in ( LrTable.NT 5, ( result, ID1left, RatVars1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.RatVars (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.RatVar(ID))
end)
 in ( LrTable.NT 5, ( result, ID1left, ID1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.BoolVars BoolVars1, _, BoolVars1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.BoolVars (fn _ => let val  (ID as ID1) = ID1 ()
 val  (BoolVars as BoolVars1) = BoolVars1 ()
 in (AST.BoolVars(ID, BoolVars))
end)
 in ( LrTable.NT 6, ( result, ID1left, BoolVars1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.BoolVars (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.BoolVar(ID))
end)
 in ( LrTable.NT 6, ( result, ID1left, ID1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.IntVars IntVars1, _, IntVars1right)) :: _
 :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  
result = MlyValue.IntVars (fn _ => let val  (ID as ID1) = ID1 ()
 val  (IntVars as IntVars1) = IntVars1 ()
 in (AST.IntVars(ID, IntVars))
end)
 in ( LrTable.NT 7, ( result, ID1left, IntVars1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.IntVars (fn _ => let val  (ID as ID1) = 
ID1 ()
 in (AST.IntVar(ID))
end)
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.Cmds Cmds1,
 _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result
 = MlyValue.CmdSeq (fn _ => let val  (Cmds as Cmds1) = Cmds1 ()
 in (Cmds)
end)
 in ( LrTable.NT 8, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 16, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( _, LBRACE1left, _))
 :: rest671)) => let val  result = MlyValue.CmdSeq (fn _ => (
AST.emptyCmds))
 in ( LrTable.NT 8, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.Cmds Cmds1, _, Cmds1right)) :: _ :: ( _, ( 
MlyValue.Cmd Cmd1, Cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmds (fn _ => let val  (Cmd as Cmd1) = Cmd1 ()
 val  (Cmds as Cmds1) = Cmds1 ()
 in (AST.cmds(Cmd, Cmds))
end)
 in ( LrTable.NT 9, ( result, Cmd1left, Cmds1right), rest671)
end
|  ( 18, ( ( _, ( _, _, SEMICOLON1right)) :: ( _, ( MlyValue.Cmd Cmd1,
 Cmd1left, _)) :: rest671)) => let val  result = MlyValue.Cmds (fn _
 => let val  (Cmd as Cmd1) = Cmd1 ()
 in (AST.command(Cmd))
end)
 in ( LrTable.NT 9, ( result, Cmd1left, SEMICOLON1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Exp Exp1, _, Exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmd (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Exp as Exp1) = Exp1 ()
 in (AST.assignCmd(AST.Assign(ID, Exp)))
end)
 in ( LrTable.NT 10, ( result, ID1left, Exp1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: ( _, ( _, 
CALL1left, _)) :: rest671)) => let val  result = MlyValue.Cmd (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (AST.callCmd(AST.Call(ID)))
end)
 in ( LrTable.NT 10, ( result, CALL1left, ID1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ID ID1, _, _
)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) => let val  result = 
MlyValue.Cmd (fn _ => let val  (ID as ID1) = ID1 ()
 in (AST.readCmd(AST.Read(ID)))
end)
 in ( LrTable.NT 10, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.Exp Exp1, _,
 _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  result
 = MlyValue.Cmd (fn _ => let val  (Exp as Exp1) = Exp1 ()
 in (AST.printCmd(AST.Print(Exp)))
end)
 in ( LrTable.NT 10, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 23, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.Cmds Cmds2, _, _
)) :: _ :: ( _, ( MlyValue.Cmds Cmds1, _, _)) :: _ :: ( _, ( 
MlyValue.BoolExp BoolExp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.Cmd (fn _ => let val  (BoolExp as 
BoolExp1) = BoolExp1 ()
 val  (Cmds as Cmds1) = Cmds1 ()
 val  Cmds2 = Cmds2 ()
 in (AST.ifCmd(AST.If(BoolExp, Cmds, Cmds)))
end)
 in ( LrTable.NT 10, ( result, IF1left, FI1right), rest671)
end
|  ( 24, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.Cmds Cmds1, _, _
)) :: _ :: ( _, ( MlyValue.BoolExp BoolExp1, _, _)) :: ( _, ( _, 
WHILE1left, _)) :: rest671)) => let val  result = MlyValue.Cmd (fn _
 => let val  (BoolExp as BoolExp1) = BoolExp1 ()
 val  (Cmds as Cmds1) = Cmds1 ()
 in (AST.whileCmd(AST.While(BoolExp, Cmds)))
end)
 in ( LrTable.NT 10, ( result, WHILE1left, OD1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.BoolExp BoolExp1, BoolExp1left, 
BoolExp1right)) :: rest671)) => let val  result = MlyValue.Exp (fn _
 => let val  (BoolExp as BoolExp1) = BoolExp1 ()
 in (AST.BoolExp(BoolExp))
end)
 in ( LrTable.NT 11, ( result, BoolExp1left, BoolExp1right), rest671)

end
|  ( 26, ( ( _, ( MlyValue.RatExp RatExp1, RatExp1left, RatExp1right))
 :: rest671)) => let val  result = MlyValue.Exp (fn _ => let val  (
RatExp as RatExp1) = RatExp1 ()
 in (AST.RatExp(RatExp))
end)
 in ( LrTable.NT 11, ( result, RatExp1left, RatExp1right), rest671)

end
|  ( 27, ( ( _, ( _, TT1left, TT1right)) :: rest671)) => let val  
result = MlyValue.BoolExp (fn _ => (AST.TT))
 in ( LrTable.NT 12, ( result, TT1left, TT1right), rest671)
end
|  ( 28, ( ( _, ( _, FF1left, FF1right)) :: rest671)) => let val  
result = MlyValue.BoolExp (fn _ => (AST.FF))
 in ( LrTable.NT 12, ( result, FF1left, FF1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.BoolExp BoolExp2, _, BoolExp2right)) :: _
 :: ( _, ( MlyValue.BoolExp BoolExp1, BoolExp1left, _)) :: rest671))
 => let val  result = MlyValue.BoolExp (fn _ => let val  (BoolExp as 
BoolExp1) = BoolExp1 ()
 val  BoolExp2 = BoolExp2 ()
 in (AST.Binopr_bool(AST.Or, BoolExp , BoolExp))
end)
 in ( LrTable.NT 12, ( result, BoolExp1left, BoolExp2right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.BoolExp BoolExp2, _, BoolExp2right)) :: _
 :: ( _, ( MlyValue.BoolExp BoolExp1, BoolExp1left, _)) :: rest671))
 => let val  result = MlyValue.BoolExp (fn _ => let val  (BoolExp as 
BoolExp1) = BoolExp1 ()
 val  BoolExp2 = BoolExp2 ()
 in (AST.Binopr_bool(AST.And, BoolExp , BoolExp))
end)
 in ( LrTable.NT 12, ( result, BoolExp1left, BoolExp2right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.BoolExp BoolExp1, _, BoolExp1right)) :: ( _
, ( _, NOT1left, _)) :: rest671)) => let val  result = 
MlyValue.BoolExp (fn _ => let val  (BoolExp as BoolExp1) = BoolExp1 ()
 in (AST.Unopr_bool(AST.Not, BoolExp))
end)
 in ( LrTable.NT 12, ( result, NOT1left, BoolExp1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Eq, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 33, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Neq, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Lt, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 35, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Gt, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 36, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Leq, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 37, ( ( _, ( MlyValue.RatExp RatExp2, _, RatExp2right)) :: _ :: (
 _, ( MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let
 val  result = MlyValue.BoolExp (fn _ => let val  (RatExp as RatExp1)
 = RatExp1 ()
 val  RatExp2 = RatExp2 ()
 in (AST.relationalOpr(AST.Geq, RatExp , RatExp))
end)
 in ( LrTable.NT 12, ( result, RatExp1left, RatExp2right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let val  
result = MlyValue.RatExp (fn _ => let val  (RatExp as RatExp1) = 
RatExp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.Binopr(AST.Add, RatExp , Term))
end)
 in ( LrTable.NT 13, ( result, RatExp1left, Term1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.Term Term1, _, Term1right)) :: _ :: ( _, ( 
MlyValue.RatExp RatExp1, RatExp1left, _)) :: rest671)) => let val  
result = MlyValue.RatExp (fn _ => let val  (RatExp as RatExp1) = 
RatExp1 ()
 val  (Term as Term1) = Term1 ()
 in (AST.Binopr(AST.Sub, RatExp , Term))
end)
 in ( LrTable.NT 13, ( result, RatExp1left, Term1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.Term Term1, Term1left, Term1right)) :: 
rest671)) => let val  result = MlyValue.RatExp (fn _ => let val  (Term
 as Term1) = Term1 ()
 in (Term)
end)
 in ( LrTable.NT 13, ( result, Term1left, Term1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.Unit Unit1, _, Unit1right)) :: _ :: ( _, ( 
MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  result = 
MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Unit as Unit1) = Unit1 ()
 in (AST.Binopr(AST.Mul, Term , Unit))
end)
 in ( LrTable.NT 14, ( result, Term1left, Unit1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.Unit Unit1, _, Unit1right)) :: _ :: ( _, ( 
MlyValue.Term Term1, Term1left, _)) :: rest671)) => let val  result = 
MlyValue.Term (fn _ => let val  (Term as Term1) = Term1 ()
 val  (Unit as Unit1) = Unit1 ()
 in (AST.Binopr(AST.Div, Term , Unit))
end)
 in ( LrTable.NT 14, ( result, Term1left, Unit1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.Unit Unit1, Unit1left, Unit1right)) :: 
rest671)) => let val  result = MlyValue.Term (fn _ => let val  (Unit
 as Unit1) = Unit1 ()
 in (Unit)
end)
 in ( LrTable.NT 14, ( result, Unit1left, Unit1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.Unit (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (AST.Int (INT))
end)
 in ( LrTable.NT 15, ( result, INT1left, INT1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Parser_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ADD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun MUL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun TT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun RATIONAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun INTEGER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOLEAN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun PROCEDURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
end
end
