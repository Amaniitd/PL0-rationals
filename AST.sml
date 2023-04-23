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
and ifCmd = If of exp * cmds * cmds
and whileCmd = While of exp * cmds

and exp = TT | FF | Unopr_bool of unop_bool * exp
| Binopr_bool of binop_bool * exp * exp
| relationalOpr of relational_op * exp * exp
| Int of R.rational
| Binopr of binop * exp * exp
| UnRatOpr of unoprat * exp
| Var of string


and binop_bool = And | Or 
and unop_bool = Not
and relational_op = Eq | Neq | Lt | Gt | Leq | Geq

and unoprat = Inverse 

and binop = Add | Sub | Mul | Div | Mod

and stable = symbols of symbol * stable | symbol of symbol | emptyStable

and symbol = Symbol of string * typ * value

and typ = Rat | Bool | IntT | Proc
and value = RatVal of R.rational | BoolVal of bool | IntVal of R.rational | ProcVal of Block


val scopeStack = ref ([]:stable list)

fun pushScope(s:stable) = scopeStack := s::(!scopeStack)
fun popScope() = scopeStack := tl(!scopeStack)

fun lookup(s:string, scopeSt) =
   let
      fun lookupInScope(s:string, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v),scope') => if s = s' then v else lookupInScope(s,scope')
         | symbol(Symbol(s',t,v)) => if s = s' then v else raise Fail("Symbol not found | Scope stack is empty")
         | emptyStable => raise Fail("Symbol not found | Scope stack is empty")
      
      fun isInScope (s:string, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v),scope') => if s = s' then true else isInScope(s,scope')
         | symbol(Symbol(s',t,v)) => if s = s' then true else false
         | emptyStable => false
      
      fun globalScope (scopeSt:stable list): stable =
         case scopeSt of
         [] => raise Fail("Symbol not found | Scope stack is empty")
         | scope::[] => scope
         | scope::scopes => globalScope(scopes)
      
   in
      case scopeSt of
         [] => raise Fail("Symbol not found | Scope stack is empty")
      | scope::scopes => 
         if isInScope(s,scope) then lookupInScope(s,scope) 
         else 
            if isInScope(s,globalScope(scope::scopes)) then lookupInScope(s,globalScope(scope::scopes))
            else raise Fail("Symbol not found | Scope stack is empty")
   end

fun update(s:string, v:value) =
   let
      fun isSameType (t1:typ, t2:typ) =
         case t1 of
         Rat => if t2 = Rat orelse t2 = IntT then true else false
         | Bool => if t2 = Bool then true else false
         | IntT => if t2 = IntT orelse t2 = Rat then true else false
         | Proc => if t2 = Proc then true else false

      fun typeOf (v:value):typ =
         case v of
         RatVal(r) => Rat
         | BoolVal(b) => Bool
         | IntVal(i) => IntT
         | ProcVal(b) => Proc

      fun checkType (s:string, v:value, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v'),scope') => if s = s' then if isSameType(t, typeOf(v)) then true else raise Fail("Type mismatch") else checkType(s,v,scope')
         | symbol(Symbol(s',t,v')) => if s = s' then if isSameType(t, typeOf(v)) then true else raise Fail("Type mismatch") else raise Fail("Symbol not found | Scope stack is empty")
         | emptyStable => raise Fail("Symbol not found | Scope stack is empty")

      fun updateInScope(s:string, v:value, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v'),scope') => 
         if s = s' then
            if checkType(s,v,scope) then symbols(Symbol(s',t,v),scope') 
            else raise Fail("Type mismatch")
         else symbols(Symbol(s',t,v'),updateInScope(s,v,scope'))
         | symbol(Symbol(s',t,v')) =>
         if s = s' then
            if checkType(s,v,scope) then symbol(Symbol(s',t,v)) 
            else raise Fail("Type mismatch")
         else raise Fail("Symbol not found | Scope stack is empty")

      fun isInScope (s:string, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v),scope') => if s = s' then true else isInScope(s,scope')
         | symbol(Symbol(s',t,v)) => if s = s' then true else false
         | emptyStable => false
      
      fun globalScope (scopeSt:stable list): stable =
         case scopeSt of
         [] => raise Fail("Symbol not found | Scope stack is empty")
         | scope::[] => scope
         | scope::scopes => globalScope(scopes)
      
      fun updateGlobalScope (s:string, v:value, scopeSt:stable list): stable list =
         case scopeSt of
         [] => raise Fail("Symbol not found | Scope stack is empty")
         | scope::[] => updateInScope(s,v,scope)::[]
         | scope::scopes => scope::(updateGlobalScope(s,v,scopes))
      
   in
      case !scopeStack of
         [] => raise Fail("Symbol not found | Scope stack is empty")
      | scope::scopes => 
         if isInScope(s,scope) then scopeStack := updateInScope(s,v,scope)::scopes
         else if isInScope(s,globalScope(scope::scopes)) then 
            scopeStack := updateGlobalScope(s,v,scope::scopes)
         else raise Fail("Symbol not found | Scope stack is empty")
   end

fun addSymbol(s:string, t:typ, v:value) =
   let
      fun addSymbolToScope(s:string, t:typ, v:value, scope:stable) =
         case scope of
         symbols(Symbol(s',t',v'),scope') => symbols(Symbol(s',t',v'),addSymbolToScope(s,t,v,scope'))
         | symbol(Symbol(s',t',v')) => symbols(Symbol(s',t',v'),symbol(Symbol(s,t,v)))
         | emptyStable => symbol(Symbol(s,t,v))
   in
      case !scopeStack of
         [] => raise Fail("Symbol not found | Scope stack is empty")
      | scope::scopes => scopeStack := addSymbolToScope(s,t,v,scope)::scopes
   end

fun newScope () = scopeStack := emptyStable::(!scopeStack)

fun defineRatDecl (var) = addSymbol(var, Rat, RatVal(R.zero))
fun defineBoolDecl (var) = addSymbol(var, Bool, BoolVal(false))
fun defineIntDecl (var) = addSymbol(var, IntT, IntVal(R.zero))

fun defineRatDecls (RatVars(name, v)) = (defineRatDecl(name); defineRatDecls(v))
   | defineRatDecls (RatVar(name)) = defineRatDecl(name)

fun defineBoolDecls (BoolVars(name, v)) = (defineBoolDecl(name); defineBoolDecls(v))
   | defineBoolDecls (BoolVar(name)) = defineBoolDecl(name)

fun defineIntDecls (IntVars(name, v)) = (defineIntDecl(name); defineIntDecls(v))
   | defineIntDecls (IntVar(name)) = defineIntDecl(name)



fun defineVarDecls (v) =
   case v of
   ratDecls(vars) => defineRatDecls(vars)
   | boolDecls(vars) => defineBoolDecls(vars)
   | intDecls(vars) => defineIntDecls(vars)
   | ratDecl(var) => defineRatDecls(var)
   | boolDecl(var) => defineBoolDecls(var)
   | intDecl(var) => defineIntDecls(var)

fun defineProc (ProcDef(name, blk)) = addSymbol(name, Proc, ProcVal(blk))

fun defineDecls (decls) =
   let
      fun defineDecls' (decls) =
         case decls of
         varDecls(vars,decls') => (defineVarDecls(vars); defineDecls'(decls'))
         | procDecls(proc,decls') => (defineProc(proc); defineDecls'(decls'))
         | procDecl(proc) => defineProc(proc)
         | varDecl(vars) => defineVarDecls(vars)
         | emptyDecls => ()
   in
      defineDecls'(decls)
   end


fun printValue(v:value) = case v of
   IntVal(i) => print(R.showInt(i)^"\n")
   | RatVal(r) => print(R.showDecimal(r)^"\n")
   | BoolVal(b) => if b then print("tt\n") else print("ff\n")
   | ProcVal(b) => print("cannot print proc\n")


fun printScopeStack () =
   let
      fun printType (t:typ) =
         case t of
         Rat => print("Rat")
         | Bool => print("Bool")
         | IntT => print("Int")
         | Proc => print("Proc")
      
      fun printValue (v:value) =
         case v of
         RatVal(r) => print(R.showRat(r))
         | BoolVal(b) => if b then print("true") else print("false")
         | IntVal(i) => print(R.showRat(i))
         | ProcVal(b) => print("ProcVal")
      fun printScope (scope:stable) =
         case scope of
         symbols(Symbol(s,t,v),scope') => (print(s); print(" "); printType(t); print(" "); printValue(v); print(" "); printScope(scope'); print("\n"))
         | symbol(Symbol(s,t,v)) => (print(s); print(" "); printType(t); print(" "); printValue(v); print("\n"))
         | emptyStable => ()
      
      fun helper(st) = case st of
         [] => ()
      | scope::scopes => (printScope(scope); helper(scopes))
   in
      helper(!scopeStack)
   end

fun cast_Rat (v:value) =
   case v of
   RatVal(r) => r
   | BoolVal(b) => raise Fail("Cannot cast Bool to Rat")
   | IntVal(i) => i
   | ProcVal(b) => raise Fail("Cannot cast Proc to Rat")

fun cast_Bool (v:value) =
   case v of
   RatVal(r) => raise Fail("Cannot cast Rat to Bool")
   | BoolVal(b) => b
   | IntVal(i) => raise Fail("Cannot cast Int to Bool")
   | ProcVal(b) => raise Fail("Cannot cast Proc to Bool")

fun isEqual (v1:value, v2:value) =
   case v1 of
   RatVal(r1) => (case v2 of
      RatVal(r2) => R.equal(r1,r2)
      | BoolVal(b2) => raise Fail("Cannot compare Rat to Bool")
      | IntVal(i2) => R.equal(r1, i2)
      | ProcVal(b2) => raise Fail("Cannot compare Rat to Proc"))
   | BoolVal(b1) => (case v2 of
      RatVal(r2) => raise Fail("Cannot compare Bool to Rat")
      | BoolVal(b2) => b1 = b2
      | IntVal(i2) => raise Fail("Cannot compare Bool to Int")
      | ProcVal(b2) => raise Fail("Cannot compare Bool to Proc"))
   
   | IntVal(i1) => (case v2 of
      RatVal(r2) => R.equal(i1, r2)
      | BoolVal(b2) => raise Fail("Cannot compare Int to Bool")
      | IntVal(i2) => R.equal(i1, i2)
      | ProcVal(b2) => raise Fail("Cannot compare Int to Proc"))

   | ProcVal(b1) => raise Fail("Cannot compare Proc to anything")

fun isLessThan (v1:value, v2:value) =
   case v1 of
   RatVal(r1) => 
   (case v2 of
      RatVal(r2) => R.less(r1,r2)
      | BoolVal(b2) => raise Fail("Cannot compare Rat to Bool")
      | IntVal(i2) => R.less(r1, i2)
      | ProcVal(b2) => raise Fail("Cannot compare Rat to Proc")
   )
   | IntVal (i1) =>
   (case v2 of
      RatVal(r2) => R.less(i1, r2)
      | BoolVal(b2) => raise Fail("Cannot compare Int to Bool")
      | IntVal(i2) => R.less(i1, i2)
      | ProcVal(b2) => raise Fail("Cannot compare Int to Proc")
   )
   | BoolVal(b1) => raise Fail("Cannot compare Bool to anything")
   | ProcVal(b1) => raise Fail("Cannot compare Proc to anything")


fun varType(s) = case lookup(s, !scopeStack) of
   RatVal(r) => Rat
   | BoolVal(b) => Bool
   | IntVal(i) => IntT
   | ProcVal(b) => Proc

fun evalBlock ((Block(decls,stmts))) =
   let
      fun evalCmds(cs) =
         let
            fun evalExp(expr:exp) =
            case expr of
            TT => BoolVal(true)
            | FF => BoolVal(false)
            | Int(r) => if R.isInt(r) then IntVal(r) else RatVal(r)
            | Var(s) => lookup(s, !scopeStack)
            | Binopr(Add, e1, e2) => RatVal(R.add(cast_Rat(evalExp(e1)), cast_Rat(evalExp(e2))))
            | Binopr(Sub, e1, e2) => RatVal(R.subtract(cast_Rat(evalExp(e1)), cast_Rat(evalExp(e2))))
            | Binopr(Mul, e1, e2) => RatVal(R.multiply(cast_Rat(evalExp(e1)), cast_Rat(evalExp(e2))))
            | Binopr(Div, e1, e2) => RatVal(valOf(R.divide(cast_Rat(evalExp(e1)), cast_Rat(evalExp(e2)))))
            | Binopr(Mod, e1, e2) => RatVal(valOf(R.modulo(cast_Rat(evalExp(e1)), cast_Rat(evalExp(e2)))))
            | Binopr_bool(And, e1, e2) => BoolVal(cast_Bool(evalExp(e1)) andalso cast_Bool(evalExp(e2)))
            | Binopr_bool(Or, e1, e2) => BoolVal(cast_Bool(evalExp(e1)) orelse cast_Bool(evalExp(e2)))
            | relationalOpr(Eq, e1, e2) => BoolVal(isEqual(evalExp(e1), evalExp(e2)))
            | relationalOpr(Lt, e1, e2) => BoolVal(isLessThan(evalExp(e1), evalExp(e2)))
            | relationalOpr(Gt, e1, e2) => BoolVal(isLessThan(evalExp(e2), evalExp(e1)))
            | relationalOpr(Leq, e1, e2) => BoolVal(isLessThan(evalExp(e1), evalExp(e2)) orelse isEqual(evalExp(e1), evalExp(e2)))
            | relationalOpr(Geq, e1, e2) => BoolVal(isLessThan(evalExp(e2), evalExp(e1)) orelse isEqual(evalExp(e1), evalExp(e2)))
            | relationalOpr(Ne, e1, e2) => BoolVal(not(isEqual(evalExp(e1), evalExp(e2))))
            | Unopr_bool (Not, e1) => BoolVal(not(cast_Bool(evalExp(e1))))
            | UnRatOpr (Inverse, e1) => RatVal(valOf(R.inverse(cast_Rat(evalExp(e1)))))


            fun evalIfCmd (If(bln, cmds1, cmds2)) =
               case evalExp(bln) of
               BoolVal(true) => evalCmds(cmds1)
               | BoolVal(false) => evalCmds(cmds2)
               | _ => raise Fail("Type mismatch")
            
            fun evalWhileCmd(While(bln, cmds1)) =
               case evalExp(bln) of
               BoolVal(true) => (evalCmds(cmds1); evalWhileCmd(While(bln, cmds1)))
               | BoolVal(false) => ()
               | _ => raise Fail("Type mismatch")

            fun evalPrintCmd(Print(exp)) = printValue(evalExp(exp))

            fun evalCallCmd (Call(s)) = 
               case lookup(s, !scopeStack) of
               ProcVal(b) => evalBlock(b)
               | _ => raise Fail("Type mismatch: " ^ s ^ " is not a procedure")
            
            fun evalReadCmd (Read(s)) = 
               let
                  val a = valOf (TextIO.inputLine TextIO.stdIn)
                  val b = String.substring(a, 0, String.size(a) - 1)
               in
                  if varType(s) = Rat then
                     update (s, RatVal(R.fromDecimal(b)))
                  else if varType(s) = IntT then
                     update (s, IntVal(valOf(R.rat(Bigint.make_bigint(b)))))
                  else if varType(s) = Bool then
                     if b = "tt" then
                        update (s, BoolVal(true))
                     else if b = "ff" then
                        update (s, BoolVal(false))
                     else 
                        raise Fail("Type mismatch: " ^ b ^ " is not a valid value")
                  else
                     raise Fail("Type mismatch: " ^ b ^ " is not a valid value")
               end

            fun evalCmd(cmdarg) = 
               case cmdarg of
               assignCmd(Assign(s, exp)) => update(s, evalExp(exp))
               | ifCmd(ifCmdarg) => evalIfCmd(ifCmdarg)
               | whileCmd(whileCmdarg) => evalWhileCmd(whileCmdarg)
               | printCmd(printCmdarg) => evalPrintCmd(printCmdarg)
               | callCmd(callCmdarg) => evalCallCmd(callCmdarg)
               | readCmd(readCmdarg) => evalReadCmd(readCmdarg)

         in
            case cs of
            cmds(cmd, cmds') => (evalCmd(cmd); evalCmds(cmds'))
            | command(cmd) => evalCmd(cmd)
            | emptyCmds => ()
         end
                              
   in
      newScope();
      defineDecls(decls);
      evalCmds(stmts);
      (* printScopeStack(); *)
      popScope()
   end




end

   