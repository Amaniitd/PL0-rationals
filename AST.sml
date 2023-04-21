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

and stable = symbols of symbol * stable | symbol of symbol | emptyStable

and symbol = Symbol of string * typ * value

and typ = Rat | Bool | IntT | Proc
and value = RatVal of R.rational | BoolVal of bool | IntVal of int | ProcVal of Block


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
   in
      case scopeSt of
         [] => raise Fail("Symbol not found | Scope stack is empty")
      | scope::scopes => if isInScope(s,scope) then lookupInScope(s,scope) else lookup(s,scopes)
   end

fun update(s:string, v:value, scopeStack) =
   let
      fun typeOf (v:value) =
         case v of
         RatVal(r) => Rat
         | BoolVal(b) => Bool
         | IntVal(i) => IntT
         | ProcVal(b) => Proc
      fun checkType (s:string, v:value, scope:stable) =
         case scope of
         symbols(Symbol(s',t,v'),scope') => if s = s' then if t = typeOf(v) then true else raise Fail("Type mismatch") else checkType(s,v,scope')
         | symbol(Symbol(s',t,v')) => if s = s' then if t = typeOf(v) then true else raise Fail("Type mismatch") else raise Fail("Symbol not found | Scope stack is empty")
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
   in
      case !scopeStack of
         [] => raise Fail("Symbol not found | Scope stack is empty")
      | scope::scopes => if isInScope(s,scope) then scopeStack := updateInScope(s,v,scope)::scopes else update(s,v,ref scopes)

   end

fun addSymbol(s:string, t:typ, v:value, scopeStack) =
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

fun newScope (scopeStack) = scopeStack := emptyStable::(!scopeStack)

fun defineRatDecl (var, scopeStack) = addSymbol(var, Rat, RatVal(R.zero), scopeStack)
fun defineBoolDecl (var, scopeStack) = addSymbol(var, Bool, BoolVal(false), scopeStack)
fun defineIntDecl (var, scopeStack) = addSymbol(var, IntT, IntVal(0), scopeStack)

fun defineRatDecls (RatVars(name, v), scopeStack) = (defineRatDecl(name, scopeStack); defineRatDecls(v, scopeStack))
   | defineRatDecls (RatVar(name), scopeStack) = defineRatDecl(name, scopeStack)

fun defineBoolDecls (BoolVars(name, v), scopeStack) = (defineBoolDecl(name, scopeStack); defineBoolDecls(v, scopeStack))
   | defineBoolDecls (BoolVar(name), scopeStack) = defineBoolDecl(name, scopeStack)

fun defineIntDecls (IntVars(name, v), scopeStack) = (defineIntDecl(name, scopeStack); defineIntDecls(v, scopeStack))
   | defineIntDecls (IntVar(name), scopeStack) = defineIntDecl(name, scopeStack)



fun defineVarDecls (v, scopeStack) =
   case v of
   ratDecls(vars) => defineRatDecls(vars,scopeStack)
   | boolDecls(vars) => defineBoolDecls(vars,scopeStack)
   | intDecls(vars) => defineIntDecls(vars,scopeStack)
   | ratDecl(var) => defineRatDecls(var,scopeStack)
   | boolDecl(var) => defineBoolDecls(var,scopeStack)
   | intDecl(var) => defineIntDecls(var,scopeStack)

fun defineProc (ProcDef(name, blk), scopeStack) = addSymbol(name, Proc, ProcVal(blk), scopeStack)

fun defineDecls (decls, scopeStack) =
   let
      fun defineDecls' (decls) =
         case decls of
         varDecls(vars,decls') => (defineVarDecls(vars,scopeStack); defineDecls'(decls'))
         | procDecls(proc,decls') => (defineProc(proc,scopeStack); defineDecls'(decls'))
         | procDecl(proc) => defineProc(proc,scopeStack)
         | varDecl(vars) => defineVarDecls(vars,scopeStack)
         | emptyDecls => ()
   in
      defineDecls'(decls)
   end

fun evalStmt(stmt, scopeStack) = "howdy"


fun printScopeStack (scopeStack) =
   let
      fun printType (t:typ) =
         case t of
         Rat => print("Rat")
         | Bool => print("Bool")
         | IntT => print("Int")
         | Proc => print("Proc")
      
      fun printValue (v:value) =
         case v of
         RatVal(r) => print("rat")
         | BoolVal(b) => if b then print("true") else print("false")
         | IntVal(i) => print("int")
         | ProcVal(b) => print("ProcVal")
      fun printScope (scope:stable) =
         case scope of
         symbols(Symbol(s,t,v),scope') => (print(s); print(" "); printType(t); print(" "); printValue(v); print(" "); printScope(scope'))
         | symbol(Symbol(s,t,v)) => (print(s); print(" "); printType(t); print(" "); printValue(v); print(" "))
         | emptyStable => ()
   in
      case !scopeStack of
         [] => ()
      | scope::scopes => (printScope(scope); printScopeStack(ref scopes))
   end




fun isEqual(v1:value, v2:value):bool =
   case v1 of
   RatVal(r1) => (case v2 of
                  RatVal(r2) => R.equal(r1,r2)
                  | _ => false)
   | BoolVal(b1) => (case v2 of
                     BoolVal(b2) => b1 = b2
                     | _ => false)
   | IntVal(i1) => (case v2 of
                    IntVal(i2) => i1 = i2
                    | _ => false)


fun evalBln (b:BoolExp, scopeStack):value =
   case b of
   TT => BoolVal(true)
   | FF => BoolVal(false)
   | Unopr_bool(Not, bln) => 
      if isEqual(evalBln(bln, scopeStack), BoolVal(true)) then
         BoolVal(false)
      else
         BoolVal(true)
   | Binopr_bool(And, bln1, bln2) => BoolVal(isEqual(evalBln(bln1, scopeStack), BoolVal(true)) andalso isEqual(evalBln(bln2, scopeStack), BoolVal(true)))
   | Binopr_bool(Or, bln1, bln2) => BoolVal(isEqual(evalBln(bln1, scopeStack), BoolVal(true)) orelse isEqual(evalBln(bln2, scopeStack), BoolVal(true)))
   (* | relationalOpr(Eq, exp1, exp2) => BoolVal(isEqual(evalExp(exp1, scopeStack), evalExp(exp2, scopeStack)))
   | relationalOpr(Neq, exp1, exp2) => BoolVal(not(isEqual(evalExp(exp1, scopeStack), evalExp(exp2, scopeStack)))) *)




fun evalBlock ((Block(decls,stmts)), scopeStack) =
   let
      fun evalBln (b, scopeStack) =
         case b of
         TT => BoolVal(true)
         | FF => BoolVal(false)
         | Unopr_bool(Not, bln) => case evalBln(bln, scopeStack) of
                                    BoolVal(true) => BoolVal(false)
                                    | BoolVal(false) => BoolVal(true)
                                    | _ => raise Fail("Type mismatch")
         (* | Binopr_bool(And, bln1, bln2) => case (evalBln(bln1, scopeStack), evalBln(bln2, scopeStack)) of
                                    (BoolVal(true), BoolVal(true)) => BoolVal(true)
                                    | _ => BoolVal(false)
         | Binopr_bool(Or, bln1, bln2) => case (evalBln(bln1, scopeStack), evalBln(bln2, scopeStack)) of
                                    (BoolVal(false), BoolVal(false)) => BoolVal(false)
                                    | _ => BoolVal(true) *)
         (* | relationalOpr(Eq, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(R.eq(r1,r2))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(i1 = i2)
                                    | (BoolVal(b1), BoolVal(b2)) => BoolVal(b1 = b2)
                                    | _ => raise Fail("Type mismatch")
         | relationalOpr(Neq, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(not(R.eq(r1,r2)))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(not(i1 = i2))
                                    | (BoolVal(b1), BoolVal(b2)) => BoolVal(not(b1 = b2))
                                    | _ => raise Fail("Type mismatch")
         | relationalOpr(Lt, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(R.lt(r1,r2))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(i1 < i2)
                                    | _ => raise Fail("Type mismatch")
         | relationalOpr(Gt, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(R.gt(r1,r2))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(i1 > i2)
                                    | _ => raise Fail("Type mismatch")
         | relationalOpr(Leq, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(R.leq(r1,r2))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(i1 <= i2)
                                    | _ => raise Fail("Type mismatch")
         | relationalOpr(Geq, e1, e2) => case (evalExpr(e1, scopeStack), evalExpr(e2, scopeStack)) of
                                    (RatVal(r1), RatVal(r2)) => BoolVal(R.geq(r1,r2))
                                    | (IntVal(i1), IntVal(i2)) => BoolVal(i1 >= i2)
                                    | _ => raise Fail("Type mismatch") *)
   
   
      (* fun evalIfCmd (If(bln, cmds1, cmds2)) =
         case evalBln(bln, scopeStack) of
         BoolVal(true) => evalCmds(cmds1, scopeStack)
         | BoolVal(false) => evalCmds(cmds2, scopeStack)
         | _ => raise Fail("Type mismatch")
      
      fun evalWhileCmd(While(bln, cmds)) =
         case evalBln(bln, scopeStack) of
         BoolVal(true) => (evalCmds(cmds, scopeStack); evalWhileCmd(While(bln, cmds)))
         | BoolVal(false) => ()
         | _ => raise Fail("Type mismatch") *)

                                          


   in
      newScope(scopeStack);
      defineDecls(decls,scopeStack);
      printScopeStack(scopeStack)
   end


end

   