structure R = Rational(Bigint)

structure AST = 
struct 
datatype exp = Int of R.rational
| Binopr of binop * exp * exp

and binop = Add | Sub | Mul | Div 

fun eval (exp) = 
case exp of 
Int i => i
| Binopr (Add, e1, e2) => R.add (eval e1, eval e2)
| Binopr (Sub, e1, e2) => R.subtract (eval e1, eval e2)
| Binopr (Mul, e1, e2) => R.multiply (eval e1, eval e2)
| Binopr (Div, e1, e2) => valOf(R.divide(eval e1, eval e2))


end