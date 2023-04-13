signature BIGINT = 
sig
type bigint
exception bigint_error
val make_bigint: string -> bigint
val fromInt: int -> bigint
val equal : bigint * bigint -> bool (* equality *)
val less : bigint * bigint -> bool (* less than *)
val add : bigint * bigint -> bigint (* addition *)
val sub : bigint * bigint -> bigint (* subtraction *)
val showBigint : bigint -> string
val mul : bigint * bigint -> bigint (* multiplication *)
val max : bigint * bigint -> bigint
val min : bigint * bigint -> bigint
val abs : bigint -> bigint
val sign : bigint -> int
val modulo : bigint * bigint -> bigint option
val neg : bigint -> bigint
val zero : bigint
val divide : bigint * bigint -> bigint option
val printer : bigint -> string
val toString : bigint -> string
val fromString : string -> bigint
end;


structure Bigint :> BIGINT =
struct
(* pair of sign and digits 0 - positive 1 - negative  *)
type bigint = int * int list
exception bigint_error 
fun first (x, _) = x
fun second (_, y) = y
fun make_bigint (s: string) = 
   let
         fun helper [] = []
            | helper (c::cs) = if Char.isDigit c then (Char.ord c - 48) :: helper cs else raise bigint_error
   in
      if String.size s = 0 then (0, [0])
      else if String.substring (s, 0, 1) = "~" then (1, helper (String.explode (String.substring (s, 1, String.size s - 1))))
      else if String.substring (s, 0, 1) = "+" then (0, helper (String.explode (String.substring (s, 1, String.size s - 1))))
      else (0, helper (String.explode s))
   end



fun fromInt (i: int) = 
   let
       fun helper 0 = []
          | helper i = helper (i div 10) @ [i mod 10]
   in
      if i < 0 then (1, helper (0 - i))
      else (0, helper i)
   end

fun removeLeadingZeros(a:bigint) = 
   let 
      val digits = #2 a
      fun helper [] = [0]
         | helper [0] = [0]
         | helper (x::xs) = if x = 0 then helper xs else x::xs
   in
      (first a, helper digits)
   end

fun equal (a:bigint, b:bigint) = 
   let 
      val a2 = removeLeadingZeros a
      val b2 = removeLeadingZeros b
   in
      if (second a2) = (second b2) then 
         if second(a2) = [0] then 
            true
         else 
            if first a2 = first b2 then true else false
      else false
   end

fun lessList (a:int list, b:int list) = 
   let
      val lenA = List.length a
      val lenB = List.length b

      fun helper [] [] = false
         | helper [] (y::ys) = true
         | helper (x::xs) [] = false
         | helper (x::xs) (y::ys) = 
            if x < y then true
            else if x > y then false
            else helper xs ys
   in
      if lenA < lenB then true
      else if lenA > lenB then false
      else helper a b
   end
   
fun less (a:bigint, b:bigint) =
   if #1 a = #1 b then 
      if #1 a = 0 then lessList (#2 a, #2 b)
      else lessList (#2 b, #2 a)
   else if #1 a = 0 then false
   else true

fun addList (a:int list, b:int list) = 
   let
       fun helper [] [] 0 = []
          | helper [] [] c = [c]
          | helper [] (y::ys) c = 
               if y + c < 10 then helper [] ys 0 @ [y + c]
               else helper [] ys 1 @ [(y + c) mod 10]
            | helper (x::xs) [] c = 
               if x + c < 10 then helper xs [] 0 @ [x + c]
               else helper xs [] 1 @ [(x + c) mod 10]
            | helper (x::xs) (y::ys) c =
               if x + y + c < 10 then helper xs ys 0 @ [x + y + c]
               else helper xs ys 1 @ [(x + y + c) mod 10]
   in
      helper (List.rev a) (List.rev b) 0
   end

fun subList (a: int list, b: int list) = 
   let
       fun helper [] [] 0 = []
          | helper [] [] c = [c]
          | helper [] (y::ys) c = 
               if y - c >= 0 then helper [] ys 0 @ [y - c]
               else helper [] ys 1 @ [(y - c + 10) mod 10]
            | helper (x::xs) [] c = 
               if x - c >= 0 then helper xs [] 0 @ [x - c]
               else helper xs [] 1 @ [(x - c + 10) mod 10]
            | helper (x::xs) (y::ys) c =
               if x - y - c >= 0 then helper xs ys 0 @ [x - y - c]
               else helper xs ys 1 @ [(x - y - c + 10) mod 10]
   in
      helper (List.rev a) (List.rev b) 0
   end

fun mulList (a : int list, b : int list) =
    let
        val xs = List.rev a
        val ys =  b
        fun addZeros (zs, n) = if n = 0 then zs else addZeros ((0 :: zs), n - 1)
        
        fun multDigit (xs : int list, y : int) : int list =
            let
                fun multDigitHelper ([], carry) = if carry = 0 then [] else [carry]
                  | multDigitHelper ((x :: xs), carry) =
                    let
                        val product = x * y + carry
                        val digit = product mod 10
                        val newCarry = product div 10
                        val rest = multDigitHelper (xs, newCarry)
                    in
                        digit :: rest
                    end
                val result = multDigitHelper (xs, 0)
            in
                result
            end
        
        fun addLists (a:int list, b:int list) = 
         let
            fun helper [] [] 0 = []
               | helper [] [] c = [c]
               | helper [] (y::ys) c = 
                     if y + c < 10 then helper [] ys 0 @ [y + c]
                     else helper [] ys 1 @ [(y + c) mod 10]
                  | helper (x::xs) [] c = 
                     if x + c < 10 then helper xs [] 0 @ [x + c]
                     else helper xs [] 1 @ [(x + c) mod 10]
                  | helper (x::xs) (y::ys) c =
                     if x + y + c < 10 then helper xs ys 0 @ [x + y + c]
                     else helper xs ys 1 @ [(x + y + c) mod 10]
            
            val tmp = helper a b 0
         in
            List.rev tmp
         end
        
        fun multLists ([], ys) = [0]
          | multLists (xs, []) = [0]
          | multLists (xs, (y :: ys)) =
            let
                val digitResult = multDigit (xs, y)
                val shiftedResult = addZeros (digitResult, (length ys))
                val restResult = multLists (xs, ys)
                val totalResult = addLists (shiftedResult, restResult)
            in
               totalResult
            end
        
        val result = multLists (xs, ys)
    in
        List.rev result
    end

fun divList (xs : int list, ys : int list) =
    let
        fun addZeros (zs, n) = if n = 0 then zs else addZeros ((0 :: zs), n - 1)
        fun removeLeadingZeros zs =
            case zs of
                0 :: rest => removeLeadingZeros rest
              | _ => zs
        
        fun compare (xs : int list) (ys : int list) =
            let
                val xs = removeLeadingZeros xs
                val ys = removeLeadingZeros ys
            in
                if length xs > length ys then GREATER
                else if length xs < length ys then LESS
                else compareHelper xs ys
            end
        
        and compareHelper [] [] = EQUAL
          | compareHelper [] ys = LESS
          | compareHelper xs [] = GREATER
          | compareHelper (x :: xs) (y :: ys) =
            if x > y then GREATER
            else if x < y then LESS
            else compareHelper xs ys
        
        fun subList (a: int list, b: int list) = 
            let
                fun helper [] [] 0 = []
                    | helper [] [] c = [c]
                    | helper [] (y::ys) c = 
                        if y - c >= 0 then helper [] ys 0 @ [y - c]
                        else helper [] ys 1 @ [(y - c + 10) mod 10]
                        | helper (x::xs) [] c = 
                        if x - c >= 0 then helper xs [] 0 @ [x - c]
                        else helper xs [] 1 @ [(x - c + 10) mod 10]
                        | helper (x::xs) (y::ys) c =
                        if x - y - c >= 0 then helper xs ys 0 @ [x - y - c]
                        else helper xs ys 1 @ [(x - y - c + 10) mod 10]
            in
                helper (List.rev a) (List.rev b) 0
            end

        fun findDigit (x: int list, y : int list) : int = 
            let
                fun helper (x: int list, y : int list, n : int) : int = 
                    let
                        val cmp = compare x y
                    in
                        case cmp of
                            LESS => n
                          | EQUAL => n + 1
                          | GREATER => helper (subList(x,y), y, n + 1)
                    end
            in
                helper (x, y, 0)
            end
        
        fun splitList(x: int list) (n : int) = 
            let
                val result1 = ref []
                val result2 = ref []

                fun helper [] _ = ()
                | helper (a : int list) (n : int) = 
                    let val x = hd a
                        val xs = tl a
                    in
                        if (n = 0) then (result2 := a)
                        else (result1 := (!result1)@[x]; helper xs (n-1))
                    end
            in 
            helper x n;
            (!result1, !result2)
            end
        
    
        fun divideHelper (xs : int list) (ys : int list) =
            let
                val ans = ref []
                val remn = ref []
                val cmp = compare xs ys
                fun rec_remaining (rem, a, b) = 
                    if a = [] then (
                        remn := rem; 
                        ()
                    )
                    else
                        let
                            val rem = rem @ [hd a]
                            val cmp = compare rem b
                            val dig = findDigit (rem, b)
                        in
                        (
                            ans := (!ans)@[dig];
                            rec_remaining (subList (rem, (mulList (b, [dig]))), tl a, b )
                        )
                        end
                fun firstpart(xs, ys) = 
                    let
                        val l1 = length ys
                        val (a, b) = splitList xs l1
                        val cmp = compare a ys
                    in
                        if cmp = LESS then
                            let
                                val (a, b) = splitList xs (l1 + 1)
                                val dig = findDigit (a, ys)
                                val new_a = subList (a, (mulList (ys, [dig])))
                            in
                                (
                                    ans := (!ans)@[dig];
                                    rec_remaining (new_a, b, ys)
                                )
                            end
                        else
                            let
                                val dig = findDigit (a, ys)
                                val new_a = subList (a, (mulList (ys, [dig])))
                            in
                                (
                                    ans := (!ans)@[dig];
                                    rec_remaining (new_a, b, ys)
                                )
                            end
                    end

                
            in
                case cmp of
                    LESS => ([0], xs)
                  | EQUAL => ([1], [0])
                  | GREATER => (
                        firstpart(xs, ys);
                            (!ans, !remn)
                        )
            end
    in
        divideHelper xs ys
    end









fun add (a:bigint, b:bigint) = 
   let
      fun helper (a, b) = 
         if first(a) = first(b) then (first(a), addList (second(a), second(b)))
         else if first(a) = 0 then 
            if lessList (second(a), second(b)) then (1, subList (second(b), second(a)))
            else (0, subList (second(a), second(b)))
         else if first(b) = 0 then 
            if lessList (second(b), second(a)) then (1, subList (second(a), second(b)))
            else (0, subList (second(b), second(a)))
         else raise bigint_error
      
   in
      helper (a, b)
   end

fun sub (a:bigint, b:bigint) =
   let
      fun helper (a, b) = 
         if first(a) = first(b) then 
            if first(a) = 0 then 
               if lessList (second(a), second(b)) then (1, subList (second(b), second(a)))
               else (0, subList (second(a), second(b)))
            else 
               if lessList (second(b), second(a)) then (1, subList (second(a), second(b)))
               else (0, subList (second(b), second(a)))
         else if first(a) = 0 then (0, addList (second(a), second(b)))
         else if first(b) = 0 then (1, addList (second(a), second(b)))
         else raise bigint_error
   in
      helper (a, b)
   end


fun mul (a:bigint, b:bigint) = 
   let
      fun helper (a, b) = 
         if first(a) = first(b) then (0, mulList (second(a), second(b)))
         else (1, mulList (second(a), second(b)))
   in
      helper (a, b)
   end

fun to_bigint_option(a: bigint) = SOME a


fun divideR (x:bigint, y:bigint):bigint option * bigint option = 
let 
   val a = removeLeadingZeros x
   val b = removeLeadingZeros y
in
   if (second b) = [0] then raise bigint_error
   else if second(a) = [0] then (to_bigint_option(0, [0]), to_bigint_option(0, [0]))
   else if second(b) = [1] then (to_bigint_option(a), to_bigint_option(0, [0]))
   else
   let
      val (d, r) = divList (second(a), second(b))
   in
      if first(a) = first(b) then (to_bigint_option(0, d), to_bigint_option(0, r))
      else (to_bigint_option(1, d), to_bigint_option(0, r))
   end
end

fun divide(a:bigint, b:bigint):bigint option = first(divideR(a, b))
fun modulo(a:bigint, b:bigint):bigint option = second(divideR(a, b))




fun showBigint (a:bigint) =
   let
       fun helper [] = ""
          | helper (x::xs) =  (Int.toString x) ^ (helper xs)
      fun removeZeros [] = []
         | removeZeros (x::xs) = if x = 0 then removeZeros xs else x::xs
      fun helper2 [] = "0"
         | helper2 (x::xs) = helper (x::xs)
   in
      if #1 a = 0 then helper2 (removeZeros (#2 a))
      else "~" ^ helper2 (removeZeros (#2 a))
   end

fun printer(a :bigint) =
   let 
      val b = second a
      fun helper [] = ""
      | helper (x::xs) =  (Int.toString x) ^ (helper xs)
   in 
      helper (b)
   end


fun max (a:bigint, b:bigint) = 
   if less (a, b) then b
   else a

fun min (a:bigint, b:bigint) =
   if less (a, b) then a
   else b

fun abs (a:bigint) =
   if #1 a = 0 then a
   else (0, #2 a)

fun sign (a:bigint) =
   if #1 a = 0 then 0
   else 1


(* fun modulo (a: bigint, b: bigint) = 
   let
      fun helper (a, b) = 
         if less (a, b) then a
         else helper (sub (a, b), b)
   in
      helper (a, b)
   end *)


fun neg (a: bigint) = 
   if #1 a = 0 then (1, #2 a)
   else (0, #2 a)

val zero = fromInt (0)

fun toString (a: bigint) = showBigint a

fun fromString (a: string) = make_bigint a

end;
