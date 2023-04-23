use "bigint.sml";

signature RATIONAL =
sig
type rational
type bigint
exception rat_error
val make_rat: bigint * bigint -> rational option
val rat: bigint -> rational option
val reci: bigint -> rational option
val neg: rational -> rational
val inverse : rational -> rational option
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val add : rational * rational -> rational (* addition *)
val subtract : rational * rational -> rational (* subtraction *)
val multiply : rational * rational -> rational (* multiplication *)
val divide : rational * rational -> rational option (* division *)
val showRat : rational -> string
val showDecimal : rational -> string
val fromDecimal : string -> rational
val toDecimal : rational -> string
val isInt : rational -> bool
val zero : rational
end;

(* 
• showRat displays a rational number in fractional-normal form.
• showDecimal displays the rational number in decimal-normal form.
• bigint is an integer of arbitrary size (not limited by the implementation) and could have a magnitude
much larger than the maxInt or much smaller than the minInt defined by the SML-NJ structure Int.
• fromDecimal may take a possibly non-standard decimal form and create a rational from it.
• toDecimal converts a rational number into its standard decimal representation.
• rat inputs an integer i and converts it into the rational i
1
.
• inverse finds the reciprocal of a non-zero rational.
• reci finds the reciprocal of a non-zero integer.
• make rat takes any two integers and creates a rational number fractional-normal form. Hence make rat
(4, ~10) = (~2,5).
• neg takes any rational number and negates it.
*)


functor Rational (Bigint: BIGINT) =
struct
type bigint = Bigint.bigint
type rational = bigint * bigint
exception rat_error

fun rational2option(a:rational) = SOME (a)
fun changetype(a:bigint * bigint) = (a: rational)
 
fun gcd (x, y) =
   let 
      val a = Bigint.abs(x)
      val b = Bigint.abs(y)
   in
      if Bigint.equal(b, Bigint.zero) then a
      else gcd(b, valOf(Bigint.modulo(a, b)))
   end

fun make_rat (a:bigint, b:bigint) =
   if Bigint.equal(b, Bigint.zero) then
      raise rat_error
   else rational2option((a, b))

fun rat a = make_rat(a, Bigint.fromInt 1)

fun reci a = if Bigint.equal(a, Bigint.zero) then raise rat_error else make_rat(Bigint.fromInt (1), a)

fun first (a, b) = a
fun second (a, b) = b

fun neg(a) = (Bigint.neg(first(a)), second(a))

fun inverse a =
   if Bigint.equal(first(a), Bigint.zero) then
      raise rat_error
   else make_rat(second(a), first(a))


fun fract_norm (a:rational):rational =
   let
      fun correct_sign (a) = 
         if Bigint.less(first(a), Bigint.zero) then
            if Bigint.less(second(a), Bigint.zero) then
               (Bigint.neg(first(a)), Bigint.neg(second(a)))
            else
               a
         else
            if Bigint.less(second(a), Bigint.zero) then
               (Bigint.neg(first(a)), Bigint.neg(second(a)))
            else
               a

      val x = correct_sign(a)
      val y = gcd(first(x), second(x))
      val n = valOf(Bigint.divide(first(x), y))
      val d = valOf(Bigint.divide(second(x), y))
   in 
      changetype(n, d)
   end


fun showRat (r: rational) = 
   let
      val a = fract_norm(r)
      val x = first(a)
      val y = second(a)
   in
      Bigint.showBigint(x) ^ "/" ^ Bigint.showBigint(y)
   end

fun add (a, b) = 
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
      val n = Bigint.add(Bigint.mul(x, v), Bigint.mul(u, y))
      val d = Bigint.mul(y, v)
   in
      fract_norm(n, d)
   end

fun subtract (a, b) =
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
      val n = Bigint.sub(Bigint.mul(x, v), Bigint.mul(u, y))
      val d = Bigint.mul(y, v)
   in
      fract_norm(n, d)
   end

fun multiply (a, b) =
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
      val n = Bigint.mul(x, u)
      val d = Bigint.mul(y, v)
   in
      fract_norm(n, d)
   end

fun divide (a:rational, b:rational) =
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
   in
      if Bigint.equal(u, Bigint.zero) then
         raise rat_error
      else
         let
            val n = Bigint.mul(x, v)
            val d = Bigint.mul(y, u)
         in
            make_rat(n, d)
         end
   end

fun equal (a, b) =
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
   in
      if Bigint.equal(Bigint.mul(x, v), Bigint.mul(u, y)) then
         true
      else
         false
   end

fun less (a, b) =
   let
      val x = first(a)
      val y = second(a)
      val u = first(b)
      val v = second(b)
   in
      if Bigint.less(Bigint.mul(x, v), Bigint.mul(u, y)) then
         true
      else
         false
   end

fun isDecimal (s: string) :bool = 
   let
      val l = String.explode s
      val len = List.length l
      val count = ref 0
      val only_digits = ref true
      fun loop(i) = 
         if i < len then
            if List.nth(l, i) = #"." then
               (count := !count + 1; loop(i + 1))
            else if List.nth(l, i) < #"0" orelse List.nth(l, i) > #"9" then
               (only_digits := false; loop(i + 1))
            else
               loop(i + 1)
         else
            () 
   in 
      loop(0);
      if !count < 2 andalso !only_digits then
         true
      else
         false
   end

(* recurring decimal : 0.13333333... = 0.1(3) *)
fun split_decimal (s : string) = 
   let 
      val n = String.size s
      val recurring_point = ref ~1;
      val dot_idx = ref ~1;
      fun loop(i) = 
         if i < n then
            if String.sub(s, i) = #"." then
               (dot_idx := i; loop(i + 1))
            else if String.sub(s, i) = #"(" then
               (recurring_point := i; loop(i + 1))
            else
               loop(i + 1)
         else
            ()
   in
      loop(0);
      (String.substring(s, 0, !dot_idx), String.substring(s, !dot_idx + 1, !recurring_point - !dot_idx -1), String.substring(s, !recurring_point + 1, n - !recurring_point - 2))
   end 

fun onlyDigit (s: string) = 
   let
      val l = String.explode s
      val len = List.length l
      val only_digits = ref true
      fun loop(i) = 
         if i < len then
            if List.nth(l, i) < #"0" orelse List.nth(l, i) > #"9" then
               (only_digits := false; loop(i + 1))
            else
               loop(i + 1)
         else
            () 
   in 
      loop(0);
      if !only_digits then
         true
      else
         false
   end



fun fromDecimalHelper (s: string) = 
if (onlyDigit s) then 
   let 
      val x = Bigint.make_bigint(s)
      val y = Bigint.make_bigint("1")
   in 
      (x, y)
   end
else
let 
   val (a, b, c) = split_decimal(s)
   val l1 = String.size b 
   val l2 = String.size c
   val x = Bigint.make_bigint(a ^ b ^ c)
   val y = Bigint.make_bigint(a ^ b)
   val z = Bigint.sub(x, y)
   fun ten_power (i) = 
      if i = 0 then
         Bigint.fromInt 1
      else
         Bigint.mul(Bigint.make_bigint("10"), ten_power(i - 1))
   val w = ten_power(l1 + l2)
   val v = ten_power(l1)
   val u = Bigint.sub(w, v)
   in 
      (z, u)
   end

fun fromDecimal (s: string) =
if (s = "") then (Bigint.zero, Bigint.make_bigint("1"))
else if String.substring(s, 0, 1) = "~" then
   let 
      val (x, y) = fromDecimalHelper(String.substring(s, 1, String.size s - 1))
   in 
      (Bigint.neg(x), y)
   end
else
   let 
      val (x, y) = fromDecimalHelper(s)
   in 
      (x, y)
   end

   fun belongs([], b, i) = (false, i)  
    | belongs(x, b, i) = 
        if Bigint.equal(hd(x), b) then (true, i)
        else belongs(tl(x), b, i + 1)


fun dec_div(rem, den, digits, li) = 
   if Bigint.equal(rem, Bigint.zero) then (digits, NONE)
   else
      let 
            val r = Bigint.mul(rem, Bigint.make_bigint("10"))
            val q = Bigint.second(valOf(Bigint.divide(r, den)))
            val new_r = valOf(Bigint.modulo(r, den))
            val (bool, index) = belongs(li, new_r, 0)
      in 
            if Bigint.equal(new_r, Bigint.zero) then (digits@q, NONE) 
            else if bool = false then dec_div(new_r, den, digits@q, li@[new_r])
            else (digits@q, SOME (index))
      end;



fun toDecimal (a:rational) =
   let
      val x = fract_norm(a)
      val s = Bigint.sign(first(x))
      val n = Bigint.abs(first(x))
      val d = make_rat(n, second(x))
      val q = valOf(Bigint.divide(n, second(x)))
      val r = valOf(Bigint.modulo(n, second(x)))
      val q_str = Bigint.toString q
      val li = [r]
      val (digits, index) = dec_div(r, second(x), [], li)
      val i = case index of 
                  NONE => 0
                | SOME i => i
      fun toString [] = ""
       | toString (x::xs) = Int.toString x ^ toString xs
      
      in
         if s = 0 then
            if digits = [] then q_str^".(0)"
            else if index = NONE then
               if q_str = "0" then
                  ("."^(toString  digits)^"(0)")
               else
                  (q_str^"."^(toString digits)^"(0)")
            else
               if q_str = "0" then
                  ("."^(toString(List.take(digits, i)))^"("^(toString(List.drop(digits, i)))^")")
               else 
                  (q_str^"."^(toString(List.take(digits, i)))^"("^(toString(List.drop(digits, i)))^")")
         else
            if digits = [] then "~"^(q_str)^".(0)"
            else if index = NONE then
               if q_str = "0" then
                  ("~."^(toString digits)^"(0)")
               else
                  ("~"^(q_str)^"."^(toString digits)^"(0)")
            else
               if q_str = "0" then
                  ("~."^(toString(List.take(digits, i)))^"("^(toString(List.drop(digits, i)))^")")
               else 
                  ("~"^(q_str)^"."^(toString(List.take(digits, i)))^"("^(toString(List.drop(digits, i)))^")")

      end

fun showDecimal (a:rational) = toDecimal a

fun isInt (a:rational) = 
   let 
      val x = first(a)
      val y = second(a)
   in 
      if Bigint.equal(y, Bigint.make_bigint("1")) then
         true
      else
         false
   end

val zero = (Bigint.zero, Bigint.make_bigint("1")):rational


end;