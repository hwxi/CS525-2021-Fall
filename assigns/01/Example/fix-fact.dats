(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)

(*
fun fact(n: int): int =
if n > 0 then n * fact(n-1) else 1
*)

val
fact =
fix f(x: int): int =>
if x > 0 then x * f(x-1) else 1

(* ****** ****** *)

(* end of [fix-fact.dats] *)
