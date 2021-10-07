
#include
"share/atspre_staload.hats"

(* ****** ****** *)
(*
#staload "./../../mylib/mylib.dats"
#staload "./../../mylib2/mylib2.dats"
*)
(* ****** ****** *)

implement main0() = ()

(* ****** ****** *)

(*
concrete syntax!!!
A parser turns
concrete syntax into abstract syntax
*)

(*
fun
fact(n: int): int =
if n > 0 then n * fact(n-1) else 1
*)
val
fact =
fix f(n: int): int =>
if n > 0 then n * f(n-1) else 1
val () =
println!("fact(10) = ", fact(10))

(* ****** ****** *)

val
f91 =
fix f(n: int): int =>
if n > 100 then n - 10 else f(f(n+11))
val () =
println!("f91(10) = ", f91(10))
val () =
println!("f91(41) = ", f91(41))
val () =
println!("f91(57) = ", f91(57))

(* ****** ****** *)

(* end of [fix-funs.dats] *)
