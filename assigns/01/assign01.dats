(* ****** ****** *)
(*
Due:
Wednesday
the 6th of October
*)
(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
(*
#staload
"./../../mylib/mylib.sats"
#staload
"./../../mylib/mylib.dats"
*)
(* ****** ****** *)
//
// Please finish
// the implementation of
// of the function term0_fvset
//
(*
Note that you need to implement
mylist_append and mylist_remove
If you are not clear about what
these functions are supposed to do,
please ask your questions on Piazza
*)
//
(* ****** ****** *)

(*
HX: 20 points
*)
(*
implement
term0_fvset(t0) =
(
case+ t0 of
| T0Mint _ =>
  mylist_nil()
| T0Mvar(x0) =>
  mylist_sing(x0)
| T0Mapp(t1, t2) =>
  mylist_append
  (term0_fvset(t1), term0_fvset(t2))
| T0Mlam(x0, t1) =>
  mylist_remove(term0_fvset(t1), x0)
)
*)

(* ****** ****** *)

(*
//
HX: 10 points
Please construct a term in LAMBDA0 that
computes the following function fibo:
//
fun
fibo(n: int): int =
if n >= 2 then fibo(n-1)+fibo(n-2) else n
//
*)
val tfibo : term0 // See [tfact] as an example

(* ****** ****** *)
//
(*
HX: 70 points
//
please
implement an interpreter for LAMBDA0
//
fun term0_interp(prgm: term0): term0
//
Note that term0_interp is the same as
term0_eval given during lectures. This problem
asks you to read the code of term0_eval and use
it (mostly, copy/paste it) to implement term0_interp
//
*)
//
(* ****** ****** *)

(* end of [assign01.dats] *)
