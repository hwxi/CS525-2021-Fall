(* ****** ****** *)
#staload "./lambda0.sats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload
"./../../mylib/mylib.sats"
#staload
"./../../mylib/mylib.dats"
(* ****** ****** *)
//
// Please finish
// the implementation of
// of the function t0erm_fvset
//
(*
Note that you need to implement
mylist_append and mylist_remove
If you are not clear about what
these function are supposed to do,
please ask your questions on Piazza
*)
//
(* ****** ****** *)

(*
HX: 20 points
*)
(*
implement
t0erm_fvset(t0) =
(
case+ t0 of
| T0Mint _ =>
  mylist_nil()
| T0Mvar(x0) =>
  mylist_sing(x0)
| T0Mapp(t1, t2) =>
  mylist_append
  (t0erm_fvset(t1), t0erm_fvset(t2))
| T0Mlam(x0, t1) =>
  mylist_remove(t0erm_fvset(t1), x0)
)
*)

(* ****** ****** *)

(*
HX: 10 points
*)
(*
Note that the datatype [t0erm]
is extended with the following
constructor:
//
| T0Mbtf of bool // booleans
//
which is for representing boolean
values (true and false). Please make
changes to the functions t0erm_fprint
and t0erm_interp accordingly.
*)

(* ****** ****** *)

(*
HX: 10 points
*)
(*
Please extend t0erm_interp
to handle the following integer
comparision functions:

<, <=, >, >=, ==, and !=

*)

(* ****** ****** *)

(* end of [assign01.dats] *)
