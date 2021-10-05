(* ****** ****** *)
(*
Due:
Wednesday
the 6th of October
*)
(* ****** ****** *)
#staload "./../lambda0.sats"
#staload "./../assign01.dats"
(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#staload "./../../../mylib/mylib.dats"
(* ****** ****** *)

implement
term0_fvset(t0) =
(
case- t0 of
| T0Mint _ =>
  mylist_nil()
//
| T0Mvar(x0) =>
  mylist_sing(x0)
//
| T0Mapp(t1, t2) =>
  mylist_append
  (term0_fvset(t1), term0_fvset(t2))
//
| T0Mlam(x0, t1) =>
  mylist_remove(term0_fvset(t1), x0)
//
| T0Mifb(t1, t2, t3) =>
  mylist_append
  (term0_fvset(t1),
   mylist_append
   (term0_fvset(t2), term0_fvset(t3))
  )
//
)

(* ****** ****** *)

(* end of [assign01_sol.dats] *)
