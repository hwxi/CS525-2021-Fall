(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
//
#include
"./../../mylib/mylib.dats"
//
(*
#include
"./../../mylib2/mylib2.dats"
*)
//
(* ****** ****** *)
//
// Total points: 100 points
//
(* ****** ****** *)
typedef tnam0 = string
(* ****** ****** *)

datatype
type0 =
| T0Pbas of tnam0
| T0Pfun of
  ( type0(*arg*)
  , type0(*res*))
| T0Ptup of mylist(type0)

(* ****** ****** *)
typedef tvar0 = string
typedef topr0 = string
(* ****** ****** *)

datatype
tctx0 =
| CTXnil of ()
| CTXcons of (tvar0, tctx0, tctx0)

(* ****** ****** *)
//
datatype term0 =
//
| T0Mint of int // integers
| T0Mbtf of bool // booleans
//
| T0Mvar of tvar0 // variable/name
//
| T0Mlam of
  (tvar0, type0, term0) // abstraction
//
| T0Mapp of (term0, term0) // application
//
| T0Mfix of (tvar0, type0, term0)
//
| T0Mtup of mylist(term0)
//
| T0Msel of (term0(*tup*), int(*prj*))
//
| T0Mopr of (topr0, mylist(term0)(*arg*))
//
| T0Mifb of
  (term0(*test*), term0(*then*), term0(*else*))
//
(* ****** ****** *)

extern
fun
print_term0: (term0) -> void
extern
fun
fprint_term0
(out: FILEref, trm: term0): void

overload print with print_term0
overload fprint with fprint_term0

(* ****** ****** *)

extern
fun
term0_teval0
(prgm: term0): type0
extern
fun
term0_teval1
(prgm: term0, ctx0: tctx0): type0
//
(* ****** ****** *)

(* end of [lambda0.dats] *)
