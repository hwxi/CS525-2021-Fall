(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#include
"./../../mylib/mylib.dats"
(*
#include
"./../../mylib2/mylib2.dats"
*)
(* ****** ****** *)
//
// Total points: 120 points
//
(* ****** ****** *)

typedef tvar0 = string
typedef topr0 = string

(* ****** ****** *)
//
datatype term0 =
//
| T0Mint of int // integers
| T0Mbtf of bool // booleans
//
| T0Mvar of tvar0 // variable/name
| T0Mlam of (tvar0, term0) // abstraction
| T0Mapp of (term0, term0) // application
//
| T0Mfix of (tvar0, term0)
//
| T0Mtup of mylist(term0)
(*
| T0Msel of (term0(*tup*), int(*prj*))
*)
| T0Msel of (term0(*tup*), term0(*prj*))
//
| T0Mopr of (topr0, mylist(term0))
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

datatype
tval0 =
| T0Vint of int
| T0Vbtf of bool
| T0Vtup of mylist(tval0)
| T0Vlam of (term0(*T0Mlam*), tenv0)
| T0Vfix of (term0(*T0Mfix*), tenv0)

and
tenv0 =
| ENVnil of ()
| ENVcons of (tvar0, tval0, tenv0)

(* ****** ****** *)

extern
fun
print_tval0: (tval0) -> void
extern
fun
fprint_tval0
(out: FILEref, trm: tval0): void

overload print with print_tval0
overload fprint with fprint_tval0

(* ****** ****** *)

extern
fun
term0_subst
( t0: term0
, x0: tvar0, sub: term0): term0

(* ****** ****** *)

extern
fun
term0_eval0
(prgm: term0): tval0
extern
fun
term0_eval1
(prgm: term0, env0: tenv0): tval0
//
(* ****** ****** *)
//
// HX-2021-10-16: 10 points
// Please construct a term0-value that
// encodes a tail-recursive implementation
// of the factorial function.
//
extern val term0_fact_trec: term0
//
(* ****** ****** *)
//
// HX-2021-10-16: 10 points
// Please construct a term0-value that
// encodes an implemenation of the so-called
// map function on a tuple.
// For instance, given (1, 2, 3) and the function
// lam x => x*x, this map function returns
// (1, 4, 9)
//
extern val term0_tuple_map: term0
//
(* ****** ****** *)

(* end of [lambda0.dats] *)
