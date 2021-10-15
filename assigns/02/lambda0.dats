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

implement
fprint_val<term0> = fprint_term0
implement
fprint_val<tval0> = fprint_tval0

(* ****** ****** *)

implement
print_term0(trm) =
fprint_term0(stdout_ref, trm)
implement
fprint_term0(out, trm) =
(
case+ trm of
//
| T0Mint(i0) =>
  fprint!(out, "T0Mint(", i0, ")")
| T0Mbtf(b0) =>
  fprint!(out, "T0Mbtf(", b0, ")")
//
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x1, t2) =>
  fprint!(out, "T0Mlam(", x1, "; ", t2, ")")
| T0Mapp(t1, t2) =>
  fprint!(out, "T0Mapp(", t1, "; ", t2, ")")
//
| T0Mfix(f1, t2) =>
  fprint!(out, "T0Mlam(", f1, "; ", t2, ")")
//
| T0Mopr(nm, ts) =>
  fprint!(out, "T0Mopr(", nm, "; ", ts, ")")
//
| T0Mifb(t1, t2, t3) =>
  fprint!(out, "T0Mifb(", t1, "; ", t2, "; ", t3, ")")
) where
{
(*
implement fprint_mylist_sep<>(out) = fprint(out, "->")
*)
}

(* ****** ****** *)

implement
print_tval0(tvl) =
fprint_tval0(stdout_ref, tvl)
implement
fprint_tval0(out, v0) =
(
case+ v0 of
| T0Vint(i0) =>
  fprintln!(out, "T0Vint(", i0, ")")
| T0Vbtf(b0) =>
  fprintln!(out, "T0Vbtf(", b0, ")")
| T0Vlam _ =>
  fprintln!(out, "T0Vlam(", "...", ")")
| T0Vfix _ =>
  fprintln!(out, "T0Vfix(", "...", ")")
)

(* ****** ****** *)

implement
term0_subst
(t0, x0, sub) =
(
case+ t0 of
//
| T0Mint _ => t0
| T0Mbtf _ => t0
//
| T0Mvar(x1) =>
  if x0 = x1 then sub else t0
| T0Mlam(x1, t2) =>
  if x0 = x1
  then t0 else T0Mlam(x1, term0_subst(t2, x0, sub))
| T0Mapp(t1, t2) =>
  T0Mapp(term0_subst(t1, x0, sub), term0_subst(t2, x0, sub))
//
| T0Mfix(f1, t2) =>
  if x0 = f1
  then t0 else T0Mfix(f1, term0_subst(t2, x0, sub))
//
| T0Mopr(nm, ts) =>
  T0Mopr(nm, ts) where
  {
    val ts =
    mylist_map(ts, lam(t1) => term0_subst(t1, x0, sub))
  }
//
| T0Mifb(t1, t2, t3) =>
  T0Mifb(term0_subst(t1, x0, sub), term0_subst(t2, x0, sub), term0_subst(t3, x0, sub))
) (* end of [term0_subst] *)

(* ****** ****** *)

(* end of [lambda0.dats] *)
