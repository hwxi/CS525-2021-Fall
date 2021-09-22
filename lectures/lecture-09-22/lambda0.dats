(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
implement main0() = ()
(* ****** ****** *)

typedef tvar1 = string

datatype term1 =
| T1Mint of int
| T1Mvar of tvar1
| T1Mlam of (tvar1, term1)
| T1Mapp of (term1, term1)

extern
fun
print_term1: (term1) -> void
extern
fun
fprint_term1
(out: FILEref, trm: term1): void

overload print with print_term1
overload fprint with fprint_term1

implement
print_term1(trm) =
fprint_term1(stdout_ref, trm)
implement
fprint_term1(out, trm) =
(
case+ trm of
| T1Mint(i0) =>
  fprint!(out, "T1Mint(", i0, ")")
| T1Mvar(x0) =>
  fprint!(out, "T1Mvar(", x0, ")")
| T1Mlam(x1, t2) =>
  fprint!(out, "T1Mlam(", x1, ", ", t2, ")")
| T1Mapp(t1, t2) =>
  fprint!(out, "T1Mapp(", t1, ", ", t2, ")")
)

val x = T1Mvar("x")
val y = T1Mvar("y")
val z = T1Mvar("z")
val I = T1Mlam("x", x)
val omega = T1Mlam("x", T1Mapp(x, x))
val Omega = T1Mapp(omega, omega)
val K = T1Mlam("x", T1Mlam("y", x))
val K' = T1Mlam("x", T1Mlam("y", y))
val S = T1Mlam("x", T1Mlam("y", T1Mlam("z", T1Mapp(T1Mapp(x, z), T1Mapp(y, z)))))

val () = println!("K = ", K)
val () = println!("S = ", S)

extern
fun
term1_subst
( t0: term1
, x0: tvar1, sub: term1): term1

implement
term1_subst
(t0, x0, sub) =
(
case+ t0 of
| T1Mint _ => t0
| T1Mvar(x1) =>
  if x0 = x1 then sub else t0
| T1Mlam(x1, t2) =>
  if x0 = x1
  then t0 else T1Mlam(x1, term1_subst(t2, x0, sub))
| T1Mapp(t1, t2) =>
  T1Mapp(term1_subst(t1, x0, sub), term1_subst(t2, x0, sub))
)
