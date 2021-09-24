(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
implement main0() = ()
(* ****** ****** *)

typedef tvar1 = string

datatype term0 =
| T0Mint of int
| T0Mvar of tvar1
| T0Mlam of (tvar1, term0)
| T0Mapp of (term0, term0)

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

implement
print_term0(trm) =
fprint_term0(stdout_ref, trm)
implement
fprint_term0(out, trm) =
(
case+ trm of
| T0Mint(i0) =>
  fprint!(out, "T0Mint(", i0, ")")
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x1, t2) =>
  fprint!(out, "T0Mlam(", x1, ", ", t2, ")")
| T0Mapp(t1, t2) =>
  fprint!(out, "T0Mapp(", t1, ", ", t2, ")")
)

(* ****** ****** *)

val x = T0Mvar("x")
val y = T0Mvar("y")
val z = T0Mvar("z")
val I = T0Mlam("x", x)
val omega = T0Mlam("x", T0Mapp(x, x))
val Omega = T0Mapp(omega, omega)
val K = T0Mlam("x", T0Mlam("y", x))
val K' = T0Mlam("x", T0Mlam("y", y))
val S = T0Mlam("x", T0Mlam("y", T0Mlam("z", T0Mapp(T0Mapp(x, z), T0Mapp(y, z)))))

val () = println!("K = ", K)
val () = println!("S = ", S)

(* ****** ****** *)

extern
fun
term0_subst
( t0: term0
, x0: tvar1, sub: term0): term0

implement
term0_subst
(t0, x0, sub) =
(
case+ t0 of
| T0Mint _ => t0
| T0Mvar(x1) =>
  if x0 = x1 then sub else t0
| T0Mlam(x1, t2) =>
  if x0 = x1
  then t0 else T0Mlam(x1, term0_subst(t2, x0, sub))
| T0Mapp(t1, t2) =>
  T0Mapp(term0_subst(t1, x0, sub), term0_subst(t2, x0, sub))
)

(* ****** ****** *)

(* end of [lambda0.dats] *)
