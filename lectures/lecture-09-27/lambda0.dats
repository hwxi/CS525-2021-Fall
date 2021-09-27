(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
implement main0() = ()
(* ****** ****** *)

typedef tvar1 = string

(* ****** ****** *)
//
datatype term0 =
| T0Mint of int // integers
//
| T0Mneg of (term0)
| T0Madd of (term0, term0)
| T0Mmul of (term0, term0)
//
| T0Mbtf of bool // booleans
//
| T0Mvar of tvar1 // variable/name
| T0Mlam of (tvar1, term0) // abstraction
| T0Mapp of (term0, term0) // application
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
//
| T0Mneg(t1) =>
  fprint!(out, "T0Mneg(", t1, ")")
| T0Madd(t1, t2) =>
  fprint!(out, "T0Madd(", t1, "; ", t2)
| T0Mmul(t1, t2) =>
  fprint!(out, "T0Mmul(", t1, "; ", t2)
//
| T0Mbtf(b0) =>
  fprint!(out, "T0Mbtf(", b0, ")")
| T0Mvar(x0) =>
  fprint!(out, "T0Mvar(", x0, ")")
| T0Mlam(x1, t2) =>
  fprint!(out, "T0Mlam(", x1, "; ", t2, ")")
| T0Mapp(t1, t2) =>
  fprint!(out, "T0Mapp(", t1, "; ", t2, ")")
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
//
// HX:
// [sub] is a closed term!!!
//
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
//
| T0Mint _ => t0
//
| T0Mneg(t1) =>
  T0Mneg(t1) where
  {
    val t1 =
    term0_subst(t1, x0, sub)
  }
| T0Madd(t1, t2) =>
  T0Madd(t1, t2) where
  {
    val t1 =
    term0_subst(t1, x0, sub)
    val t2 =
    term0_subst(t2, x0, sub)
  }
| T0Mmul(t1, t2) =>
  T0Mmul(t1, t2) where
  {
    val t1 =
    term0_subst(t1, x0, sub)
    val t2 =
    term0_subst(t2, x0, sub)
  }
//
| T0Mbtf _ => t0
| T0Mvar(x1) =>
  if x0 = x1 then sub else t0
| T0Mlam(x1, t2) =>
  if x0 = x1
  then t0 else T0Mlam(x1, term0_subst(t2, x0, sub))
| T0Mapp(t1, t2) =>
  T0Mapp(term0_subst(t1, x0, sub), term0_subst(t2, x0, sub))
)

(* ****** ****** *)

(*
val x = T0Mvar"x"
val y = T0Mvar"y"
*)
fun
square_abs(trm: term0): term0 = T0Mmul(trm, trm)

(* ****** ****** *)

extern
fun
term0_eval(prgm: term0): term0

(* ****** ****** *)

implement
term0_eval(t0) =
(
case+ t0 of
| T0Mint _ => t0
//
| T0Mneg(t1) =>
(
  T0Mint(~i1)
) where
{
    val t1 =
    term0_eval(t1)
    val- T0Mint(i1) = t1
}
| T0Madd(t1, t2) =>
  let
    val t1 =
    term0_eval(t1)
    val t2 =
    term0_eval(t2)
    val- T0Mint(i1) = t1
    val- T0Mint(i2) = t2
  in
    T0Mint(i1 + i2)
  end
| T0Mmul(t1, t2) =>
  let
    val t1 =
    term0_eval(t1)
    val t2 =
    term0_eval(t2)
    val- T0Mint(i1) = t1
    val- T0Mint(i2) = t2
  in
    T0Mint(i1 * i2)
  end
//
| T0Mbtf _ => t0
//
| T0Mlam _ => t0
//
| _(*YTBD*) =>
  exit(1) where
  {
  val () =
  println!("term0_eval: t0 = ", t0)
  }
)

(* ****** ****** *)

(* end of [lambda0.dats] *)
