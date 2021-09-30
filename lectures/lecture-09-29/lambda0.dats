(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
implement main0() = ()
(* ****** ****** *)

typedef tvar0 = string

(* ****** ****** *)
//
datatype term0 =
| T0Mint of int // integers
//
| T0Mneg of (term0)
| T0Madd of (term0, term0)
| T0Mmul of (term0, term0)
//
| T0Migt of (term0, term0)
| T0Mige of (term0, term0)
//
| T0Mbtf of bool // booleans
//
| T0Mvar of tvar0 // variable/name
| T0Mlam of (tvar0, term0) // abstraction
| T0Mapp of (term0, term0) // application
//
| T0Mfix of (tvar0, term0)
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
| T0Migt(t1, t2) =>
  fprint!(out, "T0Migt(", t1, "; ", t2)
| T0Mige(t1, t2) =>
  fprint!(out, "T0Mige(", t1, "; ", t2)
//
| T0Mbtf(b0) =>
  fprint!(out, "T0Mbtf(", b0, ")")
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
| T0Mifb(t1, t2, t3) =>
  fprint!(out, "T0Mifb(", t1, "; ", t2, "; ", t3, ")")
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
, x0: tvar0, sub: term0): term0

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
| T0Migt(t1, t2) =>
  T0Migt(t1, t2) where
  {
    val t1 =
    term0_subst(t1, x0, sub)
    val t2 =
    term0_subst(t2, x0, sub)
  }
| T0Mige(t1, t2) =>
  T0Mige(t1, t2) where
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
//
| T0Mfix(f1, t2) =>
  if x0 = f1
  then t0 else T0Mfix(f1, term0_subst(t2, x0, sub))
//
| T0Mifb(t1, t2, t3) =>
  T0Mifb(term0_subst(t1, x0, sub), term0_subst(t2, x0, sub), term0_subst(t3, x0, sub))
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
extern
fun
term0_eval_app(prgm: term0): term0
extern
fun
term0_eval_fix(prgm: term0): term0

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
| T0Migt(t1, t2) =>
  let
    val t1 =
    term0_eval(t1)
    val t2 =
    term0_eval(t2)
    val- T0Mint(i1) = t1
    val- T0Mint(i2) = t2
  in
    T0Mbtf(i1 > i2)
  end
//
| T0Mlam _ => t0
//
| T0Mapp _ =>
  term0_eval_app(t0)
//
| T0Mfix _ =>
  term0_eval_fix(t0)
//
| T0Mifb(t1, t2, t3) =>
  let
  val t1 = term0_eval(t1)
  in
    case- t1 of
    | T0Mbtf(tf) =>
      if tf then term0_eval(t2)
            else term0_eval(t3)
  end
//
| _(*YTBD*) =>
  exit(1) where
  {
  val () =
  println!("term0_eval: t0 = ", t0)
  }
)

(* ****** ****** *)

implement
term0_eval_fix(tfix) =
let
val-
T0Mfix(f1, t2) = tfix
in
term0_eval(term0_subst(t2, f1, tfix))
end

(* ****** ****** *)

implement
term0_eval_app(tapp) =
let
val-
T0Mapp(t1, t2) = tapp
//
val t1 = term0_eval(t1) // fun
val t2 = term0_eval(t2) // arg
//
in
case- t1 of
| T0Mlam(x1, body) =>
  term0_eval(term0_subst(body, x1, t2))
end

(* ****** ****** *)

local

val x = T0Mvar("x")

in
//
val
DOUBLE =
T0Mlam("x", T0Madd(x, x))
val
SQUARE =
T0Mlam("x", T0Mmul(x, x))
//
end (* end of [local] *)

(* ****** ****** *)

val dbl5_abs =
T0Mapp(DOUBLE, T0Mint(5))
val dbl5_val =
term0_eval
(
T0Mapp(DOUBLE, T0Mint(5))
)
val () =
println!("DOUBLE(5) = ", dbl5_val)

(* ****** ****** *)

val sqr5_abs =
T0Mapp(SQUARE, T0Mint(5))
val sqr5_val =
term0_eval
(
T0Mapp(SQUARE, T0Mint(5))
)
val () =
println!("SQUARE(5) = ", sqr5_val)

(* ****** ****** *)

local

#include "tfact.lam"

in(*in-of-local*)

val
tfact10_abs =
T0Mapp(tfact, T0Mint(10))
val
tfact10_val = term0_eval(tfact10_abs)
val () =
println!("tfact(10) = ", tfact10_val)

end (* end of [local] *)

(* ****** ****** *)

(* end of [lambda0.dats] *)
