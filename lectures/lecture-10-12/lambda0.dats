(* ****** ****** *)
#include
"share/atspre_staload.hats"
(* ****** ****** *)
#include
"./../../mylib/mylib.dats"
(* ****** ****** *)
implement main0() = ()
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
  fprint!(out, "T0Mopr(", nm, "; ", "...", ")")
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

implement
print_tval0(v0) =
(
case- v0 of
| T0Vint(i0) => println!("T0Vint(", i0, ")")
)

(* ****** ****** *)
//
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
term0_eval0(prgm) =
term0_eval1(prgm, ENVnil())

(* ****** ****** *)

implement
term0_eval1
(trm0, env0) =
(
case- trm0 of
//
| T0Mint(i0) => T0Vint(i0)
| T0Mbtf(b0) => T0Vbtf(b0)
//
| T0Mlam(x0, t1) => T0Vlam(trm0, env0)
//
| T0Mapp(t1, t2) =>
  let
    val v1 = term0_eval1(t1, env0)
    val v2 = term0_eval1(t2, env0)
  in
    case- v1 of
    | T0Vlam(tlam, env1) =>
      let
        val-
        T0Mlam(x1, t1bd) = tlam
      in
        term0_eval1(t1bd, ENVcons(x1, v2, env0))
      end
    | T0Vfix(tfix, env1) =>
      let
        val-
        T0Mfix(f1, tdef) = tfix
        val-
        T0Mlam(x1, t1bd) = tdef
      in
        term0_eval1(t1bd, ENVcons(x1, v2, ENVcons(f1, v1, env0)))
      end
  end
//
| T0Mfix(f1, tdef) =>
  T0Vlam(tdef, ENVcons(f1, T0Vfix(trm0, env0), env0))
//
| T0Mopr(nm, ts) =>
  let
    val vs =
    mylist_map<term0><tval0>
    (ts, lam(t1) => term0_eval1(t1, env0))
  in
    case+ nm of
    | "+" =>
      let
      val-
      mylist_cons(T0Vint(i1), vs) = vs
      val-
      mylist_cons(T0Vint(i2), vs) = vs
      in
        T0Vint(i1+i2)
      end
    | _ (*unknown operator*) =>
      exit(1) where
      {
      val () = println!("term0_eval1: T0Mopr: nm = ", nm)
      }
  end
//
| T0Mifb(t1, t2, t3) =>
  let
    val v1 = term0_eval1(t1, env0)
  in
    case- v1 of
    | T0Vbtf(b0) =>
      if b0 then term0_eval1(t2, env0) else term0_eval1(t3, env0)
  end
//
)

(* ****** ****** *)

#define nil mylist_nil
#define :: mylist_cons
#define cons mylist_cons

(* ****** ****** *)

val
add_1_2 =
T0Mopr
( "+"
, T0Mint(1) :: T0Mint(2) :: nil())

(* ****** ****** *)

val () = println!("add_1_2 = ", add_1_2)

(* ****** ****** *)

val ans = term0_eval0(add_1_2)
val () = println!("ans = ", ans)

(* ****** ****** *)

(* end of [lambda0.dats] *)
