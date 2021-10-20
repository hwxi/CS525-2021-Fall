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
implement main0() = ()
(* ****** ****** *)

typedef tnam0 = string
typedef tvar0 = string
typedef topr0 = string

(* ****** ****** *)

datatype
type0 =
| T0Pbas of tnam0
| T0Pfun of
  (type0(*arg*), type0(*res*))
| T0Ptup of
  (type0(*fst*), type0(*snd*))

(* ****** ****** *)

extern
fun
type0_equal
(T1: type0, T2: type0): bool

(* ****** ****** *)

extern
fun
print_type0: (type0) -> void
extern
fun
fprint_type0
(out: FILEref, typ: type0): void

overload print with print_type0
overload fprint with fprint_type0

(* ****** ****** *)
//
datatype term0 =
//
| T0Mint of int // integers
| T0Mbtf of bool // booleans
//
| T0Mvar of tvar0 // variable/name
| T0Mlam of
  (tvar0, type0(*arg*), term0) // abstraction
| T0Mapp of (term0, term0) // application
//
| T0Mfix of
  (tvar0, type0(*fun*), term0)
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
fprint_val<type0> = fprint_type0
implement
fprint_val<term0> = fprint_term0

(* ****** ****** *)

implement
print_type0(typ) =
fprint_type0(stdout_ref, typ)
implement
fprint_type0(out, typ) =
(
case+ typ of
//
| T0Pbas(tnm) =>
  fprint!(out, "T0Pbas(", tnm, ")")
| T0Pfun(T1, T2) =>
  fprint!(out, "T0Pfun(", T1, "; ", T2, ")")
| T0Ptup(T1, T2) =>
  fprint!(out, "T0Ptup(", T1, "; ", T2, ")")
//
) (* end of [fprint_type0] *)

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
| T0Mlam(x1, T1, t2) =>
  fprint!
  ( out
  , "T0Mlam(", x1, "; ", T1, "; ", t2, ")")
| T0Mapp(t1, t2) =>
  fprint!(out, "T0Mapp(", t1, "; ", t2, ")")
//
| T0Mfix(f1, T1, t2) =>
  fprint!
  ( out
  , "T0Mfix(", f1, "; ", T1, "; ", t2, ")")
//
| T0Mopr(nm, ts) =>
  fprint!(out, "T0Mopr(", nm, "; ", ts, ")")
//
| T0Mifb(t1, t2, t3) =>
  fprint!(out, "T0Mifb(", t1, "; ", t2, "; ", t3, ")")
) where
{
implement fprint_mylist_sep<>(out) = fprint(out, "->")
}

(* ****** ****** *)

datatype
tctx0 =
| CTXnil of ()
| CTXcons of (tvar0, type0, tctx0)

(* ****** ****** *)
//
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

implement
term0_teval0(prgm) =
term0_teval1(prgm, CTXnil())

(* ****** ****** *)

extern
fun
tvar0_teval1
( x0: tvar0
, ctx0: tctx0): myoptn(type0)

implement
tvar0_teval1
(x0, ctx0) =
(
case+ ctx0 of
| CTXnil() => myoptn_nil()
| CTXcons
  (x1, T1, ctx1) =>
  if (x0 = x1)
  then myoptn_cons(T1)
  else tvar0_teval1(x0, ctx1)
)

(* ****** ****** *)

val T0Pint = T0Pbas("int")
val T0Pbtf = T0Pbas("bool")

implement
term0_teval1
(trm0, ctx0) =
(
case- trm0 of
//
| T0Mint(i0) => T0Pint
| T0Mbtf(b0) => T0Pbtf
//
| T0Mvar(x0) =>
  let
//
(*
  val () =
  println!
  ("term0_eval1: trm0 = ", trm0)
*)
//
  val opt =
  tvar0_teval1(x0, ctx0)
  in
    case- opt of
    | myoptn_cons(v0) => v0
  end
//
| T0Mlam(x0, T0, t1) =>
  let
    val ctx1 =
    CTXcons(x0, T0, ctx0)
    val T1 =
    term0_teval1(t1, ctx1)
  in
    T0Pfun(T0, T1)
  end
//
| T0Mapp(t1, t2) =>
  let
    val T1 =
    term0_teval1(t1, ctx0)
    val T2 =
    term0_teval1(t2, ctx0)
  in
    case- T1 of
    | T0Pfun(T11, T12) =>
      T12 where
      { val-true = type0_equal(T11, T2) }
  end
//
| T0Mfix(f0, T0, tdef) =>
  let
    val ctx1 =
    CTXcons(f0, T0, ctx0)
    val T1 = term0_teval1(tdef, ctx1)
  in
    T0 where { val- true = type0_equal(T0, T1) }
  end
//
| T0Mopr(nm, ts) =>
  let
    val Ts =
    mylist_map<term0><type0>
    (ts, lam(t1) => term0_teval1(t1, ctx0))
  in
    case+ nm of
    | "+" =>
      let
      val-
      mylist_cons(T1, Ts) = Ts
      val-
      mylist_cons(T2, Ts) = Ts
//
      val-true =
        type0_equal(T1, T0Pint)
      val-true =
        type0_equal(T2, T0Pint)
//
      in
        T0Pint
      end
    | ">" =>
      let
      val-
      mylist_cons(T1, Ts) = Ts
      val-
      mylist_cons(T2, Ts) = Ts
//
      val-true =
        type0_equal(T1, T0Pint)
      val-true =
        type0_equal(T2, T0Pint)
//
      in
        T0Pbtf
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
    val T1 = term0_teval1(t1, ctx0)
    val-true = type0_equal(T1, T0Pbtf)
    val T2 = term0_teval1(t2, ctx0)
    val T3 = term0_teval1(t3, ctx0)
  in
    T2 where { val- true = type0_equal(T2, T3) }
  end
//
)

(* ****** ****** *)

(* end of [lambda0-teval.dats] *)
