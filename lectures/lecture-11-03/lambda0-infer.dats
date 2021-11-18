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
| T0Pext of (ref(myoptn(type0)))

(* ****** ****** *)

typedef
txyz0 = ref(myoptn(type0))

(* ****** ****** *)

extern
fun
txyz0_equal
(X1: txyz0, X2: txyz0): bool

implement
txyz0_equal
(X1, X2) =
(
$UNSAFE.cast{ptr}(X1)
=
$UNSAFE.cast{ptr}(X2)
)

(* ****** ****** *)

extern
fun
type0_occurs
(T1: type0, X2: txyz0): bool

implement
type0_occurs
(T1, X2) =
(
case+ T1 of
| T0Pbas _ => false
| T0Pfun(T11, T12) =>
  type0_occurs(T11, X2)||type0_occurs(T12, X2)
| T0Ptup(T11, T12) =>
  type0_occurs(T11, X2)||type0_occurs(T12, X2)
| T0Pext(X1) =>
  (if txyz0_equal(X1, X2) then true else false)
)

(* ****** ****** *)

extern
fun
type0_new_ext(): type0
implement
type0_new_ext() =
T0Pext(ref(myoptn_nil()))

extern
fun
type0_new_fun(): type0
implement
type0_new_fun() =
T0Pfun
(type0_new_ext(), type0_new_ext())

extern
fun
type0_new_tup(): type0
implement
type0_new_tup() =
T0Ptup
(type0_new_ext(), type0_new_ext())


(* ****** ****** *)

extern
fun
type0_eval
(T1: type0): type0

implement
type0_eval(T1) =
(
case+ T1 of
|
T0Pext(X1) =>
(
case+ !X1 of
| none() => T1
| some(T1) =>
  type0_eval(T1)
)
| _(*non-T0Pext*) => T1
)

(* ****** ****** *)

extern
fun
type0_unify
(T1: type0, T2: type0): int
extern
fun
type0_unify_ext1
(T1: type0, T2: type0): int
extern
fun
type0_unify_ext2
(T1: type0, T2: type0): int
extern
fun
type0_unify_main
(T1: type0, T2: type0): int

(* ****** ****** *)

(*
implement
type0_unify
(T1, T2) =
(
case+ T1 of
//
|
T0Pbas(nm1) =>
(
case+ T2 of
| T0Pbas(nm2) =>
  if nm1 = nm2 then 0 else 1
| _ (*non-T0Pbas*) => 1
)
//
|
T0Pfun
(T11, T12) =>
(
case+ T2 of
|
T0Pfun
(T21, T22) =>
type0_unify(T11, T21)+type0_unify(T12, T22)
| _ (*non-T0Pfun*) => 1
)
//
|
T0Ptup
(T11, T12) =>
(
case+ T2 of
|
T0Ptup
(T21, T22) =>
type0_unify(T11, T21)+type0_unify(T12, T22)
| _ (*non-T0Ptup*) => 1
)
//
|
T0Pext(X1) =>
(
case+ !X1 of
| none() => (!X1 := some(T2); 0)
| some(T1) => type0_unify(T1, T2)
)
)
*)

(* ****** ****** *)

implement
type0_unify
(T1, T2) =
let
val T1 = type0_eval(T1)
val T2 = type0_eval(T2)
in
//
case+ T1 of
|
T0Pext _ =>
type0_unify_ext1(T1, T2)
//
| _(*non-T0Pext*) =>
(
case+ T2 of
T0Pext _ =>
type0_unify_ext2(T1, T2)
| _(*non-T0Pext*) =>
  type0_unify_main(T1, T2)
)
//
end // end of [type0_unify]


(* ****** ****** *)

implement
type0_unify_ext1
(T1, T2) =
let
val-T0Pext(X1) = T1
in
if
type0_occurs(T2, X1)
then
(
case+ T2 of
| T0Pext _ => 0
| _(*non-T0Pext*) => 1
)  
else
  (!X1 := some(T2); 0)
end

implement
type0_unify_ext2
(T1, T2) =
type0_unify_ext1(T2, T1)

(* ****** ****** *)

implement
type0_unify_main
(T1, T2) =
(
case+ T1 of
//
|
T0Pbas(nm1) =>
(
case+ T2 of
| T0Pbas(nm2) =>
  if nm1 = nm2 then 0 else 1
| _ (*non-T0Pbas*) => 1
)
//
|
T0Pfun
(T11, T12) =>
(
case+ T2 of
|
T0Pfun
(T21, T22) =>
type0_unify(T11, T21)
+
type0_unify(T12, T22)
| _ (*non-T0Pfun*) => 1
)
//
|
T0Ptup
(T11, T12) =>
(
case+ T2 of
|
T0Ptup
(T21, T22) =>
type0_unify(T11, T21)
+
type0_unify(T12, T22)
| _ (*non-T0Ptup*) => 1
)
//
| T0Pext _ => 1 // HX: deadcode
//
) (* end of [type0_unify_main] *)

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
  (tvar0, myoptn(type0), term0) // abstraction
| T0Mapp of (term0, term0) // application
//
| T0Mtup of (term0, term0) 
| T0Mfst of (term0) | T0Msnd of (term0)
//
| T0Mfix of
  (tvar0, myoptn(type0), term0)
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
| T0Pext _ => fprint!(out, "T0Pext(", "...", ")")
//
) (* end of [fprint_type0] *)

(* ****** ****** *)

implement
print_term0(trm) =
fprint_term0(stdout_ref, trm)
implement
fprint_term0(out, trm) =
(
case- trm of
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
    val T0 =
    (
    case+ T0 of
    | myoptn_nil() =>
      type0_new_ext()
    | myoptn_cons(T0) => T0
    )
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
//
    val TF =
    type0_new_fun()
    val-0 = type0_unify(T1, TF)
//
    val T2 =
    term0_teval1(t2, ctx0)
  in
    case- TF of
    | T0Pfun(T11, T12) =>
      T12 where
      { val-0 = type0_unify(T11, T2) }
  end
//
| T0Mtup(t1, t2) =>
  let
    val T1 =
    term0_teval1(t1, ctx0)
    val T2 =
    term0_teval1(t2, ctx0)
  in
    T0Ptup(T1, T2)
  end
//
| T0Mfst(ts) =>
  let
    val TS =
    term0_teval1(ts, ctx0)
    val TT =
    type0_new_tup()
    val-0 = type0_unify(TS, TT)
  in
    case- TT of
    | T0Ptup(T11, T12) => T11
  end
| T0Msnd(ts) =>
  let
    val TS =
    term0_teval1(ts, ctx0)
    val TT =
    type0_new_tup()
    val-0 = type0_unify(TS, TT)
  in
    case- TT of
    | T0Ptup(T11, T12) => T12
  end
//
(*
| T0Mfix(f0, T0, tdef) =>
  let
    val ctx1 =
    CTXcons(f0, T0, ctx0)
    val T1 = term0_teval1(tdef, ctx1)
  in
    T0 where { val- true = type0_unify(T0, T1) }
  end
*)
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
      val-0 =
        type0_unify(T1, T0Pint)
      val-0 =
        type0_unify(T2, T0Pint)
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
      val-0 =
        type0_unify(T1, T0Pint)
      val-0 =
        type0_unify(T2, T0Pint)
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
    val-0 = type0_unify(T1, T0Pbtf)
    val T2 = term0_teval1(t2, ctx0)
    val T3 = term0_teval1(t3, ctx0)
  in
    T2 where { val-0 = type0_unify(T2, T3) }
  end
//
)

(* ****** ****** *)

(* end of [lambda0-infer.dats] *)
