(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
#staload
"./../../mylib/mylib.dats"
//
(* ****** ****** *)

abstype t1xyz_type = ptr
typedef t1xyz = t1xyz_type

(* ****** ****** *)
//
extern
fun
t1xyz_new((*void*)): t1xyz
//
(* ****** ****** *)
//
extern
fun
t1xyz_equal
(X1: t1xyz, X2: t1xyz): bool
overload = with t1xyz_equal
//
(* ****** ****** *)

extern
fun
print_t1xyz : (t1xyz) -> void
and
prerr_t1xyz : (t1xyz) -> void
extern
fun
fprint_t1xyz : (FILEref, t1xyz) -> void

overload print with print_t1xyz
overload prerr with prerr_t1xyz
overload fprint with fprint_t1xyz

(* ****** ****** *)
//
datatype t1ype =
  | T1Pext of t1xyz
  | T1Pbas of string
//
(*
  | T1Plst of (t1ype)
  | T1Pref of (t1ype)
  | T1Parr of (t1ype)
*)
//
  | T1Pfun of (t1ype, t1ype)
  | T1Ptup of (t1ype, t1ype)
//
(* ****** ****** *)

typedef t1ypelst = mylist(t1ype)
typedef t1ypeopt = myoptn(t1ype)

(* ****** ****** *)

datatype
ct1ype = CT1P of (t1ypelst, t1ype)

(* ****** ****** *)

val T1Pint = T1Pbas("int") // for ints
val T1Pbtf = T1Pbas("bool") // for booleans
val T1Pnil = T1Pbas("unit") // void in C
val T1Pstr = T1Pbas("string") // for strings

(* ****** ****** *)
//
extern
fun
t1xyz_get_t1ype(t1xyz): t1ypeopt
and
t1xyz_set_t1ype(t1xyz, t1ype): void
//
overload .t1ype with t1xyz_get_t1ype // X.t1ype()
overload .t1ype with t1xyz_set_t1ype // X.t1ype(T)
//
(* ****** ****** *)

typedef t1var = string
typedef t1opr = string

(* ****** ****** *)
//
datatype t1erm =
//
  | T1Mint of int // value
  | T1Mbtf of bool // value
//
  | T1Mnil of ()
//
  | T1Mstr of string // value
//
  | T1Mvar of t1var // not evaluated
//
  | T1Mlam of
    (t1var, t1ypeopt, t1erm) // value
  | T1Mapp of (t1erm, t1erm) // non-value
//
  | T1Mlet of (t1var, t1erm, t1erm) // let x = t1 in t2
//
  | T1Mopr of (t1opr, mylist(t1erm)) // primitive operators
//
  | T1Mifb of (t1erm, t1erm, t1erm)
//
  | T1Mfix of
    (t1var(*f*), t1var(*x*), t1ypeopt(*arg*), t1ypeopt(*res*), t1erm) // fixed-points
//
  | T1Mtup of (t1erm, t1erm)
  | T1Mfst of (t1erm) | T1Msnd of (t1erm)
//
  | T1Mann of (t1erm, t1ype) // type annotation
//
//
// HX-2018-10-20:
// For supporting lists
//
  | T1Mcons of (t1erm, t1erm)
  | T1Misnil of t1erm | T1Mhead of t1erm | T1Mtail of t1erm
//
//
// HX-2018-10-16:
// Substitution-based
// interpreter no longer works!
//
  | T1Mref_new of (t1erm)
  | T1Mref_get of (t1erm)
  | T1Mref_set of (t1erm, t1erm)
//
  | T1Marr_new of (t1erm(*size*), t1erm(*init*))
  | T1Marr_size of (t1erm(*arr*))
  | T1Marr_get_at of (t1erm(*arr*), t1erm(*ind*))
  | T1Marr_set_at of (t1erm(*arr*), t1erm(*ind*), t1erm(*val*))
//
(* ****** ****** *)
//
typedef t1ermlst = mylist(t1erm)
//
(* ****** ****** *)

extern
fun
print_t1ype : (t1ype) -> void
and
prerr_t1ype : (t1ype) -> void
extern
fun
fprint_t1ype : (FILEref, t1ype) -> void

overload print with print_t1ype
overload prerr with prerr_t1ype
overload fprint with fprint_t1ype

(* ****** ****** *)

extern
fun
print_t1erm : (t1erm) -> void
and
prerr_t1erm : (t1erm) -> void
extern
fun
fprint_t1erm : (FILEref, t1erm) -> void

overload print with print_t1erm
overload prerr with prerr_t1erm
overload fprint with fprint_t1erm

(* ****** ****** *)

implement
print_t1ype(t0) = 
fprint_t1ype(stdout_ref, t0)
implement
prerr_t1ype(t0) = 
fprint_t1ype(stderr_ref, t0)

local

implement
fprint_val<t1ype> = fprint_t1ype

in (* in-of-local *)

implement
fprint_t1ype
  (out, T0) =
(
case+ T0 of
//
(*
| T1Pvar(X) =>
  fprint!(out, "T1Pvar(", X, ")")
*)
| T1Pext(X) =>
  (
    case+
    X.t1ype() of 
    | none() =>
      fprint!
      (out, "T1Pvar(", X, ")")
    | some(T) => fprint_t1ype(out, T)
  )
//
| T1Pbas(name) =>
  fprint!(out, "T1Pbas(", name, ")")
| T1Pfun(arg, res) =>
  fprint!(out, "T1Pfun(", arg, "; ", res, ")")
| T1Ptup(fst, snd) =>
  fprint!(out, "T1Ptup(", fst, "; ", snd, ")")
//
(*
| T1Plst(elt) => 
  fprint!(out, "T1Plst(", elt, ")")
*)
//
(*
| T1Pref(elt) => 
  fprint!(out, "T1Pref(", elt, ")")
| T1Parr(elt) => 
  fprint!(out, "T1Parr(", elt, ")")
*)
//
) (* end of [fprint_t1ype] *)

end // end of [local]

(* ****** ****** *)

implement
print_t1erm(t0) = 
fprint_t1erm(stdout_ref, t0)
implement
prerr_t1erm(t0) = 
fprint_t1erm(stderr_ref, t0)

local

implement
fprint_val<t1erm> = fprint_t1erm

in (* in-of-local *)

implement
fprint_t1erm
  (out, t0) =
(
case+ t0 of
//
| T1Mint(x) =>
  fprint!(out, "T1Mint(", x, ")")
| T1Mbtf(x) =>
  fprint!(out, "T1Mbtf(", x, ")")
| T1Mnil() => fprint!(out, "T1Mnil()")
//
| T1Mstr(x) =>
  fprint!(out, "T1Mstr(", x, ")")
//
| T1Mvar(x) =>
  fprint!(out, "T1Mvar(", x, ")")
//
(*
| T1Mlam(x1, t2) =>
  fprint!(out, "T1Mlam(", x1, "; ", t2, ")")
*)
| T1Mlam(x1, T1, t2) =>
  fprint!(out, "T1Mlam(", x1, "; ", t2, ")")
//
| T1Mapp(t1, t2) =>
  fprint!(out, "T1Mapp(", t1, "; ", t2, ")")
//
| T1Mopr(opr, ts) =>
  fprint!(out, "T1Mopr(", opr, "; ", ts)
//
| T1Mlet(x, t1, t2) =>
  fprint!(out, "T1Mlet(", x, "; ", t1, "; ", t2, ")")
//
| T1Mifb(t1, t2, t3) =>
  fprint!(out, "T1Mifb(", t1, "; ", t2, "; ", t3, ")")
//
| T1Mfix(f1, x2, T1, T2, t3) =>
  fprint!(out, "T1Mfix(", f1, "; ", x2, "; ", t3, ")")
//
| T1Mtup(t1, t2) =>
  fprint!(out, "T1Mtup(", t1, "; ", t2, ")")
//
| T1Mfst(t1) =>
  fprint!(out, "T1Mfst(", t1, ")")
| T1Msnd(t1) =>
  fprint!(out, "T1Msnd(", t1, ")")
//
| T1Mann(t1, T2) =>
  fprint!(out, "T1Mann(", t1, "; ", T2, ")")
//
| T1Mcons(t1, t2) =>
  fprint!(out, "T1Mcons(", t1, "; ", t2, ")")
//
| T1Mhead(t1) =>
  fprint!(out, "T1Mhead(", t1, ")")
| T1Mtail(t1) =>
  fprint!(out, "T1Mtail(", t1, ")")
| T1Misnil(t1) =>
  fprint!(out, "T1Misnil(", t1, ")")
//
| T1Mref_new(t1) =>
  fprint!(out, "TMref_new(", t1, ")")
| T1Mref_get(t1) =>
  fprint!(out, "TMref_get(", t1, ")")
| T1Mref_set(t1, t2) =>
  fprint!(out, "TMref_set(", t1, ", ", t2, ")")
//
| T1Marr_new(t1, t2) =>
  fprint!(out, "TMarr_new(", t1, ", ", t2, ")")
| T1Marr_size(t1) =>
  fprint!(out, "TMarr_size(", t1, ")")
| T1Marr_get_at(t1, t2) =>
  fprint!(out, "TMarr_get_at(", t1, ", ", t2, ")")
| T1Marr_set_at(t1, t2, t3) =>
  fprint!(out, "TMarr_set_at(", t1, ", ", t2, ", ", t3, ")")
//
)

end // end of [local]

(* ****** ****** *)

(* end of [lambda1.dats] *)
