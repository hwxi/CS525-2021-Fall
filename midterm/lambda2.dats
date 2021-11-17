(* ****** ****** *)
//
#include
"share/atspre_staload.hats"
//
#staload "./../mylib/mylib.dats"
//
(* ****** ****** *)
//
#staload "./lambda1.dats"
//
(* ****** ****** *)

abstype t2var_type = ptr
typedef t2var = t2var_type
typedef t2varlst = mylist(t2var)

(* ****** ****** *)
//
extern
fun
t2var_new(): t2var
//
(* ****** ****** *)
//
extern
fun
t2var_equal
( x1: t2var
, x2: t2var): bool
overload = with t2var_equal
//
(* ****** ****** *)
//
extern
fun
t2var_get_t1ype(t2var): t1ypeopt
and
t2var_set_t1ype(t2var, t1ype): void
//
overload .t1ype with t2var_get_t1ype
overload .t1ype with t2var_set_t1ype
//
(* ****** ****** *)

extern
fun
print_t2var : (t2var) -> void
and
prerr_t2var : (t2var) -> void
extern
fun
fprint_t2var : (FILEref, t2var) -> void

overload print with print_t2var
overload prerr with prerr_t2var
overload fprint with fprint_t2var

(* ****** ****** *)

datatype t2erm =
//
  | T2Memp of ((*void*))
//
  | T2Mint of int // value
  | T2Mbtf of bool // value
//
(*
  | T2Mfloat of bool // value
*)
  | T2Mstring of string // value
//
  | T2Mvar of t2var // not evaluated
//
  | T2Mlam of
    (t2var, t1ypeopt, t2erm) // value
  | T2Mapp of (t2erm, t2erm) // non-value
//
  | T2Mlet of
    (t2var, t2erm, t2erm) // let x = t1 in t2
  | T2Mopr of
    (t1opr, mylist(t2erm)) // primitive operators
//
  | T2Mift of (t2erm, t2erm, t2erm)
  | T2Mfix of
    ( t2var(*f*)
    , t2var(*x*)
    , t1ypeopt(*arg*)
    , t1ypeopt(*res*), t2erm) // fixed-point opr
//
  | T2Mtup of (t2erm, t2erm)
  | T2Mfst of (t2erm) | T2Msnd of (t2erm)
//
  | T2Mann of (t2erm, t1ype) // type annotation
//
  | T2Mnil of ()
  | T2Mcons of (t2erm, t2erm)
  | T2Misnil of (t2erm)
  | T2Mhead of (t2erm) | T2Mtail of (t2erm)
//
  | T2Mref_new of (t2erm)
  | T2Mref_get of t2erm | T2Mref_set of (t2erm, t2erm)
//
(*
  | T2Marr_new of (t2erm(*size*), t2erm(*init*))
  | T2Marr_size of (t2erm(*arr*))
  | T2Marr_get_at of (t2erm(*arr*), t2erm(*ind*))
  | T2Marr_set_at of (t2erm(*arr*), t2erm(*ind*), t2erm(*val*))
*)
//
(* ****** ****** *)

typedef t2ermlst = mylist(t2erm)

(* ****** ****** *)

extern
fun
print_t2erm : (t2erm) -> void
and
prerr_t2erm : (t2erm) -> void
extern
fun
fprint_t2erm : (FILEref, t2erm) -> void

overload print with print_t2erm
overload prerr with prerr_t2erm
overload fprint with fprint_t2erm

(* ****** ****** *)

implement
print_t2erm(t0) = 
fprint_t2erm(stdout_ref, t0)
implement
prerr_t2erm(t0) = 
fprint_t2erm(stderr_ref, t0)

local

implement
fprint_val<t2erm> = fprint_t2erm

in (* in-of-local *)

implement
fprint_t2erm
  (out, t0) =
(
case+ t0 of
//
| T2Memp() =>
  fprint!(out, "T2Memp(", ")")
//
| T2Mint(x) =>
  fprint!(out, "T2Mint(", x, ")")
| T2Mbtf(x) =>
  fprint!(out, "T2Mbtf(", x, ")")
//
| T2Mnil() =>
  fprint!(out, "T2Mnil(", ")")
//
| T2Mstring(x) =>
  fprint!(out, "T2Mstring(", x, ")")
//
| T2Mvar(x) =>
  fprint!(out, "T2Mvar(", x, ")")
//
| T2Mlam(x1, T1, t2) =>
  fprint!(out, "T2Mlam(", x1, "; ", t2, ")")
//
| T2Mapp(t1, t2) =>
  fprint!(out, "T2Mapp(", t1, "; ", t2, ")")
//
| T2Mopr(opr, ts) =>
  fprint!(out, "T2Mopr(", opr, "; ", ts)
//
| T2Mlet(x, t1, t2) =>
  fprint!(out, "T2Mlet(", x, "; ", t1, "; ", t2, ")")
//
| T2Mift(t1, t2, t3) =>
  fprint!(out, "T2Mift(", t1, "; ", t2, "; ", t3, ")")
//
| T2Mfix(f1, x2, T1, T2, t3) =>
  fprint!(out, "T2Mfix(", f1, "; ", x2, "; ", t3, ")")
//
| T2Mtup(t1, t2) =>
  fprint!(out, "T2Mtup(", t1, "; ", t2, ")")
//
| T2Mfst(t1) =>
  fprint!(out, "T2Mfst(", t1, ")")
| T2Msnd(t1) =>
  fprint!(out, "T2Msnd(", t1, ")")
//
| T2Mann(t1, T2) =>
  fprint!(out, "T2Mann(", t1, "; ", T2, ")")
//
| T2Mcons(t1, t2) =>
  fprint!(out, "T2Mcons(", t1, "; ", t2, ")")
| T2Mhead(t1) =>
  fprint!(out, "T2Mhead(", t1, ")")
| T2Mtail(t1) =>
  fprint!(out, "T2Mtail(", t1, ")")
| T2Misnil(t1) =>
  fprint!(out, "T2Misnil(", t1, ")")
//
| T2Mref_new(t1) =>
  fprint!(out, "TMref_new(", t1, ")")
| T2Mref_get(t1) =>
  fprint!(out, "TMref_get(", t1, ")")
| T2Mref_set(t1, t2) =>
  fprint!(out, "TMref_set(", t1, ", ", t2, ")")
//
(*
| T2Marr_new(t1, t2) =>
  fprint!(out, "TMarr_new(", t1, ", ", t2, ")")
| T2Marr_size(t1) =>
  fprint!(out, "TMarr_size(", t1, ")")
| T2Marr_get_at(t1, t2) =>
  fprint!(out, "TMarr_get_at(", t1, ", ", t2, ")")
| T2Marr_set_at(t1, t2, t3) =>
  fprint!(out, "TMarr_set_at(", t1, ", ", t2, ", ", t3, ")")
*)
//
) (* end of [fprint_t2erm] *)

end // end of [local]

(* ****** ****** *)

datatype
t1env =
|
T1ENVnil of ()
|
T1ENVcons of
(t1var(*str*), t2var(*obj*), t1env)

(* ****** ****** *)

extern
fun
trans12(prgm: t1erm): t2erm
extern
fun
trans12_env
(prgm: t1erm, tenv: t1env): t2erm

(* ****** ****** *)

implement
trans12(prgm) =
trans12_env(prgm, T1ENVnil())

(* ****** ****** *)
(*
extern
fun
t1env_find
(t1env, t1var): myoptn(t2var)
*)
(* ****** ****** *)

(*
implement
trans12_env
(trm0, env0) =
(
case- trm0 of
//
| T1Mint(i0) => T2Mint(i0)
| T1Mbtf(b0) => T2Mbtf(b0)
//
| T1Mvar(x1) =>
  T2Mvar(y1) where
  { val-
    myoptn_cons(y1) =
    t1env_find(env0, x1) }
//
| T1Mlam(x1, T1, t2) =>
  T2Mlam(y1, T1, u2) where
  {
    val y1 = t2var_new()
    val u2 =
    trans12_env(t2, env1) where
    {
      val env1 = T1ENVcons(x1, y1, env0)
    }
  }
//
) (* end of [trans12_env] *)
*)

(* ****** ****** *)

(* end of [lambda2.dats] *)
