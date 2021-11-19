(* ****** ****** *)
//
#include "./../lambda0.dats"
//
(* ****** ****** *)

fun
T0Madd
( x1: term0
, x2: term0): term0 =
T0Mopr
( "+"
, mylist_cons
  ( x1
  , mylist_cons(x2, mylist_nil()))
)

fun
T0Mmul
( x1: term0
, x2: term0): term0 =
T0Mopr
( "*"
, mylist_cons
  ( x1
  , mylist_cons(x2, mylist_nil()))
)

(* ****** ****** *)

fun
T0Mtuplen
(tup: term0): term0 =
T0Mopr
("tuplen", mylist_cons(tup, mylist_nil()))

(* ****** ****** *)

extern
fun
term0_eval1_opr
(prgm: term0, env0: tenv0): tval0

(* ****** ****** *)

implement
term0_eval1
(prgm, env0) =
(
case-
prgm of
|
T0Mopr(nm, ts) =>
term0_eval1_opr(prgm, env0)
) (* end of [term0_eval1] *)

(* ****** ****** *)

implement
term0_eval1_opr
(prgm, env0) =
let
val-
T0Mopr(nm, ts) = prgm
val
vs =
mylist_map<term0><tval0>
(ts, lam(t1) => term0_eval1(t1, env0))
in
case nm of
|
"+" =>
let
  val-
  mylist_cons
  (T0Vint(i1), vs) = vs
  val-
  mylist_cons
  (T0Vint(i2), vs) = vs in T0Vint(i1 + i2)
end
//
|
"*" =>
let
  val-
  mylist_cons
  (T0Vint(i1), vs) = vs
  val-
  mylist_cons
  (T0Vint(i2), vs) = vs in T0Vint(i1 * i2)
end
//
|
"tuplen" =>
let
  val-
  mylist_cons
  (T0Vtup(vs), vs) = vs
in
  T0Vint(mylist_length(vs))
end
//
end // end of [term0_eval1_opr]

(* ****** ****** *)

(* end of [assign02_sol.dats] *)
