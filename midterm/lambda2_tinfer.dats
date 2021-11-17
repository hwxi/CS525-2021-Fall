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
#staload "./lambda2.dats"
//
(* ****** ****** *)

datatype
t2ctx =
| CTXnil of ()
| CTXcons of (t2var, t1ype, t2ctx)

(* ****** ****** *)

extern
fun
tinfer0(prgm: t2erm): t1ype
extern
fun
tinfer0_env
(prgm: t2erm, ctx0: t2ctx): t1ype

(* ****** ****** *)

(* end of [lambda2_tinfer.dats] *)
