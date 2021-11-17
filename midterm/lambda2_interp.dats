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
t2val =
| T2Vint of int
| T2Vbtf of bool
and
t2env =
| ENVnil of ()
| ENVcons of (t2var, t2val, t2env)

(* ****** ****** *)

extern
fun
interp(prgm: t2erm): t2val
extern
fun
interp_env
(prgm: t2erm, env0: t2env): t2val

(* ****** ****** *)

(* end of [lambda2_interp.dats] *)
