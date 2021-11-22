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
| T2Vref of (ref(t2val))
| T2Vtup of (t2val, t2val)
and
t2env =
| ENVnil of ()
| ENVcons of (t2var, t2val, t2env)

(* ****** ****** *)

extern
fun
interp0(prgm: t2erm): t2val
extern
fun
interp0_env
(prgm: t2erm, env0: t2env): t2val

(* ****** ****** *)

(* end of [lambda2_interp.dats] *)
