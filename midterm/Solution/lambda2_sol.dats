(* ****** ****** *)
//
#include
"./../lambda2.dats"
//
(* ****** ****** *)
#dynload
"./../lambda1.dats"
(* ****** ****** *)
implement
main0() = ((*dummy*))
(* ****** ****** *)

typedef stamp = int

(* ****** ****** *)

local

assume
t2var_type =
ref@{
t2var_t1var= t1var
,
t2var_t1ype= t1ype
,
t2var_stamp= stamp
}

val
the_t2var_stamp =
ref_make_elt<stamp>(1)

fun
t2var_stamp_new
((*void*)): int =
let
val
stamp =
!the_t2var_stamp
in
!the_t2var_stamp := stamp+1; stamp
end // end of [t2var_stamp_new]

in

(* ****** ****** *)
implement
t2var_get_stamp
(X) = X->t2var_stamp
implement
t2var_get_t1ype
(X) = X->t2var_t1ype
(* ****** ****** *)

implement
t2var_new() =
ref@{
t2var_t1var= ""
,
t2var_t1ype= t1ype_new_ext()
,
t2var_stamp= t2var_stamp_new()
}

(* ****** ****** *)

implement
t2var_new_t1var
(name) =
let
val X = t2var_new()
in
  X->t2var_t1var := name; X
end

(* ****** ****** *)
implement
fprint_t2var(out, X) =
{
val () =
fprint!(out, X->t2var_t1var)
val () =
fprint!(out, "[", X->t2var_stamp, "]")
}
(* ****** ****** *)

end // end of [local]

(* ****** ****** *)
extern
fun
t1env_find
( t1env
, t1var): myoptn(t2var)
(* ****** ****** *)

implement
t1env_find
(env0, x0) = myoptn_nil()

(* ****** ****** *)

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
  T2Mlam(y1, T1, t2) where
  {
    val y1 =
    t2var_new_t1var(x1)
    val t2 =
    trans12_env(t2, env1) where
    {
      val
      env1 =
      T1ENVcons( x1, y1, env0 )
    }
  }
//
| T1Mapp(t1, t2) =>
  T2Mapp(t1, t2) where
  {
    val t1 = trans12_env(t1, env0)
    val t2 = trans12_env(t2, env0)
  }
//
) (* end of [trans12_env] *)

(* ****** ****** *)

(* end of [lambda2_sol.dats] *)
