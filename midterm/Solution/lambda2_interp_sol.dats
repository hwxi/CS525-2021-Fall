(* ****** ****** *)
#dynload
"./../lambda1.dats"
(* ****** ****** *)
//
#include
"./../lambda2_interp.dats"
//
(* ****** ****** *)
implement main0() = ((*dummy*))
(* ****** ****** *)
//
implement
interp0_env
(trm0, env0) =
(
case- trm0 of
//
| T2Mint(i0) => T2Vint(i0)
| T2Mbtf(b0) => T2Vbtf(b0)
//
| T2Mref_new(t1) =>
  (
    T2Vref(ref(v1))) where
  {
    val v1 = interp0_env(t1, env0)
  }
| T2Mann(t1, T1) => interp0_env(t1, env0)
//
)
//
(* ****** ****** *)

(* end of [lambda2_interp_sol.dats] *)
