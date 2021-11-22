(* ****** ****** *)
#dynload
"./../lambda1.dats"
(* ****** ****** *)
//
#include
"./../lambda2_tinfer.dats"
//
(* ****** ****** *)
implement main0() = ((*dummy*))
(* ****** ****** *)

val T1Pint = T1Pbas("int")
val T1Pbtf = T1Pbas("btf")
val T1Pstr = T1Pbas("str")

(* ****** ****** *)
//
implement
tinfer0(trm0) =
(
case- trm0 of
//
| T2Mint(i0) => T1Pint
| T2Mbtf(b0) => T1Pbtf
//
)
//
(* ****** ****** *)

(* end of [lambda2_tinfer_sol.dats] *)
