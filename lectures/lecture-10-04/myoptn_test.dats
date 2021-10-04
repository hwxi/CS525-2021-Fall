
#include
"share/atspre_staload.hats"

(* ****** ****** *)

#staload "./../../mylib/mylib.dats"

(* ****** ****** *)

#staload "./../../mylib2/mylib2.dats"

(* ****** ****** *)

implement main0() = ()

(* ****** ****** *)

fun
intdiv(x: int, y: int): myoptn(int) =
if y != 0 then myoptn_cons(x/y) else myoptn_nil()

(* ****** ****** *)

(* end of [mylist_test.dats] *)
