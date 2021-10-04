
#include
"share/atspre_staload.hats"

(* ****** ****** *)

#staload "./../../mylib/mylib.dats"

(* ****** ****** *)

#staload "./../../mylib2/mylib2.dats"

(* ****** ****** *)

implement main0() = ()

val xs = mylist_nil()
val xs = mylist_cons(1, xs)
val xs = mylist_cons(2, xs)
val xs = mylist_cons(3, xs)

val ln =
mylist_length<int>(xs)
val () = println!("ln = ", ln)

val ys =
mylist_reverse<int>(xs)
val ln = mylist_length<int>(ys)
val () = println!("ln = ", ln)

(* ****** ****** *)

(* end of [mylist_test.dats] *)
