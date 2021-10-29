(* ****** ****** *)
#include
"share\
/atspre_staload.hats"
(* ****** ****** *)
#staload "./lambda0.dats"
(* ****** ****** *)
(*
//
// HX: 20 points
// Please implement a term
// that test whether a given
// natural number is a prime
// You should use the typechecker
// implemented in Assign03 to
// type-check this implementation
// of 'isprime'.
//
fun
isprime(x: int): bool =
(
if x >= 2 then test(2) else false
) where
{
fun
test(i: int): bool =
if
i < x
then
(if x % i = 0 then false else test(i+1))
else true
}
//
*)
extern
val TMisprime : term0
//
// The term essentially encodes 'isprime'
//
(* ****** ****** *)
(*
//
// HX: 20 points
// Please implement 'isprime' in C using
// the boxed data representation covered
// in the lectures on 10/27 and 10/29.
//
*)
(* ****** ****** *)

(* end of [assign04.dats] *)
