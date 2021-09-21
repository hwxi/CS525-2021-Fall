(* ****** ****** *)

#include
"share/atspre_staload.hats"

#include "./../assign00.dats"

(* ****** ****** *)

(*
extern
fun factorial : int -> int
*)
(*
implement
factorial(n) =
if n > 0
then n * factorial(n-1) else 1
*)

(* ****** ****** *)

implement
factorial(n) =
let
//
fun
loop(i: int, res: int): int =
if i < n
then loop(i+1, (i+1)*res) else res
//
in
  loop(0, 1)
end

(* ****** ****** *)

(*
0, 1, 1, 2, 3, 5, 8, ...
*)
extern
fun fibonacci(n: int): int

(* ****** ****** *)

(*
implement
fibonacci(n) =
if
(n >= 2)
then
fibonacci(n-1) + fibonacci(n-2) else n
*)

(* ****** ****** *)

implement
fibonacci(n) =
let

fun
loop
(i: int, res1: int, res2: int): int =
if
i < n
then loop(i+1, res2, res1+res2) else res1

in
  loop(0, 0, 1)
end

(* ****** ****** *)

implement main0() =
(*
println!("factorial(10) = ", factorial(10))
*)
println!("fibonacci(10) = ", fibonacci(10))

(* ****** ****** *)

(* end of [assign00_sol.dats] *)
