(* ****** ****** *)

datatype
myoptn(a:t@ype) =
| myoptn_nil of ()
| myoptn_cons of (a)

(* ****** ****** *)

datatype
mylist(a:t@ype) =
| mylist_nil of ()
| mylist_cons of (a, mylist(a))

(* ****** ****** *)

extern
fun
{a:t@ype}
mylist_sing(x0: a): mylist(a)

(* ****** ****** *)

extern
fun
{a:t@ype}
mylist_length(xs: mylist(a)): int

(* ****** ****** *)
extern
fun
{a:t@ype}
mylist_append
(xs: mylist(a), ys: mylist(a)): mylist(a)
(* ****** ****** *)

extern
fun
{a:t@ype}
mylist_reverse(xs: mylist(a)): mylist(a)
extern
fun
{a:t@ype}
mylist_rappend
(xs: mylist(a), ys: mylist(a)): mylist(a)

(* ****** ****** *)

extern
fun
{a:t@ype}
mylist_remove_eqs
(xs: mylist(a), x0: a): mylist(a)

(* ****** ****** *)

extern
fun
{a:t@ype}
{b:t@ype}
mylist_map
(xs: mylist(a), f0: a -<cloref1> b): mylist(b)

(* ****** ****** *)

extern
fun
{a:t@ype}
fprint_mylist
(out: FILEref, xs: mylist(a)): void
extern
fun{}
fprint_mylist_sep(out: FILEref): void

overload fprint with fprint_mylist

(* ****** ****** *)
(*
HX-2021-10-05:
Implementation should be given below
*)
(* ****** ****** *)

implement
{a}
mylist_sing(x0) =
mylist_cons(x0, mylist_nil())

(* ****** ****** *)

implement
{a}
mylist_length(xs) =
loop(xs, 0) where
{
fun
loop(xs: mylist(a), r0: int): int =
case+ xs of
| mylist_nil() => r0
| mylist_cons(x1, xs) => loop(xs, r0+1)
}

(* ****** ****** *)

implement
{a}
mylist_append(xs, ys) =
mylist_rappend<a>
(mylist_reverse<a>(xs), ys)

(* ****** ****** *)

implement
{a}
mylist_reverse(xs) =
mylist_rappend<a>(xs, mylist_nil())

(* ****** ****** *)

implement
{a}
mylist_rappend(xs, ys) =
case+ xs of
|
mylist_nil() => ys
|
mylist_cons(x1, xs) =>
mylist_rappend<a>(xs, mylist_cons(x1, ys))

(* ****** ****** *)

implement
{a}
mylist_remove_eqs
(xs, x0) =
(
  helper(xs)
) where
{
fun
helper
(xs: mylist(a)): mylist(a) =
(
case+ xs of
| mylist_nil() =>
  mylist_nil()
| mylist_cons(x1, xs) =>
  if
  geq_val_val<a>(x0, x1)
  then helper(xs) else mylist_cons(x1, helper(xs))
)
}

(* ****** ****** *)

implement
{a}{b}
mylist_map
(xs, f0) =
(
  helper(xs)
) where
{
fun
helper
(xs: mylist(a)): mylist(b) =
case+ xs of
| mylist_nil() =>
  mylist_nil()
| mylist_cons(x1, xs) =>
  mylist_cons(f0(x1), helper(xs))
}

(* ****** ****** *)

implement
{(*tmp*)}
fprint_mylist_sep(out) = fprint(out, "; ")

(* ****** ****** *)

implement
{a}
fprint_mylist
(out, xs) =
loop(xs, 0) where
{
fun
loop
(xs: mylist(a), i0: int): void =
(
case+ xs of
| mylist_nil() => ()
| mylist_cons(x1, xs) =>
  (if i0 > 0
   then fprint_mylist_sep<>(out);
   fprint_val<a>(out, x1); loop(xs, i0+1))
)
} (* end of [fprint_mylist] *)

(* ****** ****** *)

(* end of [mylib.dats] *)
