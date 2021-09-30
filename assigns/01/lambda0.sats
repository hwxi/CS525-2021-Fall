(* ****** ****** *)

typedef tvar0 = string

(* ****** ****** *)
//
datatype term0 =
//
| T0Mint of int // integers
| T0Mbtf of bool // booleans
//
| T0Mneg of (term0)
| T0Madd of (term0, term0)
| T0Msub of (term0, term0)
| T0Mmul of (term0, term0)
| T0Mdiv of (term0, term0)
//
| T0Milt of (term0, term0) // for [<]
| T0Mile of (term0, term0) // for [<=]
| T0Migt of (term0, term0) // for [>]
| T0Mige of (term0, term0) // for [>=]
//
| T0Mvar of tvar0 // variable/name
| T0Mlam of (tvar0, term0) // abstraction
| T0Mapp of (term0, term0) // application
//
| T0Mfix of (tvar0, term0) // fixed-point
//
| T0Mifb of // if-then-else
  (term0(*test*), term0(*then*), term0(*else*))
//
(* ****** ****** *)

fun
print_term0(t0: term0): void
fun
fprint_term0
(out: FILEref, t0: term0): void

overload print with print_term0
overload fprint with fprint_term0

(* ****** ****** *)

fun
term0_interp(prgm: term0): term0

(* ****** ****** *)

(* end of [lambda0.sats] *)
