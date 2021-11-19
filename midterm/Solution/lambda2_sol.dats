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
val t2v0 = t2var_new()
in
t2v0->t2var_t1var := name; t2v0
end

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [lambda2_sol.dats] *)
