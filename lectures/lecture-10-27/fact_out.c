/* ****** ****** */

#include "runtime.h"

/* ****** ****** */

extern
lamval
LAMVAL_print(lamval x)
{
  int tag;
  tag = x->tag;
  switch( tag )
  {
    case TAGcfn:
      printf("<lamval_cfn>"); break;
    case TAGint:
      printf("%i", ((lamval_int)x)->data); break;
    case TAGstr:
      printf("%s", ((lamval_str)x)->data); break;
    default: printf("Unrecognized tag = %i", tag);
  }
}

/* ****** ****** */

lamval
fact(lamval n)
{

  lamval t0;
  lamval r0, r1, r2, r3;

  r1 = n;

  /*
  printf("fact(");
  LAMVAL_print(n);
  printf(") = ...\n");
  */

  t0 = ((lamval)LAMOPR_ieq(r1, LAMVAL_int(0)));
  if
  (((lamval_int)t0)->data)
  {
    r0 = (lamval)(LAMVAL_int(1));
  } else
  {
    r2 =
    LAMOPR_sub(r1, LAMVAL_int(1));
    r0 = LAMOPR_mul(r1, fact(r2));
  }
  return r0;
}

int main() {
  LAMVAL_print(fact(LAMVAL_int(10))); printf("\n"); return 0;
}
