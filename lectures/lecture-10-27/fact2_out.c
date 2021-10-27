/*

fun
fact(n) = loop(1, 1) where
{
fun
loop(i, r) =
if i <= n then loop(i+1, i*r) else r
}
*/

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

static
lamval
loop(lamval i, lamval r, lamval n);

extern
lamval
fact(lamval n)
{
  return loop(LAMVAL_int(1), LAMVAL_int(1), n);
}

/* ****** ****** */

#define INT(x) LAMVAL_int(x)

static
lamval
loop(lamval i, lamval r, lamval n)
{
  // if i <= n then loop(i+1, i*r) else r

  lamval t0;
  lamval t1, t2;
  lamval r0;
  
  t0 = LAMOPR_ile(i, n);
  
  if
  (((lamval_int)t0)->data)
  {
    t1 = LAMOPR_add(i, INT(1));
    t2 = LAMOPR_mul(i, r);
    r0 = loop(t1, t2, n);
  } else {
    r0 = r;
  }
  return r0;
}

/* ****** ****** */

/*
static
lamval
loop(lamval i, lamval r, lamval n)
{
  // if i <= n then loop(i+1, i*r) else r

  lamval t0;
  lamval t1, t2;
  lamval r0;


  while(1)
  {
      
  t0 = LAMOPR_ile(i, n);
  
  if
  (((lamval_int)t0)->data)
  {
    t1 = LAMOPR_add(i, INT(1));
    t2 = LAMOPR_mul(i, r);
    i = t1; r = t2; continue;
    // r0 = loop(t1, t2, n);
  } else {
    r0 = r; break;
  }

  }
  
  return r0;
}
*/

/* ****** ****** */

int main() {
  LAMVAL_print(fact(INT(1000000))); printf("\n"); return 0;
}

/* ****** ****** */

