/*
// A basic runtime for lambda
*/

/* ****** ****** */

#include <stdio.h>
#include <stdlib.h>

/* ****** ****** */

extern
void*
mymalloc(size_t);

/* ****** ****** */

#define TAGint 1
#define TAGstr 2
#define TAGdbl 3
#define TAGcfn 4 // closure function

/* ****** ****** */

typedef
struct{ int tag; } lamval_;
/*
typedef
lamval_ = struct{ int tag; }
*/

typedef lamval_ *lamval;
/*
typedef *lamval = lamval_
*/

/* ****** ****** */

int
LAMVAL_tag(lamval x){ return x->tag; }

/* ****** ****** */

typedef
struct{
  int tag; int data;
} lamval_int_;
typedef
struct{
  int tag; char *data;
} lamval_str_;
typedef
struct{
  int tag; double data;
} lamval_dbl_;

typedef lamval_int_ *lamval_int;
typedef lamval_str_ *lamval_str;
typedef lamval_dbl_ *lamval_dbl;

/* ****** ****** */

extern
void*
mymalloc(size_t n) {
  void* p0;
  p0 = malloc(n);
  if (p0 != 0) return p0;
  fprintf(stderr, "myalloc failed!!!\n");
  exit(1);
}

/* ****** ****** */

extern
lamval
LAMVAL_int(int i)
{
  lamval_int p0;
  p0 = mymalloc(sizeof(lamval_int_));
  p0->tag = TAGint; p0->data = i; return (lamval)p0;
}

/* ****** ****** */

extern
lamval
LAMOPR_add(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return
  LAMVAL_int(((lamval_int)x)->data + ((lamval_int)y)->data);
}

extern
lamval
LAMOPR_sub(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);ppp
  */
  return
  LAMVAL_int(((lamval_int)x)->data - ((lamval_int)y)->data);
}

/* ****** ****** */

extern
lamval
LAMOPR_mul(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return
  LAMVAL_int(((lamval_int)x)->data * ((lamval_int)y)->data);
}

/* ****** ****** */

extern
lamval
LAMOPR_ilt(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);
  */
  return
  LAMVAL_int(((lamval_int)x)->data < ((lamval_int)y)->data ? 1 : 0);
}

extern
lamval
LAMOPR_ile(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);ppp
  */
  return
  LAMVAL_int(((lamval_int)x)->data <= ((lamval_int)y)->data ? 1 : 0);
}

/* ****** ****** */

extern
lamval
LAMOPR_igt(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);ppp
  */
  return
  LAMVAL_int(((lamval_int)x)->data > ((lamval_int)y)->data ? 1 : 0);
}

extern
lamval
LAMOPR_ige(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);ppp
  */
  return
  LAMVAL_int(((lamval_int)x)->data >= ((lamval_int)y)->data ? 1 : 0);
}

/* ****** ****** */

extern
lamval
LAMOPR_ieq(lamval x, lamval y)
{
  /*
  assert(x->tag == TAGint);
  assert(y->tag == TAGint);ppp
  */
  return
  LAMVAL_int(((lamval_int)x)->data == ((lamval_int)y)->data ? 1 : 0);
}
