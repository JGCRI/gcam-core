#ifndef FLTCMP_HH_
#define FLTCMP_HH_

#include <stdint.h>
#include <stdlib.h>


/*!
 * \brief Compare two doubles for approximate equality
 *
 * This function compares two doubles by computing their difference in
 * "units in the last place" (ulp).  Two doubles that are adjacent in
 * the floating point representation differ by 1 ulp.  If they have
 * one representable number between them the difference is 2 ulp, etc.
 * For two numbers that are nominally the result of the same short
 * sequence of calculations, the difference should be no more than a
 * few ulp (for basic operations like addition and multiplication,
 * it's roughly sqrt(N)/2, where N is the number of operations).  For
 * more complicated formulae, the discrepancy could be a few hundred
 * ulp.  For results that are the output of a lengthy algorithm (e.g.,
 * testing an iterative solver for convergence), you shouldn't use
 * this function at all; use a fractional tolerance instead.
 *
 * \warning For floating point numbers near +/- FLT_MAX the "numbers"
 * a few ulp away might be +/- Inf, and the next number after (before)
 * + (-) Inf is a NaN.  If you will be operating in these regimes, use
 * appropriate precautions.
 */
inline bool dblcmp(double x, double y, int64_t tol=5)
{
  const int64_t moffset = int64_t(1)<<63;
  union {int64_t i; double f;} x1,x2;
  x1.f = x;
  x2.f = y;
  
  if(x1.i < 0)
    x1.i = moffset-x1.i;
  if(x2.i < 0)
    x2.i = moffset-x2.i;

  int64_t diff = abs(x1.i-x2.i);

  if(diff <= tol)
    return true;
  else
    return false;
  
}


#endif
