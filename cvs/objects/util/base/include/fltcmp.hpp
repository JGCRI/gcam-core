#ifndef FLTCMP_HPP_
#define FLTCMP_HPP_

#include <stdint.h>
#include <stdlib.h>


/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

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

/*! \brief "loose" tolerance for dblcmp
 *
 *   This tolerance is very loose.  The cases where we use it arguably
 *   contradict the proscription against using dblcmp to test the
 *   output of a lengthy algorithm, but we do it anyway.  Nobody's
 *   perfect.
 */
static const int64_t DBL_CMP_LOOSE = 4096;

/*! \brief "strict" tolerance for dblcmp */
static const int64_t DBL_CMP_STRICT = 8;


#endif
