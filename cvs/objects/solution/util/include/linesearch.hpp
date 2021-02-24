#ifndef LINESEARCH_HPP_
#define LINESEARCH_HPP_

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
 * \file linesearch.hpp
 * \ingroup Solution
 * \brief Line search helper function for multidimensional root finders
 * \remark Because this function is defined as a template, we have to put the entire body in
 *         the header file.  It's ugly, and I don't like it, but the alternatives are worse,
 *         and nonportable to boot.
 */

#include "util/base/include/util.h"
#include "solution/util/include/functor.hpp"
#include "solution/util/include/ublas-helpers.hpp"
#include <algorithm>
#include <iostream>


/*!
 * Perform a line search for use in multidimensional root finders.  This is NOT a 1-D
 * minimization routine!
 * \tparam FTYPE: floating point type.  Note we require the function have the same input and
 *                output types
 * \param[in]  f: A function adaptor for computing f(x) = F*F (for vector-valued F) 
 * \param[in] x0: Starting point for the step
 * \param[in] f0: Value of f(x0)
 * \param[in] g0: Value of grad-f(x0)
 * \param[in] dx: Proposed step from the root finder
 * \param[out] x: Final step determined by the search
 * \param[out]fx: Value of f(x) 
 * \param[inout]neval: number of function evaluations. The subroutine
 * adds whatever value is passed in, allowing the caller to keep a
 * running total.
 * \return : 0= success, anything else= fail
 *
 */
int linesearch(VecFVec &f, const UBVECTOR &x0,
               double f0, const UBVECTOR &g0,
               const UBVECTOR &dx, UBVECTOR &x,
               double &fx, int &neval, std::ostream *solverlog = 0)
{
  const double lseps = 1.0e-7;   // part of the definition of "sufficient" decrease
  const double TOLX = 1.0e-6;    // tolerance for x values
  // NB: This value of TOLX is way too small for single-precision,
  // but we're running GCAM in double precision anyhow, and the small
  // tolerance seems to be necessary for solving global unconventional
  // oil, which has a nasty step function in its demand.  If we ever
  // contemplate using this function in a single-precision
  // application, we are going to have to adjust this tolerance.
  int n = x0.size();
  double g0dx=g0.dot(dx); // initial rate of decrease, df/dlambda
  double lambda = 1.0;           // start with full step
  double maxval = 0.0;
  UBVECTOR fxVec(x0.size());

  if(g0dx >= 0) {
    if(solverlog)
      (*solverlog) << "Linesearch aborted.  Not an initial descent direction.  g0dx= "
                   << g0dx << "\n";
    return 1;                   // If we're being called by Broyden's
                                // method, this will trigger a recalc
                                // on the Jacobian
  }
  
  // set lmin (minimum admissable value for lambda)
  for(int i=0; i<n; ++i) {
    double tmp = fabs(dx[i] / (x0[i] + TOLX)); // fractional change in ith component of x over a unit step
    maxval = std::max(tmp,maxval);          // pick the largest
  }
  double lmin = TOLX / maxval;     // when lambda gets this small, even
                                // the fastest changing x is changing
                                // by less than TOLX

  if(solverlog)
    (*solverlog) << "Beginning linesearch: lmin = " << lmin << "  f0 = " << f0 << "\n";
  
  while(lambda > lmin) {
    x  = x0 + lambda*dx;
    f(x, fxVec);
    fx = fxVec.dot(fxVec);
    neval++;

    if(solverlog) {
        (*solverlog) << "\tlambda = " << lambda << "  fx = " << fx << std::endl;
    }

    // check for exit condition.  The term after the + is negative, so
    // it specifies a minimum rate of decrease.  lseps is set fairly
    // small, so we're biased toward accepting steps unless their rate
    // of decrease is painfully slow
    if(fx <= f0 + lseps*lambda*g0dx)
      // SUCCESS
      return 0;

    // last step increased or decreased too slowly --- backtrack
    double tl0 = 0.1*lambda; // never decrease lambda by more than a factor of 10
    double tl1 = 0.5*lambda; // always decrease lambda by at least half
    // We fit a quadratic approximation to f(lambda) using the values
    // we've computed already, and we try the minimum of the quadratic
    // as our next lambda (subject to the constraints above).  NR
    // suggests that on iterations after the first we fit a cubic, but
    // it's not clear that that buys us a whole lot, so we'll try just
    // using the quadratic every time for now.
    double denom = fx - f0 - g0dx*lambda;
    if(denom != 0.0)
      lambda = -g0dx * (lambda*lambda)/(2.0 * denom);
    else
      lambda = 0.5*lambda;

    lambda = std::max(tl0, std::min(tl1,lambda));
  }
  
  // If we get here, then the line search failed.  Depending on the
  // multidimensional solver we're using, we may just need to refresh
  // the Jacobian.  Otherwise, try starting with a new initial guess.
  return 1;              
}

#endif
