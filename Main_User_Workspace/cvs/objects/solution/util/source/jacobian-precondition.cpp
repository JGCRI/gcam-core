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

#include <boost/numeric/ublas/lu.hpp>
#if USE_LAPACK
#include <boost/numeric/bindings/traits/ublas_vector.hpp>
#include <boost/numeric/bindings/traits/ublas_matrix.hpp>
#include <boost/numeric/bindings/lapack/gesvd.hpp>
#endif
#include <boost/numeric/ublas/operation.hpp>
#include "solution/util/include/functor-subs.hpp"
#include "solution/util/include/fdjac.hpp" 
#include "util/base/include/util.h"
#include "solution/util/include/ublas-helpers.hpp"

#include "solution/util/include/svd_invert_solve.hpp"
#include "solution/util/include/jacobian-precondition.hpp"

#include "util/base/include/timer.h"

#if USE_LAPACK
#define UBMATRIX boost::numeric::ublas::matrix<double,boost::numeric::ublas::column_major>
#else
#define UBMATRIX boost::numeric::ublas::matrix<double>
#endif
#define UBVECTOR boost::numeric::ublas::vector<double> 


/* ensure that markets going into a solver that uses a jacobian start
   off in price regimes that produce nonsingular jacobians. */
int jacobian_precondition(UBVECTOR &x, UBVECTOR &fx, UBMATRIX &J, VecFVec<double,double> &F,
                          std::ostream *diagnostic, double FTOL)
{
  const double JPCMIN = util::getVerySmallNumber(); // if max column value is less than this, the column is "singular".
  const double JPCINCR = 1.0;   // corresponds to an e-fold increase in price if x is a log-price
  const int ITMAX = 50;
  int fail = 0;
  int change = 0;
  int ncol = (int) x.size();

  Timer& jacPreTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::JAC_PRE );
  jacPreTimer.start();
  UBVECTOR fxx(fx),fxdiff(fx.size()); // fxx = last distinct value of F(x)
  
  
  /* We will focus on ill-conditioning that results from an entire
     column of (nearly) zero entries.  This corresponds to a situation
     where changing a price doesn't affect the ED of any market in the
     calculation.  This situation comes up rather frequently when, for
     instance, a market is below the turn-on price for its supply
     curve, and it can often be fixed by changing the starting price.

  */

  for(int j=0; j<ncol; ++j) {
    // Restrict our attention to the diagonal term.  If it's
    // (effectively) zero, then we're in a bad part of the parameter
    // space.
    double diagval = fabs(J(j,j));

    if(diagval < JPCMIN && fabs(fx[j]) > FTOL) {
      double incr;
      double t = x[j];          // store the original value
      // the diagonal element is zero, so we must try to find a price that makes it nonzero

      // We have to guess which way to go to get to a viable price
      // regime. We'll use the current excess demand as our guide.
      // If ED>0, our price is probably at the low end, so we will
      // search upward in price.  Otherwise, we'll search downward
      if(fx[j] > 0)
        incr = JPCINCR;
      else
        incr = -JPCINCR;

      if(diagnostic)
        (*diagnostic) << "Searching j= " << j << "  x[j] = " << x[j] << "  fx[j] = " << fxx[j]
                      << "  J(j,j) = " << J(j,j) << "  incr = " << incr << "\n";
      
      int count = 0;
      double deltafx=0.0;
      do {
        x[j] += incr;           // since we expect x to be a
                                // log-price, this is actually a
                                // multiplicative change in price.

        F(x,fx);
        deltafx = fabs(fxx[j]-fx[j]);

        if(diagnostic)
          (*diagnostic) << "\txtry = " << x[j] << "  fxtry = " << fx[j] << "\n";
        
        if(deltafx > JPCMIN) {
          change = 1;
          // success (probably -- it's theoretically possible we
          // jumped completely over the "good" range of the
          // parameters.  We'll add checks for that (only) if it looks like
          // it's becoming a problem.)
          fxx = fx;
        }
      } while(deltafx < JPCMIN && ++count < ITMAX);

      if(deltafx < JPCMIN) {
        fail = 1;
        x[j] = t;               // restore the old value.
        if(diagnostic)
          (*diagnostic) << "jacobian_preconditioner: Unable to find a good price for j= " << j << "\n";
      }
      else if(diagnostic) {
        (*diagnostic) << "jacobian_preconditioner: Revised initial guess for x[" << j << "] = " << x[j] << "\n";
      }
    }
  }

  Timer& jacPreJacTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::JAC_PRE_JAC );
  jacPreJacTimer.start();
              
  if(change)
    fdjac(F,x,fx,J,true); // recalculate the jacobian

  jacPreJacTimer.stop();
  jacPreTimer.stop();

  return fail;
}


void broyden_singular_B_reset(UBVECTOR &x, UBVECTOR &fx, UBMATRIX &B, VecFVec<double,double> &F,
                             std::ostream *diagnostic, double FTOL)
{
  const double JPCMIN = util::getVerySmallNumber(); // if diagonal value is less than this, the column is "singular".

  /* If we get a singular matrix in the Broyden solver, we have a
     simple solution.  Since B is just an approximate Jacobian, and
     since we know that "normally" an increase a market's in price
     reduces excess demand in that market, we just set any deficient
     diagonal terms to -JPCMIN and keep on trucking. */
  int nrow = B.size1(), ncol = B.size2();

  for(int j=0;j<nrow;++j)
    if(fabs(B(j,j)) < JPCMIN && fabs(fx[j]) > FTOL) {
      if(diagnostic)
        (*diagnostic) << "Resetting diagonal term at j = " << j << "  x = " << x[j]
                      << "  fx = " << fx[j] << "  old B = " << B(j,j) << "\n";
      B(j,j) = -JPCMIN;
    }
}

