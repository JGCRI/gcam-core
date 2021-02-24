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

#include "solution/util/include/functor.hpp"
#include "solution/util/include/fdjac.hpp"
#include "util/base/include/util.h"
#include "solution/util/include/ublas-helpers.hpp"

#include "solution/util/include/jacobian-precondition.hpp"

#include "util/base/include/timer.h"


/* ensure that markets going into a solver that uses a jacobian start
   off in price regimes that produce nonsingular jacobians. */
int jacobian_precondition(UBVECTOR &x, UBVECTOR &fx, UBMATRIX &J, VecFVec &F,
                          std::ostream *diagnostic, bool loginputsp, double FTOL)
{
  const double JPCMIN = util::getVerySmallNumber(); // if max column value is less than this, the column is "singular".
  const double JPCLOGINCR = 1.0;   // corresponds to an e-fold increase in price if x is a log-price.  This must always be >0
  const double JPCLINEARFAC = 2.0; // This must always be >1.
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
        // Tiny derivative on the diagonal, which will likely cause
        // the Jacobian to be singular.  If fx[j] is nearly zero,
        // ignore it (we'll deal with it later).  Otherwise, search
        // for a price that is giving us a reasonable derivative.
        double incr;
        double t = x[j];          // store the original value
        // the diagonal element is zero, so we must try to find a price that makes it nonzero

        // We have to guess which way to go to get to a viable price
        // regime. We'll use the current excess demand as our guide.
        // If ED>0, our price is probably at the low end, so we will
        // search upward in price.  Otherwise, we'll search downward
        if(loginputsp) {
          // for log inputs this additive increment will actually be a
          // multiplicative change in the price value
          if(fx[j] > 0)
            incr = JPCLOGINCR;
          else
            incr = -JPCLOGINCR;
        }
        else {
          // for linear inputs things are a little more complicated.
          // For the most part we still want a multiplicative change,
          // but we need to be careful when the absolute value of the
          // price is low, and we need to make sure that the price moves
          // in the correct direction when the price is negative.
        
          // You might have noticed that we make no effort to make the
          // treatment of positive and negative values symmetric.  That
          // is, for positive prices we multiply the price if we want to
          // increase it, and we divide it if we want to decrease it.
          // But for negative prices we add the same increment that we
          // would add to a positive price of the same absolute value to
          // increase it, and likewise for subtraction.  That's because
          // negative prices should be unusual, and if we're seeing zero
          // derivatives for negative prices, then we probably don't
          // need to be in negative price territory.  Therefore, the big
          // steps are always in the positive direction, and the small
          // steps are always in the negative direction.
          if(fx[j] > 0) {
            // want to increase the price.  JPCLINEARFAC is supposed to
            // be a multiplier, but we're going to calculate an
            // equivalent adder here.  That way we can be certain that a
            // positive number always corresponds to an increase.
            incr = (JPCLINEARFAC-1.0) * std::max(fabs(x[j]), 1.0);
          }
          else {
            // want to decrease the price.  Note that the first term
            // gives us the correct sign for the increment.
            incr = (1.0/JPCLINEARFAC - 1.0) * std::max(fabs(x[j]), 1.0);
          }
        }

        if(diagnostic)
          (*diagnostic) << "Searching j= " << j << "  x[j] = " << x[j] << "  fx[j] = " << fxx[j]
                        << "  J(j,j) = " << J(j,j) << "  incr = " << incr << "\n";
      
        int count = 0;
        double deltafx=0.0;
        do {
          x[j] += incr;
          
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
            (*diagnostic) << "jacobian_preconditioner: Unable to find a good price for j= " << j
                          << "  x[j]= " << x[j] << "  fx[j]= " << fx[j] << "\n";
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
