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

#include "solution/util/include/fdjac.hpp"

#if GCAM_PARALLEL_ENABLED
#include <tbb/task_group.h>
#include <tbb/parallel_for_each.h>
#endif

#include "util/base/include/timer.h"
#include "containers/include/scenario.h"
#include "util/base/include/manage_state_variables.hpp"

extern Scenario* scenario;

/*!
 * Compute a single column in a Jacobian matrix.  We have broken this
 * out from the fdjac subroutine so that we can easily test a single
 * column for nonsingularity without duplicating any code.
 */
inline void jacol(VecFVec &F, const UBVECTOR &x,
                  const UBVECTOR &fx, int j,
                  UBMATRIX &J,
                  bool usepartial, std::ostream *diagnostic) {
  const double heps = 1.0e-6;
  const double TINY = 1.0e-6;
  UBVECTOR xx(x); // temporary, so we can respect the const on x
  UBVECTOR fxx(fx.size());        // hold the values of F(xx)
  double t = xx[j];            // store the old value
  double h = heps * (fabs(t)+TINY);
  
  xx[j] = t+h;
  h     = xx[j]-t; // reduce roundoff error, since (t+h)-t is not
                   // necessarily identical to the original h
  if(diagnostic) {
      (*diagnostic) << "j= " << j << "\th= " << h << "\nxx:\n" << xx << "\n";
  }
  if(usepartial) {F.partial(j);}    // hint to the function that this is a partial derivative calculation
    F(xx,fxx, usepartial ? j : -1);       // eval the function
  xx[j] = t;       // restore the old value
  
  if(diagnostic) {
    (*diagnostic) << "fxx:\n" << fxx << "\n";
  }
  
  // compute the finite difference derivatives
  double hinv = 1.0/h;
  for(size_t i=0; i<fxx.size(); ++i) {
    J(i,j) = (fxx[i] - fx[i]) * hinv;
  }
}

/*!
 * Compute the Jacobian of a vector function F at point x.
 * \tparam FTYPE: The floating point type of the input and output vectors
 * \param[in] F: The function to have its Jacobian calculated
 * \param[in] x: The point at which to calculate the Jacobian
 * \param[in] fx: F(x)
 * \param[out] J: The Jacobian of F
 * \param[in] usepartial: (optional) use partial model evaluation for partial derivatives
 * \param[in] diagnostic: (optional) ostream pointer to which to send additional diagnostics
 *
 */
void fdjac(VecFVec &F, const UBVECTOR &x,
           const UBVECTOR &fx, UBMATRIX &J, bool usepartial,
           std::ostream *diagnostic)
{
  if(diagnostic) {
    (*diagnostic) << "fdjac: usepartial = " << usepartial << "\nInitial x:\n" << x
        << "\nInitial fx:\n" << fx << "\n";
  }

  Timer& jacTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::JACOBIAN );
  jacTimer.start();
    if(usepartial) { scenario->getManageStateVariables()->setPartialDeriv(true); }
  
#if !GCAM_PARALLEL_ENABLED
  for(size_t j=0; j<x.size(); ++j) {
    jacol(F, x, fx, j, J, usepartial, diagnostic);
  }
#else
    tbb::task_arena& threadPool = scenario->getManageStateVariables()->mThreadPool;
    tbb::task_group tg;
    threadPool.execute([&](){
        tg.run([&](){
            tbb::parallel_for_each( x, [&]( const double& j ) {
                jacol(F, x, fx, (&j - &x[0]), J, usepartial, 0/*diagnostic*/);
            });
        });
    });
    threadPool.execute([&tg](){ tg.wait(); });
#endif
    if(usepartial) { F.partial(-1); }

  jacTimer.stop();
}

/*!
 * Compute the Jacobian of a vector function F at point x.
 * \param[in] F: The function to have its Jacobian calculated
 * \param[in] x: The point at which to calculate the Jacobian
 * \param[out] J: The Jacobian of F
 * \remark This function evaluates F(x) and then calls fdjac(F,x,Fx,J).  If you have already
 *         evaluated F(x), you should call the latter version directly.
 * \warning The Jacobian calculated here is defined as J(i,j) = \partial F_i / \partial x_j.
 *          The version in use in GCAM looks as though it might reverse the roles of i and j;
 *          therefore, any function making use of either of these functions should take care
 *          that the right convention is being used.
 */
void fdjac(VecFVec &F, const UBVECTOR &x,
           UBMATRIX &J, bool usepartial)
{
    UBVECTOR fx(F.nrtn());
    
    F(x,fx);                      // fx = F(x)
    fdjac(F,x,fx,J,usepartial);
}

