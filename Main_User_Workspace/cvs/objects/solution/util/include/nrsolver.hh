#ifndef NRSOLVER_HH_
#define NRSOLVER_HH_

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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*!
 * @file nrsolver.hh
 * @ingroup Solution
 * @brief Newton-Raphson multi-dimensional root finder
 * @remark The solver functions are defined as a template, so the entire bodies are included here
 */

#include <cassert>
#include <iostream>
#include <boost/numeric/ublas/lu.hpp>
#include <math.h>
#include "functor.hh"
#include "functor-subs.hh"
#include "linesearch.hh"
#include "fdjac.hh"


#define UBLAS boost::numeric::ublas

/*!
 * Perform the multidimensional Newton-Raphson root finding algorithm with backtracking
 * @tparam FTYPE: The floating point type to use
 * @param[in]  F: A vector function of a vector input
 * @param[inout] x: On input, an initial guess for the solution.  On output, the solution.
 * @param[inout] fx: On input, the value of F(x), on output the value of the new F(x).
 * @param[inout] J: On input, the Jacobian at the input value of x.  On output, the Jacobian at the output
 *                  value of x. 
 * @param[inout] neval: Number of evaluations of F.  The count is added to the value passed in. 
 * @param[in] ITMAX: (Optional) maximum number of iterations. Default = 250
 * @param[in] FTOL: (Optional) convergence tolerance. Default = 1.0e-7
 * @return A flag indicating success (0) or failure(>0 means singular matrix; <0 means some other error)
 * @remark This subroutine can fail for a lot of reasons, not all of them catastrophic.  Upon
 *         encountering a failure, a solver component that uses this subroutine should try
 *         restarting with a new initial guess.  One useful heuristic for making a new initial
 *         guess is to force the system into a price region where caps/floors on supplies and
 *         demands are not in effect, since these features make the derivatives used in the
 *         algorithm unreliable.
 *         
 */
template <class FTYPE>
int nrsolve(VecFVec<FTYPE,FTYPE> &F, UBLAS::vector<FTYPE> &x,
            UBLAS::vector<FTYPE> &fx, UBLAS::matrix<FTYPE> &J,
            int &neval, int ITMAX=250, FTYPE FTOL=1.0e-7)
{
  using UBLAS::vector;
  using UBLAS::matrix;
  using UBLAS::permutation_matrix;
  using UBLAS::lu_factorize;
  using UBLAS::lu_substitute;
  const FTYPE FTINY = FTOL*FTOL;
  UBLAS::matrix<FTYPE> Jsav(J); // stash a copy of J before we run L-U
  UBLAS::permutation_matrix<int> p(F.narg()); // permutation vector for pivoting in L-U
                             // decomposition (note that a
                             // "permutation matrix" is actually a
                             // vector).
  vector<FTYPE> dx(F.narg());
  vector<FTYPE> xnew(F.narg());
  vector<FTYPE> gx(F.narg());
  assert(F.nrtn() == F.narg());
  assert(x.size() == F.narg());
  assert(fx.size() == F.nrtn());

  // We create a functor that computes f(x) = F(x)*F(x).  It also
  // stores the value of F that it produces as an intermediate.
  FdotF<FTYPE,FTYPE> fnorm(F);
  FTYPE f0 = inner_prod(fx,fx); // already have a value of F on input, so no need to call fnorm yet
  if(f0 < FTINY)
    // Guard against F=0 since it can cause a NaN in our solver.  This
    // is a more stringent test than our regular convergence test
    return 0;
  
  for(int iter=0; iter<ITMAX; ++iter) {
    axpy_prod(fx,J,gx);         // compute the gradient of F*F (= fx^T * J == J^T * fx)
                                // axpy_prod clears gx on entry, so we don't have to do it.
                                // NB: the order of fx and J in that last call is significant!

    // Check for zero gradient.  This indicates a local minimum in f,
    // from which we are unlikely to escape.  We will need to try
    // again with a different initial guess.  (This should be very
    // uncommon)
    if(inner_prod(gx,gx) / (f0+FTINY) < FTOL) {
      return -3;
    }

    for(size_t i=0; i<p.size(); ++i) p[i] = i;
    Jsav = J;
    int sing = lu_factorize(J,p);
    if(sing>0) {
      J = Jsav;
      return sing;
    }
    
    // At this point, J contains the L-U decomposition of the original J
    dx = -1.0*fx;
    try {
      lu_substitute(J,p,dx);      // solve dx = J^-1 F
    }
    catch(const UBLAS::internal_logic &err) {
      // This seems to happen when the Jacobian is ill-conditioned.
      // We'll let it go (possibly with some logging), since there
      // isn't much we can do about it.  Either the solver will muddle
      // through, or it will eventually bomb out with a genuinely
      // singular matrix or with a failure in linesearch.
      //std::cout << "\tFailure in L-U Substitute.\n";
      //std::cout << "\tFailure in L-U Substitute.\n\tx: " << x << "\tdx: " << dx << "\n";
    }
    
    // Now dx contains the full newton step.  Execute the line search
    // along that direction.
    FTYPE fnew;
    int lserr = linesearch(fnorm,x,f0,gx,dx, xnew,fnew, neval);

    if(lserr != 0) {
      // line search failed.  This means that the descent direction
      // for F only extends a very short way (roughly 10^-7 * x0).
      // The most likely way for this to happen is if we're close to a
      // discontinuity in (probably several components of) the
      // Jacobian.  Using smoother supply and/or demand functions
      // might help here.
      
      //return -2;

      // take a unit step in the dx direction and hope for the best
#ifndef NDEBUG
      std::cout << "\tlinesearch failure\n";
#endif
      float dxnorm = 1.0f/inner_prod(dx,dx);
      xnew = x + dxnorm*dx;
      fnew = fnorm(xnew);
      neval++;              // fnorm performs a function evaluation
    }
    
    f0 = fnew;
    x  = xnew;
    fnorm.lastF(fx);            // get the last value of big-F

  
    // test for convergence
    FTYPE maxval = 0.0;
    for(size_t i=0; i<fx.size(); ++i) {
      FTYPE val = fabs(fx[i]);
      maxval = val>maxval ? val : maxval;
    }
    if(maxval <= FTOL) {
      J = Jsav;
      return 0;                 // SUCCESS 
    }
    
    fdjac(F,x,fx,J);            // calculate finite difference Jacobian for the next iteration
    neval += x.size();          // N evaluations from calculating the Jacobian
  }

  // if we get here, then we didn't converge in the number of
  // iterations allowed us.  Return an error code
  J = Jsav;
  return -1;
}

/*!
 * Perform the multidimensional Newton-Raphson root finding algorithm with backtracking
 * @tparam FTYPE: The floating point type to use
 * @param[in]  F: A vector function of a vector input
 * @param[inout] x: On input, an initial guess for the solution.  On output, the solution.
 * @param[out] fx: On output, the last value of F(x).  MUST be initialized to have length==x.size()
 *                 (== F.nrtn()), but need not have its values initialized. 
 * @param[inout] neval: Running count of F evaluations 
 * @param[in] ITMAX: (Optional) maximum number of iterations
 * @param[in] FTOL: (Optional) convergence tolerance. Default = 1.0e-7
 * @return A flag indicating success (0) or failure(>0 means singular matrix; <0 means some other error)
 * @remark Entry into nrsolve for cases where we haven't already calculated the Jacobian
 *         
 */
template <class FTYPE>
int nrsolve(VecFVec<FTYPE,FTYPE> &F, UBLAS::vector<FTYPE> &x,
            UBLAS::vector<FTYPE> &fx, int &neval, int ITMAX=250,
            FTYPE FTOL=1.0e-7)
{
  UBLAS::matrix<FTYPE> J(F.narg(),F.nrtn());
  F(x,fx);
  fdjac(F,x,fx,J);            // calculate finite difference Jacobian

  neval += 1 + x.size();        // initial function evaluation + jacobian calculations
  
  return nrsolve(F,x,fx,J,neval,ITMAX,FTOL);
}

#undef UBLAS

#endif
