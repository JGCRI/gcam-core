#ifndef FDJAC_HH_
#define FDJAC_HH_

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
 * \file fdjac.hh
 * \ingroup Solution
 * \brief Finite-difference Jacobian helper functions for multidimensional root finders
 * \remark Because this function is actually a template, the entire definition goes in the header file.
 * \remark We already have a procedure for finding the Jacobian, but it is a little confusing
 *         to use.  This version will use the functors we have defined to abstract much of that
 *         complexity.  A consequence of this is that if there is any lazy evaluation or other
 *         optimization to be done in the function evaluation, it will be up to the functor to
 *         arrange it.
 */

#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include "functor.hh"
#include <iostream>
#include "solution/util/include/ublas-helpers.hh"

#define UBLAS boost::numeric::ublas

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
template <class FTYPE>
void fdjac(VecFVec<FTYPE,FTYPE> &F, const UBLAS::vector<FTYPE> &x,
           UBLAS::matrix<FTYPE> &J, bool usepartial=true)
{
  UBLAS::vector<FTYPE> fx(F.nrtn());

  F(x,fx);                      // fx = F(x)
  fdjac(F,x,fx,J,usepartial);
}

/*!
 * Compute the Jacobian of a vector function F at point x.
 * \tparam FTYPE: The floating point type of the input and output vectors
 * \param[in] F: The function to have its Jacobian calculated
 * \param[in] x: The point at which to calculate the Jacobian
 * \param[in] fx: F(x)
 * \param[out] J: The Jacobian of F
 */
template<class FTYPE>
void fdjac(VecFVec<FTYPE,FTYPE> &F, const UBLAS::vector<FTYPE> &x,
           const UBLAS::vector<FTYPE> &fx, UBLAS::matrix<FTYPE> &J, bool usepartial=true,
           std::ostream *diagnostic=NULL)
{
  const FTYPE heps = 1.0e-6;
  const FTYPE TINY = 1.0e-6;
  UBLAS::vector<FTYPE> xx = x; // temporary, so we can respect the const on x
  UBLAS::vector<FTYPE> fxx(fx.size());        // hold the values of F(xx)

  if(diagnostic) {
    (*diagnostic) << "fdjac: usepartial = " << usepartial << "\nInitial x:\n" << x
        << "\nInitial fx:\n" << fx << "\n";
  }
    
  
  for(size_t j=0; j<xx.size(); ++j) {
    FTYPE t = xx[j];            // store the old value
    FTYPE h = heps * (fabs(t)+TINY);
    xx[j] = t+h;
    h     = xx[j]-t; // reduce roundoff error, since (t+h)-t is not
                     // necessarily identical to the original h
    if(diagnostic) {
      (*diagnostic) << "j= " << j << "\th= " << h << "\nxx:\n" << xx << "\n";
    }
    if(usepartial) {F.partial(j);}    // hint to the function that this is a partial derivative calculation
    F(xx,fxx);       // eval the function
    xx[j] = t;       // restore the old value

    if(diagnostic) {
      (*diagnostic) << "fxx:\n" << fxx << "\n";
    }
    
    // compute the finite difference derivatives
    FTYPE hinv = 1.0/h;
    for(size_t i=0; i<fxx.size(); ++i) {
      J(i,j) = (fxx[i] - fx[i]) * hinv;
    }
  }
}

#undef UBLAS

#endif
