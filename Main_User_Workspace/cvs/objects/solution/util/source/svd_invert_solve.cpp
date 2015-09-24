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

#if USE_LAPACK
/* This whole file is used only if we are using lapack */
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/vector.hpp>

#include <boost/numeric/bindings/traits/ublas_vector.hpp>
#include <boost/numeric/bindings/traits/ublas_matrix.hpp>
#include <boost/numeric/ublas/operation.hpp>

#include "solution/util/include/ublas-helpers.hpp"

#include "solution/util/include/svd_invert_solve.hpp"
#include "util/base/include/util.h"

namespace ublas = boost::numeric::ublas;

// Solve U*S*VT * x = b,
// Where U,S,VT are the SVD of a matrix.  x will replace b on output
int svdInvertSolve(const ublas::matrix<double,ublas::column_major> &U,
                     const ublas::vector<double> &S,
                     const ublas::matrix<double,ublas::column_major> &VT,
                     ublas::vector<double> &b,
                     std::ostream &logf)
{
  const double small = 1.0e-8;
  int nsing = 0;
  ublas::matrix<double,ublas::column_major> Utmp(U.size2(),U.size1()), Vtmp(VT.size2(),VT.size1());
  ublas::matrix<double,ublas::column_major> Ainv(Vtmp.size1(),Utmp.size2());
  ublas::vector<double> tmpvec(Vtmp.size2()); // able to store a row of VT (== a column of V)
  

  Utmp = ublas::trans(U);
  Vtmp = ublas::trans(VT);

  int nrow = Utmp.size1();
  int ncol = Utmp.size2();

  logf << "Largest singular value = " << S[0] << "\n"; 
  
  // multiply Utmp from the left by the inverse of the diagonal matrix S
  double singthresh = S[0]*small;
  for(int i=0;i<nrow;++i) {
    double sinv = S[i];
    if(sinv < singthresh) {
      // Note that the elements of S are all >=0.  For "sufficiently
      // small" singular values we set 1/s = 1/singthresh and log the singular
      // component
      double s= sinv;           // store for output without moving the meat of this branch down below all the output 
      nsing++;
      sinv = 1.0/singthresh;    // keep the multiplier on this singular component "reasonable".
      
      // log the singular component (everything between here and the
      // marker below is strictly diagnostic and not necessary for the
      // algorithm.)
      
      // the columns of V (== trans(VT)) give the basis vectors for the nullspace
      int kmax = 0;
      double vtmax = fabs(Vtmp(0,i));
      for(int k=0; k<Vtmp.size1(); ++k) {
        tmpvec[k] = Vtmp(k,i);  // column i, kth entry
        double vtabs = fabs(tmpvec[k]);
        if(vtabs > vtmax) {
          vtmax = vtabs;
          kmax = k;
        }
      }
      // suppress printing of small values.
      double prnthresh = 1.0e-4*vtmax;
      for(int k=0; k<tmpvec.size(); ++k)
        if(fabs(tmpvec[k]) < prnthresh)
          tmpvec[k] = 0.0;
      
      logf << "Singular component:  s= " << s
           << "  kmax= " << kmax << "  vtval= " << vtmax
           << "  sinv set to: " << sinv
           << "\n";
      logf << tmpvec << "\n"; 
      // end of singular component logging 
    }
    else {
      sinv = 1.0/sinv;
    }
    for(int j=0;j<ncol;++j)
      Utmp(i,j) *= sinv;
  }

  Ainv = prod(Vtmp,Utmp);

  ublas::vector<double> bb(b);
  axpy_prod(Ainv,bb,b);

  return nsing;
}

#endif
