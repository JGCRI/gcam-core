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

#ifndef UBLAS_HELPERS_HH_
#define UBLAS_HELPERS_HH_

#include <iostream>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/matrix.hpp>

#define UBLAS boost::numeric::ublas

template <class FTYPE>
std::ostream & operator<<(std::ostream &ostrm, const UBLAS::vector<FTYPE> &v) {
  ostrm << "(";
  for(size_t i=0; i<v.size(); ++i) {
    if(i>0) ostrm << ", ";
    ostrm << v[i];
  }
  ostrm << ")";
  return ostrm;
}

template <class FTYPE>
std::ostream & operator<<(std::ostream &ostrm, const UBLAS::matrix<FTYPE> &M) {
  int m = M.size1();
  int n = M.size2();
  
  for(int i=0;i<m;++i) {
    for(int j=0;j<n;++j)
      ostrm << M(i,j) << "  ";
    ostrm << "\n";
  }
  
  return ostrm;
}


#undef UBLAS

#endif
