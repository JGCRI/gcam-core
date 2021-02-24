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

#ifndef UBLAS_HELPERS_HPP_
#define UBLAS_HELPERS_HPP_

#include <iostream>

// In principal we could easily swap out implementations, or define different
// storage semantics just by changing the types in this one header file and all
// the other solver methods will update accordingly.  For now we will just use
// the Eigen defaults but it may be useful to explore alternatives for increased
// performance.

//include <boost/numeric/ublas/vector.hpp>
//#include <boost/numeric/ublas/matrix.hpp>
#include <Eigen/Core>

//#define UBLAS boost::numeric::ublas
using UBVECTOR = Eigen::VectorXd;
using UBMATRIX = Eigen::MatrixXd;

std::ostream & operator<<(std::ostream &ostrm, const UBVECTOR &v);
std::ostream & operator<<(std::ostream &ostrm, const UBMATRIX &M);

//#undef UBLAS

#endif
