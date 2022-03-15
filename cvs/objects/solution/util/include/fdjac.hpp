#ifndef FDJAC_HPP_
#define FDJAC_HPP_

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
 * \file fdjac.hpp
 * \ingroup Solution
 * \brief Finite-difference Jacobian helper functions for multidimensional root finders
 * \remark Because this function is actually a template, the entire definition goes in the header file.
 * \remark We already have a procedure for finding the Jacobian, but it is a little confusing
 *         to use.  This version will use the functors we have defined to abstract much of that
 *         complexity.  A consequence of this is that if there is any lazy evaluation or other
 *         optimization to be done in the function evaluation, it will be up to the functor to
 *         arrange it.
 */

#include <iostream>
#include "solution/util/include/functor.hpp"
#include "solution/util/include/ublas-helpers.hpp"
#include "util/base/include/definitions.h"

void jacol(VecFVec &F, const UBVECTOR &x,
                  const UBVECTOR &fx, int j,
                  UBMATRIX &J,
           bool usepartial=true, std::ostream *diagnostic=NULL);

void fdjac(VecFVec &F, const UBVECTOR &x,
           const UBVECTOR &fx, UBMATRIX &J,
           const std::list<int>& cols,
           bool usepartial=true,
           std::ostream *diagnostic=NULL);

void fdjac(VecFVec &F, const UBVECTOR &x,
           UBMATRIX &J,
           const std::list<int>& cols,
           bool usepartial=true);

#endif
