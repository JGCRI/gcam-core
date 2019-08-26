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
 * \file emiss_downscale.cpp
 * \brief This file processes gridded data from E3SM into region/land type specific scalers
 *
 * \author Kate Calvin
 */

#include <iostream>
#include <fstream>

#include "util/base/include/auto_file.h"
#include "../include/emiss_downscale.h"

using namespace std;

// Constructor
EmissDownscale::EmissDownscale(int aSize):
ASpatialData(aSize),
mBaseYearEmissVector(aSize, 0),
mCurrYearEmissVector(aSize, 0) {
}

// Destructor
EmissDownscale::~EmissDownscale() {
}

// Downscale emissions
void EmissDownscale::downscaleCO2Emissions(double aBaseYearEmissions, double aCurrYearEmissions) {
    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();
    
    // Calculate current year emissions vector by scaling base year emissions up
    mCurrYearEmissVector = mBaseYearEmissVector;
    double scaler = aCurrYearEmissions / aBaseYearEmissions;
    std::transform(mCurrYearEmissVector.begin(), mCurrYearEmissVector.end(),
                   mCurrYearEmissVector.begin(), [scaler](double i) { return i * scaler; });
    
    // Finally, re-set the value vector to be the final emissions, since this will be written out
    setValueVector( mCurrYearEmissVector );
    
    return;
}
