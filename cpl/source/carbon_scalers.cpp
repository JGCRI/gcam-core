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
 * \file carbon_scalers.cpp
 * \brief This file processes gridded data from E3SM into region/land type specific scalers
 *
 * \author Kate Calvin
 */

#include <iostream>
#include <fstream>

#include "util/base/include/auto_file.h"
#include "../include/carbon_scalers.h"

using namespace std;

// Constructor
CarbonScalers::CarbonScalers(int aSize):
ASpatialData( aSize ),
mNPPVector( aSize ),
mNPPLatVector( aSize ),
mNPPLonVector( aSize ),
mNPPIDVector( aSize ),
mPFTFractVector( aSize ),
mPFTFractLatVector( aSize ),
mPFTFractLonVector( aSize ),
mPFTFractIDVector( aSize ),
mAreaVector( aSize ),
mAreaLatVector( aSize ),
mAreaLonVector( aSize ),
mLandFractVector( aSize ),
mLandFractLatVector( aSize ),
mLandFractLonVector( aSize )
{
}

// Destructor
CarbonScalers::~CarbonScalers() {
}

// Read each component of the carbon scaler data, using the
// ASpatialData::readSpatialData method and then copying the vectors.
void CarbonScalers::readAllSpatialData(){
    
    // Read in average NPP
    readSpatialData("../cpl/data/npp_mean_pft.txt", true, false);
    mNPPVector = getValueVector();
    mNPPLatVector = getLatVector();
    mNPPLonVector = getLonVector();
    mNPPIDVector = getIDVector();
    
    // Read in PFT weight in grid cell
    readSpatialData("../cpl/data/pft_wt.txt", true, false);
    mPFTFractVector = getValueVector();
    mPFTFractLatVector = getLatVector();
    mPFTFractLonVector = getLonVector();
    mPFTFractIDVector = getIDVector();
    
    // Print some debugging information
    for ( int i = 1000; i < 1011; i++ ) {
        cout << "NPP is " << mNPPVector[ i ] << " and PFT Fract is " << mPFTFractVector[ i ] << endl;
    }

}

void CarbonScalers::calcScalers() {
}

