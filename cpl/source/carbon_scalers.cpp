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
mNPPVector( aSize, 0 ),
mNPPLatVector( aSize, 0 ),
mNPPLonVector( aSize, 0 ),
mNPPIDVector( aSize, 0 ),
mPFTFractVector( aSize, 0 ),
mPFTFractLatVector( aSize, 0 ),
mPFTFractLonVector( aSize, 0 ),
mPFTFractIDVector( aSize, 0 ),
mAreaVector( aSize, 0 ),
mAreaLatVector( aSize, 0 ),
mAreaLonVector( aSize, 0 ),
mLandFractVector( aSize, 0 ),
mLandFractLatVector( aSize, 0 ),
mLandFractLonVector( aSize, 0 ),
mOutputVector( aSize, 0 )
{
}

// Destructor
CarbonScalers::~CarbonScalers() {
}

// Read each component of the carbon scaler data, using the
// ASpatialData::readSpatialData method and then copying the vectors.
void CarbonScalers::readAllSpatialData(){
    
    cout << "Read NPP" << endl;
    // Read in average NPP
    readSpatialData("../cpl/data/npp_mean_pft.txt", true, true, false);
    mNPPVector = getValueVector();
    mNPPLatVector = getLatVector();
    mNPPLonVector = getLonVector();
    mNPPIDVector = getIDVector();
    
    cout << "Read PFT weight" << endl;
    // Read in PFT weight in grid cell
    readSpatialData("../cpl/data/pft_wt.txt", true, true, false);
    mPFTFractVector = getValueVector();
    mPFTFractLatVector = getLatVector();
    mPFTFractLonVector = getLonVector();
    mPFTFractIDVector = getIDVector();
    
    cout << "Read area" << endl;
    // Read in area of grid cell
    readSpatialData("../cpl/data/area.txt", true, false, false);
    mAreaVector = getValueVector();
    mAreaLatVector = getLatVector();
    mAreaLonVector = getLonVector();
    
    cout << "Read land fract" << endl;
    // Read in area of grid cell
    readSpatialData("../cpl/data/landfrac.txt", true, false, false);
    mLandFractVector = getValueVector();
    mLandFractLatVector = getLatVector();
    mLandFractLonVector = getLonVector();
    
    // Print some debugging information
    for ( int i = 1000; i < 1011; i++ ) {
        cout << "NPP is " << mNPPVector[ i ] << " and PFT Fract is " << mPFTFractVector[ i ] << endl;
    }

}

// Calculate scalers
// TODO: This still needs mappings to GCAM regions and crops
// TODO: Add outlier test/removal
void CarbonScalers::calcScalers() {
    // First, read spatial data
    readAllSpatialData();
    
    // Loop over PFTs and grid cells to calculate weighted average NPP
    // TODO: currently assumes grid cells are in the same order in each vector
    // TODO: Need to get number of PFTs and number of grid cells from somewhere
    vector<int> total(17);
    int index = 0; // Could calculate this from i & j
    for( int i = 0; i < 17; i++ ) {
        for ( int j = 0; j < 64800; j++ ) {
            // Calculate total as NPP of the PFT * area of of the PFT
            mOutputVector[index] = mNPPVector[index] * mPFTFractVector[index] * mLandFractVector[j] * mAreaVector[j];
            
            // Add to total
            total[i] += mOutputVector[index];
            
            // Increment the index
            index++;
        }
    }
    
    // Divide by the total
    index = 0;
    for( int i = 0; i < 17; i++ ) {
        for ( int j = 0; j < 64800; j++ ) {
            mOutputVector[index] /= total[i];
            index++;
        }
    }
    
    // Set the value vector
    setValueVector(mOutputVector);
}

