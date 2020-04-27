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

// Separate the emissions vectors into individual months
void EmissDownscale::separateMonthlyEmissions(double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                              double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                              double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                              double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                              int aNumLon, int aNumLat) {
    
    int gridPerMonth = aNumLat * aNumLon;
    std::copy(mCurrYearEmissVector.begin(),
              mCurrYearEmissVector.begin() + gridPerMonth - 1, gcamoco2sfcjan);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth,
              mCurrYearEmissVector.begin() + gridPerMonth * 2 - 1, gcamoco2sfcfeb);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 2,
              mCurrYearEmissVector.begin() + gridPerMonth * 3 - 1, gcamoco2sfcmar);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 3,
              mCurrYearEmissVector.begin() + gridPerMonth * 4 - 1, gcamoco2sfcapr);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 4,
              mCurrYearEmissVector.begin() + gridPerMonth * 5 - 1, gcamoco2sfcmay);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 5,
              mCurrYearEmissVector.begin() + gridPerMonth * 6 - 1, gcamoco2sfcjun);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 6,
              mCurrYearEmissVector.begin() + gridPerMonth * 7 - 1, gcamoco2sfcjul);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 7,
              mCurrYearEmissVector.begin() + gridPerMonth * 8 - 1, gcamoco2sfcaug);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 8,
              mCurrYearEmissVector.begin() + gridPerMonth * 9 - 1, gcamoco2sfcsep);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 9,
              mCurrYearEmissVector.begin() + gridPerMonth * 10 - 1, gcamoco2sfcoct);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 10,
              mCurrYearEmissVector.begin() + gridPerMonth * 11 - 1, gcamoco2sfcnov);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 11,
              mCurrYearEmissVector.begin() + gridPerMonth * 12 - 1, gcamoco2sfcdec);
 }

// Separate the emissions vectors into individual months
void EmissDownscale::separateMonthlyEmissionsWithVertical(double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar,
                                                          double *gcamoco2airloapr, double *gcamoco2airlomay, double *gcamoco2airlojun,
                                                          double *gcamoco2airlojul, double *gcamoco2airloaug, double *gcamoco2airlosep,
                                                          double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                                                          double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar,
                                                          double *gcamoco2airhiapr, double *gcamoco2airhimay, double *gcamoco2airhijun,
                                                          double *gcamoco2airhijul, double *gcamoco2airhiaug, double *gcamoco2airhisep,
                                                          double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                                                          int aNumLon, int aNumLat) {
    
    int gridPerMonth = aNumLat * aNumLon;
    std::copy(mCurrYearEmissVector.begin(),
              mCurrYearEmissVector.begin() + gridPerMonth - 1, gcamoco2airlojan);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth,
              mCurrYearEmissVector.begin() + gridPerMonth * 2 - 1, gcamoco2airlofeb);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 2,
              mCurrYearEmissVector.begin() + gridPerMonth * 3 - 1, gcamoco2airlomar);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 3,
              mCurrYearEmissVector.begin() + gridPerMonth * 4 - 1, gcamoco2airloapr);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 4,
              mCurrYearEmissVector.begin() + gridPerMonth * 5 - 1, gcamoco2airlomay);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 5,
              mCurrYearEmissVector.begin() + gridPerMonth * 6 - 1, gcamoco2airlojun);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 6,
              mCurrYearEmissVector.begin() + gridPerMonth * 7 - 1, gcamoco2airlojul);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 7,
              mCurrYearEmissVector.begin() + gridPerMonth * 8 - 1, gcamoco2airloaug);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 8,
              mCurrYearEmissVector.begin() + gridPerMonth * 9 - 1, gcamoco2airlosep);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 9,
              mCurrYearEmissVector.begin() + gridPerMonth * 10 - 1, gcamoco2airlooct);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 10,
              mCurrYearEmissVector.begin() + gridPerMonth * 11 - 1, gcamoco2airlonov);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 11,
              mCurrYearEmissVector.begin() + gridPerMonth * 12 - 1, gcamoco2airlodec);
    
    int gridPerYear = gridPerMonth * 12; // Offset for the second height emissions
    std::copy(gridPerYear + mCurrYearEmissVector.begin(),
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth - 1, gcamoco2airhijan);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 2 - 1, gcamoco2airhifeb);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 2,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 3 - 1, gcamoco2airhimar);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 3,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 4 - 1, gcamoco2airhiapr);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 4,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 5 - 1, gcamoco2airhimay);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 5,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 6 - 1, gcamoco2airhijun);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 6,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 7 - 1, gcamoco2airhijul);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 7,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 8 - 1, gcamoco2airhiaug);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 8,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 9 - 1, gcamoco2airhisep);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 9,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 10 - 1, gcamoco2airhioct);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 10,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 11 - 1, gcamoco2airhinov);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 11,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 12 - 1, gcamoco2airhidec);
    
 }
