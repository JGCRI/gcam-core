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
EmissDownscale::EmissDownscale(int aNumLon, int aNumLat, int aNumMon, int aNumLev, int aNumReg, int aNumSector) : ASpatialData(aNumLat * aNumLon * aNumMon * aNumLev),
                                            mBaseYearEmissVector(aNumLat * aNumLon * aNumMon * aNumLev, 0),
                                            mCurrYearEmissVector(aNumLat * aNumLon * aNumMon * aNumLev, 0),
                                            mBaseYearEmissions_sfc(aNumReg, 0),
                                            mBaseYearEmissions_air(aNumReg, 0),
                                            mNumLon( aNumLon ),
                                            mNumLat( aNumLat ),
                                            mNumMon( aNumMon ),
                                            mNumLev( aNumLev ),
                                            mNumReg( aNumReg ),
                                            mNumSector(  aNumSector )
                                            {
}

// Destructor
EmissDownscale::~EmissDownscale() {
}

// Read in a regional mapping data from a file
void EmissDownscale::readRegionalMappingData(std::string aFileName)
{
    ifstream data(aFileName);
    if (!data.is_open())
    {
        ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
        coupleLog.setLevel( ILogger::ERROR );
        coupleLog << "File not found: " << aFileName << endl;
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        int lon;
        int lat;
        string region;
        string subregion;

        // Skip region & GLU ID
        getline(iss, token, ',');
        getline(iss, token, ',');

        // Parse longitude
        getline(iss, token, ',');
        lon = std::stoi(token);

        // Parse latitude
        getline(iss, token, ',');
        lat = std::stoi(token);

        string gridID = std::to_string(lon) + "_" + std::to_string(lat);

        // Parse Region Name
        getline(iss, token, ',');
        region = token;
        region.erase(remove(region.begin(), region.end(), '\"'), region.end());

        // Skip SubRegion Name
        getline(iss, token, ',');

        // Create region ID
        string regID = region;

        // Add region ID to the mapping vector.
        // Note that there maybe more than one regID per gridID (hence, a vector)
        if (mRegionMapping.find(gridID) == mRegionMapping.end())
        {
            // If gridID is not found, then add it with this regID in its vector
            vector<string> temp;
            temp.push_back(regID);
            mRegionMapping[gridID] = temp;
        }
        else
        {
            // If gridID is found, then add to the existing regID vector
            auto currGrid = mRegionMapping.find(gridID);
            (*currGrid).second.push_back(regID);
        }

        // Parse Weight -- this is the fraction of the grid cell in a particular GCAM region
        getline(iss, token, ',');
        value = std::stod(token);

        mRegionWeights[std::make_pair(gridID, regID)] = value;
    }

    return;
}

// Read in regional Base-Year Emission Data from a file
void EmissDownscale::readRegionalBaseYearEmissionData(std::string aFileName)
{
    mBaseYearGlobalSfcCO2Emiss = 0.0;
    mBaseYearGlobalAirCO2Emiss = 0.0;

    ifstream data(aFileName);
    if (!data.is_open())
    {
        ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
        coupleLog.setLevel( ILogger::ERROR );
        coupleLog << "File not found: " << aFileName << endl;
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        string regID;
        string sectorID;

        // Parse Region Name
        getline(iss, token, ',');
        regID = token;
        regID.erase(remove(regID.begin(), regID.end(), '\"'), regID.end());
        regID.erase(remove(regID.begin(), regID.end(), ' '), regID.end()); // remove spaces in the region names

        // Parse Sector
        getline(iss, token, ',');
        sectorID = token;
        sectorID.erase(remove(sectorID.begin(), sectorID.end(), '\"'), sectorID.end());

        // Skip Year
        getline(iss, token, ',');

        // Parse Base-Year Emission
        getline(iss, token, ',');
        value = std::stod(token);

       // Get the index of the region
        auto currReg = mRegionIDName.find(regID);
        int regIndex = (*currReg).second - 1;
        
        if (sectorID == "surface")
        { mBaseYearEmissions_sfc[regIndex] = value;
        mBaseYearGlobalSfcCO2Emiss += value;}
        else if (sectorID == "aircraft")
        { mBaseYearEmissions_air[regIndex] = value;
          mBaseYearGlobalAirCO2Emiss += value;}
          else {
            ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
            coupleLog.setLevel( ILogger::ERROR );
            coupleLog << "Sector" << sectorID << " in" << aFileName << "not present in current co2.xml output mapping" << endl;
            exit(EXIT_FAILURE);
        }
        
        
    }

    return;
}


// Downscale emissions
void EmissDownscale::downscaleSurfaceCO2Emissions(double *aCurrYearEmissions)
{ // baseYearEmission need to be updated

    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();

    // Calculate current year emissions vector by scaling base year emissions up
    mCurrYearEmissVector = mBaseYearEmissVector;
    // double scaler = mCurrYearEmissions / mBaseYearEmissions;
    // std::transform(mCurrYearEmissVector.begin(), mCurrYearEmissVector.end(),
    //                 mCurrYearEmissVector.begin(), [scaler](double i) { return i * scaler; });

    int gridIndex = 0;   // Index used for Grid vectors
    int valIndex = 0;    // Index used for PFT x Grid vectors
    double scalar = 0.0; // Define the scalar
    double weight = 0.0; // Define the weight

    for (int k = 1; k <= mNumLat; k++)
    {
        for (int j = 1; j <= mNumLon; j++)
        {

            gridIndex = (k - 1) * mNumLon + (j - 1);

            // Get region for this entry
            string gridID = std::to_string(j) + "_" + std::to_string(k);
            auto tempGrid = mRegionMapping.find(gridID);
            if (mRegionMapping.find(gridID) == mRegionMapping.end())
            {
                // Grid isn't found in the mapping. Currently, this probably means it is an ocean grid.
                // TODO: set up loop only over land grids, either using the mRegionMapping or one of the files from ELM
            }
            else
            {
                vector<string> regInGrd = (*tempGrid).second;
                scalar = 0;
                weight = 0;
                // Loop over all regions this grid is mapped to and calculate the scalars
                for (auto regID : regInGrd)
                {
                    // Calculate total as NPP/HR of the PFT * area of the PFT
                    // pft value is fraction of grid cell
                    auto currReg = mRegionIDName.find(regID);
                    int regIndex = (*currReg).second - 1;

                    scalar += aCurrYearEmissions[regIndex] / mBaseYearEmissions_sfc[regIndex] * mRegionWeights[std::make_pair(gridID, regID)];
                    weight += mRegionWeights[std::make_pair(gridID, regID)];
                }
                scalar = scalar / weight; // normalized by the total eright
                for (int mon = 1; mon <= mNumMon; mon++)
                {
                    valIndex = (mon - 1) * mNumLon * mNumLat + (k - 1) * mNumLon + (j - 1);
                    mCurrYearEmissVector[valIndex] = mBaseYearEmissVector[valIndex] * scalar;
                }
            }
        }
    }

    // Finally, re-set the value vector to be the final emissions, since this will be written out
    setValueVector(mCurrYearEmissVector);

    return;
}

// Downscale emissions
void EmissDownscale::downscaleAircraftCO2Emissions(double *aCurrYearEmissions)
{ // baseYearEmission need to be updated

    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();

    // Calculate current year emissions vector by scaling base year emissions up
    mCurrYearEmissVector = mBaseYearEmissVector;

    double scalar = 0.0; // Define the scalar
    
    double aCurrYearGlobalAirCO2Emiss = 0.0;

    for(int k = 1; k <= mNumReg; k++)
        aCurrYearGlobalAirCO2Emiss += aCurrYearEmissions[k-1];

    scalar =  aCurrYearGlobalAirCO2Emiss / mBaseYearGlobalAirCO2Emiss;

    std::transform(mCurrYearEmissVector.begin(), mCurrYearEmissVector.end(),
                   mCurrYearEmissVector.begin(), [scalar](double i) { return i * scalar; });

    // Finally, re-set the value vector to be the final emissions, since this will be written out
    setValueVector(mCurrYearEmissVector);

    return;
}


// Separate the emissions vectors into individual months
void EmissDownscale::separateMonthlyEmissions(double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                                              double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                                              double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                                              double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                              int aNumLon, int aNumLat)
{

    int gridPerMonth = aNumLat * aNumLon;
    std::copy(mCurrYearEmissVector.begin(),
              mCurrYearEmissVector.begin() + gridPerMonth, gcamoco2sfcjan);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth,
              mCurrYearEmissVector.begin() + gridPerMonth * 2, gcamoco2sfcfeb);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 2,
              mCurrYearEmissVector.begin() + gridPerMonth * 3, gcamoco2sfcmar);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 3,
              mCurrYearEmissVector.begin() + gridPerMonth * 4, gcamoco2sfcapr);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 4,
              mCurrYearEmissVector.begin() + gridPerMonth * 5, gcamoco2sfcmay);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 5,
              mCurrYearEmissVector.begin() + gridPerMonth * 6, gcamoco2sfcjun);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 6,
              mCurrYearEmissVector.begin() + gridPerMonth * 7, gcamoco2sfcjul);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 7,
              mCurrYearEmissVector.begin() + gridPerMonth * 8, gcamoco2sfcaug);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 8,
              mCurrYearEmissVector.begin() + gridPerMonth * 9, gcamoco2sfcsep);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 9,
              mCurrYearEmissVector.begin() + gridPerMonth * 10, gcamoco2sfcoct);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 10,
              mCurrYearEmissVector.begin() + gridPerMonth * 11, gcamoco2sfcnov);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 11,
              mCurrYearEmissVector.begin() + gridPerMonth * 12, gcamoco2sfcdec);
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
                                                          int aNumLon, int aNumLat)
{

    int gridPerMonth = aNumLat * aNumLon;
    std::copy(mCurrYearEmissVector.begin(),
              mCurrYearEmissVector.begin() + gridPerMonth, gcamoco2airlojan);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth,
              mCurrYearEmissVector.begin() + gridPerMonth * 2, gcamoco2airlofeb);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 2,
              mCurrYearEmissVector.begin() + gridPerMonth * 3, gcamoco2airlomar);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 3,
              mCurrYearEmissVector.begin() + gridPerMonth * 4, gcamoco2airloapr);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 4,
              mCurrYearEmissVector.begin() + gridPerMonth * 5, gcamoco2airlomay);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 5,
              mCurrYearEmissVector.begin() + gridPerMonth * 6, gcamoco2airlojun);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 6,
              mCurrYearEmissVector.begin() + gridPerMonth * 7, gcamoco2airlojul);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 7,
              mCurrYearEmissVector.begin() + gridPerMonth * 8, gcamoco2airloaug);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 8,
              mCurrYearEmissVector.begin() + gridPerMonth * 9, gcamoco2airlosep);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 9,
              mCurrYearEmissVector.begin() + gridPerMonth * 10, gcamoco2airlooct);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 10,
              mCurrYearEmissVector.begin() + gridPerMonth * 11, gcamoco2airlonov);
    std::copy(mCurrYearEmissVector.begin() + gridPerMonth * 11,
              mCurrYearEmissVector.begin() + gridPerMonth * 12, gcamoco2airlodec);

    int gridPerYear = gridPerMonth * 12; // Offset for the second height emissions
    std::copy(gridPerYear + mCurrYearEmissVector.begin(),
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth, gcamoco2airhijan);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 2, gcamoco2airhifeb);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 2,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 3, gcamoco2airhimar);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 3,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 4, gcamoco2airhiapr);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 4,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 5, gcamoco2airhimay);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 5,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 6, gcamoco2airhijun);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 6,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 7, gcamoco2airhijul);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 7,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 8, gcamoco2airhiaug);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 8,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 9, gcamoco2airhisep);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 9,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 10, gcamoco2airhioct);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 10,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 11, gcamoco2airhinov);
    std::copy(gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 11,
              gridPerYear + mCurrYearEmissVector.begin() + gridPerMonth * 12, gcamoco2airhidec);
}
