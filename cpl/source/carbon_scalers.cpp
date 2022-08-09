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
// TODO: I don't think this needs to be a derived class anymore.
CarbonScalers::CarbonScalers(int aNumLon, int aNumLat, int aNumPFT):
ASpatialData( aNumLat * aNumLon * aNumPFT ),
mBaseNPPVector( aNumLat * aNumLon * aNumPFT, 0),
mBasePFTFractVector( aNumLat * aNumLon * aNumPFT, 0),
mNumLon( aNumLon ),
mNumLat( aNumLat ),
mNumPFT( aNumPFT )
{
}

// Destructor
CarbonScalers::~CarbonScalers() {
}

// Read each component of the base year data
// This is used to calculate the scalar baseline.
// ASpatialData::readSpatialData method and then copying the vectors.
void CarbonScalers::readBaseYearData(){
    cout << "Read Base NPP" << endl;
    // Read in average NPP
    readSpatialData("../cpl/data/base_npp_mean_pft.txt", true, true, false);
    mBaseNPPVector = getValueVector();
    
    cout << "Read Base PFT weight" << endl;
    // Read in PFT weight in grid cell
    readSpatialData("../cpl/data/base_pft_wt.txt", true, true, false);
    mBasePFTFractVector = getValueVector();
}

// Read in a regional mapping data from a file
void CarbonScalers::readRegionalMappingData(std::string aFileName) {
    ifstream data(aFileName);
    if (!data.is_open())
    {
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
            
        // Parse longitude
        getline(iss, token, ',');
        lon = std::stoi(token);
        
        // Parse latitude
        getline(iss, token, ',');
        lat = std::stoi(token);
        
        string gridID = std::to_string(lon) + "_" + std::to_string(lat);
        
        // Parse Region ID
        getline(iss, token, ',');
        region = token;
        
        // Parse SubRegion ID
        getline(iss, token, ',');
        subregion = token;

        // Create reion ID
        string regID = region + "_" + subregion;
        
        // Add region ID to the mapping vector.
        // Note that there maybe more than one regID per gridID (hence, a vector)
        if ( mRegionMapping.find(gridID) == mRegionMapping.end() ) {
            // If gridID is not found, then add it with this regID in its vector
            vector<string> temp;
            temp.push_back(regID);
            mRegionMapping[gridID] = temp;
        } else {
            // If gridID is found, then add to the existing regID vector
            auto currGrid = mRegionMapping.find( gridID );
            (*currGrid).second.push_back(regID);
        }
        
        // Parse Weight -- this is the fraction of the grid cell in a particular GCAM region
        getline(iss, token, ',');
        value = std::stod(token);
        
        mRegionWeights[std::make_pair(gridID,regID)] = value;
    }
    
    return;
}

// Calculate scalers
// TODO: Add outlier test/removal
// TODO: Add HR
// TODO: Set the data in the passed vectors
void CarbonScalers::calcScalers(int *ymd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                                std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    // First, read spatial data
    readBaseYearData();
    
    // Pre-process the weights
    // TODO: Fortran & R code divide the mPFTFractVector by the number of GCAM land types
    // mapped to this PFT and replicate the output vector for each of those types
    
    // TODO: Outlier exclusion
    
    // Loop over PFTs and grid cells to calculate weighted average NPP for each GCAM region
    // TODO: currently assumes grid cells are in the same order in each vector
    // TODO: Need to get number of land types, regions, and number of grid cells from somewhere
    std::map<std::pair<std::string,std::string>, double> totalArea;
    std::map<std::pair<std::string,std::string>, double> baseTotalArea;
    std::map<std::pair<std::string,std::string>, double> totalNPP;
    std::map<std::pair<std::string,std::string>, double> baseTotalNPP;
    std::map<std::pair<std::string,std::string>, double> gcamScalar;
    
    int gridIndex = 0; // Index used for Grid vectors
    int valIndex = 0; // Index used for PFT x Grid vectors
    double scalar = 0.0; // Define the scalar
    double base_scalar = 0.0; // Define the base scalar
    for( int pft = 1; pft <= mNumPFT; pft++ ) {
        for ( int j = 1; j <= mNumLon; j++ ) {
            for ( int k = 1; k <= mNumLat; k++ ) {
                gridIndex = ( j - 1 ) * mNumLat + ( k - 1 );
                valIndex = ( pft - 1 ) * mNumLat * mNumLon + ( j - 1 ) * mNumLat + ( k - 1 );
                // Get region, subregion, and pft for this entry
                // TODO: What to do about grid cells with multiple entries?
                string gridID = std::to_string(j) + "_" + std::to_string(k);
                auto tempGrid = mRegionMapping.find( gridID );
                if ( mRegionMapping.find(gridID) == mRegionMapping.end() ) {
                    // Grid isn't found in the mapping. Currently, this probably means it is an ocean grid.
                    // TODO: set up loop only over land grids, either using the mRegionMapping or one of the files from ELM
                } else {
                    vector<string> regInGrd = (*tempGrid).second;
                    regInGrd = (*tempGrid).second;
                    // Loop over all regions this grid is mapped to and calculate the scalars
                    for(auto regID : regInGrd) {
                        // Calculate total as NPP of the PFT * area of the PFT
                        // TODO: Add in HR
                        scalar = mRegionWeights[std::make_pair(gridID,regID)] * aELMPFTFract[valIndex] * aELMLandFract[gridIndex] * aELMArea[gridIndex];
                        base_scalar = mRegionWeights[std::make_pair(gridID,regID)] * mBasePFTFractVector[valIndex] * aELMLandFract[gridIndex] * aELMArea[gridIndex];
                       
                        // Find current PFT in the PFT2Crop map
                        auto currPFT = mPFT2GCAMCropMap.find( pft );
                        vector<string> cropsInPFT = (*currPFT).second;
                        
                        // Adjust scalars based on number of crops in PFT
                        scalar = scalar / cropsInPFT.size();
                        base_scalar = base_scalar / cropsInPFT.size();
                        
                        // Then add the npp and area for both current and base periods to the region/crop totals
                        // Note: this assumes that if you find the pair in totalArea that it exists for the baseTotalArea,
                        //       totalNPP and baseTotalNPP. This should be true since they are all created simultaneously.
                        for(auto currCrop : cropsInPFT) {
                            if( totalArea.count(std::make_pair(regID,currCrop)) > 0 ) {
                                totalArea[std::make_pair(regID,currCrop)] += scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] += base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] += aELMNPP[valIndex] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] += mBaseNPPVector[valIndex] * base_scalar;
                            } else {
                                totalArea[std::make_pair(regID,currCrop)] = scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] = base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] = aELMNPP[valIndex] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] = mBaseNPPVector[valIndex] * base_scalar;
                            }
                            
                            if( regID == "3_4") {
                                cout << "PFT " << pft << " for crop " << currCrop << ": " << scalar << ", " << base_scalar;
                                cout << ", " << aELMNPP[valIndex] << ", " << mBaseNPPVector[valIndex];
                                cout << ", " << aELMNPP[valIndex]*scalar << ", " << mBaseNPPVector[valIndex]*base_scalar << endl;
                            }
                        }
                        
                        
                    }
                }
                
            }
        }
    }
    
    // Calculate scalars
    // Loop over all items in the map
    std::string crop;
    std::string regID;
    double avgNPP;
    double baseAvgNPP;
    for(const auto &curr : totalArea) {
        regID = curr.first.first;
        crop = curr.first.second;
        
        // Calculate average NPP and average NPP in the base year
        if ( totalArea[std::make_pair(regID,crop)] > 0.0 ) {
            avgNPP = totalNPP[std::make_pair(regID,crop)] / totalArea[std::make_pair(regID,crop)];
        } else {
            avgNPP = 0.0;
        }
        
        if ( baseTotalArea[std::make_pair(regID,crop)] > 0.0 ) {
            baseAvgNPP = baseTotalNPP[std::make_pair(regID,crop)] / baseTotalArea[std::make_pair(regID,crop)];
        } else {
            baseAvgNPP = 0.0;
        }
        
        if( regID == "3_4") {
            cout << "SCALAR for " << crop << ": ";
            cout << ", " << totalNPP[std::make_pair(regID,crop)] << ", " << totalArea[std::make_pair(regID,crop)];
            cout << ", " << baseTotalNPP[std::make_pair(regID,crop)] << ", " << baseTotalArea[std::make_pair(regID,crop)] << endl;
        }
        
        // Calculate scalar
        if ( baseAvgNPP > 0 ) {
            gcamScalar[std::make_pair(regID,crop)] = avgNPP / baseAvgNPP;
        } else {
            gcamScalar[std::make_pair(regID,crop)] = 1.0;
        }
        
     }
    
    // DEBUG: Write output
    // TODO: This should be moved to a separate method that will write output (if the boolean is set)
    ofstream oFile;
    oFile.open("./test.txt");
    for(const auto &curr : gcamScalar) {
        oFile << curr.first.first << ", " << curr.first.second << ": " << curr.second << endl;
    }
    oFile.close();
}

// Read in scalers from a csv file
// Note: this is used for diagnostics and testing. In fully coupled E3SM-GCAM, these scalers
// are calculated based on data passed in code through the wrapper
void CarbonScalers::readScalers(int *yyyymmdd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    
    // TODO: Get this file name from either a configuration or passed argument
    ifstream data("../cpl/data/scaler_data.csv");
    if (!data.is_open())
    {
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    int row = 0;
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        int year;
        std::string region;
        std::string tech;
        double scaler;
        
        // Parse current year
        getline(iss, token, ',');
        year = std::stoi(token);
        
        // Parse region
        getline(iss, region, ',');
        
        // Parse ag production technology name
        getline(iss, tech, ',');
        
        // Parse scaler
        getline(iss, token, ',');
        scaler = std::stod(token);
        
        aYears.at(row) = year;
        aRegions[row] = region;
        aLandTechs[row] = tech;
        aScalers[row] = scaler;
        
        row++;
    }
    
    
}


