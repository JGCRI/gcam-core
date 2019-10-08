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
CarbonScalers::CarbonScalers(int aNumLat, int aNumLon, int aNumPFT):
ASpatialData( aNumLat * aNumLon * aNumPFT ),
mNPPVector( aNumLat * aNumLon * aNumPFT, 0 ),
mBaseNPPVector( aNumLat * aNumLon * aNumPFT, 0 ),
mPFTFractVector( aNumLat * aNumLon * aNumPFT, 0 ),
mBasePFTFractVector( aNumLat * aNumLon * aNumPFT, 0 ),
mAreaVector( aNumLat * aNumLon, 0 ),
mLandFractVector( aNumLat * aNumLon, 0 )
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
    
    cout << "Read Base NPP" << endl;
    // Read in average NPP
    readSpatialData("../cpl/data/base_npp_mean_pft.txt", true, true, false);
    mBaseNPPVector = getValueVector();
    
    cout << "Read PFT weight" << endl;
    // Read in PFT weight in grid cell
    readSpatialData("../cpl/data/pft_wt.txt", true, true, false);
    mPFTFractVector = getValueVector();
    
    cout << "Read Base PFT weight" << endl;
    // Read in PFT weight in grid cell
    readSpatialData("../cpl/data/base_pft_wt.txt", true, true, false);
    mBasePFTFractVector = getValueVector();
    
    cout << "Read area" << endl;
    // Read in area of grid cell
    readSpatialData("../cpl/data/area.txt", true, false, false);
    mAreaVector = getValueVector();
    
    cout << "Read land fract" << endl;
    // Read in area of grid cell
    readSpatialData("../cpl/data/landfrac.txt", true, false, false);
    mLandFractVector = getValueVector();
    
    cout << "Read region map" << endl;
    // Read in region ID for each grid cell
    readRegionalMappingData("../cpl/data/regionmap.csv");
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
    int row = 0;
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
        
        row++;
    }
    
    return;
}

// Calculate scalers
// TODO: Add outlier test/removal
// TODO: Add HR
void CarbonScalers::calcScalers(int *ymd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    // First, read spatial data
    readAllSpatialData();
    
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
    
    int index = 0; // Could calculate this from i & j
    int reg = 0; // Define a region index
    int subregion = 0; // Define a sub-region index
    std::string regID;
    int pft = 0;
    double scalar = 0.0; // Define the scalar
    double base_scalar = 0.0; // Define the base scalar
    for( int pft = 0; pft < 17; pft++ ) {
        for ( int j = 1; j <= 360; j++ ) {
            for ( int k = 1; k <= 180; k++ ) {
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
                    cout << "Looking at grid " << gridID << endl;
                    // Loop over all regions this grid is mapped to and calculate the scalars
                    for(auto regID : regInGrd) {
                        cout << regID << endl;
                        
                        // Calculate total as NPP of the PFT * area of the PFT
                        // TODO: Add in HR
                        scalar = mRegionWeights[std::make_pair(gridID,regID)] * mPFTFractVector[index] * mLandFractVector[j] * mAreaVector[j];
                        base_scalar = mRegionWeights[std::make_pair(gridID,regID)] * mBasePFTFractVector[index] * mLandFractVector[j] * mAreaVector[j];
                        
                        // Find current PFT in the PFT2Crop map
                        // Then add the npp and area for both current and base periods to the region/crop totals
                        // Note: this assumes that if you find the pair in totalArea that it exists for the baseTotalArea,
                        //       totalNPP and baseTotalNPP. This should be true since they are all created simultaneously.
                        auto currPFT = mPFT2GCAMCropMap.find( pft );
                        vector<string> cropsInPFT = (*currPFT).second;
                        for(auto currCrop : cropsInPFT) {
                            if( totalArea.count(std::make_pair(regID,currCrop)) > 0 ) {
                                totalArea[std::make_pair(regID,currCrop)] += scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] += base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] += mNPPVector[index] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] += mBaseNPPVector[index] * base_scalar;
                            } else {
                                totalArea[std::make_pair(regID,currCrop)] = scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] = base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] = mNPPVector[index] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] = mBaseNPPVector[index] * base_scalar;
                            }
                        }
                        
                        
                    }
                }
                
                // Increment the index
                index++;
            }
        }
    }
    
    // Calculate scalars
    // Loop over all items in the map
    std::string crop;
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
        
        // Calculate scalar
        if ( baseAvgNPP > 0 ) {
            gcamScalar[std::make_pair(regID,crop)] = avgNPP / baseAvgNPP;
        } else {
            gcamScalar[std::make_pair(regID,crop)] = 1.0;
        }
        
     }
    
    // DEBUG: Write output
    ofstream oFile;
    oFile.open("./test.txt");
    for(const auto &curr : gcamScalar) {
        oFile << curr.first.first << ", " << curr.first.second << ": " << curr.second << endl;
    }
    oFile.close();
}

// Read in scalers from a csv file
// Note: this is used for diagnostics and testing. In fully coupled E3SM-GCAM, these scalers
// are passed in code to the wrapper
void CarbonScalers::readScalers(int *yyyymmdd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers) {
    
    // TEMPORARY: Read scaler data from a file
    // NOTE: This will be passed from E3SM eventually
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


