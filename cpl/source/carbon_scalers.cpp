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
#include <boost/algorithm/string/split.hpp>
#include <boost/algorithm/string/trim.hpp>

#include "util/base/include/auto_file.h"
#include "../include/carbon_scalers.h"

using namespace std;

// Constructor
// TODO: I don't think this needs to be a derived class anymore.
CarbonScalers::CarbonScalers(int aNumLon, int aNumLat, int aNumPFT):
ASpatialData( aNumLat * aNumLon * aNumPFT ),
mBaseNPPVector( aNumLat * aNumLon * aNumPFT, 0),
mBaseHRVector( aNumLat * aNumLon * aNumPFT, 0),
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
void CarbonScalers::readBaseYearData(std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName){
    // Read in average NPP
    readSpatialDataCSV(aBaseNPPFileName, true, true, false);
    mBaseNPPVector = getValueVector();
    
    // Read in average HR
    readSpatialDataCSV(aBaseHRFileName, true, true, false);
    mBaseHRVector = getValueVector();
    
    // Read in PFT weight in grid cell
    readSpatialDataCSV(aBasePFTWtFileName, true, true, false);
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
        
        // Parse Region ID
        getline(iss, token, ',');
        region = token;
        region.erase( remove( region.begin(), region.end(), '\"' ), region.end() );
        
        // Parse SubRegion ID
        getline(iss, token, ',');
        subregion = token;
        subregion.erase( remove( subregion.begin(), subregion.end(), '\"' ), subregion.end() );

        // Create reion ID
        string regID = region + "." + subregion;
        
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
void CarbonScalers::calcScalers(int aE3SMYear, double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                                std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers, std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName) {
    // First, read spatial data
    readBaseYearData(aBaseNPPFileName, aBaseHRFileName, aBasePFTWtFileName);
    
    // Exclude outliers from the scalar calculation
    excludeOutliers(aELMNPP, aELMHR);
   
    // generate diagnostic files for the incoming elm data
    string nppName = "./npp2GCAM_" + std::to_string(aE3SMYear) + ".csv";
    ILogger& npp2GCAM = ILogger::getLogger( nppName );
    npp2GCAM.setLevel( ILogger::NOTICE );
    npp2GCAM.precision(20);
    npp2GCAM << "pft_id,lon_ind,lat_ind,npp_gC_per_m2_per_s" << endl;

    string hrName = "./hr2GCAM_" + std::to_string(aE3SMYear) + ".csv";
    ILogger& hr2GCAM = ILogger::getLogger( hrName );
    hr2GCAM.setLevel( ILogger::NOTICE );
    hr2GCAM.precision(20);
    hr2GCAM << "pft_id,lon_ind,lat_ind,hr_gC_per_m2_per_s" << endl;

    string pftName = "./pft2GCAM_" + std::to_string(aE3SMYear) + ".csv";
    ILogger& pft2GCAM = ILogger::getLogger( pftName );
    pft2GCAM.setLevel( ILogger::NOTICE );
    pft2GCAM.precision(20);
    pft2GCAM << "pft_id,lon_ind,lat_ind,pft_wt_cell_frac" << endl;

    string areaName = "./area2GCAM.csv";
    ILogger& area2GCAM = ILogger::getLogger( areaName );
    area2GCAM.setLevel( ILogger::NOTICE );
    area2GCAM.precision(20);
    // only do this once, and the first call is in 2016
    if(aE3SMYear == 2016) {
        area2GCAM << "lon_ind,lat_ind,cell_area_km2" << endl;
    }

    // diagnostics to find out where the data are being lost
    string SdName = "./scaler_diagnostic.csv";
    ILogger& Sd = ILogger::getLogger( SdName );
    Sd.setLevel( ILogger::NOTICE );
    Sd.precision(20);

    // Create mappings to store intermediate information
    std::map<std::pair<std::string,std::string>, double> totalArea;
    std::map<std::pair<std::string,std::string>, double> baseTotalArea;
    std::map<std::pair<std::string,std::string>, double> totalNPP;
    std::map<std::pair<std::string,std::string>, double> baseTotalNPP;
    std::map<std::pair<std::string,std::string>, double> aboveScalarMap;
    std::map<std::pair<std::string,std::string>, double> totalHR;
    std::map<std::pair<std::string,std::string>, double> baseTotalHR;
    std::map<std::pair<std::string,std::string>, double> belowScalarMap;
    
    // Loop over PFTs and grid cells to calculate weighted average NPP/HR for each land type in each GCAM land unit
    // the average for each land type is based on the relevant pfts and cells within a land unit
    // the assumption is that a given land type is proportionally distributed across the relevant pfts and cells
    // thus a weighted pft average is desired, which does not depend on the existence or amount of a given land type
    // Note: E3SM data will have longitude moving fastest, then latitude, then pft, so loop in that order
    int gridIndex = 0; // Index used for Grid vectors
    int valIndex = 0; // Index used for PFT x Grid vectors
    double scalar = 0.0; // Define the scalar
    double base_scalar = 0.0; // Define the base scalar
    for( int pft = 0; pft < mNumPFT; pft++ ) {
        for ( int k = 1; k <= mNumLat; k++ ) {
            for ( int j = 1; j <= mNumLon; j++ ) {
                gridIndex = ( k - 1 ) * mNumLon + ( j - 1 );
                valIndex = ( pft ) * mNumLon * mNumLat + ( k - 1 ) * mNumLon + ( j - 1 );
                // Get region, subregion, and pft for this entry
                // TODO: What to do about grid cells with multiple entries? - doesn't seem like this should happen
                string gridID = std::to_string(j) + "_" + std::to_string(k);
                auto tempGrid = mRegionMapping.find( gridID );

                // write diagnostics of the incoming elm data; entire grid is in these files (and the base files)
                npp2GCAM << pft << "," << j << "," << k << "," << aELMNPP[valIndex] << endl;
                hr2GCAM << pft << "," << j << "," << k << "," << aELMHR[valIndex] << endl;
                pft2GCAM << pft << "," << j << "," << k << "," << aELMPFTFract[valIndex] << endl;
                if(aE3SMYear == 2016 && pft == 0) {
                    area2GCAM << j << "," << k << "," << aELMArea[gridIndex] << endl;
                }

                if ( mRegionMapping.find(gridID) == mRegionMapping.end() ) {
                    // Grid isn't found in the mapping. Currently, this probably means it is an ocean grid.
                    // TODO: set up loop only over land grids, either using the mRegionMapping or one of the files from ELM
                } else {
                    vector<string> regInGrd = (*tempGrid).second;
                    regInGrd = (*tempGrid).second;
                    // Loop over all regions this grid is mapped to and calculate the scalars
                    for(auto regID : regInGrd) {
                        // Calculate total flux as NPP/HR of the PFT * area of the PFT
                        // pft value is fraction of grid cell
                        // area is in km^2, but npp and hr are in gC/m^2/sec
                        // the average below includes area in both numerator and denominator
                        //  so no need to convert to m^2
                        scalar = mRegionWeights[std::make_pair(gridID,regID)] * aELMPFTFract[valIndex] * aELMArea[gridIndex];
                        base_scalar = mRegionWeights[std::make_pair(gridID,regID)] * mBasePFTFractVector[valIndex] * aELMArea[gridIndex];
                      
                        // the annual average do not have bad npp values anymore, at least when res matches
                        //    so do not need to check for base_scalar == 0 and then set scalar to 0
 
                        // Find current PFT in the PFT2Crop map
                        auto currPFT = mPFT2GCAMCropMap.find( pft );
                        vector<string> cropsInPFT = (*currPFT).second;
                        
                        // Then add the npp and area for both current and base periods to the region/crop totals
                        // Note: this assumes that if you find the pair in totalArea that it exists for the baseTotalArea,
                        //       totalNPP/HR and baseTotalNPP/HR. This should be true since they are all created simultaneously.
                        for(auto currCrop : cropsInPFT) {
                            if( totalArea.count(std::make_pair(regID,currCrop)) > 0 ) {
                                totalArea[std::make_pair(regID,currCrop)] += scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] += base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] += aELMNPP[valIndex] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] += mBaseNPPVector[valIndex] * base_scalar;
                                totalHR[std::make_pair(regID,currCrop)] += aELMHR[valIndex] * scalar;
                                baseTotalHR[std::make_pair(regID,currCrop)] += mBaseHRVector[valIndex] * base_scalar;
                            } else {
                                totalArea[std::make_pair(regID,currCrop)] = scalar;
                                baseTotalArea[std::make_pair(regID,currCrop)] = base_scalar;
                                totalNPP[std::make_pair(regID,currCrop)] = aELMNPP[valIndex] * scalar;
                                baseTotalNPP[std::make_pair(regID,currCrop)] = mBaseNPPVector[valIndex] * base_scalar;
                                totalHR[std::make_pair(regID,currCrop)] = aELMHR[valIndex] * scalar;
                                baseTotalHR[std::make_pair(regID,currCrop)] = mBaseHRVector[valIndex] * base_scalar;
                            } // end if else new region land type pair

                            // check one instance of not matching output 
                            //if( currCrop == "FiberCrop" && regID == "Africa_Southern.AfrCstSW") {
                            //   Sd << regID << ", " << currCrop << ", pft=" << pft << ", lon=" << j << ", lat=" << k << endl;
                            //   Sd << "scalar=" << scalar << ", base_scalar=" << base_scalar << endl;
                            //   Sd << "totalArea=" << totalArea[std::make_pair(regID,currCrop)] << ", totalNPP=" << totalNPP[std::make_pair(regID,currCrop)] << ", totalHR=" << totalHR[std::make_pair(regID,currCrop)] << endl;
                            //   Sd << "aELMNPP=" << aELMNPP[valIndex] << ", aELMHR=" << aELMHR[valIndex] << ", val_Index=" << valIndex << endl << endl;
                            //}

                         } // end for loop over land types for this pft
                    } // end for loop over region in this grid cell
                } // if else this grid cell is found
            } // end for j loop over lon
        } // end for k loop over lat
    } // end for pft loop over pft
   
    // Calculate scalars
    // Loop over all items in the map
    std::string crop;
    std::string regID;
    double avgNPP;
    double baseAvgNPP;
    double avgHR;
    double baseAvgHR;
    double hrScalar;
    for(const auto &curr : totalArea) {
        regID = curr.first.first;
        crop = curr.first.second;
        
        // Calculate average NPP and average HR for each region-crop 
        if ( totalArea[std::make_pair(regID,crop)] > 0.0 ) {
            avgNPP = totalNPP[std::make_pair(regID,crop)] / totalArea[std::make_pair(regID,crop)];
            avgHR = totalHR[std::make_pair(regID,crop)] / totalArea[std::make_pair(regID,crop)];
        } else {
            avgNPP = 0.0;
            avgHR = 0.0;
        }
        
        if ( baseTotalArea[std::make_pair(regID,crop)] > 0.0 ) {
            baseAvgNPP = baseTotalNPP[std::make_pair(regID,crop)] / baseTotalArea[std::make_pair(regID,crop)];
            baseAvgHR = baseTotalHR[std::make_pair(regID,crop)] / baseTotalArea[std::make_pair(regID,crop)];
        } else {
            baseAvgNPP = 0.0;
            baseAvgHR = 0.0;
        }
        
        // Calculate scalar
        // npp can be negative
        // gcam only have positive accumulation
        // so have to ignore negative base values
        // but if a case npp value goes negative then the scalar should go to zero
        // TODO: check this
        if ( baseAvgNPP > 0 && avgNPP > 0 ) {
            aboveScalarMap[std::make_pair(regID,crop)] = avgNPP / baseAvgNPP;
        } else if ( avgNPP < 0 ) {
            aboveScalarMap[std::make_pair(regID,crop)] = 0.0;
        } else {
            aboveScalarMap[std::make_pair(regID,crop)] = 1.0;
        }
        
        // Calculate scalar
        if ( baseAvgHR > 0 ) {
            // The belowground scalar is a combination of NPP and HR...BUT HR needs to be "flipped" around 1
            // This is because higher heterotrophic respiration means lower C density, everything else being equal
            // Check for values that would send this negative, but don't change anything yet
            hrScalar = 2.0 - ( avgHR / baseAvgHR );
            if(hrScalar <= 0) {
               //hrScalar = 0.0;
               Sd << "Bad hrScalar" << hrScalar << "for region" << regID << "crop" << crop << endl;
            }
            belowScalarMap[std::make_pair(regID,crop)] = ( aboveScalarMap[std::make_pair(regID,crop)] + hrScalar ) / 2.0;
        } else {
            belowScalarMap[std::make_pair(regID,crop)] = 1.0;
        }
    
        // check the essential data
        if(aE3SMYear == 2016) {
           Sd << regID << "," << crop << endl;
           Sd << "CASE: total area " << totalArea[std::make_pair(regID,crop)];
           Sd << " total npp " << totalNPP[std::make_pair(regID,crop)] << " total hr " << totalHR[std::make_pair(regID,crop)];
           Sd << " avgNPP " << avgNPP << " avgHR " << avgHR << endl;
           Sd << "BASE: total area " << baseTotalArea[std::make_pair(regID,crop)];
           Sd << " total npp " << baseTotalNPP[std::make_pair(regID,crop)] << " total hr " << baseTotalHR[std::make_pair(regID,crop)];
           Sd << " avgNPP " << baseAvgNPP << " avgHR " << baseAvgHR << endl;
           Sd << "above scalar " << aboveScalarMap[std::make_pair(regID,crop)];
           Sd << " below scalar " << belowScalarMap[std::make_pair(regID,crop)] << endl;
        }
     } // end for loop over totalArea 

    createScalerVectors(aE3SMYear, aYears, aRegions, aLandTechs, aAboveScalers, aBelowScalers, aboveScalarMap, belowScalarMap);

    // check the vectors
    Sd << endl << "Check vectors" << endl;
    Sd << "year " << "region " << "tech_basin " << "above scalar " << "below scalar" << endl;
    for (int r = 0; r < 17722; r++) {
       Sd << aYears[r] << " "  << aRegions[r] << " " << aLandTechs[r] << " " << aAboveScalers[r] << " " << aBelowScalers[r] << endl;
    }

}


// This function transforms the mappings used for internal scalar calculation
// into the vectors needed to set data within GCAM
void CarbonScalers::createScalerVectors(int aE3SMYear, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                                        std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers,
                                        std::map<std::pair<std::string,std::string>, double> aAboveScalarMap,
                                        std::map<std::pair<std::string,std::string>, double> aBelowScalarMap) {
    
    // Loop through the map and create the vectors
    std::string regID;
    std::string crop;
    std::vector<string> strs;
    int row = 0;
    for(const auto &curr : aAboveScalarMap) {
        regID = curr.first.first;
        crop = curr.first.second;

        // Split the region name into region and basin
        boost::split(strs, regID, boost::is_any_of("."));
        
        // Set values in each vector
        // Note that we need to combine the basin with the crop name for the `aLandTechs` vector
        // and separate the region from the basin for the `aRegions` vector.
        aYears[row] = aE3SMYear;
        aRegions[row] = strs[0];
        aLandTechs[row] = crop + "_" + strs[1];

        aAboveScalers[row] = curr.second;
        aBelowScalers[row] = aBelowScalarMap[std::make_pair(regID,crop)];
    
        row++;
    }
    
}

// Write scalar data to a file. This is for non-synchronous experiments, and is a diagnostic.
void CarbonScalers::writeScalers(std::string aFileName, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers, int aLength) {
    ofstream oFile;
    oFile.open(aFileName);
    if (!oFile.is_open())
    {
        exit(EXIT_FAILURE);
    }
    for(int i = 0; i < aLength; i++) {
        oFile << aYears[i] << "," << aRegions[i] << "," << aLandTechs[i] << "," << aAboveScalers[i] << "," << aBelowScalers[i] << endl;
    }
    oFile.close();
}

// Read in scalers from a csv file
// Note: this is used for diagnostics or special experiments. In fully coupled E3SM-GCAM, these scalers
// are calculated based on data passed in code through the wrapper
void CarbonScalers::readScalers(std::string aFileName, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                                 std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers) {
    
    // TODO: Also, do we need to worry about length?

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
        int year;
        std::string region;
        std::string tech;
        double aboveScaler;
        double belowScaler;
        
        // Parse current year
        getline(iss, token, ',');
        year = std::stoi(token);
        
        // Parse region
        getline(iss, region, ',');
        
        // Parse ag production technology name
        getline(iss, tech, ',');
        
        // Parse above scaler
        getline(iss, token, ',');
        aboveScaler = std::stod(token);

        // Parse below scaler
        getline(iss, token, ',');
        belowScaler = std::stod(token);
        
        aYears.at(row) = year;
        aRegions[row] = region;
        aLandTechs[row] = tech;
        aAboveScalers[row] = aboveScaler;
        aBelowScalers[row] = belowScaler;       
 
        row++;
    }
    
    
}

void CarbonScalers::excludeOutliers( double *aELMNPP, double *aELMHR ) {
    int length = mNumLat * mNumLon * mNumPFT;
    std::vector<double> scaledNPP(aELMNPP+0, aELMNPP+length);
    std::vector<double> scaledHR(aELMHR+0, aELMHR+length);
    
    // Calculate raw scalars
    std::transform(scaledNPP.begin(), scaledNPP.end(), mBaseNPPVector.begin(), scaledNPP.begin(), std::divides<double>());
    std::transform(scaledHR.begin(), scaledHR.end(), mBaseHRVector.begin(), scaledHR.begin(), std::divides<double>());
    
    // Remove zero and nan and inf values -- this excludes cells with 0 base values from median calcs
    // zero values could skew the median calcs because the case records with zeros may not have pft areas
    std::vector<double>::iterator newIter = std::remove_if( scaledNPP.begin(), scaledNPP.end(), [](double x){return x == 0 || !isfinite(x);});
    std::vector<double>::iterator newHRIter = std::remove_if( scaledHR.begin(), scaledHR.end(), [](double x){return x == 0 || !isfinite(x);});
    scaledNPP.resize( newIter -  scaledNPP.begin() );
    scaledHR.resize( newHRIter -  scaledHR.begin() );

    // Compute the median and median absolute deviation
    // See: Davies, P.L. and Gather, U. (1993), "The identification of multiple outliers"
    // J. Amer. Statist. Assoc., 88:782-801.
    double madLimit = 5.2;

    // First, sort the scaler and find median
    std::sort(scaledNPP.begin(), scaledNPP.end());
    std::sort(scaledHR.begin(), scaledHR.end());
    double median = 0.5 * (scaledNPP[scaledNPP.size() / 2 - 1] + scaledNPP[scaledNPP.size() / 2]);
    double medianHR = 0.5 * (scaledHR[scaledHR.size() / 2 - 1] + scaledHR[scaledHR.size() / 2]);
    
    // Then, find the median absolute deviation
    transform(scaledNPP.begin(), scaledNPP.end(), scaledNPP.begin(), [median](double x){return abs(x - median);});
    transform(scaledHR.begin(), scaledHR.end(), scaledHR.begin(), [medianHR](double x){return abs(x - medianHR);});
    std::sort(scaledNPP.begin(), scaledNPP.end());
    std::sort(scaledHR.begin(), scaledHR.end());
    double mad = 0.5 * (scaledNPP[scaledNPP.size() / 2 - 1] + scaledNPP[scaledNPP.size() / 2]);
    double madHR = 0.5 * (scaledHR[scaledHR.size() / 2 - 1] + scaledHR[scaledHR.size() / 2]);

    // Now, calculate upper and lower bounds as median +/- madLimit * mad
    double upperBound = median + madLimit * mad;
    double lowerBound = median - madLimit * mad;
    double upperBoundHR = medianHR + madLimit * madHR;
    double lowerBoundHR = medianHR - madLimit * madHR;
    
    // Remove Outliers. These are set to zero so they will be excluded from scaler calculation
    for( int i = 0; i < length; i++ ) {
        if( scaledNPP[i] > upperBound || scaledNPP[i] < lowerBound ||
            scaledHR[i] > upperBoundHR || scaledHR[i] < lowerBoundHR ) {
            aELMNPP[i] = 0;
            mBaseNPPVector[i] = 0;
            aELMHR[i] = 0;
            mBaseHRVector[i] = 0;
        }
    }
    
}


