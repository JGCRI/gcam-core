#ifndef __CARBON_SCALERS__
#define __CARBON_SCALERS__

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

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <list>
#include <vector>

#include "../include/aspatial_data.h"

class CarbonScalers : public ASpatialData {
public:
    CarbonScalers(int aNumLat, int aNumLon, int aNumPFT);
    ~CarbonScalers();
    void readScalers(int *ymd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aScalers);
    void calcScalers(int *ymd, double *aELMArea, double *aELMLandFract, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                     std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers);
    void createScalerVectors(int *ymd, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                                            std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers,
                                            std::map<std::pair<std::string,std::string>, double> aAboveScalarMap,
                                            std::map<std::pair<std::string,std::string>, double> aBelowScalarMap);
    void writeScalers(std::string aFileName, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers, int aLength);
    void readBaseYearData();
    void readRegionalMappingData(std::string aFileName);
    void excludeOutliers(double *aELMNPP, double *aELMHR);
private:
    // Data for calculating the scalar baseline
    std::vector<double> mBaseNPPVector;
    std::vector<double> mBaseHRVector;
    std::vector<double> mBasePFTFractVector;
    
    // Number of latitude, longitude, and PFTs. Storing this so it doesn't have to be passed to every method
    int mNumLat;
    int mNumLon;
    int mNumPFT;
    
    // Map grid cells to regions. Key is a string with longitude and latitude ("lon_lat").
    // Key maps to a vector of strings containing the region and subregion
    // Note that this map will be longer than lat * lon since some grid cells map to multiple regions
    std::map<std::string, std::vector<std::string>> mRegionMapping;
    
    // Map region weights (these indicate the fraction of a grid cell assigned to each region)
    // Key is a pair indicating the grid cell and the region/subregion
    // Key maps to a double representing the fraction of the grid cell in that region/subregion
    std::map<std::pair<std::string,std::string>, double> mRegionWeights;
    
    //! Map PFTs to GCAM crops
    std::map<int, std::vector<std::string>> mPFT2GCAMCropMap {
        //{ 0, { "RockIceDesert", "UrbanLand" } },
        { 1, { "Forest", "UnmanagedForest" } },
        { 2, { "Forest", "UnmanagedForest" } },
        { 3, { "Forest", "UnmanagedForest" } },
        { 4, { "Forest", "UnmanagedForest", "PalmFruit", "willow", "eucalyptus" } },
        { 5, { "Forest", "UnmanagedForest", "willow" } },
        { 6, { "Forest", "UnmanagedForest", "PalmFruit", "willow", "eucalyptus" } },
        { 7, { "Forest", "UnmanagedForest", "willow" } },
        { 8, { "Forest", "UnmanagedForest" } },
        { 9, { "Shrubland", "Jatropha" } },
        { 10, { "Shrubland", "Jatropha" } },
        { 11, { "Shrubland" } },
        { 12, {  "Grassland", "Tundra", "Pasture", "UnmanagedPasture", "FodderGrass"} },
        { 13, { "Grassland", "Pasture", "UnmanagedPasture", "FodderGrass" } },
        { 14, { "Grassland", "Pasture", "UnmanagedPasture", "FodderGrass", "miscanthus" } },
        { 15, { "Corn", "SugarCrop", "Rice", "OtherArableLand" } },
        { 16, { "Wheat", "MiscCrop", "OtherGrain", "OilCrop", "FiberCrop",
            "FodderHerb", "Root_Tuber", "OtherArableLand", "biomass" } }
    };
};

#endif // __CARBON_SCALERS__
