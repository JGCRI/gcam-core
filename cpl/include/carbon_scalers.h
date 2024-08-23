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
    int readScalers(std::string aFileName, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                      std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers);
    void calcScalers(int aGCAMYear, double *aELMArea, double *aELMPFTFract, double *aELMNPP, double *aELMHR,
                     std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs, std::vector<double>& aAboveScalers,
                     std::vector<double>& aBelowScalers, std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName,
                     int& aNumScalars);
    void createScalerVectors(int aGCAMYear, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                                            std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers,
                                            std::map<std::pair<std::string,std::string>, double> aAboveScalarMap,
                                            std::map<std::pair<std::string,std::string>, double> aBelowScalarMap);
    void writeScalers(std::string aFileName, std::vector<int>& aYears, std::vector<std::string>& aRegions, std::vector<std::string>& aLandTechs,
                      std::vector<double>& aAboveScalers, std::vector<double>& aBelowScalers, int aLength);
    void readBaseYearData(std::string aBaseNPPFileName, std::string aBaseHRFileName, std::string aBasePFTWtFileName);
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

    // the cell area associated with each pft in each cell; memory dealt with on construction and destruction
    double *mELMArea;
    
    // Map grid cells to regions. Key is a string with longitude and latitude ("lon_lat").
    // Key maps to a vector of strings containing the region and subregion
    // Note that this map will be longer than lat * lon since some grid cells map to multiple regions
    std::map<std::string, std::vector<std::string>> mRegionMapping;
    
    // Map region weights (these indicate the fraction of a grid cell assigned to each region)
    // Key is a pair indicating the grid cell and the region/subregion
    // Key maps to a double representing the fraction of the grid cell in that region/subregion
    std::map<std::pair<std::string,std::string>, double> mRegionWeights;
    
    //! Map PFTs to GCAM crops
    // These are matched for pft relationship over location; for example, several crop area types are in forest or c4 grass rather than cropland
    // So if the natural types are not present in the cell, but the crops are, then these crops will have a scalar of one
    std::map<int, std::vector<std::string>> mPFT2GCAMCropMap {
     { 0, { "RockIceDesert", "UrbanLand" } }, // 0. BPFT, Bare ground/not vegetated
     { 1, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest" } }, // 1. NEMPFT, Needleleaf evergreen temperate tree
     { 2, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest" } }, // 2. NEBPFT, Needleleaf evergreen boreal tree
     { 3, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest" } }, // 3. NDBPFT, Needleleaf deciduous boreal tree
     { 4, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest", "OilCropTree", "MiscCropTree" } }, // 4. BETPFT, Broadleaf evergreen tropical tree
     { 5, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest", } }, // 5. BEMPFT, Broadleaf evergreen temperate tree
     { 6, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest", "OilCropTree", "MiscCropTree" } }, // 6. BDTPFT, Broadleaf deciduous tropical tree
     { 7, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest", "NutsSeedsTree", "FruitsTree" } }, // 7. BDMPFT, Broadleaf deciduous temperate tree
     { 8, { "Forest", "UnmanagedForest", "ProtectedUnmanagedForest" } }, // 8. BDBPFT, Broadleaf deciduous boreal tree
     { 9, { "Shrubland", "ProtectedShrubland" } }, // 9. SEMPFT, Broadleaf evergreen temperate shrub
     { 10, { "Shrubland", "ProtectedShrubland" } }, // 10. SDMPFT, Broadleaf deciduous temperate shrub
     { 11, { "Shrubland", "ProtectedShrubland" } }, // 11. SDBPFT, Broadleaf deciduous boreal shrub
     { 12, { "Grassland", "ProtectedGrassland", "Tundra", "Pasture", "UnmanagedPasture", "ProtectedUnmanagedPasture"} }, // 12. GA3PFT, C3 arctic grass
     { 13, { "Grassland", "ProtectedGrassland", "Pasture", "UnmanagedPasture", "ProtectedUnmanagedPasture"} }, // 13. GC3PFT, C3 non-arctic grass
     { 14, { "Grassland", "ProtectedGrassland", "Pasture", "UnmanagedPasture", "ProtectedUnmanagedPasture", "MiscCropC4", "FodderHerbC4" } }, // 14. GC4PFT, C4 grass
     { 15, { "MiscCrop", "NutsSeeds", "FiberCrop", "FodderHerb", "OtherArableLand", "Fruits", "Legumes", "Vegetables" } }, // 15. CPFT, Cropland
     { 16, {  } }, // 16. NA
     { 17, { "CornC4" } }, // 17. CFT Corn
     { 18, {  } }, // 18. CFT Irrigated Corn
     { 19, { "Wheat" } }, // 19. CFT spring_temperate_cereal
     { 20, {  } }, // 20. CFT Irrigated spring_temperate_cereal
     { 21, {  } }, // 21. CFT winter_temperate_cereal
     { 22, {  } }, // 22. CFT Irrigated winter_temperate_cereal
     { 23, { "Soybean" } }, // 23. CFT Soybean
     { 24, {  } }, // 24. CFT Irrigated Soybean
     { 25, {  } }, // 25. CFT cassava
     { 26, {  } }, // 26. CFT Irrigated cassava
     { 27, {  } }, // 27. CFT cotton
     { 28, {  } }, // 28. CFT Irrigated cotton
     { 29, { "FodderGrass" } }, // 29. CFT foddergrass
     { 30, {  } }, // 30. CFT Irrigated foddergrass
     { 31, { "OilPalmTree" } }, // 31. CFT Oilpalm
     { 32, {  } }, // 32. CFT Irrigated Oilpalm
     { 33, { "OtherGrainC4", "OtherGrain" } }, // 33. CFT other_grains
     { 34, {  } }, // 34. CFT Irrigated other_grains
     { 35, { "OilCrop" } }, // 35. CFT rapeseed
     { 36, {  } }, // 36. CFT Irrigated rapeseed
     { 37, { "Rice" } }, // 37. CFT Rice
     { 38, {  } }, // 38. CFT Irrigated Rice
     { 39, { "RootTuber" } }, // 39. CFT root_tubers
     { 40, {  } }, // 40. CFT Irrigated root_tubers
     { 41, { "SugarCropC4", "SugarCrop" } }, // 41. CFT Sugarcane
     { 42, {  } }, // 42. CFT Irrigated Sugarcane
     { 43, { "biomassGrass" } }, // 43. CFT Miscanthus
     { 44, {  } }, // 44. CFT Irrigated Miscanthus
     { 45, { "biomassGrass" } }, // 45. CFT Switchgrass
     { 46, {  } }, // 46. CFT Irrigated Switchgrass
     { 47, { "biomassTree" } }, // 47. CFT Poplar
     { 48, {  } }, // 48. CFT Irrigated Poplar
     { 49, { "biomassTree" } }, // 49. CFT Willow
     { 50, {  } } // 50. CFT Irrigated Willow
    };
};

#endif // __CARBON_SCALERS__
