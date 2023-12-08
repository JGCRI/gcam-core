#ifndef __EMISS_DOWNSCALE__
#define __EMISS_DOWNSCALE__

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

class EmissDownscale : public ASpatialData {
public:
    EmissDownscale(int aNumLon, int aNumLat, int aNumMon, int aNumLev, int aNumReg, int aNumCty, int aNumSector, int aNumPeriod);
    ~EmissDownscale();
    // TODO: Eventually these will need to be vectors of regional emissions instead of global totals
    // void downscaleCO2Emissions(const std::string sector, std::vector<double> aCurrYearRegionEmissVector);
    // Proportion-based method directly from regional to grid
    void downscaleSurfaceCO2EmissionsFromRegion2Grid(double *aCurrYearEmissions, std::vector<double>& aBaseYearEmissions_sfc, std::vector<double>& aBaseYearEmissionsGrid_sfc);
    
    // Convergence-based method
    void calculateCountryBaseYearEmissionData(std::vector<double>& aBaseYearEmissions_sfc, std::vector<double>& aBaseYearEmissionsGrid_sfc);
    void downscaleSurfaceCO2EmissionsFromRegion2Country(double *aCurrYearEmissions, int CurrentYear);
    void downscaleSurfaceCO2EmissionsFromCountry2Grid(double *aCurrYearEmissionsi, std::vector<double>& aBaseYearEmissions_sfc, std::vector<double>& aBaseYearEmissionsGrid_sfc);
    
    // downscale international shipment and aircraft CO2 emission from global to grid
    void downscaleInternationalShipmentCO2Emissions(double *aCurrYearEmissions, double aBaseYearGlobalShipCO2Emiss, std::vector<double>& aBaseYearEmissionsGrid_ship);
    void downscaleAircraftCO2Emissions(double *aCurrYearEmissions, double aBaseYearGlobalAirCO2Emiss, std::vector<double>& aBaseYearEmissionsGrid_air);
    
    void separateMonthlyEmissions(double *gcamoco2sfcjan, double *gcamoco2sfcfeb, double *gcamoco2sfcmar,
                                  double *gcamoco2sfcapr, double *gcamoco2sfcmay, double *gcamoco2sfcjun,
                                  double *gcamoco2sfcjul, double *gcamoco2sfcaug, double *gcamoco2sfcsep,
                                  double *gcamoco2sfcoct, double *gcamoco2sfcnov, double *gcamoco2sfcdec,
                                  int aNumLon, int aNumLat);
    void separateMonthlyEmissionsWithVertical(double *gcamoco2airlojan, double *gcamoco2airlofeb, double *gcamoco2airlomar,
                                                              double *gcamoco2airloapr, double *gcamoco2airlomay, double *gcamoco2airlojun,
                                                              double *gcamoco2airlojul, double *gcamoco2airloaug, double *gcamoco2airlosep,
                                                              double *gcamoco2airlooct, double *gcamoco2airlonov, double *gcamoco2airlodec,
                                                              double *gcamoco2airhijan, double *gcamoco2airhifeb, double *gcamoco2airhimar,
                                                              double *gcamoco2airhiapr, double *gcamoco2airhimay, double *gcamoco2airhijun,
                                                              double *gcamoco2airhijul, double *gcamoco2airhiaug, double *gcamoco2airhisep,
                                                              double *gcamoco2airhioct, double *gcamoco2airhinov, double *gcamoco2airhidec,
                                                              int aNumLon, int aNumLat);
    void readRegionMappingData(std::string aFileName);
    void readRegionBaseYearEmissionData(std::string aFileName);
    void readCountryMappingData(std::string aFileName);
    void readCountry2RegionMappingData(std::string aFileName);
    void readPOPGDPCO2Data(std::string POPIIASAFileName, std::string GDPIIASAFileName, std::string POPGCAMFileName, std::string GDPGCAMFileName, std::string CO2GCAMFileName);
    
    std::vector<double> mBaseYearEmissions_sfc;
    std::vector<double> mBaseYearEmissions_air;
    std::vector<double> mBaseYearEmissions_ship;
    std::vector<double> mCountryBaseYearEmissions_sfc;
    std::vector<double> mCountryCurrYearEmissions_sfc;
    std::vector<double> mCurrYearEmissVector;
    
private:
    void readRegionGCAMData(std::string aFileName, std::string aVariableName);
    void calculateRegionPOPIIASAData();
    void calculateRegionBaseYearGDPIIASAData();
    void downscalePOPFromRegion2Country();
    void downscaleGDPFromRegion2Country();
    
    std::vector<double> mBaseYearEmissVector;

    double mBaseYearGlobalSfcCO2Emiss;
    double mBaseYearGlobalShipCO2Emiss;
    double mBaseYearGlobalAirCO2Emiss;
    //std::vector<std::string> mSectors;
    //std::vector<std::string> mRegions;

    // Number of latitude, longitude, and PFTs. Storing this so it doesn't have to be passed to every method
    int mNumLat;
    int mNumLon;
    int mNumMon;
    int mNumLev;
    int mNumReg;
    int mNumCty;
    int mNumSector;
    int mNumPeriod;
    
    std::vector<std::vector<double>> mPOPCountryIIASA;
    std::vector<std::vector<double>> mPOPRegionIIASA;
    std::vector<std::vector<double>> mPOPCountryGCAM;
    std::vector<std::vector<double>> mPOPRegionGCAM;
    std::vector<std::vector<double>> mGDPCountryIIASA;
    std::vector<std::vector<double>> mGDPRegionIIASA;
    std::vector<std::vector<double>> mGDPCountryGCAM;
    std::vector<std::vector<double>> mGDPRegionGCAM;
    std::vector<std::vector<double>> mSfcCO2RegionGCAM;
    
    // Map grid cells to regions. Key is a string with longitude and latitude ("lon_lat").
    // Key maps to a vector of strings containing the region
    // Note that this map will be longer than lat * lon since some grid cells map to multiple regions
    std::map<std::string, std::vector<std::string>> mRegionMapping;
    std::map<std::string, std::vector<std::string>> mCountryMapping;
    std::map<std::string, std::string> mCountry2RegionNameMapping;
    std::map<int, int> mCountry2RegionIDMapping;

    // Map region weights (these indicate the fraction of a grid cell assigned to each region)
    // Key is a pair indicating the grid cell and the region/subregion
    // Key maps to a double representing the fraction of the grid cell in that region/subregion
    std::map<std::pair<std::string,std::string>, double> mRegionWeights;
    std::map<std::pair<std::string,std::string>, double> mCountryWeights;
    
    std::map<std::string, int> mRegionIDName{
        {"USA", 1},
        {"Africa_Eastern", 2},
        {"Africa_Northern", 3},
        {"Africa_Southern", 4},
        {"Africa_Western", 5},
        {"Australia_NZ", 6},
        {"Brazil", 7},
        {"Canada", 8},
        {"CentralAmericaandCaribbean", 9},
        {"CentralAsia", 10},
        {"China", 11},
        {"EU-12", 12},
        {"EU-15", 13},
        {"Europe_Eastern", 14},
        {"Europe_Non_EU", 15},
        {"EuropeanFreeTradeAssociation", 16},
        {"India", 17},
        {"Indonesia", 18},
        {"Japan", 19},
        {"Mexico", 20},
        {"MiddleEast", 21},
        {"Pakistan", 22},
        {"Russia", 23},
        {"SouthAfrica", 24},
        {"SouthAmerica_Northern", 25},
        {"SouthAmerica_Southern", 26},
        {"SouthAsia", 27},
        {"SouthKorea", 28},
        {"SoutheastAsia", 29}, 
        {"Taiwan", 30},
        {"Argentina", 31},
        {"Colombia", 32}
    };
    
    std::map<std::string, int> mCountryIDName;
};

#endif // __EMISS_DOWNSCALE__
