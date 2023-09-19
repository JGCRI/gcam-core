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
EmissDownscale::EmissDownscale(int aNumLon, int aNumLat, int aNumMon, int aNumLev, int aNumReg, int aNumCty, int aNumSector, int aNumPeriod) : ASpatialData(aNumLat * aNumLon * aNumMon * aNumLev),
                                            mBaseYearEmissVector(aNumLat * aNumLon * aNumMon * aNumLev, 0),
                                            mCurrYearEmissVector(aNumLat * aNumLon * aNumMon * aNumLev, 0),
                                            mBaseYearEmissions_sfc(aNumReg, 0),
                                            mBaseYearEmissions_air(aNumReg, 0),
                                            mCountryBaseYearEmissions_sfc(aNumCty, 0),
                                            mCountryCurrYearEmissions_sfc(aNumCty, 0),
                                            mPOPCountryIIASA(aNumCty, std::vector<double> (aNumPeriod, 0)),
                                            mPOPRegionIIASA(aNumReg, std::vector<double> (aNumPeriod, 0)),
                                            mPOPCountryGCAM(aNumCty, std::vector<double> (aNumPeriod, 0)),
                                            mPOPRegionGCAM(aNumReg, std::vector<double> (aNumPeriod, 0)),
                                            mGDPCountryGCAM(aNumCty, std::vector<double> (aNumPeriod, 0)),
                                            mGDPRegionGCAM(aNumReg, std::vector<double> (aNumPeriod, 0)),
                                            mGDPCountryIIASA(aNumCty, std::vector<double> (aNumPeriod, 0)),
                                            mGDPRegionIIASA(aNumReg, std::vector<double> (aNumPeriod, 0)),
                                            mSfcCO2RegionGCAM(aNumReg, std::vector<double> (aNumPeriod, 0)),
                                            mNumLon( aNumLon ),
                                            mNumLat( aNumLat ),
                                            mNumMon( aNumMon ),
                                            mNumLev( aNumLev ),
                                            mNumReg( aNumReg ),
                                            mNumCty( aNumCty ),
                                            mNumSector( aNumSector ),
                                            mNumPeriod( aNumPeriod )
{
}

// Destructor
EmissDownscale::~EmissDownscale()
{
}

// Read in a regional mapping data from a file
void EmissDownscale::readRegionMappingData(std::string aFileName)
{
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

// Read in a country mapping data from a file
void EmissDownscale::readCountryMappingData(std::string aFileName)
{
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
        string country;

        // Parse country index
        getline(iss, token, ',');
        // Parse country name
        getline(iss, token, ',');
        // Parse country iso code
        getline(iss, token, ',');
        country = token;
        country.erase(remove(country.begin(), country.end(), '\"'), country.end());

        // Parse longitude
        getline(iss, token, ',');
        lon = std::stoi(token);

        // Parse latitude
        getline(iss, token, ',');
        lat = std::stoi(token);

        string gridID = std::to_string(lon) + "_" + std::to_string(lat);
        

        // Create country ID
        string ctyID = country;

        // Add country ID to the mapping vector.
        // Note that there maybe more than one rctyID per gridID (hence, a vector)
        if (mCountryMapping.find(gridID) == mCountryMapping.end())
        {
            // If gridID is not found, then add it with this ctyID in its vector
            vector<string> temp;
            temp.push_back(ctyID);
            mCountryMapping[gridID] = temp;
        }
        else
        {
            // If gridID is found, then add to the existing ctyID vector
            auto currGrid = mCountryMapping.find(gridID);
            (*currGrid).second.push_back(ctyID);
        }

        // Parse Weight -- this is the fraction of the grid cell in a particular GCAM Country
        getline(iss, token, ',');
        value = std::stod(token);

        mCountryWeights[std::make_pair(gridID, ctyID)] = value;
    }

    return;
}

// Read in a country mapping data from a file
void EmissDownscale::readCountry2RegionMappingData(std::string aFileName)
{
    ifstream data(aFileName);
    if (!data.is_open())
    {
        exit(EXIT_FAILURE);
    }
    string str;
    getline(data, str); // skip the first line
    getline(data, str); // skip the second line
    getline(data, str); // skip the third line
    getline(data, str); // skip the fourth line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        string countryName;
        string regionName;
        int countryID;
        int regionID;
        // Parse country iso code
        getline(iss, token, ',');
        countryName = token;
        countryName.erase(remove(countryName.begin(), countryName.end(), '\"'), countryName.end());

        // Parse country name
        getline(iss, token, ',');
 
        // Parse country ID
        getline(iss, token, ',');
        countryID = std::stoi(token);

        // Parse gcam region name
        getline(iss, token, ',');
        regionName = token;
        regionName.erase(remove(regionName.begin(), regionName.end(), '\"'), regionName.end());

        // Parse the regional ID number
        getline(iss, token, ',');
        regionID = std::stoi(token);
        
        // Add country ID to the mapping vector.
        // Note that one country just belongs to one region.
        mCountry2RegionIDMapping[countryID] = regionID;
        mCountry2RegionNameMapping[countryName] = regionName;
        mCountryIDName[countryName] = countryID;

    }

    return;
}

// Read in regional Base-Year Emission Data from a file
void EmissDownscale::readRegionBaseYearEmissionData(std::string aFileName)
{
    mBaseYearGlobalSfcCO2Emiss = 0.0;
    mBaseYearGlobalShipCO2Emiss = 0.0;
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
        else if (sectorID == "shipment")
        { mBaseYearEmissions_ship[regIndex] = value;
          mBaseYearGlobalShipCO2Emiss += value;}
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

// Calculate country Base year surface CO2 emission
void EmissDownscale::calculateCountryBaseYearEmissionData()
{
    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();
        
    int gridIndex = 0;   // Index used for Grid vectors
    int valIndex = 0;    // Index used for PFT x Grid vectors
    double weight = 0.0; // Define the total weight
    
    for (int k = 1; k <= mNumLat; k++)
    {
        for (int j = 1; j <= mNumLon; j++)
        {
            
            gridIndex = (k - 1) * mNumLon + (j - 1);
            
            // Get country for this entry
            string gridID = std::to_string(j) + "_" + std::to_string(k);
            auto tempGrid = mCountryMapping.find(gridID);
            if (mCountryMapping.find(gridID) == mCountryMapping.end())
            {
                // Grid isn't found in the mapping. Currently, this probably means it is an ocean grid.
                // TODO: set up loop only over land grids, either using the mCountryMapping or one of the files from ELM
            }
            else
            {
                vector<string> ctyInGrd = (*tempGrid).second;
                weight = 0;
                // Loop over all regions this grid is mapped to and calculate the scalars
                for (auto ctyID : ctyInGrd)
                {
                    // Calculate total as NPP/HR of the PFT * area of the PFT
                    // pft value is fraction of grid cell
                    auto currCty = mCountryIDName.find(ctyID);
                    int ctyIndex = (*currCty).second - 1;
                    
                    weight += mCountryWeights[std::make_pair(gridID, ctyID)];
                }
                
                for (auto ctyID : ctyInGrd)
                {
                    // Calculate total as NPP/HR of the PFT * area of the PFT
                    // pft value is fraction of grid cell
                    auto currCty = mCountryIDName.find(ctyID);
                    int ctyIndex = (*currCty).second - 1;
                    
                    for (int mon = 1; mon <= mNumMon; mon++)
                    {
                        valIndex = (mon - 1) * mNumLon * mNumLat + (k - 1) * mNumLon + (j - 1);
                        mCountryBaseYearEmissions_sfc[ctyIndex] = mBaseYearEmissVector[valIndex] * mCountryWeights[std::make_pair(gridID, ctyID)] / weight;
                    }
                }
            }
        }
    }
    
    // Finally, re-set the value vector to be the final emissions, since this will be written out
    setValueVector(mCurrYearEmissVector);
    
    return;
}
// Read POP, GDP and CO2 emission Data from files
void EmissDownscale::readPOPGDPCO2Data(std::string aPOPIIASAFileName, std::string aGDPIIASAFileName, std::string aPOPGCAMFileName, std::string aGDPGCAMFileName, std::string aCO2GCAMFileName)
{
    string str;
    
    // read country IIASA POP data
    ifstream data(aPOPIIASAFileName);
    if (!data.is_open())
    {
        exit(EXIT_FAILURE);
    }
    
    getline(data, str); // skip the first line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        int ctyID;
        int ctyIndex;
        int yearIndex;
        
        // Parse country ID
        getline(iss, token, ',');
        ctyID = std::stod(token);
        
        // Parse Value
        for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
        {
            
            ctyIndex = ctyID - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            getline(iss, token, ',');
            value = std::stod(token);
            
            mPOPCountryIIASA[ctyIndex][yearIndex] = value;
        }
    }
    
    
    // calculate regional IIASA POP
    calculateRegionPOPIIASAData();
    
    // read country base-year IIASA GDP data
    ifstream data1(aGDPIIASAFileName);
    if (!data1.is_open())
    {
        exit(EXIT_FAILURE);
    }
    
    getline(data1, str); // skip the first line
    while (getline(data1, str))
    {
        istringstream iss(str);
        string token;
        double value;
        int ctyID;
        int ctyIndex;
        int yearIndex;
        int yearID;
        
        // Parse country ID
        getline(iss, token, ',');
        ctyID = std::stod(token);
        
        // Parse Value
        yearID = 2015;
        ctyIndex = ctyID - 1;
        yearIndex = ceil((yearID - 2015) / 5);
        
        getline(iss, token, ',');
        value = std::stod(token);
        
        mGDPCountryIIASA[ctyIndex][yearIndex] = value;
    }
    
    // calculate regional IIASA GDP
    calculateRegionBaseYearGDPIIASAData();
    
    // read regional GCAM output
    // read regional POP data
    readRegionGCAMData(aPOPGCAMFileName, "POP");
    
    //read regional GDP data
    readRegionGCAMData(aGDPGCAMFileName, "GDP");
    
    //read regional surface CO2 emission data
    readRegionGCAMData(aCO2GCAMFileName, "CO2");
    
    return;
}


// Read in country POP Data from a file
void EmissDownscale::readRegionGCAMData(std::string aFileName, std::string aVariableName)
{
    string str;
    
    ifstream data(aFileName);
    if (!data.is_open())
    {
        exit(EXIT_FAILURE);
    }
    
    getline(data, str); // skip the first line
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        int regID;
        int regIndex;
        int yearIndex;
        
        // Parse region ID
        getline(iss, token, ',');
        regID = std::stod(token);
        
        // Parse Value
        for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
        {
            
            regIndex = regID - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            getline(iss, token, ',');
            value = std::stod(token);
            
            if(aVariableName == "POP")
            {mPOPRegionGCAM[regID][yearIndex] = value;}
            else if(aVariableName == "GDP")
            {mGDPRegionGCAM[regID][yearIndex] = value;}
            else if(aVariableName == "CO2")
            {mSfcCO2RegionGCAM[regID][yearIndex] = value;}
            else
            {
                ILogger& coupleLog = ILogger::getLogger( "coupling_log" );
                coupleLog.setLevel( ILogger::ERROR );
                coupleLog << "Variable not found: " << aVariableName << endl;
                exit(EXIT_FAILURE);
            }
        }
    }
    return;
}


// Calculate regional POP  from country POP
void EmissDownscale::calculateRegionPOPIIASAData()
{
    int yearIndex = 0;
    int ctyIndex = 0;
    int regIndex = 0;
    
    // initialize variables
    for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
    {
        
        for (int regID = 1; regID <= mNumReg; regID++)
        {
            regIndex = regID - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            mPOPRegionIIASA[regIndex][yearIndex] = 0.0;
        }
    }
    // aggregate values from country- to regional levels
    for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
    {
        for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
        {
            ctyIndex = ctyID - 1;
            regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            mPOPRegionIIASA[regIndex][yearIndex] = mPOPRegionIIASA[regIndex][yearIndex]  + mPOPCountryIIASA[ctyIndex][yearIndex];
        }
    }
    return;
}


// Calculate regional POP from country POP
void EmissDownscale::calculateRegionBaseYearGDPIIASAData()
{
    int yearIndex = 0;
    int ctyIndex = 0;
    int regIndex = 0;
    
    
    int yearID = 2015;
    yearIndex = ceil((yearID - 2015) / 5);
    // initialize variables
    for (int regID = 1; regID <= mNumReg; regID++)
    {
        regIndex = regID - 1;
        
        mGDPRegionIIASA[regIndex][yearIndex] = 0.0;
    }
    
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        mGDPRegionIIASA[regIndex][yearIndex] = mGDPRegionIIASA[regIndex][yearIndex]  + mGDPCountryIIASA[ctyIndex][yearIndex];
    }
    return;
}


// Downscale POP using the IIASA Data
void EmissDownscale::downscalePOPFromRegion2Country()
{
    int yearIndex = 0;
    int ctyIndex = 0;
    int regIndex = 0;
    
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
        {
            yearIndex = ceil((yearID - 2015)/5);
            mPOPCountryGCAM[ctyIndex][yearIndex] = mPOPCountryIIASA[ctyIndex][yearIndex] * mPOPRegionGCAM[regIndex][yearIndex] / mPOPRegionIIASA[regIndex][yearIndex];
        }
    }
    return;
}

// Downscale GDP using the convergence method
void EmissDownscale::downscaleGDPFromRegion2Country()
{
    int regIndex = 0;
    int ctyIndex = 0;
    int yearIndex = 0;
    int yearIndex1 = 0;
    int yearIndex2 = 0;
    double tmp = 0.0;
    double PPPGrowthRate = 0.0;
    double GDPRegionPred[mNumReg][mNumPeriod], GDPRegionIncrease[mNumReg][mNumPeriod], GDPRegionDiff[mNumReg][mNumPeriod];
    double GDPCountryShare[mNumCty][mNumPeriod], PPPCountryGCAM[mNumCty][mNumPeriod];
    
    //initialize the variables
    for (int regID = 1; regID <= mNumReg; regID++)
    {
        for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
        {
            regIndex = regID - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            GDPRegionPred[regIndex][yearIndex] = 0.0;
            GDPRegionIncrease[regIndex][yearIndex] = 0.0;
            GDPRegionDiff[regIndex][yearIndex] = 0.0;
        }
    }
    
    //initialize the variables
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        for (int yearID = 2015; yearID <= 2100; yearID = yearID + 5)
        {
            ctyIndex = ctyID - 1;
            yearIndex = ceil((yearID - 2015) / 5);
            
            GDPCountryShare[ctyIndex][yearIndex] = 0.0;
        }
    }
    
    
    //calculate Base-year GDP
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        yearIndex = ceil((2015 - 2015)/5);
        mGDPCountryGCAM[ctyIndex][yearIndex] = mGDPCountryIIASA[ctyIndex][yearIndex] * mGDPRegionGCAM[regIndex][yearIndex] /mGDPRegionIIASA[regIndex][yearIndex];
    }
    
    // predict future GDP using the partial convergence methods
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        // calculate growth rate from 2090 to 2100
        yearIndex1 = ceil((2100 - 2015)/5);
        yearIndex2 = ceil((2090 - 2015)/5);
        tmp = pow((mGDPRegionGCAM[regIndex][yearIndex1] / mPOPRegionGCAM[regIndex][yearIndex1]) / (mGDPRegionGCAM[regIndex][yearIndex2] / mPOPRegionGCAM[regIndex][yearIndex2]), 1.0/(10.0));
        
        // calculate GDP per capital in 2150
        tmp =  (mGDPRegionGCAM[regIndex][yearIndex1] / mPOPRegionGCAM[regIndex][yearIndex1]) * pow(tmp, 2150-2100);
        
        // calculate growth rate from 2015 to 2150
        yearIndex1 = ceil((2015 - 2015)/5); //Base year index
        PPPGrowthRate = (tmp / (mGDPCountryGCAM[regIndex][yearIndex1] / mPOPCountryGCAM[regIndex][yearIndex1]), 1.0/(2150.0 - 2015.0));
        
        for (int yearID = 2020; yearID <= 2100; yearID = yearID + 5)
        {
            yearIndex = ceil((yearID - 2015) / 5);
            PPPCountryGCAM[ctyIndex][yearIndex] = (mGDPCountryGCAM[ctyIndex][yearIndex1] / mPOPCountryGCAM[regIndex][yearIndex1]) * pow(PPPGrowthRate, yearID - 2015);
            GDPRegionPred[regIndex][yearIndex] = GDPRegionPred[regIndex][yearIndex] + PPPCountryGCAM[ctyIndex][yearIndex] * mPOPCountryGCAM[ctyIndex][yearIndex];
            GDPRegionIncrease[regIndex][yearIndex] = GDPRegionIncrease[regIndex][yearIndex] + PPPCountryGCAM[ctyIndex][yearIndex] * mPOPCountryGCAM[ctyIndex][yearIndex] - PPPCountryGCAM[ctyIndex][yearIndex-1] * mPOPCountryGCAM[ctyIndex][yearIndex-1];;
        }
    }
    
    // calculate regional difference between GCAM estimates and predicted as above
    for (int regID = 1; regID <= mNumReg; regID++)
    {
        regIndex = regID - 1;
        for (int yearID = 2020; yearID <= 2100; yearID = yearID + 5)
        {
            yearIndex = ceil((yearID - 2015) / 5);
            GDPRegionDiff[regIndex][yearIndex] = mGDPRegionGCAM[regIndex][yearIndex]  - GDPRegionPred[regIndex][yearIndex];
        }
    }
    
    
    // adjust bias
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        for (int yearID = 2020; yearID <= 2100; yearID = yearID + 5)
        {
            yearIndex = ceil((yearID - 2015) / 5);
            GDPCountryShare[ctyIndex][yearIndex] = PPPCountryGCAM[ctyIndex][yearIndex] * mPOPCountryGCAM[ctyIndex][yearIndex] - PPPCountryGCAM[ctyIndex][yearIndex-1] * mPOPCountryGCAM[ctyIndex][yearIndex-1];
            PPPCountryGCAM[ctyIndex][yearIndex] = PPPCountryGCAM[ctyIndex][yearIndex] + GDPRegionDiff[regIndex][yearIndex] * GDPCountryShare[ctyIndex][yearIndex] / mPOPCountryGCAM[ctyIndex][yearIndex];
            mGDPCountryGCAM[ctyIndex][yearIndex] = PPPCountryGCAM[ctyIndex][yearIndex] * mPOPCountryGCAM[ctyIndex][yearIndex];
        }
    }
    
    return;
}

// Downscale emissions using the convergence method
void EmissDownscale::downscaleSurfaceCO2EmissionsFromRegion2Country(double *aRegionCurrYearEmissions, int currentYear)
{
    //calculate Future Country-level POP and GPP data
    downscalePOPFromRegion2Country();
    downscaleGDPFromRegion2Country();
    
    //define variables
    int regIndex = 0;
    int ctyIndex = 0;
    int yearIndex1 = 0;
    int yearIndex2 = 0;
    double currentYearGDP = 0.0;
    double EIGrowthRate = 0.0;
    double weight = 0.0; // Define the weight
    double tmp = 0.0;
    double CO2RegionPred[mNumReg], CO2RegionShare[mNumReg], CO2RegionDiff[mNumReg];
    double EICountryGCAM[mNumCty];
    
    int yearIndex = floor((currentYear - 2015)/5);   // need interploate
    weight = (currentYear - yearIndex1)/5;   // define the weight
    
    //initize the variables
    for (int regID = 1; regID <= mNumReg; regID++)
    {
        regIndex = regID - 1;
        CO2RegionPred[regIndex] = 0.0;
        CO2RegionShare[regIndex] = 0.0;
        CO2RegionDiff[regIndex] = 0.0;
    }
    
    //initize the variables
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        
        EICountryGCAM[ctyIndex] = 0.0;
    }
    
    
    // predict future co2 emission using the partial convergence methods
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        // calculate growth rate from 2090 to 2100; need to check whether it is smaller than zero
        yearIndex1 = 2100;
        yearIndex2 = 2090;
        tmp = pow((mSfcCO2RegionGCAM[regIndex][yearIndex1] / mGDPRegionGCAM[regIndex][yearIndex1]) / (mSfcCO2RegionGCAM[regIndex][yearIndex2] / mGDPRegionGCAM[regIndex][yearIndex2]), 1.0/(10.0));
        // calculate GDP per capital in 2150
        tmp =  (mSfcCO2RegionGCAM[regIndex][yearIndex1] / mGDPRegionGCAM[regIndex][yearIndex1]) * pow(tmp, 2150-2100);
        // calculate growth rate from 2015 to 2150
        yearIndex1 = 2015; //Base year index
        EIGrowthRate = (tmp / (mCountryCurrYearEmissions_sfc[ctyIndex] / mGDPCountryGCAM[ctyIndex][yearIndex1]), 1.0/(2150.0 - 2015.0));
        
        EICountryGCAM[ctyIndex] = (mCountryCurrYearEmissions_sfc[regIndex] / mGDPCountryGCAM[regIndex][yearIndex1]) * pow(EIGrowthRate, currentYear - 2015);
        
        currentYearGDP = mGDPCountryGCAM[ctyIndex][yearIndex] * (1-weight) + mGDPCountryGCAM[ctyIndex][yearIndex+1] * weight;
        CO2RegionPred[regIndex] = CO2RegionPred[regIndex] + EICountryGCAM[ctyIndex] * currentYearGDP;
        CO2RegionShare[regIndex] = CO2RegionShare[regIndex] + EICountryGCAM[ctyIndex] * currentYearGDP;
    }
    
    // calculate regional difference between GCAM estimates and predicted as above
    for (int regIndex = 1; regIndex <= mNumReg; regIndex++)
    {
        CO2RegionDiff[regIndex] = aRegionCurrYearEmissions[regIndex]  - CO2RegionPred[regIndex];
    }
    
    // adjust bias
    for (int ctyID = 1; ctyID <= mNumCty; ctyID++)
    {
        ctyIndex = ctyID - 1;
        regIndex = mCountry2RegionIDMapping[ctyIndex] - 1;
        
        currentYearGDP = mGDPCountryGCAM[ctyIndex][yearIndex] * (1-weight) + mGDPCountryGCAM[ctyIndex][yearIndex+1] * weight;
        mCountryCurrYearEmissions_sfc[ctyIndex] = EICountryGCAM[ctyIndex] * currentYearGDP + CO2RegionDiff[regIndex] * EICountryGCAM[ctyIndex] * currentYearGDP / CO2RegionShare[regIndex];
    }
    
    return;
}

// Downscale emissions using the convergence method
void EmissDownscale::downscaleSurfaceCO2EmissionsFromCountry2Grid()
{ // baseYearEmission need to be updated
    
    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();
    
    // Calculate current year emissions vector by scaling base year emissions up
    mCurrYearEmissVector = mBaseYearEmissVector;
    // double scaler = aCurrYearEmissions / aBaseYearEmissions;
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
            
            // Get country for this entry
            string gridID = std::to_string(j) + "_" + std::to_string(k);
            auto tempGrid = mCountryMapping.find(gridID);
            if (mCountryMapping.find(gridID) == mCountryMapping.end())
            {
                // Grid isn't found in the mapping. Currently, this probably means it is an ocean grid.
                // TODO: set up loop only over land grids, either using the mCountryMapping or one of the files from ELM
            }
            else
            {
                vector<string> ctyInGrd = (*tempGrid).second;
                scalar = 0;
                weight = 0;
                // Loop over all regions this grid is mapped to and calculate the scalars
                for (auto ctyID : ctyInGrd)
                {
                    // Calculate total as NPP/HR of the PFT * area of the PFT
                    // pft value is fraction of grid cell
                    auto currCty = mCountryIDName.find(ctyID);
                    int ctyIndex = (*currCty).second - 1;
                    
                    scalar += mCountryCurrYearEmissions_sfc[ctyIndex] / mBaseYearCountryEmissions_sfc[ctyIndex] * mCountryWeights[std::make_pair(gridID, ctyID)];
                    weight += mCountryWeights[std::make_pair(gridID, ctyID)];
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
void EmissDownscale::downscaleSurfaceCO2EmissionsFromRegion2Grid(double *aCurrYearEmissions)
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
void EmissDownscale::downscaleInternationalShipmentCO2Emissions(double *aCurrYearEmissions)
{ // baseYearEmission need to be updated

    // First, set the values that were read in as the BaseYearEmissions
    mBaseYearEmissVector = getValueVector();

    // Calculate current year emissions vector by scaling base year emissions up
    mCurrYearEmissVector = mBaseYearEmissVector;

    double scalar = 0.0; // Define the scalar
    
    double aCurrYearGlobalShipCO2Emiss = 0.0;

    for(int k = 1; k <= mNumReg; k++)
        aCurrYearGlobalShipCO2Emiss += aCurrYearEmissions[k-1];

    scalar =  aCurrYearGlobalShipCO2Emiss / mBaseYearGlobalShipCO2Emiss;

    std::transform(mCurrYearEmissVector.begin(), mCurrYearEmissVector.end(),
                   mCurrYearEmissVector.begin(), [scalar](double i) { return i * scalar; });

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
