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
 * \file aspatial_data.cpp
 * \brief This file processes gridded data either from file or from memory. It is an abstract
 * class from which the carbon scaler and emissions downscale code is derived.
 *
 * \author Kate Calvin
 */

#include <iostream>
#include <fstream>

#include "util/base/include/auto_file.h"
#include "../include/aspatial_data.h"

using namespace std;

// Constructor
ASpatialData::ASpatialData(int aSize):
mLatVector(aSize, 0),
mLonVector(aSize, 0),
mIDVector(aSize, 0),
mValueVector(aSize, 0) {
}

// Destructor
ASpatialData::~ASpatialData() {
}

// Read in spatial data from a csv file
// Note: this is used for diagnostics and testing. In fully coupled E3SM-GCAM, this data
// are passed in code to the wrapper
double ASpatialData::readSpatialData(std::string aFileName, bool aHasLatLon, bool aHasID, bool aCalcTotal) {
    // Create a double to store totals (if aCalcTotal == true)
    double total = 0.0;
    
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
    int row = 0;
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        
        if ( aHasID ) {
            int id;
            
            // Parse ID
            getline(iss, token, ' ');
            id = std::stoi(token);
            mIDVector.at(row) = id;
         }
        
        if ( aHasLatLon ) {
            double lon;
            int lat;
            
            // Parse longitude
            getline(iss, token, ' ');
            lon = std::stod(token);
            mLonVector[row] = lon;
            
            // Parse latitude
            getline(iss, token, ' ');
            lat = std::stod(token);
            mLatVector[row] = lat;
        }
        
        // Parse Value
        getline(iss, token, ' ');
        value = std::stod(token);
        mValueVector[row] = value;
         
        // if aCalcTotal == true, then add this to the total
        total += value;
                
        row++;
    }
    
    return total;
}

// Read in spatial data from a csv file directly into an array
double ASpatialData::readSpatialData(std::string aFileName, bool aHasLatLon, bool aHasID, bool aCalcTotal, double *aValueArray) {
    // Create a double to store totals (if aCalcTotal == true)
    double total = 0.0;
    
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
    int row = 0;
    while (getline(data, str))
    {
        istringstream iss(str);
        string token;
        double value;
        
        if ( aHasID ) {
            int id;
            
            // Parse ID
            getline(iss, token, ' ');
            id = std::stoi(token);
            mIDVector.at(row) = id;
        }
        
        if ( aHasLatLon ) {
            double lon;
            int lat;
            
            // Parse longitude
            getline(iss, token, ' ');
            lon = std::stod(token);
            mLonVector[row] = lon;
            
            // Parse latitude
            getline(iss, token, ' ');
            lat = std::stod(token);
            mLatVector[row] = lat;
        }
        
        // Parse Value
        getline(iss, token, ' ');
        value = std::stod(token);
        aValueArray[row] = value;
        
        // if aCalcTotal == true, then add this to the total
        total += value;
        
        row++;
    }

    return total;
}

void ASpatialData::writeSpatialData(std::string aFileName, bool aWriteID) {
    ofstream oFile;
    oFile.open(aFileName);
    for (int i = 0; i < mValueVector.size(); i++) {
        if( aWriteID ) {
            oFile << mLonVector[i] << "," << mLatVector[i] << "," << mIDVector[i] << ",";
        }
        oFile << mValueVector[i] << endl;
    }
    oFile.close();
    
    return;
}

void ASpatialData::readMapping(std::string aFileName) {
    return;
}


void ASpatialData::setValueVector(std::vector<double> aValueVector) {
    mValueVector = aValueVector;
    return;
}

std::vector<double> ASpatialData::getValueVector() {
    return mValueVector;
}

std::vector<int> ASpatialData::getIDVector() {
    return mIDVector;
}

std::vector<double> ASpatialData::getLatVector() {
    return mLatVector;
}

std::vector<double> ASpatialData::getLonVector() {
    return mLonVector;
}

