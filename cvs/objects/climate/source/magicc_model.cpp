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
* \file magicc_model.cpp
* \ingroup Objects
* \brief This file contains the source for the MagiccModel class.
* \author Sonny Kim, Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <iomanip>
#include <fstream>
#include <string>
#include <cassert>

#include "climate/include/magicc_model.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/util.h"
#include "util/base/include/auto_file.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;

extern Scenario* scenario;

#include "climate/include/ObjECTS_MAGICC.h"

// MAGICC 5.3 expects a 2000 year line in gas.emk
const int MagiccModel::GAS_EMK_CRIT_YEAR = 2000;

// Setup the gas name vector.
const string MagiccModel::sInputGasNames[] = { "CO2",
                                               "CO2NetLandUse",
                                               "CH4",
                                               "N2O",
                                               "SOXreg1",
                                               "SOXreg2",
                                               "SOXreg3",
                                               "CF4",
                                               "C2F6",
                                               "HFC125",
                                               "HFC134a",
                                               "HFC143a",
                                               "HFC227ea",
                                               "HFC245ca",
                                               "SF6",
                                               "NOx",
                                               "NMVOCs",
                                               "CO",
                                               "BC",
                                               "OC" };

// Setup the gas units vector.
const string MagiccModel::sInputGasUnits[] = { "(Pg C)",
                                               "(Pg C)",
                                               "(Tg)",
                                               "(Tg N)",
                                               "(Tg S)",
                                               "(Tg S)",
                                               "(Tg S)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(kton)",
                                               "(Mt N)",
                                               "(Mt)",
                                               "(Mt)",
                                               "(Mt)",
                                               "(Mt)" };

/*! \brief Constructor
* \param aModeltime Pointer to the global modeltime object.
*/
MagiccModel::MagiccModel()
{
    mGHGInputFileName = "";
    mIsValid = false;
    mClimateSensitivity = -1;
    mSoilTempFeedback = -1;
    mHumusTempFeedback = -1;
    mGPPTempFeedback = -1;
    mOceanCarbFlux80s = -1;
    mNetDeforestCarbFlux80s = -1;
    mSO2Dir1990 = 0; // initialize to zero since -1 is a legit value
    mSO2Ind1990 = 0; // initialize to zero since -1 is a legit value
    mBCUnitForcing = 0;
    mOCUnitForcing = 0; // initialize to zero since -1 is a legit value
    mLastHistoricalYear = 0; // default to zero -- use only model data
    mNumberHistoricalDataPoints = 0; // internal counter
}

/*! \brief Complete the initialization of the MagiccModel.
* \details This function first resizes the internal vectors which store
*          emissions by gas and period. It then reads in the default set of data
*          for emissions by gas from an input file. These emissions can be
*          overridden by calls to setEmission.
* \param aScenarioName Name of the current model scenario.
*/
void MagiccModel::completeInit( const string& aScenarioName ){
    // The climate model should not be reinitialized.
    assert( !mIsValid );

    mScenarioName = aScenarioName;
    // Resize to the number of gases based on the gas name vector.
    mModelEmissionsByGas.resize( getNumInputGases() );
    
    const Modeltime* modeltime = scenario->getModeltime();

    // Resize all the vectors to the number of data points for MAGICC.
    const int numGasPoints = modeltime->getmaxper() + getNumAdditionalGasPoints() + 1; //Add space for 1975 to make later code clearer
    for( unsigned int i = 0; i < mModelEmissionsByGas.size(); ++i ){
        mModelEmissionsByGas[ i ].resize( numGasPoints );
    }
	
	// Resize the LUC CO2 vector to the number of years plus the number of additional points
	const int numYears = modeltime->getEndYear() - modeltime->getStartYear()
							+ getNumAdditionalGasPoints();
	mLUCEmissionsByYear.resize( numYears );
	
    // Read in the data from the file. These values may later be overridden.
    readFile();
    mIsValid = false;
    
    // Set up correspondence for output gas names
    mOutputGasNameMap[ "total" ] = 0;
    mOutputGasNameMap[ "CO2" ] = 1;
    mOutputGasNameMap[ "CH4" ] = 2;  // Direct CH4 only
    mOutputGasNameMap[ "N2O" ] = 3;
    mOutputGasNameMap[ "C2F6" ] = 4;
    mOutputGasNameMap[ "HCFC125" ] = 5;
    mOutputGasNameMap[ "HCFC134A" ] = 6;
    mOutputGasNameMap[ "HCFC143A" ] = 7;
    mOutputGasNameMap[ "HCFC245fa" ] = 8;
    mOutputGasNameMap[ "SF6" ] = 9;
    mOutputGasNameMap[ "CF4" ] = 10;
    mOutputGasNameMap[ "HFC227ea" ] = 11;
    mOutputGasNameMap[ "OtherHC" ] = 12; // Forcing of "other" halocarbons
    mOutputGasNameMap[ "SO2" ] = 13; // Total SO2 forcing
    mOutputGasNameMap[ "DirSO2" ] = 14; // Direct SO2 forcing
    mOutputGasNameMap[ "TropO3" ] = 15; // TropO3 forcing, including CH4 component
    mOutputGasNameMap[ "O3_CH4" ] = 16; // TropO3 CH4 component only
    mOutputGasNameMap[ "H2O_CH4" ] = 17; // Stratospheric water vapor forcing from CH4
    mOutputGasNameMap[ "Montreal" ] = 18; // Montreal Protocol Gases forcing
    mOutputGasNameMap[ "O3_CFC" ] = 19; // Stratospheric Ozone Forcing due to CFC
    mOutputGasNameMap[ "BioBurn" ] = 20; // Biomass burning OC + BC forcing (old magicc version only)
    mOutputGasNameMap[ "FOC" ] = 21; // Fossil BC & OC (old magicc version only)
    mOutputGasNameMap[ "Land" ] = 22; // Land surface albedo forcing
    mOutputGasNameMap[ "MinNOx" ] = 23; // Mineral Dust and Nitrate forcing
    mOutputGasNameMap[ "BC" ] = 24; // Total BC forcing (land and combustion)
    mOutputGasNameMap[ "OC" ] = 25; // Total OC forcing(land and combustion)
    mOutputGasNameMap[ "EXTRA" ] = 26; // Extra forcing
    mOutputGasNameMap[ "RCP" ] = 27; // RCP radiative forcing (total - nitrate, albedo, mineral dust)
    
    overwriteMAGICCParameters( );
}

/*! \brief Overwrite MAGICC default parameters with new values.
*/
void MagiccModel::overwriteMAGICCParameters( ){
    // Override parameters in MAGICC if necessary
    int varIndex = 1;
    SETPARAMETERVALUES( varIndex, mClimateSensitivity );
    varIndex = 2;
    SETPARAMETERVALUES( varIndex, mSoilTempFeedback );
    varIndex = 3;
    SETPARAMETERVALUES( varIndex, mHumusTempFeedback );
    varIndex = 4;
    SETPARAMETERVALUES( varIndex, mGPPTempFeedback );
    varIndex = 5;
    SETPARAMETERVALUES( varIndex, mNetDeforestCarbFlux80s );
    varIndex = 6;
    SETPARAMETERVALUES( varIndex, mOceanCarbFlux80s );
    varIndex = 7;
    SETPARAMETERVALUES( varIndex, mSO2Dir1990 );
    varIndex = 8;
    SETPARAMETERVALUES( varIndex, mSO2Ind1990 );
    varIndex = 9;
    SETPARAMETERVALUES( varIndex, mBCUnitForcing );
    varIndex = 10;
    SETPARAMETERVALUES( varIndex, mOCUnitForcing );
}

//! Writes data members to debugging data stream in XML format.
void MagiccModel::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );

    // Write out mLastHistoricalYear
    XMLWriteElement( mLastHistoricalYear, "last-historical-year", out, tabs );

    // Write out mClimateSensitivity
    XMLWriteElement( mClimateSensitivity, "climateSensitivity", out, tabs );

    // Write out Soil Feedback Factor.
    XMLWriteElement( mSoilTempFeedback, "soilTempFeedback", out, tabs );

    // Write out Humus Feedback Factor.
    XMLWriteElement( mHumusTempFeedback, "humusTempFeedback", out, tabs );

    // Write out GPP Feedback Factor.
    XMLWriteElement( mGPPTempFeedback, "GPPTempFeedback", out, tabs );

    // Write out 1980s Ocean Uptake.
    XMLWriteElement( mOceanCarbFlux80s, "oceanFlux80s", out, tabs );

    // Write out 1980s net terrestrial Deforestation.
    XMLWriteElement( mNetDeforestCarbFlux80s, "deforestFlux80s", out, tabs );

    // Write out aerosol forcing parameters.
    XMLWriteElement( mSO2Dir1990, "base-so2dir-forcing", out, tabs );
    XMLWriteElement( mSO2Ind1990, "base-so2ind-forcing", out, tabs );
    XMLWriteElement( mBCUnitForcing, "bc-unit-forcing", out, tabs );
    XMLWriteElement( mOCUnitForcing, "oc-unit-forcing", out, tabs );
    
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

/*! \brief Checks that the year is a valid value for this climate model
* \details For the MAGICC model valid years range from 1765 to 2500
* \param aYear year
*/
bool MagiccModel::isValidClimateModelYear( const int aYear ) const {
    const int MIN_YEAR = 1765;
    const int MAX_YEAR = 2500;
    
    if ( aYear < MIN_YEAR || aYear > MAX_YEAR ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid year " << aYear << " for MAGICC climate model." << endl;
        return false;    
    }
    
    return true;
}

/*! \brief Set the level of emissions for a particular for a period.
* \details This function overrides the read in emissions level for a given gas
*          with a specified value. Emissions are not added but reset each time.
* \param aGasName Name of the gas for which to set emissions.
* \param aPeriod Period in which to set emissions.
* \param aEmission Amount of emissions to set.
* \return Whether the emissions were successfully added.
*/
bool MagiccModel::setEmissions( const string& aGasName, const int aPeriod, const double aEmission ){
    // Lookup the gas index.
    const int gasIndex = getInputGasIndex( aGasName );
       
    // Check for error.
    if( gasIndex == INVALID_GAS_NAME ){
        // no longer log an error when we get a gas we don't model.
        // GCAM climate models are required to ignore gas names they
        // don't understand.
        return false;
    }

    // Check that the period is valid.
    if( aPeriod < 1 || aPeriod >= scenario->getModeltime()->getmaxper() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid period " << aPeriod << " passed to MAGICC model wrapper." << endl;
        return false;
    }

    // Set the gas level. Base year values are not passed to MAGICC, so remove
    // one from the model period to determine the index.
    mModelEmissionsByGas[ gasIndex ][ aPeriod ] = aEmission;

    // Invalidate the output because an input has changed.
    mIsValid = false;

    return true;
}

/*! \brief Set the level of emissions for a particular for a year.
 * \details This function overrides the read in emissions level for a given gas
 *          with a specified value. Emissions are not added but reset each time.
 *          This is only relevant for LUC CO2 emissions.
 * \param aGasName Name of the gas for which to set emissions.
 * \param aYear Year in which to set emissions.
 * \param aEmission Amount of emissions to set.
 * \return Whether the emissions were successfully added.
 */
bool MagiccModel::setLUCEmissions( const string& aGasName, const int aYear, const double aEmission ){
	// Check for error.
    if( aGasName != "CO2NetLandUse" ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid gas name " << aGasName << " passed to the MAGICC model." << endl;
        return false;
    }
	
    // Check that the period is valid.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear < modeltime->getStartYear() || aYear > modeltime->getEndYear() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid year " << aYear << " passed to MAGICC model wrapper." << endl;
        return false;
    }
	
    // Set the gas level. Base year values are not passed to MAGICC, so remove
    // one from the model period to determine the index.
    mLUCEmissionsByYear[ aYear - modeltime->getStartYear() - 1 ] = aEmission;
	
    // Invalidate the output because an input has changed.
    mIsValid = false;
	
    return true;
}

/*!
 * \brief Get the GCAM emissions that would currently be set to be sent to MAGICC.
 * \details This method only retrieves the values that are currently would be sent and
 *          provides no indication if they are the latest emissions from GCAM nor
 *          if they were the actual emissions MAGICC ran with. This DOES NOT
 *          attempt to retrieve emissions from within MAGICC. This does not return 
 *          the read-in default emissions, only emissions generated by GCAM.
 * \param aGasName The name of the gas to get emissions for.
 * \param aYear Any valid year.  Emissions may be linearly interpolated if they
 *              are not stored in the given year.
 * \return The emissions for the given gas and year or -1 if an unknown gas or
 *         invalid year was given.
 */
double MagiccModel::getEmissions( const string& aGasName,
                                  const int aYear ) const
{
    // Check if a valid year is passed in.
    if( !isValidClimateModelYear( aYear ) ){
        return -1;
    }

    // Lookup the gas index.
    const int gasIndex = getInputGasIndex( aGasName );
    
    // Check for error.
    if( gasIndex == INVALID_GAS_NAME ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid gas name " << aGasName << " passed to the MAGICC model wrapper." << endl;
        return -1;
    }

    const Modeltime* modeltime = scenario->getModeltime();

    int currPeriod = modeltime->getyr_to_per( aYear );

    // If invalid model period is passed, return -1 to indicate no valid emission data
    if( currPeriod <= 0 ) {
        return -1;
    }
    
    // For first model period, return marker value indicating invalid emissions,
    // since GCAM does not return valid values for 1975
    if( currPeriod == 0 ) {
        return 99999999;
    }
    
    // Check if we don't need to interpolate.
    if( aYear == modeltime->getper_to_yr( currPeriod ) ){
         return mModelEmissionsByGas[ gasIndex ][ currPeriod ];
    }

    // Find the year which is the last year of the previous period.
    int x1 = modeltime->getper_to_yr( currPeriod );

    // Find the year which is the last year of the current period.
    int x2 = modeltime->getper_to_yr( currPeriod + 1 );

    // Emissions level in the last year of the current period.
    double y2 = mModelEmissionsByGas[ gasIndex ][ currPeriod ];

    // Emissions level in the last year of the previous period.
    double y1 = mModelEmissionsByGas[ gasIndex ][ currPeriod - 1 ];

    // Linearly interpolate the emissions for the current year based on the
    // emissions levels in the surrounding periods.
    return static_cast<double>( ( aYear - x1 ) * ( y2 - y1 ) ) / static_cast<double>( ( x2 - x1 ) ) + y1;
}

/*! \brief Write out the MAGICC emissions file.
 * \details This function writes emissions to the MAGICC input file.
 *          The first part of this function writes out historical data from
 *          the default emissions file. This data can be for any years, but
 *          must include the model critical year (2000). 
 *          GCAM emissions are used for years past the last historical year
 *          as specified by the user. 
 *          Emissions are interpolated in-between years without data.
 */
void MagiccModel::writeMAGICCEmissionsFile(){
    const int OUT_PRECISION = 4; // Number of decimals
    
    // Open a stringstream until are ready to print to file
    ostringstream gasFileData;
    
    // Setup the output format.
    gasFileData.setf( ios::right, ios::adjustfield );
    gasFileData.setf( ios::fixed, ios::floatfield );
    gasFileData.setf( ios::showpoint );

    int lastHistoricalData = 0; // Last historical data point written out
    int numberOfDataPoints = 0; // Number of data points written to magicc input file

    // First write out data for historical years
    for( unsigned int index = 0; index < mNumberHistoricalDataPoints; ++index ){
        int year = static_cast<int>( floor( mDefaultEmissionsByGas[ 0 ][ index ] ) );
        if ( ( year <= mLastHistoricalYear ) ) {
            gasFileData << setw( 4 ) <<  year << ",";
            lastHistoricalData = index;
            for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ){
                // Write out exogenous emissions for each gas.
                gasFileData << setw( 6 + OUT_PRECISION ) << setprecision( OUT_PRECISION ) << mDefaultEmissionsByGas[ gasNumber +1 ][ index ];
                
                // Need to deal with 1) interpolation between history and model, 
                writeComma( gasNumber, numberOfDataPoints, gasFileData );
            }
        }
        else { // If are past last historical year, exit loop, finished writting default emissions
            break;
        }
    }
    
    const Modeltime* modeltime = scenario->getModeltime();
	const int startYear = modeltime->getper_to_yr( 1 );
	const int endYear = modeltime->getEndYear();
	const int finalCalYear = modeltime->getper_to_yr( modeltime->getFinalCalibrationPeriod() );
    
    // Keep track of the next model year to use for interpolating in-between historical and model data.
    int firstModelYear = startYear; 

    // Now begin to write out model output emissions
    // We want to write model emissions to MAGICC annually to eliminate errors
    // with the LUC CO2 emissions
    for( unsigned int year = startYear; year <= endYear; year++ ) {
		int period =  modeltime->getyr_to_per( year );
        
        // If are past historical years, write out model emissions for every year past final cal year, a model year, and GAS_EMK_CRIT_YEAR 
        if ( year > mLastHistoricalYear ) { 
            if ( modeltime->isModelYear( year ) || year == GAS_EMK_CRIT_YEAR || year > finalCalYear ) {
                std::vector<int>::iterator yearIterator; // Need this several times
               
                // Write out model emissions for all the gases if past historical emissions year.
                for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ){
                     if ( gasNumber == 0 ) {
                        gasFileData << setw( 4 ) << year << ",";
                    }
                    // We are always passing GCAM LUC carbon emissions to MAGICC annually.
                    // Therefore, LUC Emissions are not interpolated between historical and GCAM values.
                    // Historical LUC emissions vary from year-to year in any event, so some jumps between historical
                    // and model data are acceptable
                    if ( sInputGasNames[ gasNumber ] == "CO2NetLandUse" ) { 
                        gasFileData << setw(  6 + OUT_PRECISION ) << setprecision( OUT_PRECISION )
                                    << mLUCEmissionsByYear[ year - modeltime->getStartYear() - 1 ];
                    }
                    // For all emissions other than LUC carbon
                    else {
                        bool needToInterpolate = !modeltime->isModelYear( year );
                        if( needToInterpolate ) {
                            int nextYear = modeltime->getper_to_yr( period );
                            int prevYear = modeltime->getper_to_yr( period - 1);
                            double nextValue = mModelEmissionsByGas[ gasNumber ][ period ];
                            double previousValue = mModelEmissionsByGas[ gasNumber ][ period - 1 ];
                            
                            // If are making the transition from historical to model data, then interpolate 
                            // between the two by providing previous year value from historical data.
                            if ( firstModelYear == nextYear ) {
                                prevYear = floor( mDefaultEmissionsByGas[ 0 ][ lastHistoricalData ] );
                                previousValue = mDefaultEmissionsByGas[ gasNumber + 1 ] [ lastHistoricalData ];
                            }
                            
                            gasFileData << setw(  6 + OUT_PRECISION ) << setprecision( OUT_PRECISION )
                             << util::linearInterpolateY( year, prevYear, nextYear, previousValue, nextValue );
                        }
                        else {
                            // Write out model emission for this gas.
                            gasFileData << setw(  6 + OUT_PRECISION ) << setprecision( OUT_PRECISION )
                                        << mModelEmissionsByGas[ gasNumber ][ period ];
                        }
                    }
                    writeComma( gasNumber, numberOfDataPoints, gasFileData );
                } // end gasnumber loop 
            } // end loop - write-out model emissions.
        } 
        else {
            // If this was a historical year, then keep track of next model period.
            firstModelYear = modeltime->getper_to_yr( period );
        }
    } // End of year loop
    
    // Add an additional data point if needed so that MAGICC has data at least out to 2100.
    if ( mLastHistoricalYear < 2100 && endYear < 2100 ) {
        int year = endYear;
        int period = modeltime->getmaxper();
        for ( unsigned int extra = 0; extra < getNumAdditionalGasPoints(); extra++ ) {
            year = year + 10;
            gasFileData << setw( 4 ) << year << ",";
            // Write out all the gases.
            for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ){
                if ( sInputGasNames[ gasNumber ] == "CO2NetLandUse" ) {
                    int index = modeltime->getEndYear() - modeltime->getStartYear() + extra - 1;
                    gasFileData << setw(  6 + OUT_PRECISION ) << setprecision( OUT_PRECISION )
                    << mLUCEmissionsByYear[ index ];
                }
                else {
                    gasFileData << setw(  6 + OUT_PRECISION ) << setprecision( OUT_PRECISION ) 
                                << mModelEmissionsByGas[ gasNumber ][ period ]; //sjsTEMP - this should be +1, but that's strange.
                }
                writeComma( gasNumber, numberOfDataPoints, gasFileData );
            }
        }
    }
    
    // Open the gas stream to write emissions into so that MAGICC can get them by
    // reading this stream.
    ostringstream gasStream;
    
    // Write out header information
    gasStream << numberOfDataPoints << endl;
	
    // line 2: Name of the scenario
    gasStream << " Scenario " << mScenarioName << endl;
    // line 3: Blank line
    gasStream << endl;
    // line 4: Gas names includes one extra space for commas
    gasStream << setw(5) << " ,"; // Year
    for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ) {
        gasStream << setw(9) << sInputGasNames[ gasNumber ] << ',';
    }
    gasStream << endl;
    // line 5: Gas units
    gasStream << setw(5) << "Year,";
    for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ) {
        gasStream << setw(9) << sInputGasUnits[ gasNumber ] << ',';
    }
    gasStream << endl;
    
    // Transfer bulk of data to output stream.
    gasStream << gasFileData.str(); 
    
    // Set the gas data into MAGICC.
    SET_GAS_EMK( gasStream.str() );
    
    // Check if the users still wants the gas data saved as a file which may be
    // useful for debugging or to use as input for a stand alone MAGICC run.
    AutoOutputFile gasFile( "climatFileName", "gas.emk" );
    string gasEMKData = gasStream.str();
    gasFile << gasEMKData;
}

/*! \brief Write comma.
 * \details Helper function that writes comma separator for emissions and adds one to line counter.
 */
void MagiccModel::writeComma( int gasNumber, int& numberOfDataPoints, ostringstream& gasFile ){
    // Write a comma as long as this is not the last gas.
    if( gasNumber != getNumInputGases() - 1 ){
        gasFile << ",";
    }
    else {
        gasFile << endl;
        numberOfDataPoints++;
    }
}
    
/*! \brief Run the MAGICC emissions model.
* \details This function will run the MAGICC model with the currently stored
*          emissions levels. It will first extrapolate future points for each
*          gas, set equal to the last period. It then prints the gases to the
*          MAGICC input file and calls MAGICC.
* \return Whether the model ran successfully.
*/
enum MagiccModel::runModelStatus MagiccModel::runModel(){
    // Add on extra periods MAGICC needs. 
    // Loop through the gases and copy forward emissions.
    for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ){
        const int finalPeriod = scenario->getModeltime()->getmaxper() - 1;
        // Fill in data for extra periods
        fill( mModelEmissionsByGas[ gasNumber ].begin() + finalPeriod,
              mModelEmissionsByGas[ gasNumber ].end(),
              mModelEmissionsByGas[ gasNumber ][ finalPeriod ] );
    }
    
    writeMAGICCEmissionsFile( );
    
    // First overwrite parameters
    overwriteMAGICCParameters( );

    // now actually run the model.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Calling the climate model..."<< endl;
    CLIMAT();
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Finished with CLIMAT()" << endl;
    mIsValid = true;
    return SUCCESS;
}

/* \brief Get the number of input gases to MAGICC.
* \return The fixed number of gases.
*/
unsigned int MagiccModel::getNumInputGases(){
    // This is a C++ trick to find the number of elements in
    // an array by dividing its size by the size of the first element.
    return sizeof( sInputGasNames ) / sizeof( sInputGasNames[ 0 ] );
}

double MagiccModel::getConcentration( const string& aGasName,
                                      const int aYear ) const
{
    /*! \pre A gas was requested. */
    assert( !aGasName.empty() );

    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }

    int year = aYear;
    int gasNumber = util::searchForValue( mOutputGasNameMap, aGasName );
    if ( gasNumber != 0 ) {
        return GETGHGCONC( gasNumber, year );
    }
    return -1;
}

double MagiccModel::getTemperature( const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }

    // Need to store the year locally so it can be passed by reference.
    int year = aYear;
    return GETGMTEMP( year );
}

double MagiccModel::getForcing( const string& aGasName, const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }

    int year = aYear;
    int gasNumber = util::searchForValue( mOutputGasNameMap, aGasName );
    if ( gasNumber != 0 ) {
        return GETFORCING( gasNumber, year );
    }
    return -1;
}

double MagiccModel::getNetTerrestrialUptake( const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }
    
    // MAGICC array is not sized to include 1990 for net terrestrial uptake
    // so just return 0.
    if( aYear < 1990 ) {
        return 0;
    }

    int year = aYear;
    int itemNumber = 10;
    return GETCARBONRESULTS( itemNumber, year );
}

double MagiccModel::getNetOceanUptake( const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }
    
    // MAGICC array is not sized to include 1990 for net ocean uptake
    // so just return 0.
    if( aYear < 1990 ) {
        return 0;
    }

    int year = aYear;
    int itemNumber = 4;
    return GETCARBONRESULTS( itemNumber, year );
}

double MagiccModel::getNetLandUseChangeEmission( const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }
    
    // MAGICC array is not sized to include 1990 for net land use change emissions
    // so just return 0.
    if( aYear < 1990 ) {
        return 0;
    }

    int itemNumber = 2;
    int year = aYear;
    return GETCARBONRESULTS( itemNumber, year );
}

double MagiccModel::getTotalForcing( const int aYear ) const {
    // Check if the climate model has been run and that valid year is passed in.
    if( !mIsValid || !isValidClimateModelYear( aYear ) ){
        return -1;
    }

    // Need to store the year and gas number locally so it can be passed by reference.
    int year = aYear;
    int gasNumber = 0; // global forcing
    return GETFORCING( gasNumber, year );
}

/*! \brief Updates a visitor with information from the the climate model.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to perform the update, -1 means all periods.
* \todo Add a function to the magicc library to get the temperature.
*/
void MagiccModel::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitClimateModel( this, aPeriod );
    aVisitor->endVisitClimateModel( this, aPeriod );
}

/*! \brief Return the name of the XML node for this object.
* \return The XML element name for the object.
*/
const string& MagiccModel::getXMLNameStatic(){
    const static string XML_NAME = "MagiccModel";
    return XML_NAME;
}

/*! \brief Return the index of a gas name within the internal matrix or
*          INVALID_GAS_NAME if the gas name is not found.
* \param aGasName Name of the gas to search for.
* \return The index of the gas or INVALID_GAS_NAME if it is not found.
*/
int MagiccModel::getInputGasIndex( const string& aGasName ) const {
    // Search the gas name vector.
    for( unsigned int i = 0; i < getNumInputGases(); ++i ){
        if( sInputGasNames[ i ] == aGasName ){
            return i;
        }
    }
    // The gas does not exist.
    return INVALID_GAS_NAME;
}

/*! \brief Read initial emissions data from a file.
* \details This function reads in the initial values for each gas from an input
*          file specified by the configuration file.
* \warning input file data lines must end in a ","
* \warning Input data, in order to be used, must contain the year 2000 (GAS_EMK_CRIT_YEAR)
*/
void MagiccModel::readFile(){
    // Open up the file with default emissions data and read it in.
    // Default to value given in config file, but use read-in value if that exists
    string gasFileName = Configuration::getInstance()->getFile( "GHGInputFileName" );
    if ( mGHGInputFileName != "" ) {
        gasFileName = mGHGInputFileName;
    }
    ifstream inputGasFile;
    inputGasFile.open( gasFileName.c_str(), ios::in ); // open input file for reading
    util::checkIsOpen( inputGasFile, gasFileName );

    // Read in all other gases from fossil fuels. 
    
    // The first line will tell us how many
    // lines to parse.  We are assuming there are a maximum of getNumAdditionalGasPoints()
    // years beyond the end of the model years.
    int numInputGasPoints;
    inputGasFile >> numInputGasPoints;  //reads the # of data points in the first line

    // Resize to the number of gases plus 1 to accomidate year
    mDefaultEmissionsByGas.resize( getNumInputGases() + 1 );
    
    // Resize all the vectors to the number of data points in input file + potential extras.
    for( unsigned int i = 0; i < mDefaultEmissionsByGas.size(); ++i ){
        mDefaultEmissionsByGas[ i ].resize( numInputGasPoints + getNumAdditionalGasPoints() );
    }
    mDefaultEmissionsYears.resize ( numInputGasPoints + getNumAdditionalGasPoints() );
    
    // Check to see if read-in data has critical year
    bool hasCriticalYear = false;
    
    
    // Ignore initial header lines of file.
    const int SKIP_LINES = 5;
    for ( unsigned int i = 0; i < SKIP_LINES; ++i ){
        inputGasFile.ignore( 200, '\n' );
    }
    
    int gasYearIndex = -1;
    // Now read in all the gases. 
    for ( int gasInputIndex = 0; gasInputIndex <= numInputGasPoints; ++gasInputIndex ) {
        // Parse the year as a double so that the comma after the year does
        // not cause parsing to fail.
        double year = 0;
        inputGasFile >> year;
        inputGasFile.ignore( 80, ',' ); // skip comma
        int yearAsInteger = static_cast<int>( floor( year ) );
        
        // A potentially valid data year.
        if ( yearAsInteger >= 1990 ) {
            gasYearIndex++;
            mDefaultEmissionsByGas[ 0 ][ gasYearIndex ] = year;
            mDefaultEmissionsYears[ gasYearIndex ] = year;
            
            // Count number of valid data points. 
            // This way 1) can deal with any pre 1990 data by ignoring it,
            // and does not rely on read-in file being correct about number of points.
            mNumberHistoricalDataPoints++;
        }
        
        if ( yearAsInteger == GAS_EMK_CRIT_YEAR ) {
            hasCriticalYear = true;
        }
        
        // Loop through all the gases.
        for( unsigned int gasNumber = 0; gasNumber < getNumInputGases(); ++gasNumber ){
            double value;
            inputGasFile >> value;
            // Only set the parsed value if we have a valid year, no warnings are necessary
            // as they should have already been given.
            if( gasYearIndex != -1 && yearAsInteger >= 1990 ) {
                mDefaultEmissionsByGas[ gasNumber  + 1 ][ gasYearIndex ] = value;
            }
            inputGasFile.ignore( 80, ','); // skip comma
        }

        inputGasFile.ignore( 80, '\n' ); // next line
    }
    inputGasFile.close();
    
    // If critical year data is not read-in, then don't use exogenous emissions at all.
    if ( !hasCriticalYear ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Data For Critical Year " << GAS_EMK_CRIT_YEAR << " not found in input emissions file. Model data will be used instead. " << endl;
        mNumberHistoricalDataPoints = 0;
    }
}

/*! \brief Return the name of the XML node for this object.
* \return The XML element name for the object.
*/
const string& MagiccModel::getnetDefor80sName(){
    const static string NET_DEF_80s_NAME = "netDef80s";
    return NET_DEF_80s_NAME;
}

/*!
 * \brief Get the maximum number of data points that we are expecting beyond the
 *        last model year.
 * \return The constant number of additional data points that will be read-in and
 *         written-out for MAGICC input files.
 */
int MagiccModel::getNumAdditionalGasPoints() {
    static const int ADDITIONAL_POINTS = 1;
    return ADDITIONAL_POINTS;
}
