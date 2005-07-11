/*! 
* \file magicc_model.cpp
* \ingroup Objects
* \brief This file contains the source for the MagiccModel class.
* \author Sonny Kim, Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include "util/base/include/definitions.h"
#include <iomanip>
#include <fstream>
#include <string>

#include "climate/include/magicc_model.h"
#include "util/base/include/model_time.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/util.h"

using namespace std;

#if(__HAVE_FORTRAN__)
extern "C" { void _stdcall CLIMAT(void); };
extern "C" { double _stdcall CO2CONC(int&); };
#endif

// Setup the gas name vector.
const string MagiccModel::sGasNames[] = { "CO2",
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
									   "SF6" };

/*! \brief Constructor
* \param aModeltime Pointer to the global modeltime object.
*/
MagiccModel::MagiccModel( const Modeltime* aModeltime ):
mModeltime( aModeltime )
{
}

/*! \brief Complete the initialization of the MagiccModel.
* \details This function first resizes the internal vectors which store
*          emissions by gas and period. It then reads in the default set of data
*          for emissions by gas from an input file. These emissions can be
*          overridden by calls to setEmission.
* \param aScenarioName Name of the current model scenario.
*/
void MagiccModel::completeInit( const string& aScenarioName ){
    mScenarioName = aScenarioName;
    // Resize to the number of gases based on the gas name vector.
    mEmissionsByGas.resize( getNumGases() );

    // Resize all the vectors to the number of datapoints for MAGICC.
    // Add comment here.
    const int numGasPoints = mModeltime->getmaxper() + 2;
    for( unsigned int i = 0; i < mEmissionsByGas.size(); ++i ){
        mEmissionsByGas[ i ].resize( numGasPoints );
    }
    // Read in the data from the file. These values may later be overridden.
    readFile();
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
    const int gasIndex = getGasIndex( aGasName );
    
    // Check for error.
    if( gasIndex == INVALID_GAS_NAME ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid gas name " << aGasName << " passed to the MAGICC model wrapper." << endl;
        return false;
    }

    // Check that the period is valid.
    if( aPeriod < 1 || aPeriod >= mModeltime->getmaxper() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid period " << aPeriod << " passed to MAGICC model wrapper." << endl;
        return false;
    }

    // Set the gas level. Base year values are not passed to MAGICC, so remove
    // one from the model period to determine the index.
    mEmissionsByGas[ gasIndex ][ aPeriod - 1 ] = aEmission;
    return true;
}

/*! \brief Run the MAGICC emissions model.
* \details This function will run the MAGICC model with the currently stored
*          emissions levels. It will first extrapolate future points for each
*          gas, set equal to the last period. It then prints the gases to the
*          MAGICC input file and calls MAGICC.
* \return Whether the model ran successfully.
* \todo Fix up the calculation of years past the end of the model.
*/
bool MagiccModel::runModel(){
    // Add on extra periods MAGICC needs. 
    for ( unsigned int period = mModeltime->getmaxper() - 1; 
        period < static_cast<unsigned int>( mModeltime->getmaxper() + 1 ); ++period ){
        // Loop through the gases and copy forward emissions.
        for( unsigned int gasNumber = 0; gasNumber < getNumGases(); ++gasNumber ){
            mEmissionsByGas[ gasNumber ][ period ] = mEmissionsByGas[ gasNumber ][ period - 1 ];
        }
    }
    
    // Open the output file.
    ofstream gasFile;
    gasFile.open( Configuration::getInstance()->getFile( "climatFileName" ).c_str(), ios::out );

    // Calculate the number of points which will be passed to magicc.
    const int FUTURE_POINTS = 2;
    const int numPoints = mModeltime->getmaxper() -1 + FUTURE_POINTS;
    gasFile << numPoints << endl << " Scenario " << mScenarioName << endl << endl << endl << endl;
    
    // Setup the output.
    gasFile.setf( ios::right, ios::adjustfield );
    gasFile.setf( ios::fixed, ios::floatfield );
    gasFile.setf( ios::showpoint );
    
    // Write out all the gases.
    for( unsigned int period = 0; period < static_cast<unsigned int>( mModeltime->getmaxper() + 1 ); ++period ) {
        // Write out the year.
        int year;
        if( period < static_cast<unsigned int>( mModeltime->getmaxper() - 1 ) ){
			year = mModeltime->getStartYear() + ( period + 1 ) * mModeltime->gettimestep( period );
        }
        // Set the year differently for the two periods past the end of the
        // model. The year should equal the last model year plus the timestep
        // plus 140 times 1 for the first point and times 2 for the second
        // point.
        else {
            int lastPeriod = mModeltime->getmaxper() - 2;
			int lastYear = mModeltime->getStartYear() + ( lastPeriod + 2 ) * mModeltime->gettimestep( lastPeriod );
            year = lastYear + 140 * ( period - lastPeriod ) - 100;
        }

        gasFile << setw( 4 ) << year << ",";
        // Write out all the gases.
        for( unsigned int gasNumber = 0; gasNumber < mEmissionsByGas.size(); ++gasNumber ){
            gasFile << setw( 7 ) << setprecision( 2 ) << mEmissionsByGas[ gasNumber ][ period ];
            // Write a comma as long as this is not the last gas.
            if( gasNumber != mEmissionsByGas.size() - 1 ){
                gasFile << ",";
            }
            else {
                gasFile << endl;
            }
        }
    }
    gasFile.close();
    // now actually run the model.
#if(__HAVE_FORTRAN__)
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Calling the climate model..."<< endl;
    CLIMAT();
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Finished with CLIMAT()" << endl;
#endif
    return true;
}

/* \brief Get the number of input gases to MAGICC.
* \return The fixed number of gases.
*/
unsigned int MagiccModel::getNumGases(){
	// This is a C++ trick to find the number of elements in
	// an array by dividing its size by the size of the first element.
	return sizeof( sGasNames ) / sizeof( sGasNames[ 0 ] );
}

/*! \brief Write out data from the emissions model to the main csv output file.*/
void MagiccModel::printFileOutput() const {
#if( __HAVE_FORTRAN__ )
    // Fill up a vector of concentrations.
    vector<double> data( mModeltime->getmaxper() );
    for( int period = 0; period < mModeltime->getmaxper(); ++period ){
        // Need to store the year locally so it can be passed by reference.
        int year = mModeltime->getper_to_yr( period );
        data[ period ] = CO2CONC( year );
    }
    // Write out the data to the text output function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    fileoutput3( "global"," "," "," ","Concentration","PPM", data );
#endif
}

/*! \brief Write out data from the emissions model to the database. */
void MagiccModel::printDBOutput() const {
#if( __HAVE_FORTRAN__ )
    // Fill up a vector of concentrations.
    vector<double> data( mModeltime->getmaxper() );
    for( int period = 0; period < mModeltime->getmaxper(); ++period ){
        // Need to store the year locally so it can be passed by reference.
        int year = mModeltime->getper_to_yr( period );
        data[ period ] = CO2CONC( year );
    }
    // Write out the data to the database. Database function definition.
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    dboutput4( "global", "General", "Concentrations", "Period", "PPM", data );
#endif
}

/*! \brief Return the index of a gas name within the internal matrix or
*          INVALID_GAS_NAME if the gas name is not found.
* \param aGasName Name of the gas to search for.
* \return The index of the gas or INVALID_GAS_NAME if it is not found.
*/
int MagiccModel::getGasIndex( const string& aGasName ) const {
    // Search the gas name vector.
    for( unsigned int i = 0; i < getNumGases(); ++i ){
        if( sGasNames[ i ] == aGasName ){
            return i;
        }
    }
    // The gas does not exist.
    return INVALID_GAS_NAME;
}

/*! \brief Read initial emissions data from a file.
* \details This function reads in the initial values for each gas from an input
*          file specified by the configuration file.
*/
void MagiccModel::readFile(){
    // Open up the file with all the gas data and read it in.
    const string gasFileName = Configuration::getInstance()->getFile( "GHGInputFileName" );
    ifstream inputGasFile;
    inputGasFile.open( gasFileName.c_str(), ios::in ); // open input file for reading
    util::checkIsOpen( inputGasFile, gasFileName );

    // read in all other gases from fossil fuels. Ignore initial header lines of
    // file.
    const int SKIP_LINES = 5;
    for ( unsigned int i = 0; i < SKIP_LINES; ++i ){
        inputGasFile.ignore( 80, '\n' );
    }
    
    // Now read in all the gases. 
    for ( unsigned int period = 0; period < static_cast<unsigned int>( mModeltime->getmaxper() - 1 ); ++period ) {
        inputGasFile.ignore( 80, ',' ); // skip year column
        // Loop through all the gases.
        for( unsigned int gasNumber = 0; gasNumber < getNumGases(); ++gasNumber ){
            inputGasFile >> mEmissionsByGas[ gasNumber ][ period ];
            inputGasFile.ignore( 80, ','); // skip comma
        }
        inputGasFile.ignore( 80, '\n' ); // next line
    }
    inputGasFile.close();
}
