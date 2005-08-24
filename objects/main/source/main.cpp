/*!
* \file main.cpp
* \todo Update this documentation.
* \brief This is the Main program file which controls initialization of run 
* parameters and sets up scenarios to run from input file names in the 
* Configuration file.
*
* The program reads in parameters and file names stored in the 
* configuration file and assigns them to the configuration object.  It creates 
* a scenario object and triggers a read-in of all input data by calling XMLParse.
* If there are scenario components (ScenComponents), then these are read next.
* 
* A switch is checked for the program to run in a mode that just merges files
* into one xml input file rather than running the model (mergeFilesOnly).
* 
* If the model is to be run, it is triggered by the ScenarioRunner class object, which 
* calls runScenario() to trigger running of the complete model for all periods.
*
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

// include standard libraries
#include <iostream>
#include <fstream>
#include <string>
#include <memory>
#include <list>

// xerces xml headers
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "util/base/include/xml_helper.h"

// include custom headers
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "containers/include/scenario_runner.h"
#include "containers/include/mac_generator_scenario_runner.h"
#include "containers/include/single_scenario_runner.h"
#include "containers/include/merge_runner.h"
#include "containers/include/batch_runner.h"
#include "util/logger/include/ilogger.h"
#include "util/logger/include/logger_factory.h"
#include "util/base/include/timer.h"

using namespace std;
using namespace xercesc;

// define file (ofstream) objects for outputs, debugging and logs
/* \todo Finish removing globals-JPL */
ofstream outFile;

// Initialize time and set some pointers to null.
// Declared outside Main to make global.
Scenario* scenario; // model scenario info

void parseArgs( unsigned int argc, char* argv[], string& confArg, string& logFacArg );

//! Main program. 
int main( int argc, char *argv[] ) {

	// identify default file names for control input and logging controls
    string configurationArg = "configuration.xml";
    string loggerFactoryArg = "log_conf.xml";
    // Parse any command line arguments.  Can override defaults with command lone args
    parseArgs( argc, argv, configurationArg, loggerFactoryArg );

    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = configurationArg;
    const string loggerFileName = loggerFactoryArg;

    // Initialize the timer.  Create an object of the Timer class.
    Timer timer;
    timer.start();

    // Initialize the LoggerFactory
    auto_ptr<LoggerFactoryWrapper> loggerFactoryWrapper( new LoggerFactoryWrapper() );
    XMLHelper<void>::parseXML( loggerFileName, loggerFactoryWrapper.get() );
    
    // Create an auto_ptr to the scenario runner. This will automatically deallocate memory.
    auto_ptr<ScenarioRunner> runner;
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    
    // Parse configuration file.
    mainLog << "Parsing input files..." << endl;
    Configuration* conf = Configuration::getInstance();
    XMLHelper<void>::parseXML( configurationFileName, conf );
    
    // Determine the correct type of ScenarioRunner to create.
    // todo: Consider a factory method.
    if( conf->getBool( "BatchMode" ) ){
        // Create the batch runner.
        const string batchFile = conf->getFile( "BatchFileName" );
        runner.reset( new BatchRunner( batchFile ) );
    }
    else if( conf->getBool( "mergeFilesOnly" ) ) {
        runner.reset( new MergeRunner() );
    }
    else if( conf->getBool( "createCostCurve" ) ){
        const string abatedGas = conf->getString( "AbatedGasForCostCurves", "CO2" );
        const unsigned int numPoints = conf->getInt( "numPointsForCO2CostCurve", 5 );
        runner.reset( new MACGeneratorScenarioRunner( abatedGas, numPoints ) );
    }
    else { // Run a standard scenario.
        runner.reset( new SingleScenarioRunner() );
    }
    // Setup the scenario.
    runner->setupScenario( timer );

    // Run the scenario.
    bool success = runner->runScenario( timer );

    // Print the output.
    runner->printOutput( timer );
    mainLog << "Model exiting successfully." << endl;
    // Cleanup Xerces. This should be encapsulated with an initializer object to ensure against leakage.
    XMLHelper<void>::cleanupParser();
	
    // Return exit code based on whether the model succeeded(Non-zero is failure by convention).
    return 0 ? 1 : success; 
}

//! Function to parse the arguments.
void parseArgs( unsigned int argc, char* argv[], string& confArg, string& logFacArg ) {
    for( unsigned int i = 1; i < argc; i++ ){
        string temp( argv[ i ] );
        if( temp.compare(0,2,"-C" ) == 0 ){
            confArg = temp.substr( 2, temp.length() );
        } 
        else if( temp.compare(0,2,"-L" ) == 0 ){
            logFacArg = temp.substr( 2, temp.length() );
        }
        else {
            cout << "Invalid argument: " << temp << endl;
            cout << "Usage: " << argv[ 0 ] << " [-CconfigurationFileName ][ -LloggerFactoryFileName ]";
        }
    }
}
