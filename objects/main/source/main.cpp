/*!
* \file main.cpp															 
* \brief This is the Main program file which controls the initialization,
*  model looping over time steps, and outputs results  for the model.
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

Scenario* scenario = 0; // model scenario info
auto_ptr<ErrorHandler> XMLHelper<void>::mErrHandler;
auto_ptr<XercesDOMParser> XMLHelper<void>::mParser;
void parseArgs( unsigned int argc, char* argv[], string& confArg, string& logFacArg );

//! Main program. 
int main( int argc, char *argv[] ) {

    // Use a smart pointer for configuration so that if the main is exited before the end the memory is freed.
    string configurationArg = "configuration.xml";
    string loggerFactoryArg = "log_conf.xml";

    // Parse any command line arguments. 
    parseArgs( argc, argv, configurationArg, loggerFactoryArg );
    cout << "after parseArgs " << endl;

    char* p = new char[500]; // sjsTEMP
    getcwd(p, 499);
    cout << "cwd in ObjECTS MAIN: " << p << endl;
    
    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = string( __ROOT_PREFIX__ ) + configurationArg;
    const string loggerFileName = string( __ROOT_PREFIX__ ) + loggerFactoryArg;

    // Initialize the timer.
    Timer timer;
    timer.start();

    // Initialize the LoggerFactory
    auto_ptr<LoggerFactoryWrapper> loggerFactoryWrapper( new LoggerFactoryWrapper() );
    XMLHelper<void>::parseXML( loggerFileName, loggerFactoryWrapper.get() );
    cout << "after loggerFactoryWrapper " << endl;

    // Create an auto_ptr to the scenario runner. This will automatically deallocate memory.
    auto_ptr<ScenarioRunner> runner;
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    
    // Parse configuration file.
    mainLog << "Parsing input files..." << endl;
    auto_ptr<Configuration> conf( Configuration::getInstance() );
    XMLHelper<void>::parseXML( configurationFileName, conf.get() );
    cout << "after parseXML( configurationFileName " << endl;
    
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
    // sleep(10000000); // Un-comment to use OSX memory analysis tool. sjs
    
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
