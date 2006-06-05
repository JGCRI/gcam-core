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
#include "containers/include/iscenario_runner.h"
#include "containers/include/scenario_runner_factory.h"
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
    bool success = XMLHelper<void>::parseXML( loggerFileName, loggerFactoryWrapper.get() );
    
    // Check if parsing succeeded. Non-zero return codes from main indicate
    // failure.
    if( !success ){
        return 1;
    }

    // Create an auto_ptr to the scenario runner. This will automatically
    // deallocate memory.
    auto_ptr<IScenarioRunner> runner;
    // Get the main log file.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    
    // Parse configuration file.
    mainLog << "Parsing input files..." << endl;
    Configuration* conf = Configuration::getInstance();
    success = XMLHelper<void>::parseXML( configurationFileName, conf );
    // Check if parsing succeeded. Non-zero return codes from main indicate
    // failure.
    if( !success ){
        return 1;
    }

    // Determine the correct type of ScenarioRunner to create. Note that this
    // ordering must be preserved because certain scenario runners can contain
    // other scenario runners.
    if( conf->getBool( "BatchMode" ) ){
        runner = ScenarioRunnerFactory::create( "batch-runner" );
    }
    else if( conf->getBool( "find-path" ) ){
        runner = ScenarioRunnerFactory::create( "policy-target-runner" );
    }
    else if( conf->getBool( "simple-find-path" ) ){
        runner = ScenarioRunnerFactory::create( "simple-policy-target-runner" );
    }
    else if( conf->getBool( "mergeFilesOnly" ) ) {
        runner = ScenarioRunnerFactory::create( "merge-runner" );
    }
    else if( conf->getBool( "createCostCurve" ) ){
        runner = ScenarioRunnerFactory::create( "mac-generator-scenario-runner" );
    }
    else { // Run a standard scenario.
        runner = ScenarioRunnerFactory::create( "single-scenario-runner" );
    }
    
    // Need to set the scenario pointer. This has to be done before XML parse is
    // called because that requires the modeltime. TODO: Remove the global
    // pointer!
    // TODO: This may fail set the global scenario pointer for the batchrunner.
    // The batch runner adjusts the pointer later.
    scenario = runner->getInternalScenario();
    
    // Setup the scenario.
    success = runner->setupScenario( timer );
    // Check if setting up the scenario, which often includes parsing,
    // succeeded.
    if( !success ){
        return 1;
    }

    // Run the scenario.
    success = runner->runScenario( Scenario::RUN_ALL_PERIODS, timer );

    // Print the output.
    runner->printOutput( timer );
    mainLog.setLevel( ILogger::WARNING ); // Increase level so that user will know that model is done
    mainLog << "Model exiting successfully." << endl;
    // Cleanup Xerces. This should be encapsulated with an initializer object to ensure against leakage.
    XMLHelper<void>::cleanupParser();
    
    // Return exit code based on whether the model succeeded(Non-zero is failure by convention).
    return success ? 0 : 1; 
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
