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
#include <ctime>
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
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/base/include/timer.h"

#if( BUILD_TESTS )
// Unit test prototype
int runTests();
#endif

using namespace std;
using namespace xercesc;

// define file (ofstream) objects for outputs, debugging and logs
/* \todo Finish removing globals-JPL */
ofstream bugoutfile, outFile, logfile;	

Scenario* scenario = 0; // model scenario info
time_t ltime;
XercesDOMParser* XMLHelper<void>::parser = 0;
ErrorHandler* XMLHelper<void>::errHandler = 0;

void parseArgs( unsigned int argc, char* argv[], string& confArg, string& logFacArg );

//! Main program. 
int main( int argc, char *argv[] ) {

    // Use a smart pointer for configuration so that if the main is exited before the end the memory is freed.
    auto_ptr<Configuration> confPtr( Configuration::getInstance() );
    Configuration* conf = confPtr.get();
    string configurationArg = "configuration.xml";
    string loggerFactoryArg = "logger_factory.xml";
    
    // Parse any command line arguments. 
    parseArgs( argc, argv, configurationArg, loggerFactoryArg );
    
    // Initialize the XML Parser.
    XercesDOMParser* parser = XMLHelper<void>::getParser();
    DOMNode* root = 0;
    
    // Add OS dependent prefixes to the arguments.
    const string configurationFileName = string( __ROOT_PREFIX__ ) + configurationArg;
    const string loggerFileName = string( __ROOT_PREFIX__ ) + loggerFactoryArg;

    // Initialize the timer.
    Timer timer;
    timer.start();
    
    // Get time and date before model run
    time(&ltime); 

    // Initialize the LoggerFactory
    root = XMLHelper<void>::parseXML( loggerFileName, parser );
    LoggerFactory::XMLParse( root );

    // Parse configuration file.
    root = XMLHelper<void>::parseXML( configurationFileName, parser );
    conf->XMLParse( root );

    // Open various files.
    const string logFileName = conf->getFile( "logOutFileName" );
    logfile.open( logFileName.c_str(), ios::out );
    util::checkIsOpen( logfile, logFileName  );

    const string bugOutFileName = conf->getFile( "bugOutFileName" );
    bugoutfile.open( bugOutFileName.c_str(), ios::out );
    util::checkIsOpen( bugoutfile, bugOutFileName );
    
    // Parse the input file.
    root = XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), parser );

    // Use a smart pointer for scenario so that if the main program exits before the end the memory is freed correctly. 
    auto_ptr<Scenario> scenarioPtr( new Scenario() );
    scenario = scenarioPtr.get();

    scenario->XMLParse( root );
    
    // Fetch the listing of Scenario Components.
    const list<string> scenComponents = conf->getScenarioComponents();
    
    // Iterate over the vector.
    typedef list<string>::const_iterator ScenCompIter;
    for( ScenCompIter currComp = scenComponents.begin(); currComp != scenComponents.end(); ++currComp ) {
        cout << "Parsing " << *currComp << " scenario component." << endl;
        root = XMLHelper<void>::parseXML( *currComp, parser );
        scenario->XMLParse( root );
    }

    cout << "XML parsing complete." << endl;
    logfile << "XML parsing complete." << endl;

    // Cleanup Xerces.
    XMLHelper<void>::cleanupParser();

    // Finish initialization.
    scenario->completeInit();

    // Print data read in time.
    timer.save();
    timer.print( cout, "XML Readin Time:" );
    timer.print( logfile, "XML Readin Time:" );

    // Create a scenario runner to handle running the scenario.
    // and possibly creating a cost curve. 
    ScenarioRunner* scenRunner = new MACGeneratorScenarioRunner( scenario );
    
    // Run the initial scenario.
    scenRunner->runScenario( timer );
    
    delete scenRunner;
    
    cout << endl << "Date & Time: " << ctime( &ltime ) << endl;
    logfile << endl << "Date & Time: " << ctime( &ltime ) << endl;

    //******** Close All Text Files
    bugoutfile.close();
    logfile.close();

    LoggerFactory::cleanUp();

    return 0;
}

//! Function to parse the arguments.
void parseArgs( unsigned int argc, char* argv[], string& confArg, string& logFacArg ) {
    for( unsigned int i = 1; i < argc; i++ ){
        string temp( argv[ i ] );
        if( temp.compare(0,2,"-C" ) == 0 ){
            confArg = temp.substr( 2, temp.length() );
            cout << "Configuration file name set to: " << confArg << endl;
        } 
        else if( temp.compare(0,2,"-L" ) == 0 ){
            logFacArg = temp.substr( 2, temp.length() );
            cout << "Logger Configuration file name set to: " << logFacArg << endl;
        }
        else {
            cout << "Invalid argument: " << temp << endl;
            cout << "Usage: " << argv[ 0 ] << " [-CconfigurationFileName ][ -LloggerFactoryFileName ]";
        }
    }
}
