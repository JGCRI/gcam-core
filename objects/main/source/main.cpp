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
ofstream bugoutfile, outfile, logfile;	

Scenario* scenario = 0; // model scenario info
time_t ltime;
XercesDOMParser* XMLHelper<void>::parser = 0;
ErrorHandler* XMLHelper<void>::errHandler = 0;

//******* Start of Main Program ********
int main( int argc, char *argv[] ) {

    // Use a smart pointer for configuration so that if the main is exited before the end the memory is freed.
    auto_ptr<Configuration> confPtr( Configuration::getInstance() );
    Configuration* conf = confPtr.get();
    string configurationArg = "configuration.xml";
    string loggerFactoryArg = "logger_factory.xml";

    // Parse the arguments 
    for( int i = 1; i < argc; i++ ){
        string temp( argv[ i ] );
        if( temp.compare(0,2,"-C" ) == 0 ){
            configurationArg = temp.substr( 2, temp.length() );
            cout << "Configuration file name set to: " << configurationArg << endl;
            } 
        else if( temp.compare(0,2,"-L" ) == 0 ){
            loggerFactoryArg = temp.substr( 2, temp.length() );
            cout << "Logger Configuration file name set to: " << loggerFactoryArg << endl;
            }
        else {
            cout << "Invalid argument: " << temp << endl;
            cout << "Usage: " << argv[ 0 ] << " [-CconfigurationFileName ][ -LloggerFactoryFileName ]";
            return 1;
            }
        }

    // Completed initializing the arguments.
    XercesDOMParser* parser = XMLHelper<void>::getParser();
    DOMNode* root = 0;

    const string configurationFileName = string( __ROOT_PREFIX__ ) + configurationArg;
    
    // Initialize the timer.
    Timer timer;
    timer.start();
    
    // Get time and date before model run
    time(&ltime); 

    // Initialize the LoggerFactory
    const string loggerFileName = string( __ROOT_PREFIX__ ) + loggerFactoryArg;
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
    
    // Pare the input file.
    root = XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), parser );

    // Use a smart pointer for scenario so that if the main program exits before the end the memory is freed correctly. 
    auto_ptr<Scenario> scenarioPtr( new Scenario() );
    scenario = scenarioPtr.get();

    scenario->XMLParse( root );
    
    /*! \todo Improve how reading additional files is done. Current idea would be to have a seperate section
    * in the configuration file for scenario add on files. There would be a method to get all of them as a list
    * which could be iterated over. Each element would also have a "use" tag which could be set to 0 to turn off
    * reading in the file. This would replace all other switches. -JPL */
    if( conf->getBool( "runningNonReference" ) ) {
        const int numAddFiles = conf->getInt( "NumberOfScenarioAddOnFiles" );
        cout << "Number of additional scenario files: " << numAddFiles << endl;
        for( int fileNum = 1; fileNum <= numAddFiles; fileNum++ ) {
            // Now read in scenario specific information.
            cout << "Reading in additional scenario file number: " << fileNum << "." << endl;
            stringstream fileNameStream;
            fileNameStream << "scenarioXmlInputFileName" << fileNum;
            string xmlFileName;
            fileNameStream >> xmlFileName;
            string addOnFileName = conf->getFile( xmlFileName );

            if( addOnFileName != "" ) {
                    root = XMLHelper<void>::parseXML( addOnFileName, parser );
                    scenario->XMLParse( root );
                }
            }  
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
