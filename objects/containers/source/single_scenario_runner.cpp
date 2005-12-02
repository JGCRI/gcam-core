/*! 
* \file single_scenario_runner.cpp
* \ingroup SingleScenarioRunner
* \brief SingleScenarioRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include "containers/include/single_scenario_runner.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/timer.h"
#include "sectors/include/ag_sector.h"
#include "util/base/include/configuration.h"
#include "util/base/include/auto_file.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

// Function Prototypes. These need a helper class. 
extern void createMCvarid();
extern void closeDB();
extern void openDB();
extern void createDBout();

/*! \brief Constructor */
SingleScenarioRunner::SingleScenarioRunner():mScenario( new Scenario ){
}

//! Destructor.
SingleScenarioRunner::~SingleScenarioRunner(){
}

/*! \brief Setup the Scenario to be run.
* \details This function opens the various output files, reads in the base input
*          file and the list of scenario components from the configuration file
*          and passed in, and sets the name of the Scenario.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \param aName The name to add on to the name read in in the Configuration file.
* \param aScenComponents A list of additional scenario components to read in.
* \return Whether the setup completed successfully.
*/
bool SingleScenarioRunner::setupScenario( Timer& timer, const string aName, const list<string> aScenComponents ){
    // Parse the input file.
    const Configuration* conf = Configuration::getInstance();
    XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ), mScenario.get() );

    // Fetch the listing of Scenario Components.
    list<string> scenComponents = conf->getScenarioComponents();

    // Add on any scenario components that were passed in.
    for( list<string>::const_iterator curr = aScenComponents.begin();
		curr != aScenComponents.end(); ++curr )
	{
        scenComponents.push_back( *curr );
    }
    
    // Iterate over the vector.
    typedef list<string>::const_iterator ScenCompIter;
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    for( ScenCompIter currComp = scenComponents.begin();
		 currComp != scenComponents.end(); ++currComp )
	{
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Parsing " << *currComp << " scenario component." << endl;
        XMLHelper<void>::parseXML( *currComp, mScenario.get() );
    }
    
    // Override scenario name from data file with that from configuration file
    const string overrideName = conf->getString( "scenarioName" ) + aName;
    if ( overrideName != "" ) {
        mScenario->setName( overrideName );
    }

    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "XML parsing complete." << endl;

    // Print data read in time.
    timer.stop();
    mainLog.setLevel( ILogger::DEBUG );
    timer.print( mainLog, "XML Readin Time:" );
    return true;
}

/*! \brief Run a single Scenario.
* \details This function completes the initialization and runs the Scenario.
* \param aSinglePeriod The single period to run or Scenario::RUN_ALL_PERIODS
*        to run all periods.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
*/
bool SingleScenarioRunner::runScenario( const int aSingleScenario, Timer& aTimer ){
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Starting a model run." << endl;
    
	bool success = false;
	if( mScenario.get() ){
		// Finish initialization.
		mScenario->completeInit();

		// Perform the initial run of the scenario.
        success = mScenario->run( aSingleScenario, true );

		// Compute model run time.
		aTimer.stop();
		mainLog.setLevel( ILogger::DEBUG );
		aTimer.print( mainLog, "Data Readin & Initial Model Run Time:" );
	}
	else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No scenario container was parsed from the input files. Aborting scenario run!" << endl;
	}

    // Return whether the scenario ran correctly. 
    return success;
}

/*! \brief Function to perform both file and database output. 
* \details This function write out the XML, file and database output. All file
*          names are defined by the configuration file. All file handles are
*          closed when the function completes.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \param aCloseDB Whether to close the database and output variable IDs.
*        Defaults to true.
*/
void SingleScenarioRunner::printOutput( Timer& aTimer, const bool aCloseDB ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Printing output" << endl;

    // Print output xml file.
    AutoOutputFile xmlOut( "xmlOutputFileName", "output.xml" );
    Tabs tabs;
    mScenario->toInputXML( *xmlOut, &tabs );

    // Write out the ag sector data.
    const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "agSectorActive" ) ){
        AgSector::internalOutput();
    }
    
    // Write csv file output
    mScenario->writeOutputFiles();

    // Perform the database output. 
	// Open MS Access database
    openDB();
	// create main database output table before calling output routines
    createDBout();
    mScenario->dbOutput();

    if( aCloseDB ){
        createMCvarid(); // create MC variable id's     
        // close MS Access database
        closeDB();
    }

     // Print the timestamps.
    aTimer.stop();
    mainLog.setLevel( ILogger::DEBUG );
    aTimer.print( mainLog, "Data Readin, Model Run & Write Time:" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Model run completed." << endl;
}

/*! \brief Get the internal scenario of the single scenario runner.
* \return The internal scenario.
*/
Scenario* SingleScenarioRunner::getInternalScenario(){
	return mScenario.get();
}

/*! \brief Get the internal scenario of the single scenario runner.
* \return Constant pointer to the internal scenario.
*/
const Scenario* SingleScenarioRunner::getInternalScenario() const {
	return mScenario.get();
}

const string& SingleScenarioRunner::getXMLNameStatic(){
	static const string XML_NAME = "single-scenario-runner";
	return XML_NAME;
}
