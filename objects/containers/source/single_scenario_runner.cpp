/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file single_scenario_runner.cpp
 * \ingroup SingleScenarioRunner
 * \brief SingleScenarioRunner class source file.
 * \author Josh Lurz
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

extern Scenario* scenario;
extern ofstream outFile;

// Function Prototypes. These need a helper class. 
extern void createMCvarid();
extern void closeDB();
extern void openDB();
extern void createDBout();

/*! \brief Constructor */
SingleScenarioRunner::SingleScenarioRunner(){
}

//! Destructor.
SingleScenarioRunner::~SingleScenarioRunner(){
}

// IParsable interface
bool SingleScenarioRunner::XMLParse( const xercesc::DOMNode* aRoot ){
    // No data to parse.
    return true;
}

bool SingleScenarioRunner::setupScenarios( Timer& timer,
                                           const string aName,
                                           const list<string> aScenComponents )
{
    // Ensure that a new scenario is created for each run.
    mScenario.reset( new Scenario );

    // Set the global scenario pointer.
    // TODO: Remove global scenario pointer.
    scenario = mScenario.get();

    // Parse the input file.
    const Configuration* conf = Configuration::getInstance();
    bool success =
        XMLHelper<void>::parseXML( conf->getFile( "xmlInputFileName" ),
                                   mScenario.get() );
    
    // Check if parsing succeeded.
    if( !success ){
        return false;
    }

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
        success = XMLHelper<void>::parseXML( *currComp, mScenario.get() );
        
        // Check if parsing succeeded.
        if( !success ){
            return false;
        }
    }
    
    // Override scenario name from data file with that from configuration file
    const string overrideName = conf->getString( "scenarioName" ) + aName;
    if ( !overrideName.empty() ) {
        mScenario->setName( overrideName );
    }

    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "XML parsing complete." << endl;

    // Print data read in time.
    timer.stop();
    mainLog.setLevel( ILogger::DEBUG );
    timer.print( mainLog, "XML Readin Time:" );

    // Finish initialization.
    if( mScenario.get() ){
        mScenario->completeInit();
    }
    return true;
}

bool SingleScenarioRunner::runScenarios( const int aSinglePeriod,
                                        const bool aPrintDebugging,
                                        Timer& aTimer )
{
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Starting a model run. Running ";
    if( aSinglePeriod == -1 ){
        mainLog << "all periods.";
    }
    else {
        mainLog << aSinglePeriod;
    }
    mainLog << endl;
    
	bool success = false;
	if( mScenario.get() ){
		// Perform the initial run of the scenario.
        success = mScenario->run( aSinglePeriod, aPrintDebugging );

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

    static const bool printDB = Configuration::getInstance()->getBool( "write-access-db", true );
    if( printDB ){
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
    }

    if( aCloseDB ){
        outFile.close();
    }

     // Print the timestamps.
    aTimer.stop();
    mainLog.setLevel( ILogger::DEBUG );
    aTimer.print( mainLog, "Data Readin, Model Run & Write Time:" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Model run completed." << endl;
}

Scenario* SingleScenarioRunner::getInternalScenario(){
	return mScenario.get();
}

const Scenario* SingleScenarioRunner::getInternalScenario() const {
	return mScenario.get();
}

/*!
 * \brief Get the XML name of the class.
 * \return The XML name of the class.
 */
const string& SingleScenarioRunner::getXMLNameStatic(){
	static const string XML_NAME = "single-scenario-runner";
	return XML_NAME;
}
