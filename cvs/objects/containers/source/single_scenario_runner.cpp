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
 * \file single_scenario_runner.cpp
 * \ingroup SingleScenarioRunner
 * \brief SingleScenarioRunner class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "containers/include/single_scenario_runner.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/timer.h"
#include "util/base/include/configuration.h"
#include "util/base/include/auto_file.h"
#include "util/logger/include/ilogger.h"
#include "util/logger/include/logger_factory.h"
#include "reporting/include/xml_db_outputter.h"

using namespace std;

extern Scenario* scenario;
extern ofstream outFile;

// Function Prototypes. These need a helper class. 
extern void createMCvarid();
extern void closeDB();
extern void openDB();
extern void createDBout();

/*! \brief Constructor */
SingleScenarioRunner::SingleScenarioRunner(){
    mXMLDBOutputter = 0;
}

//! Destructor.
SingleScenarioRunner::~SingleScenarioRunner(){
    /*!
     * \pre Memory clean up should have been done in the cleanup method.
     */
    assert( !mXMLDBOutputter );
}

const string& SingleScenarioRunner::getName() const {
    return getXMLNameStatic();
}

bool SingleScenarioRunner::setupScenarios( Timer& timer,
                                           const string aName,
                                           const list<string> aScenComponents )
{
    const Configuration* conf = Configuration::getInstance();
    // before we do anything make sure we will be able to write
    // database results if we need to do so
    if( conf->shouldWriteFile( "xmldb-location") && !XMLDBOutputter::checkJavaWorking() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Early warning Java checks failed and database output was requested" << endl;
        abort();
    }
    // Ensure that a new scenario is created for each run.
    mScenario.reset( new Scenario );

    // Set the global scenario pointer.
    // TODO: Remove global scenario pointer.
    scenario = mScenario.get();

    // Parse the input file.
    bool success =
        XMLParseHelper::parseXML( conf->getFile( "xmlInputFileName" ),
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
        success = XMLParseHelper::parseXML( *currComp, mScenario.get() );
        
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

    // Add to all loggers that a new scenario is starting so that users may more
    // easily parse which scenario the messages pertain to.
    LoggerFactory::logNewScenarioStarting( overrideName );

    // Print data read in time.
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
    if( aSinglePeriod == Scenario::UNINITIALIZED_RUN_PERIODS ) {
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Model run period was left uninitialized, will default to run all periods." << endl;
    }
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Starting a model run. Running ";
    int runPeriod;
    if( aSinglePeriod < 0 ){
        mainLog << "all periods.";
        runPeriod = Scenario::RUN_ALL_PERIODS;
    }
    else {
        mainLog << "period " << aSinglePeriod;
        runPeriod = aSinglePeriod;
    }
    mainLog << endl;

    aTimer.start();             // ensure timer is running.
    
    bool success = false;
    if( mScenario.get() ){
        // Perform the initial run of the scenario.
        success = mScenario->run( runPeriod, aPrintDebugging,
                                  mScenario->getName() );

        // Compute model run time.
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

void SingleScenarioRunner::printOutput( Timer& aTimer ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    // If QuitFirstFailure bool is set to 1 and a period fails to solve (success = FALSE), 
    // the model will exit after this period and not print the database.
    // If the model is running in target finder mode, this only applies when the final period fails.
    const Configuration* conf = Configuration::getInstance();
    bool quitFirstFailure = conf->getBool("QuitFirstFailure", false, false);
    bool runTargetFinder = conf->getBool("find-path", false, false);
    bool success = mScenario->getUnsolvedPeriods().empty();
    if (!success & quitFirstFailure & !runTargetFinder) {
        // If this is a single scenario runner (not target finder) which fails a model period while 
        // QuitFirstFailure bool is set to true (1), do not print output
        mainLog.setLevel(ILogger::ERROR);
        mainLog << "Period failed to solve. Skipping writing database." << endl;
    }
    else {
        mainLog.setLevel(ILogger::NOTICE);
        mainLog << "Printing output" << endl;

        Timer &writeTimer = TimerRegistry::getInstance().getTimer(TimerRegistry::WRITE_DATA);
        writeTimer.start();

        if (Configuration::getInstance()->shouldWriteFile("xmldb-location")) {
            mainLog.setLevel(ILogger::NOTICE);
            mainLog << "Starting output to XML Database." << endl;
            // Print the XML file for the XML database.
            assert(!mXMLDBOutputter);
            mXMLDBOutputter = new XMLDBOutputter();

            // Update the output container with information from the model.
            // -1 flags to update the output container for all periods at once.
            mScenario->accept(mXMLDBOutputter, -1);


            // Print the output.
            mXMLDBOutputter->finish();
        }
        writeTimer.stop();

        // Print the timestamps.
        aTimer.stop();
        mainLog.setLevel(ILogger::DEBUG);
        aTimer.print(mainLog, "Data Readin, Model Run & Write Time:");
        writeTimer.print(mainLog, "Write time: ");
        mainLog.setLevel(ILogger::NOTICE);
        mainLog << "Model run completed." << endl;
    }
}

void SingleScenarioRunner::cleanup() {
    // The current scenario is no longer needed since a new scenario run will be
    // created from scratch the next time a scenario is setup and run.
    mScenario.reset( 0 );
    scenario = 0;

    // If the XML database was opened then we should close it.
    if( mXMLDBOutputter ) {
        // Now that the scenario memory is cleared out we can have the XML DB
        // outputter finalize and close.
        mXMLDBOutputter->finalizeAndClose();
        delete mXMLDBOutputter;
        mXMLDBOutputter = 0;
    }
}

Scenario* SingleScenarioRunner::getInternalScenario(){
    return mScenario.get();
}

const Scenario* SingleScenarioRunner::getInternalScenario() const {
    return mScenario.get();
}

/*!
 * \brief Get the refernce to the XMLDBOutputter.
 * \return The XMLDBOutputter.
 */
XMLDBOutputter* SingleScenarioRunner::getXMLDBOutputter() const {
    return mXMLDBOutputter;
}

/*!
 * \brief Get the XML name of the class.
 * \return The XML name of the class.
 */
const string& SingleScenarioRunner::getXMLNameStatic(){
    static const string XML_NAME = "single-scenario-runner";
    return XML_NAME;
}
