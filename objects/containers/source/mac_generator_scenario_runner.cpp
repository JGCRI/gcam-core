/*! 
* \file mac_generator_scenario_runner.cpp
* \ingroup Objects
* \brief MACGeneratorScenarioRunner class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include "containers/include/mac_generator_scenario_runner.h"
#include "containers/include/scenario_runner_factory.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/total_policy_cost_calculator.h"
#include "containers/include/scenario.h"
#include "util/base/include/auto_file.h"

using namespace std;
using namespace xercesc;

extern void closeDB();
extern void createMCvarid();
extern ofstream outFile;

/*! \brief Constructor.
*/
MACGeneratorScenarioRunner::MACGeneratorScenarioRunner(){
	mSingleScenario = ScenarioRunnerFactory::create( "single-scenario-runner" );

    // Check to make sure calibration is off.
	const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "debugChecking" ) && conf->getBool( "CalibrationActive" ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Calibration is incompatable with the generation of marginal abatement curves." << endl;
    }
    else {
        // Create the policy cost calculator.
        mPolicyCostCalculator.reset( new TotalPolicyCostCalculator( mSingleScenario.get() ) );
    }
}

//! Destructor
MACGeneratorScenarioRunner::~MACGeneratorScenarioRunner(){
}

/*! \brief Setup the Scenario to be run.
* \detailed This function sets up the contained SingleScenarioRunner.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \param aName The name to add on to the name read in in the Configuration file.
* \param aScenComponents A list of additional scenario components to read in.
* \return Whether the setup completed successfully.
*/
bool MACGeneratorScenarioRunner::setupScenario( Timer& aTimer, const string aName, const list<string> aScenComponents ){
    return mSingleScenario->setupScenario( aTimer, aName, aScenComponents );
}

/*! \brief Function which handles running the scenario and optionally computing
*          a cost curve.
* \details This function wraps around the scenario so that scenario can be
*          called multiple times if neccessary to create an abatement cost
*          curve. This function first calls the scenario regularly, outputs all
*          data, and then calls scenario several more times and calculates the
*          abatement cost.
* \param aSinglePeriod This parameter is ignored currently.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \return Whether all model runs solved successfully.
* \author Josh Lurz
*/
bool MACGeneratorScenarioRunner::runScenario( const int aSinglePeriod, Timer& aTimer ) {
    // Run the base scenario. 
    bool success = mSingleScenario->runScenario( Scenario::RUN_ALL_PERIODS, aTimer );

    // Print the output now before it is overwritten.
    mSingleScenario->printOutput( aTimer, false );

    // Now calculate the abatement curves.
    if( mPolicyCostCalculator.get() ){
        success &= mPolicyCostCalculator->calculateAbatementCostCurve();
    }

    // Return whether the initial run and all datapoint calculations completed
    // successfully.
    return success;
}

//! Print the output.
void MACGeneratorScenarioRunner::printOutput( Timer& timer, const bool aCloseDB ) const {
    if( mPolicyCostCalculator.get() ){
        mPolicyCostCalculator->printOutput();
    }
    
    static const bool printDB = Configuration::getInstance()->getBool( "write-access-db", true );
    
    // Close the database.
    if( printDB && aCloseDB ){
        createMCvarid();
        closeDB();
        outFile.close();
    }
}

/*! \brief Get the internal scenario.
* \return The internal scenario.
*/
Scenario* MACGeneratorScenarioRunner::getInternalScenario(){
	return mSingleScenario->getInternalScenario();
}

/*! \brief Get the internal scenario.
* \return Constant pointer to the internal scenario.
*/
const Scenario* MACGeneratorScenarioRunner::getInternalScenario() const {
	return mSingleScenario->getInternalScenario();
}

/*! \brief Get the static class name.
* \return The static class name.
*/
const string& MACGeneratorScenarioRunner::getXMLNameStatic(){
	static const string XML_NAME = "mac-generator-scenario-runner";
	return XML_NAME;
}
