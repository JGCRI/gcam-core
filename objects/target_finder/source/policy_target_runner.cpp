/*! 
* \file policy_target_runner.cpp
* \ingroup Objects
* \brief PolicyTargetRunner class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "target_finder/include/policy_target_runner.h"
#include "target_finder/include/target_factory.h"
#include "target_finder/include/bisecter.h"
#include "target_finder/include/itarget.h"
#include "containers/include/scenario_runner_factory.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/total_policy_cost_calculator.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "emissions/include/ghg_policy.h"
#include "util/base/include/util.h"

using namespace std;
using namespace xercesc;

extern void closeDB();
extern void createMCvarid();

/*! \brief Constructor.
*/
PolicyTargetRunner::PolicyTargetRunner(){
    mSingleScenario = ScenarioRunnerFactory::create( "single-scenario-runner" );

    // Check to make sure calibration is off.
	const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "debugChecking" ) && conf->getBool( "CalibrationActive" ) ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Calibration may be incompatible with target finding." << endl;
    }

    // Initialize the total policy cost calculator if the user requested that
    // total costs should be calculated.
    if( conf->getBool( "createCostCurve" ) ){
        mPolicyCostCalculator.reset( new TotalPolicyCostCalculator( mSingleScenario.get() ) );
    }

    // Initialize member variables. Default the discount rate used on the path
    // to 5 percent. This is not required to match the social discount rate used
    // to calculate the total policy cost.
    mPathDiscountRate = conf->getDouble( "path-discount-rate", 0.05 );

    // Check if it exceeds the backstop tax rate. Default to not having a
    // backstop. Default to -1 which means there is not a backstop.
    mBackstopTax = conf->getDouble( "backstop-tax", -1 );
    
    // Determine the type of policy.
    mTargetType = Configuration::getInstance()->getString( "target-type", "concentration" ) + "-target";
    
    // These variables cannot be initialized until after the Modeltime is initialized.
    mFirstTaxYear = mTargetYear = 0;
}

//! Destructor
PolicyTargetRunner::~PolicyTargetRunner(){
}

/*! \brief Setup the Scenario to be run.
* \detailed This function sets up the contained SingleScenarioRunner.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \param aName The name to add on to the name read in in the Configuration file.
* \param aScenComponents A list of additional scenario components to read in.
* \return Whether the setup completed successfully.
*/
bool PolicyTargetRunner::setupScenario( Timer& aTimer, const string aName,
                                        const list<string> aScenComponents )
{
    bool success = mSingleScenario->setupScenario( aTimer, aName, aScenComponents );
    
    // This has to be done after the modeltime is initialzed. Determine the
    // period in which to reach the target.
    const Configuration* conf = Configuration::getInstance();
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    mTargetYear = conf->getInt( "target-year", modeltime->getEndYear() );
    
    // Calculate the year in which to begin taxing. This should be the first
    // discount year, or the present year, plus a timestep to allow time for
    // taxation to occur.
    int startDiscountYear = conf->getInt( "discount-start-year", 2005 );
    int timeStep = modeltime->gettimestep( modeltime->getyr_to_per( startDiscountYear ) );
    mFirstTaxYear = startDiscountYear + timeStep;

    /*! \pre The target year is initialized. */
    assert( mTargetYear != 0 );
    const unsigned int targetPeriod = modeltime->getyr_to_per( mTargetYear );
    
    // Create a a policy target.
    mPolicyTarget = TargetFactory::create( mTargetType,
                                           getInternalScenario()->getClimateModel(),
                                           targetPeriod,
                                           modeltime->getmaxdataper() - 1 );
    return success;
}

/*! \brief Runs the scenario and adjusts a tax until the target is reached.
* \details Perfoms a bisection search on the initial value of the hotelling
*          price path to find a value which cause the specified target to be
*          reached in the end period.
* \param aSinglePeriod This parameter is currently ignored.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \return Whether all model runs solved successfully.
* \author Josh Lurz
*/
bool PolicyTargetRunner::runScenario( const int aSinglePeriod, Timer& aTimer ) {
    
    // Search until a limit is reach or the solution is found.
    const unsigned int LIMIT_ITERATIONS = 100;

    // Tolerance as a percent.
    const double TOLERANCE = 0.5;
    
    // Solve the initial target.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Solving initial target." << endl;
    
    // Initialize the trial taxes vector so that if the initial target is found
    // without any adjustment it still has the correct size.
    const Modeltime* modeltime = getInternalScenario()->getModeltime();   
    mCurrentTaxes.resize( modeltime->getmaxper(), 0 );

    bool success = solveInitialTarget( LIMIT_ITERATIONS, TOLERANCE, aTimer );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Initial target was solved with status " << success << "." << endl;

    // Convert the target year into a period.
    /*! \pre The target year is initialized. */
    assert( mTargetYear != 0 );

    const unsigned int targetPeriod = modeltime->getyr_to_per( mTargetYear );
    
    // If the target was found successfully, iterate over each period past the
    // target period until the concentration in that period is equal to the
    // target. Print the output now before it is overwritten.
    for( int period = targetPeriod + 1; period < modeltime->getmaxper(); ++period ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Solving target for future period " << period << "." << endl;
        success &= solveFutureTarget( LIMIT_ITERATIONS, TOLERANCE, period, aTimer );
    }
    
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Target finding for all years completed with status " << success << "." << endl;

    mSingleScenario->printOutput( aTimer, false );

    // Now calculate the abatement curves.
    if( mPolicyCostCalculator.get() ){
        success &= mPolicyCostCalculator->calculateAbatementCostCurve();
    }

    // Return whether the initial run and all datapoint calculations completed
    // successfully.
    return success;
}

/*! \brief Solve the policy target for the initial year of the target.
* \details Modifies the base year price and calculates a Hotelling price path
*          such that the concentration in the target year is equal to the
*          specified target. The current tax vector will be set to the final
*          taxes used if success is returned.
* \param aLimitIterations The maximum number of iterations to perform.
* \param aTolerance The tolerance of the solution.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \return Whether the target was met successfully.
*/
bool PolicyTargetRunner::solveInitialTarget( const unsigned int aLimitIterations,
                                             const double aTolerance,
                                             Timer& aTimer )
{
    // Ensure that the policy target was created successfully.
    if( !mPolicyTarget.get() ){
        return false;
    }

    bool success = true;
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    const unsigned int targetPeriod = modeltime->getyr_to_per( mTargetYear );
    // Create the bisection object. Use 0 as the initial trial value because the
    // state of the model is unknown.
    auto_ptr<Bisecter> bisecter( new Bisecter( mPolicyTarget.get(), aTolerance, 0, targetPeriod ) );
    bool isFirstTime = true;
    while( bisecter->getIterations() < aLimitIterations ){
        // Let the model run once before setting a target.
        if( isFirstTime ){
            isFirstTime = false;
        }
        else {
            pair<double, bool> trial = bisecter->getNextValue();

            // Check for solution.
            if( trial.second ){
                break;
            }

            // Set the trial tax.
            mCurrentTaxes = calculateHotellingPath( trial.first );
            setTrialTaxes( mPolicyTarget->getTaxName(), mCurrentTaxes );
        }

        // Run the base scenario.
        success = mSingleScenario->runScenario( Scenario::RUN_ALL_PERIODS, aTimer );
    }

    if( bisecter->getIterations() >= aLimitIterations ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Exiting target finding search as the iterations limit was reached." << endl;
        success = false;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Target value was found by search algorithm in "
                << bisecter->getIterations() << " iterations." << endl;
    }
    return success;
}

/*! \brief Solve a target for a year past the target year.
* \details For years past the target year the tax must be modified such that the target remains constant for every period past the target period until the end of the model.
* \param aLimitIterations The maximum number of iterations to perform.
* \param aTolerance The tolerance of the solution.
* \param aPeriod Future period.
* \param aTimer The timer used to print out the amount of time spent performing
*        operations.
* \return Whether the target was met successfully.
*/
bool PolicyTargetRunner::solveFutureTarget( const unsigned int aLimitIterations,
                                            const double aTolerance,
                                            const int aPeriod,
                                            Timer& aTimer )
{
    // Ensure that the policy target was created successfully.
    if( !mPolicyTarget.get() ){
        return false;
    }

    // Create the bisection object.
    bool success = true;

    // Construct a bisecter which has an initial trial equal to the current tax.
    auto_ptr<Bisecter> bisecter( new Bisecter( mPolicyTarget.get(), aTolerance,
                                               mCurrentTaxes[ aPeriod ], aPeriod ) );

    while( bisecter->getIterations() < aLimitIterations ){
        pair<double, bool> trial = bisecter->getNextValue();
        
        // Check for solution.
        if( trial.second ){
            break;
        }

        // Replace the current periods tax with the calculated tax.
        assert( static_cast<unsigned int>( aPeriod ) < mCurrentTaxes.size() );
        mCurrentTaxes[ aPeriod ] = trial.first;

        // Set the trial taxes.
        setTrialTaxes( mPolicyTarget->getTaxName(), mCurrentTaxes );

        // Run the base scenario.
        success = mSingleScenario->runScenario( aPeriod, aTimer );
    }

    if( bisecter->getIterations() >= aLimitIterations ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Exiting target finding search as the iterations limit was reached." << endl;
        success = false;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Target value was found by search algorithm in "
                << bisecter->getIterations() << " iterations." << endl;
    }
    return success;
}

//! Print the output.
void PolicyTargetRunner::printOutput( Timer& timer, const bool aCloseDB ) const {
    if( mPolicyCostCalculator.get() ){
        mPolicyCostCalculator->printOutput();
    }
    
    // Close the database.
    if( aCloseDB ){
        createMCvarid();
        closeDB();
    }
}

/*! \brief Get the internal scenario.
* \return The internal scenario.
*/
Scenario* PolicyTargetRunner::getInternalScenario(){
	return mSingleScenario->getInternalScenario();
}

/*! \brief Get the internal scenario.
* \return Constant pointer to the internal scenario.
*/
const Scenario* PolicyTargetRunner::getInternalScenario() const {
	return mSingleScenario->getInternalScenario();
}

const string& PolicyTargetRunner::getXMLNameStatic(){
	static const string XML_NAME = "policy-target-runner";
	return XML_NAME;
}

/*! \brief Calculate the Hotelling price path.
* \param aPeriod Model period.
* \param aScaler Initial price or scaler.
*/
vector<double> PolicyTargetRunner::calculateHotellingPath( const double aScaler ) const {
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    const int maxPeriod = modeltime->getmaxper();
    
    // Initialize the tax period.
    vector<double> taxes( maxPeriod );

    // Calculate the tax for each year. 
    for( int per = 0; per < maxPeriod; per++ ){
        const int numYears = modeltime->getper_to_yr( per ) - mFirstTaxYear;
        if( numYears >= 0 ){
            taxes[ per ] = aScaler * exp( mPathDiscountRate * numYears );
        }

        // Negative or zero backstop means there is no backstop, not that the
        // backstop is negative.
        if( mBackstopTax > 0 ){
            taxes[ per ] = min( mBackstopTax, taxes[ per ] );
        }
    }
    return taxes;
}

/*! \brief Set the trial taxes.
* \param aTaxes Vector of taxes to set into the model.
*/
void PolicyTargetRunner::setTrialTaxes( const string& aTaxName, const vector<double> aTaxes ) {
    // Set the fixed taxes into the world. The world will clone this tax object,
    // this object retains ownership of the original.
    auto_ptr<GHGPolicy> tax( new GHGPolicy( aTaxName, "global", aTaxes ) );
    mSingleScenario->getInternalScenario()->setTax( tax.get() );
}
