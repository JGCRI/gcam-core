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
 * \file policy_target_runner.cpp
 * \ingroup Objects
 * \brief PolicyTargetRunner class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
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
extern ofstream outFile;
extern void createMCvarid();

// Maximum level of carbon tax that the model can handle.
const double MAX_SOLVABLE_TAX = 5000;

/*!
 * \brief Constructor.
 */
PolicyTargetRunner::PolicyTargetRunner():
mFirstTaxYear( 0 ),
mMaxIterations( 0 ),
mMaxStableIterations( 0 ),
mHasParsedConfig( false )
{
    // Check to make sure calibration is off.
    const Configuration* conf = Configuration::getInstance();
    if( conf->getBool( "debugChecking" ) &&
        conf->getBool( "CalibrationActive" ) )
    {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Calibration may be incompatible with target finding."
                << endl;
    }
}

//! Destructor
PolicyTargetRunner::~PolicyTargetRunner(){
}

const string& PolicyTargetRunner::getName() const {
    return mName;
}

// IParsable interface
bool PolicyTargetRunner::XMLParse( const xercesc::DOMNode* aRoot ){
    // Check for double initialization.
    assert( !mHasParsedConfig );

    // Set the configuration has been parsed.
    mHasParsedConfig = true;

    // assume we were passed a valid node.
    assert( aRoot );

    mName = XMLHelper<string>::getAttr( aRoot, "name" );

    // get the children of the node.
    DOMNodeList* nodeList = aRoot->getChildNodes();
    bool success = true;
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        const string nodeName =
            XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if ( nodeName == "target-value" ){
            mTargetValue = XMLHelper<double>::getValue( curr );
        }
        else if ( nodeName == "target-type" ){
            mTargetType = XMLHelper<string>::getValue( curr );
        }
        else if ( nodeName == "tax-name" ){
            mTaxName = XMLHelper<string>::getValue( curr );
        }
        else if ( nodeName == "target-tolerance" ){
            mTolerance = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "stable-tolerance" ){
            mStableTolerance = XMLHelper<double>::getValue( curr );
        }
        else if ( nodeName == "path-discount-rate" ){
            mPathDiscountRate = XMLHelper<double>::getValue( curr );
        }
        else if ( nodeName == "first-tax-year" ){
            mFirstTaxYear = XMLHelper<unsigned int>::getValue( curr );
        }
        else if ( nodeName == "max-iterations" ){
            mMaxIterations = XMLHelper<unsigned int>::getValue( curr );
        }
        else if( nodeName == "max-stable-iterations" ){
            mMaxStableIterations = XMLHelper<unsigned int>::getValue( curr );
        }
        // Handle unknown nodes.
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized string: " << nodeName
                    << " found while parsing " << getXMLNameStatic() 
                    << "." << endl;
            success = false;
        }
    }
    return success;
}

bool PolicyTargetRunner::setupScenarios( Timer& aTimer,
                                         const string aName,
                                         const list<string> aScenComponents )
{
    // Setup the internal single scenario runner.
    mSingleScenario = ScenarioRunnerFactory::create( "single-scenario-runner" );

    bool success = mSingleScenario->setupScenarios( aTimer, aName,
                                                    aScenComponents );
    
   // Only read from the configuration file if the data has not already been
    // directly parsed from the BatchRunner configuration file.
    if( !mHasParsedConfig ){
        // Get the name of the input file from the Configuration.
        const string fileName
            = Configuration::getInstance()->getFile( "policy-target-file" );

        if( fileName.empty() ){
            return false;
        }
        // Add note so that if XML read fails here user knows what happened
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Reading advanced target finder configuration file "
                << fileName << endl;

        // Parse the file.
        success &= XMLHelper<void>::parseXML( fileName, this );
    }

    if( !mTargetValue.isInited() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Must read in a target value for the policy." << endl;
        return false;
    }

    // Setup optional defaults.
    if( mTargetType.empty() ){
        mTargetType = "concentration";
    }

    if( mTaxName.empty() ){
        mTaxName = "CO2";
    }

    if( !mPathDiscountRate.isInited() ){
        mPathDiscountRate = 0.05;
    }
    
    if( !mTolerance.isInited() ){
        mTolerance = 0.01;
    }

    if( !mStableTolerance.isInited() ){
        mStableTolerance = 0.1;
    }

    if( mFirstTaxYear == 0 ){
        mFirstTaxYear = 2020;
    }

    if( mMaxIterations == 0 ){
        mMaxIterations = 100;
    }
    if( mMaxStableIterations == 0 ){
        mMaxStableIterations = 10;
    }
    return success;
}

bool PolicyTargetRunner::runScenarios( const int aSinglePeriod,
                                       const bool aPrintDebugging,
                                       Timer& aTimer )
{
    // Perform the initial run.
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Performing the baseline run." << endl;
    
    // Clear any existing tax.
    // TODO: This should be unneeded.
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    vector<double> taxes( modeltime->getmaxper(), 0.0 );

    setTrialTaxes( taxes );

    // Run the model without a tax target once to get a baseline for the
    // bisecter and to calculate the initial non-tax periods.
    bool success = mSingleScenario->runScenarios( Scenario::RUN_ALL_PERIODS,
                                                  true, aTimer );

    // Initialize the target which determines if the end period is stable. Use
    // the end year for stabilization.

    // The earliest year the model can stabilize. TODO: Improve this logic.
    const int EARLIEST_TARGET_YEAR = 2050;
    auto_ptr<ITarget> stableTarget =
        TargetFactory::create( "stabilization-target",
                               getInternalScenario()->getClimateModel(),
                               0 ); // TODO

    // Create a bisecter which will solve for the stabilization year.
    Bisecter stableBisecter( stableTarget.get(),
                             mStableTolerance,
                             EARLIEST_TARGET_YEAR, // minimum
                             modeltime->getEndYear(), // maximum
                             Bisecter::undefined(), // first trial
                             Bisecter::undefined(),
                             -1 ); // Use the trial value as the year.

    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Beginning the search for the target year." << endl;

    while( stableBisecter.getIterations() < mMaxStableIterations ){
        pair<double, bool> trial = stableBisecter.getNextValue();
        
        targetLog.setLevel( ILogger::DEBUG );
        targetLog << "Iteration: " << stableBisecter.getIterations()
                  << " in target year search. "
                  << "Current trial value of the target year is " << trial.first
                  << "." << endl;

        // Check for solution.
        if( trial.second ){
            break;
        }

        auto_ptr<ITarget> policyTarget =
            TargetFactory::create( mTargetType + "-target",
                                   getInternalScenario()->getClimateModel(),
                                   mTargetValue );

        // Solve the initial target with the trial target year.
        success &= solveInitialTarget( taxes, policyTarget.get(),
                                       mMaxIterations, mTolerance, 
                                       static_cast<unsigned int>(
                                         floor( trial.first ) ),
                                       aTimer );

        // Convert the target year into a period. The modeltime will convert any
        // year in a period to that period.
        // TODO: With doubles this doesn't really matter does it?
        unsigned int targetPeriod =
            modeltime->getyr_to_per(
            static_cast<unsigned int>( floor( trial.first ) ) );

        // Convert the period back into a year to determine if the year lies on
        // a period boundary.
        if( modeltime->getper_to_yr( targetPeriod ) == trial.first ){
            ++targetPeriod;
        }

        // If the target was found successfully, iterate over each period past
        // the target period until the concentration in that period is equal to
        // the target. Print the output now before it is overwritten.
        for( int period = targetPeriod; period < modeltime->getmaxper();
             ++period )
        {
            success &= solveFutureTarget( taxes, policyTarget.get(),
                                          mMaxIterations, mTolerance, period,
                                          aTimer );
        }
    }
    
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Target finding for all years completed with status "
              << success << "." << endl;

    // Print the output before the total cost calculator modifies the scenario.
    mSingleScenario->printOutput( aTimer, false );

    // Initialize the total policy cost calculator if the user requested that
    // total costs should be calculated.
    if( Configuration::getInstance()->getBool( "createCostCurve" ) ){
        mPolicyCostCalculator.reset(
            new TotalPolicyCostCalculator( mSingleScenario.get() ) );

        success &= mPolicyCostCalculator->calculateAbatementCostCurve();
    }

    // Return whether the initial run and all data point calculations completed
    // successfully.
    return success;
}

/*!
 * \brief Solve the policy target for the initial year of the target.
 * \details Modifies the base year price and calculates a Hotelling price path
 *          such that the concentration in the target year is equal to the
 *          specified target. The current tax vector will be set to the final
 *          taxes used if success is returned.
 * \param aTaxes The vector to store the taxes in.
 * \param aPolicyTarget Object which detects if the policy target has been
 *        reached.
 * \param aLimitIterations The maximum number of iterations to perform.
 * \param aTolerance The tolerance of the solution.
 * \param aTargetYear The target year.
 * \param aTimer The timer used to print out the amount of time spent performing
 *        operations.
 * \return Whether the target was met successfully.
 */
bool PolicyTargetRunner::solveInitialTarget( vector<double>& aTaxes,
                                             const ITarget* aPolicyTarget,
                                             const unsigned int aLimitIterations,
                                             const double aTolerance,
                                             const int aTargetYear,
                                             Timer& aTimer )
{
    // TODO: This will run once extra the first time solveInitialTarget
    //       is called because an initial run was done to determine the
    //       baseline for the year search.
    // Perform the initial run.
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Solving for the initial target. Performing the baseline run."
              << endl;

    // Clear any existing policy.
    fill( aTaxes.begin(), aTaxes.end(), 0.0 );
    setTrialTaxes( aTaxes );

    // Run the model without a tax target once to get a baseline for the
    // bisecter and to calculate the initial non-tax periods.
    bool success = mSingleScenario->runScenarios( Scenario::RUN_ALL_PERIODS,
                                                  false, aTimer );

    // Create the bisection object. Use 0 as the initial trial value because the
    // state of the model is unknown. TODO: Can we do better?
    Bisecter bisecter( aPolicyTarget,
                       aTolerance,
                       0,
                       Bisecter::undefined(),
                       Bisecter::undefined(),
                       Bisecter::undefined(),
                       aTargetYear );

    while( bisecter.getIterations() < aLimitIterations ){
        pair<double, bool> trial = bisecter.getNextValue();

        // Check for solution.
        if( trial.second ){
            break;
        }

        // Set the trial tax.
        aTaxes = calculateHotellingPath( trial.first,
                                         mPathDiscountRate,
                                         getInternalScenario()->getModeltime(),
                                         mFirstTaxYear,
                                         aTargetYear );

        setTrialTaxes( aTaxes );

        // Run the scenario for each period with a positive tax. These are
        // periods which will have changed from the previous iteration of the
        // model. Only calculating these periods ensures that initial periods
        // are not recalculated unnecessarily. If there was a case where there
        // was a period with a tax of zero between two periods with taxes, the
        // scenario will ensure that it is recalculated because it would be
        // invalid. This also assumes that a period cannot change from a
        // positive tax to a tax of absolute zero.
        const Modeltime* modeltime = getInternalScenario()->getModeltime();
        for( int period = 0; period < modeltime->getmaxper(); ++period ){
            if( aTaxes[ period ] > 0 ){
                success &= mSingleScenario->runScenarios( period, false,
                                                          aTimer );
            }
        }
    }

    if( bisecter.getIterations() >= aLimitIterations ){
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Exiting target finding search as the iterations limit was"
                  << " reached." << endl;
        success = false;
    }
    else {
        targetLog.setLevel( ILogger::NOTICE );
        targetLog << "Target value was found by search algorithm in "
                  << bisecter.getIterations() << " iterations." << endl;
    }
    return success;
}

/*!
 * \brief Solve a target for a year past the target year.
 * \details For years past the target year the tax must be modified such that
 *          the target remains constant for every period past the target period
 *          until the end of the model.
 * \param aTaxes The current tax vector which should be updated.
 * \param aPolicyTarget Object which detects if the policy target has been
 *        reached.
 * \param aLimitIterations The maximum number of iterations to perform.
 * \param aTolerance The tolerance of the solution.
 * \param aPeriod Future period.
 * \param aTimer The timer used to print out the amount of time spent performing
 *        operations.
 * \return Whether the target was met successfully.
 */
bool PolicyTargetRunner::solveFutureTarget( vector<double>& aTaxes,
                                            const ITarget* aPolicyTarget,
                                            const unsigned int aLimitIterations,
                                            const double aTolerance,
                                            const int aPeriod,
                                            Timer& aTimer )
{
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::DEBUG );
    targetLog << "Solving future target for period " << aPeriod << "." << endl;
    
    // Run the base scenario. This is required in single period mode because the
    // current period concentration may not be accurate and reflect the effect
    // of this tax in the current period, since this period was not run while
    // solving the previous period.
    bool success = mSingleScenario->runScenarios( aPeriod, false, aTimer );

    // Construct a bisecter which has an initial trial equal to the current tax.
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    int currYear = modeltime->getper_to_yr( aPeriod );
    Bisecter bisecter( aPolicyTarget,
                       aTolerance,
                       0,
                       MAX_SOLVABLE_TAX, // Maximum tax
                       aTaxes[ aPeriod ],
                       4.0, // TODO: Magic number. What does this mean?
                       currYear );

    while( bisecter.getIterations() < aLimitIterations ){
        pair<double, bool> trial = bisecter.getNextValue();
        
        // Check for solution.
        if( trial.second ){
            break;
        }

        // Replace the current periods tax with the calculated tax.
        assert( static_cast<unsigned int>( aPeriod ) < aTaxes.size() );
        aTaxes[ aPeriod ] = trial.first;

        // Set the trial taxes.
        setTrialTaxes( aTaxes );

        // Run the base scenario.
        success = mSingleScenario->runScenarios( aPeriod, false, aTimer );
    }

    if( bisecter.getIterations() >= aLimitIterations ){
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Exiting target finding search as the iterations limit " 
                  << "was reached." << endl;
        success = false;
    }
    else {
        targetLog.setLevel( ILogger::NOTICE );
        targetLog << "Target value was found by search algorithm in "
                  << bisecter.getIterations() << " iterations." << endl;
    }
    return success;
}

void PolicyTargetRunner::printOutput( Timer& aTimer, const bool aCloseDB ) const
{
    if( mPolicyCostCalculator.get() ){
        mPolicyCostCalculator->printOutput();
    }
    
    // Close the database.
    static const bool printDB =
        Configuration::getInstance()->getBool( "write-access-db", true );
    if( printDB && aCloseDB ){
        createMCvarid();
        closeDB();
        outFile.close();
    }
}

Scenario* PolicyTargetRunner::getInternalScenario(){
    return mSingleScenario->getInternalScenario();
}

const Scenario* PolicyTargetRunner::getInternalScenario() const {
    return mSingleScenario->getInternalScenario();
}

const string& PolicyTargetRunner::getXMLNameStatic(){
    static const string XML_NAME = "policy-target-runner";
    return XML_NAME;
}

/*!
 * \brief Calculate the Hotelling price path.
 * \brief Calculates a tax pathway that begins at the given initial tax and
 *        increases at a given rate, generally related to the interest rate,
 *        between an initial and final year.
 * \param aInitialTax The initial value of the tax. The initial tax and the rate
 *        together determine the path.
 * \param aHotellingRate The growth rate of the tax. This value is often linked
 *        to the interest rate.
 * \param aInitialYear The first year a tax will be set. The tax in this year
 *        will be the given initial tax. This may be an intermediate year, i.e.
 *        not a model period. This must be within the range of the model years.
 * \param aFinalYear The final year for which to calculate a tax. This may be an
 *        intermediate year, i.e. not a model period. This must be within the
 *        range of the model years.
 * \return A vector by time period containing the tax path. If the initial and
 *         final years are not within the initial and final periods of the model
 *         there will be zeros for the untaxed periods.
 */
vector<double>
PolicyTargetRunner::calculateHotellingPath( const double aInitialTax,
                                            const double aHotellingRate,
                                            const Modeltime* aModeltime,
                                            const int aInitialYear,
                                            const int aFinalYear )
{
    // Initialize the tax vector.
    const int maxPeriod = aModeltime->getmaxper();
    vector<double> taxes( maxPeriod );

    // Only set a tax for periods up to the period of the target year. Calculate
    // the tax for each year. Periods set to zero tax will not be solved.
    int targetPeriod = aModeltime->getyr_to_per( aFinalYear );

    // TODO: This might be wrong if the first tax was between periods.
    for( int per = aModeltime->getyr_to_per( aInitialYear );
        per <= targetPeriod; per++ )
    {
        // If the last year associated with the current period is greater than
        // the target year, only calculate a hotelling price path up to the
        // target year. Assume the tax is constant at that point.
        const int currYear = min( aModeltime->getper_to_yr( per ), aFinalYear );
        const int numYears = currYear - aInitialYear;
        assert( numYears >= 0 );

        taxes[ per ] = aInitialTax * exp( aHotellingRate * numYears );

        taxes[ per ] = min( taxes[ per ], MAX_SOLVABLE_TAX );
    }
    return taxes;
}

/*!
 * \brief Set a vector of taxes into the model.
 * \param aTaxes Vector of taxes to set into the model. Must contain one value
 *        for each model period.
 */
void PolicyTargetRunner::setTrialTaxes( const vector<double> aTaxes ) {
    // Set the fixed taxes into the world. The world will clone this tax object,
    // this object retains ownership of the original.
    GHGPolicy tax( mTaxName, "global", aTaxes );
    mSingleScenario->getInternalScenario()->setTax( &tax );
}
