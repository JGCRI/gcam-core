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
 * \file policy_target_runner.cpp
 * \ingroup Objects
 * \brief PolicyTargetRunner class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "target_finder/include/policy_target_runner.h"
#include "target_finder/include/target_factory.h"
#include "target_finder/include/itarget_solver.h"
#include "target_finder/include/bisecter.h"
#include "target_finder/include/secanter.h"
#include "target_finder/include/itarget.h"
#include "containers/include/scenario_runner_factory.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/single_scenario_runner.h"
#include "containers/include/total_policy_cost_calculator.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "policy/include/policy_ghg.h"
#include "util/base/include/util.h"
#include "marketplace/include/marketplace.h"

using namespace std;

/*!
 * \brief Constructor.
 */
PolicyTargetRunner::PolicyTargetRunner():
mInitialTaxGuess( 5 ),
mFirstTaxYear( 2020 ),
mMaxIterations( 100 ),
mInitialTargetYear( ITarget::getUseMaxTargetYearFlag() ),
mHasParsedConfig( false ),
mRunID( 0 ),
mNumForwardLooking( 0 ),
mNumBackwardsLook( 0 ),
mMaxTax( 4999 )
{
}

//! Destructor
PolicyTargetRunner::~PolicyTargetRunner(){
}

const string& PolicyTargetRunner::getName() const {
    return mName;
}

bool PolicyTargetRunner::XMLParse(rapidxml::xml_node<char>* & aNode) {
    mHasParsedConfig = true;
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if( nodeName == "stabilization" ) {
        mInitialTargetYear = ITarget::getUseMaxTargetYearFlag();
    }
    else if( nodeName == "overshoot" ) {
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        // Set the year to overshoot to the year attribute or if not provided
        // default to the last model year.  We can not set it to the last
        // year here since the model time may not have been parsed.
        string overshootYear = attrs["year"];
        mInitialTargetYear = overshootYear.empty() ? 0 : XMLParseHelper::getValue<int>( overshootYear );
    }
    else if( nodeName == "forward-look" ) {
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        auto yearIter = attrs.find("year");
        if( yearIter == attrs.end() ) {
            int value = XMLParseHelper::getValue<int>( aNode );
            fill( mNumForwardLooking.begin(), mNumForwardLooking.end(), value );
        }
        else {
            Data<vector<int>, ARRAY> forwardLookData( mNumForwardLooking, "");
            XMLParseHelper::parseData(aNode, forwardLookData);
        }
    }
    else {
        return false;
    }
    return true;
}

bool PolicyTargetRunner::setupScenarios( Timer& aTimer,
                                         const string aName,
                                         const list<string> aScenComponents )
{
    // Setup the internal single scenario runner.
    mSingleScenario = ScenarioRunnerFactory::createSingleScenarioRunner();

    bool success = mSingleScenario->setupScenarios( aTimer, aName,
                                                    aScenComponents );

    // Size the forward looking vector now that we have a scenario
    mNumForwardLooking.resize( mSingleScenario->getInternalScenario()->getModeltime()->getmaxper(), 0 );
    
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
        IScenarioRunner* temp = this;
        success &= XMLParseHelper::parseXML( fileName, temp );
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
    
    if( mInitialTargetYear == 0 ) {
        // If the overshoot tag was read in but without a year attribute then
        // we can now set the year to tbe the last model year.
        mInitialTargetYear = mSingleScenario->getInternalScenario()
            ->getModeltime()->getEndYear();
    }
    
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel(ILogger::NOTICE);
    targetLog << "Setting up scenario: name= " << aName << endl;
    targetLog << "target type: " << mTargetType << endl;
    targetLog << "tax name: " << mTaxName << endl;
    targetLog << "target value: " << mTargetValue << endl << endl;

    return success;
}

bool PolicyTargetRunner::runScenarios( const int aSinglePeriod,
                                       const bool aPrintDebugging,
                                       Timer& aTimer )
{
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );

    bool success;
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    vector<double> taxes( modeltime->getmaxper(), 0.0 );
    setTrialTaxes( taxes );

    // TODO: This is only necessary because the cost calculator has trouble solving
    // a zero carbon tax with restart turned on.  This should be unnecessary with
    // a better solver.
    const static bool usingRestartPeriod = Configuration::getInstance()->getInt(
        "restart-period", -1 ) != -1;
    if( usingRestartPeriod ) {
        mSingleScenario->getInternalScenario()->getMarketplace()->store_prices_for_cost_calculation();
    }

    // Create the target object.
    unique_ptr<ITarget> policyTarget = TargetFactory::create( mTargetType + "-target",
        getInternalScenario()->getClimateModel(), mTargetValue, mFirstTaxYear );
    // Make sure we have a know target
    if( !policyTarget.get() ) {
        return false;
    }
    
    // Find the initial target.
    success = solveInitialTarget( taxes, policyTarget.get(),
                                  mMaxIterations, mTolerance,
                                  aTimer );
    if( success ) {
        // For all years following the stabilization year adjust the tax to stay
        // on the target.
        const int targetYear = mInitialTargetYear == ITarget::getUseMaxTargetYearFlag() ?
            policyTarget->getYearOfMaxTargetValue() : mInitialTargetYear;
        unsigned int targetPeriod = modeltime->getyr_to_per( targetYear );
        
        // Convert the period back into a year to determine if the year lies on
        // a period boundary.
        if( modeltime->getper_to_yr( targetPeriod ) == targetYear ){
            ++targetPeriod;
        }

        // Do backwards look if set, do not go before the first tax period
        targetPeriod -= mNumBackwardsLook;
        unsigned int firstTaxPeriod = modeltime->getyr_to_per( mFirstTaxYear );
        targetPeriod = max( targetPeriod, firstTaxPeriod );
        
        // If the target was found successfully, iterate over each period past
        // the target period until the concentration in that period is equal to
        // the target.
        for( int period = targetPeriod; period < modeltime->getmaxper() && success;
             ++period )
        {
            int numForwardLooking = mNumForwardLooking[ period ];
            /*!
             * \pre The number of periods to look forward must be valid.
             */
            assert( numForwardLooking >= 0 );

            if( numForwardLooking == 0 ) {
                success &= solveFutureTarget( taxes, policyTarget.get(),
                                              mMaxIterations, mTolerance, period,
                                              aTimer );
            }
            else {
                const int periodsToSkip = period + numForwardLooking < modeltime->getmaxper() ?
                    numForwardLooking :
                    modeltime->getmaxper() - period - 1;
                success &= skipFuturePeriod( taxes, policyTarget.get(), mMaxIterations,
                                             mTolerance, period, period + periodsToSkip,
                                             aTimer );
            }
        }
    }
    
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Target finding for all years completed with status "
              << success << "." << endl;

    // Print the output before the total cost calculator modifies the scenario.
    mSingleScenario->printOutput( aTimer );

    // Initialize the total policy cost calculator if the user requested that
    // total costs should be calculated.
    if( success && Configuration::getInstance()->getBool( "createCostCurve" ) ){
        mPolicyCostCalculator.reset(
            new TotalPolicyCostCalculator( mSingleScenario.get() ) );

        success &= mPolicyCostCalculator->calculateAbatementCostCurve();
    }

    // Return whether the initial run and all data point calculations completed
    // successfully.
    return success;
}

void PolicyTargetRunner::cleanup() {
    mSingleScenario->cleanup();
    mPolicyCostCalculator.reset( 0 );
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
                                             Timer& aTimer )
{
    // Perform the initial run.
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    targetLog.setLevel( ILogger::NOTICE );
    targetLog << "Solving for the initial target. Performing the baseline run."
              << endl;

    const int firstTaxPeriod = getInternalScenario()->getModeltime()->getyr_to_per( mFirstTaxYear );
    const double initialTax = aTaxes[ firstTaxPeriod ];
    
    const int finalModelYear = getInternalScenario()->getModeltime()->getEndYear();
    const int finalModelPeriod = getInternalScenario()->getModeltime()->getmaxper() - 1;

    // Run the model without a tax target once to get a baseline for the
    // solver and to calculate the initial non-tax periods.
    logRunID();
    bool success = mSingleScenario->runScenarios( Scenario::RUN_ALL_PERIODS,
                                                  false, aTimer );
    
    // If we are already below the target at a zero tax then we won't be able to
    // get to the target.
    if( aPolicyTarget->getStatus( mInitialTargetYear ) < 0 ) {
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Failed because target is too high." << endl;
        return false;
    }

    // Create the solver object for determining the initial tax rate that will meet the target
    // in the current trail target year. Use 0 as the initial trial tax value because the
    // state of the model is unknown. Probably need to use this since there is no way
    // to know how low the solution tax might be.
    
    // Increment is 1+ this number, which is used to increase the initial trial price
    const double INCREASE_INCREMENT = mInitialTaxGuess - 1;
    unique_ptr<ITargetSolver> solver;
    
    solver.reset( new Secanter( aPolicyTarget,
                       aTolerance,
                       initialTax,
                       aPolicyTarget->getStatus( mInitialTargetYear ),
                       INCREASE_INCREMENT,
                       mInitialTargetYear ) );
    

    while( solver->getIterations() < aLimitIterations ) {
        pair<double, bool> trial = solver->getNextValue();

        // Check for solution.
        if( trial.second ){
            break;
        }
        
        if( !util::isValidNumber( trial.first ) ) {
            targetLog.setLevel( ILogger::ERROR );
            targetLog << "Failed due to invalid trial price generated by solver." << endl;
            return false;
        }

        
        targetLog << "Iteration " << solver->getIterations() << " trial value = "
                  << trial.first << endl;

        // Set the trial tax.
        calculateHotellingPath( trial.first,
                                         mPathDiscountRate,
                                         getInternalScenario()->getModeltime(),
                                         mFirstTaxYear,
                                         finalModelYear,
                                         aTaxes );

        setTrialTaxes( aTaxes );
        for( int period = firstTaxPeriod; period <= finalModelPeriod; ++period ) {
            getInternalScenario()->invalidatePeriod( period );
        }

        // Run the scenario at the trial tax.
        // TODO: If the run failed to solve then the target status may be unreliable.
        logRunID();
        success = mSingleScenario->runScenarios( finalModelPeriod, false, aTimer );

        targetLog << "Scenario run complete.  Return status = " << success << endl;
    }

    if( solver->getIterations() >= aLimitIterations ){
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Exiting target finding search as the iterations limit was"
                  << " reached." << endl;
        success = false;
    }
    else if( !success ) {
        // This is the case that we found the target however the run in which we
        // found the target had periods that did not solve.  If only periods after
        // target year did not solve then we will allow it.
        success = true;
        const vector<int>& unsolvedPeriods = mSingleScenario->getInternalScenario()->getUnsolvedPeriods();
        const Modeltime* modeltime = mSingleScenario->getInternalScenario()->getModeltime();
        const int targetYear = mInitialTargetYear == ITarget::getUseMaxTargetYearFlag() ?
            aPolicyTarget->getYearOfMaxTargetValue() : mInitialTargetYear;
        for( vector<int>::const_iterator it = unsolvedPeriods.begin();
             it != unsolvedPeriods.end() && success; ++it )
        {
            if( targetYear >= modeltime->getper_to_yr( *it ) ) {
                targetLog.setLevel( ILogger::ERROR );
                targetLog << "Failed due to unsolved model period: " << *it << endl;
                success = false;
            }
        }
    }
        
    if( success ) {
        targetLog.setLevel( ILogger::NOTICE );
        targetLog << "Target value was found by search algorithm in "
                  << solver->getIterations() << " iterations." << endl;
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
    
    // Run the base scenario. The first guess will be the tax from the previous
    // period which is likely closer to than that which was rising on the hotelling
    // path.
    aTaxes[ aPeriod ] = aTaxes[ aPeriod - 1 ];
    setTrialTaxes( aTaxes );
    logRunID();
    bool success = mSingleScenario->runScenarios( aPeriod, false, aTimer );

    // Construct a solver which has an initial trial equal to the current tax.
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    int currYear = modeltime->getper_to_yr( aPeriod );
    unique_ptr<ITargetSolver> solver;
    /* Note that the following code is left commented out incase a user wanted
     to use the bisection routine rather then the secant.
     solver.reset( new Bisecter( aPolicyTarget,
                       aTolerance,
                       0,
                       MAX_SOLVABLE_TAX, // Maximum tax
                       aTaxes[ aPeriod ],
                       4.0, // Note the hard coded value is the initial bracket interval
                       currYear ) );*/
    solver.reset( new Secanter( aPolicyTarget,
                       aTolerance,
                       aTaxes[ aPeriod ],
                       aPolicyTarget->getStatus( currYear ),
                       0.2, // Note the hard coded value is the initial percent change
                            // for the second initial guess.
                       currYear ) );

    while( solver->getIterations() < aLimitIterations ){
        pair<double, bool> trial = solver->getNextValue();
        
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
        // TODO: If the run failed to solve then the target status may be unreliable.
        logRunID();
        success = mSingleScenario->runScenarios( aPeriod, false, aTimer );
    }

    if( solver->getIterations() >= aLimitIterations ){
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Exiting target finding search as the iterations limit " 
                  << "was reached." << endl;
        success = false;
    }
    else if( !success ) {
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Failed due to unsolved model period: " << aPeriod << endl;
    }
    else {
        targetLog.setLevel( ILogger::NOTICE );
        targetLog << "Target value was found by search algorithm in "
                  << solver->getIterations() << " iterations." << endl;
    }
    return success;
}

/*!
 * \brief Solve a target for a year past the target year skipping in-between model
 *        periods.
 * \details For years past the target year the tax must be modified such that
 *          the target remains constant.  A use may want to skip periods to get
 *          a smoother tax path or because it was not feasable which can occur
 *          when too much momentum has built-up in the climate target.
 * \param aTaxes The current tax vector which should be updated.  The tax used in
 *        skipped periods will be linearly interpolated between the last solved
 *        period and the trial being used to solve in aPeriod.
 * \param aPolicyTarget Object which detects if the policy target has been
 *        reached.
 * \param aLimitIterations The maximum number of iterations to perform.
 * \param aTolerance The tolerance of the solution.
 * \param aFirstSkippedPeriod The first model period which was not required to
 *        stay on target.
 * \param aPeriod Future period to get on target.
 * \param aTimer The timer used to print out the amount of time spent performing
 *        operations.
 * \return Whether the target was met successfully.
 */
bool PolicyTargetRunner::skipFuturePeriod( vector<double>& aTaxes,
                                           const ITarget* aPolicyTarget,
                                           const unsigned int aLimitIterations,
                                           const double aTolerance,
                                           const int aFirstSkippedPeriod,
                                           const int aPeriod,
                                           Timer& aTimer )
{
    const Modeltime* modeltime = getInternalScenario()->getModeltime();
    
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    if( aPeriod >= modeltime->getmaxper() ) {
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Attempted to skip beyond the end model year." << endl;
        return false;
    }
    targetLog.setLevel( ILogger::DEBUG );
    targetLog << "Skipping to future target in period " << aPeriod << " from "
              << aFirstSkippedPeriod << "." << endl;
    
    const int lastPeriodToCalc = aPeriod;
    const int currYear = modeltime->getper_to_yr( aPeriod );
    const int lastTaxYear = modeltime->getper_to_yr( aFirstSkippedPeriod - 1 );
    // The last solved price tends to be a better starting point then the last
    // hotelling price.
    aTaxes[ aPeriod ] = aTaxes[ aFirstSkippedPeriod ];
    for( int period = aFirstSkippedPeriod; period < aPeriod; ++period ) {
        const int year = modeltime->getper_to_yr( period );
        aTaxes[ period ] = util::linearInterpolateY( year, lastTaxYear, currYear,
                                                     aTaxes[ aFirstSkippedPeriod - 1 ],
                                                     aTaxes[ aPeriod ] );
        getInternalScenario()->invalidatePeriod( period );
    }
    setTrialTaxes( aTaxes );
    logRunID();
    bool success = mSingleScenario->runScenarios( lastPeriodToCalc, false, aTimer );
    
    // Construct a solver which has an initial trial equal to the current tax.
    unique_ptr<ITargetSolver> solver;
    /* Note that the following code is left commented out incase a user wanted
     to use the bisection routine rather then the secant.
     solver.reset( new Bisecter( aPolicyTarget,
                                 aTolerance,
                                 0,
                                 MAX_SOLVABLE_TAX, // Maximum tax
                                 aTaxes[ aPeriod ],
                                 4.0, // Note the hard coded value is the initial bracket interval
                                 currYear ) );*/
    solver.reset( new Secanter( aPolicyTarget,
                                aTolerance,
                                aTaxes[ aPeriod ],
                                aPolicyTarget->getStatus( currYear ),
                                0.2, // Note the hard coded value is the initial percent change
                                     // for the second initial guess.
                                currYear ) );
    
    while( solver->getIterations() < aLimitIterations ){
        pair<double, bool> trial = solver->getNextValue();
        
        // Check for solution.
        if( trial.second ){
            break;
        }
        
        if( !util::isValidNumber( trial.first ) ) {
            targetLog.setLevel( ILogger::ERROR );
            targetLog << "Failed due to invalid trial price generated by solver." << endl;
            return false;
        }
        
        // Replace the current periods tax with the calculated tax.
        assert( static_cast<unsigned int>( aPeriod ) < aTaxes.size() );
        aTaxes[ aPeriod ] = trial.first;
        for( int period = aFirstSkippedPeriod; period < aPeriod; ++period ) {
            const int year = modeltime->getper_to_yr( period );
            aTaxes[ period ] = util::linearInterpolateY( year, lastTaxYear, currYear,
                                                         aTaxes[ aFirstSkippedPeriod - 1 ],
                                                         aTaxes[ aPeriod ] );
            getInternalScenario()->invalidatePeriod( period );
        }
        
        // Set the trial taxes.
        setTrialTaxes( aTaxes );
        
        // Run the base scenario.
        // TODO: If the run failed to solve then the target status may be unreliable.
        logRunID();
        success = mSingleScenario->runScenarios( lastPeriodToCalc, false, aTimer );
    }
    
    if( solver->getIterations() >= aLimitIterations ){
        targetLog.setLevel( ILogger::ERROR );
        targetLog << "Exiting target finding search as the iterations limit " 
                  << "was reached." << endl;
        success = false;
    }
    else if( !success ) {
        // This is the case that we found the target however the run in which we
        // found the target had periods that did not solve.  If only periods after
        // aPeriod did not solve then we will allow it.
        success = true;
        const vector<int>& unsolvedPeriods = mSingleScenario->getInternalScenario()->getUnsolvedPeriods();
        for( vector<int>::const_iterator it = unsolvedPeriods.begin();
             it != unsolvedPeriods.end() && success; ++it )
        {
            if( aPeriod >= *it ) {
                targetLog.setLevel( ILogger::ERROR );
                targetLog << "Failed due to unsolved model period: " << *it << endl;
                success = false;
            }
        }
    }
    
    if( !success ) {
        targetLog.setLevel( ILogger::NOTICE );
        targetLog << "Target value was found by search algorithm in "
                  << solver->getIterations() << " iterations." << endl;
    }
    return success;
}

void PolicyTargetRunner::printOutput( Timer& aTimer ) const
{
    if( mPolicyCostCalculator.get() ){
        mPolicyCostCalculator->printOutput();
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
void
PolicyTargetRunner::calculateHotellingPath( const double aInitialTax,
                                            const double aHotellingRate,
                                            const Modeltime* aModeltime,
                                            const int aInitialYear,
                                            const int aFinalYear,
                                            vector<double>& aTaxes )
{
    // Only set a tax for periods up to the period of the target year. Calculate
    // the tax for each year. Periods set to zero tax will not be solved.
    int targetPeriod = aModeltime->getyr_to_per( aFinalYear );

    for( int per = aModeltime->getyr_to_per( aInitialYear );
        per <= targetPeriod; per++ )
    {
        // If the last year associated with the current period is greater than
        // the target year, only calculate a hotelling price path up to the
        // target year. Assume the tax is constant at that point.
        const int currYear = min( aModeltime->getper_to_yr( per ), aFinalYear );
        const int numYears = currYear - aInitialYear;
        assert( numYears >= 0 );

        aTaxes[ per ] = min( aInitialTax * pow( 1 + aHotellingRate, numYears ), mMaxTax );
    }
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

/*!
 * \brief Write a unique identifier into each of several log files
 */
void PolicyTargetRunner::logRunID() {
    ILogger& targetLog = ILogger::getLogger( "target_finder_log" );
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    ILogger& climateLog = ILogger::getLogger( "climate-log" );

    ILogger::WarningLevel oldTargetLevel = targetLog.setLevel( ILogger::NOTICE );
    ILogger::WarningLevel oldSolverLevel = solverLog.setLevel( ILogger::NOTICE );
    ILogger::WarningLevel oldMainLevel = mainLog.setLevel( ILogger::NOTICE );
    ILogger::WarningLevel oldClimateLog = climateLog.setLevel( ILogger::NOTICE );

    targetLog << "Policy Target Runner:  scenario dispatch #" << mRunID << endl << endl;
    solverLog << "Policy Target Runner:  scenario dispatch #" << mRunID  << endl << endl;
    mainLog   << "Policy Target Runner:  scenario dispatch #" << mRunID  << endl << endl;
    climateLog   << "Policy Target Runner:  scenario dispatch #" << mRunID  << endl << endl;

    mRunID++;
    
    targetLog.setLevel( oldTargetLevel );
    solverLog.setLevel( oldSolverLevel );
    mainLog.setLevel( oldMainLevel );
    climateLog.setLevel( oldClimateLog ); 
}

