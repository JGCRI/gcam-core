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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file bisection_nr_solver.cpp
* \ingroup objects
* \brief BisectionNRSolver class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>

#include "solution/solvers/include/bisection_nr_solver.h"
#include "containers/include/world.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "solution/util/include/solver_library.h"
#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/solver.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "containers/include/world.h"
#include "util/logger/include/ilogger.h"
#include "solution/util/include/calc_counter.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
BisectionNRSolver::BisectionNRSolver( Marketplace* aMarketplace, World* aWorld ):Solver( aMarketplace, aWorld ){
    // Construct components.
    mCalcCounter.reset( new CalcCounter() );
    mLogNewtonRaphson = SolverComponent::getSolverComponent( "LogNewtonRaphson", marketplace, world, mCalcCounter.get() );
    mBisectAll = SolverComponent::getSolverComponent( "BisectAll", marketplace, world, mCalcCounter.get() );
    mBisectOne = SolverComponent::getSolverComponent( "BisectOne", marketplace, world, mCalcCounter.get() );
    if( scenario->getWorld()->getCalibrationSetting() ){
        mLogNewtonRaphsonSaveDeriv = SolverComponent::getSolverComponent( "LogNewtonRaphsonSaveDeriv", marketplace, world, mCalcCounter.get() );
    }
}

//! Destructor
BisectionNRSolver::~BisectionNRSolver() {
}

//! Get the solver name.
const string& BisectionNRSolver::getName(){
    const static string name = "BisectionNRSolver";
    return name;
}

//! Initialize the solver at the beginning of the model.
void BisectionNRSolver::init() {
    // Set the pointer into the world for the CalcCounter so that it is automatically updated.
    world->setCalcCounter( mCalcCounter.get() );
}

/*! \brief Solution method to solve all markets for one period.
* \details This is the main solution function called from within the Marketplace. It is called once for each period to clear all 
* markets which should be solved. This solve method first brackets the markets, then uses several iterations of bisection_all to move 
* the prices into the range of the solution, and then uses Newton-Raphson to clear the markets.
* \param period The period to solve.
* \return Whether the markets all solved.
*/
bool BisectionNRSolver::solve( const int period ) {
    // Constants. Make these configuration variables.
    // relative tolerance for solution criteria
    const Configuration* conf = Configuration::getInstance();
    static const double SOLUTION_TOLERANCE = conf->getDouble( "SolutionTolerance", 0.001 );
    
    // relative tolerance for calibrations
    const double CALIBRATION_ACCURACY = max( 0.0001, SOLUTION_TOLERANCE * 5 );
    // Note, adds significant time if CALIBRATION_ACCURACY = SOLUTION_TOLERANCE. If CALIBRATION_ACCURACY > SOLUTION_TOLERANCE then speeds things up quite a bit. sjs
    // Set calibration accuracy to 0.01%, which is arbitrary, but generally achievable by the model. Max function used in case that solution tolerance is set particularly high.
    // The calibration numbers are generally not good to this accuracy in any event, so having a slightly higher CALIBRATION_ACCURACY should be ok.
    
    // minimum value below which solution is assumed to be found.
    static const double ED_SOLUTION_FLOOR = conf->getDouble( "SolutionFloor", 0.01 );
    
    // Read in the bracket interval until MiniCAM and SGM can consistently solve
    // with the same one.
    static const double BRACKET_INTERVAL = conf->getDouble( "bracket-interval", 0.5 );
    static const double MAX_REL_ED_FOR_NR = 10000;
    static const double MIN_ED_FOR_BISECT_ALL = 1;
    static const int MAX_CALCS = 2000;
    static const int MAX_CALCS_BISECT_ALL = 25;
    static const int MAX_CALCS_BISECT_ONE = 35;
    static const int MAX_CALCS_NR = 500; // Should be based on number of markets.
    static const int CAL_LOOP_LIMIT = 50; // Max number of iterations to allow inner loop to run for calibration fix (this does not have to be terribly small, since loop will exit once model does not solve)
    static const int MAX_CAL_ATTEMPTS = 50; // Number of times to try to fix calibration
    
    int maxTotalCalcs = MAX_CALCS; // put this into a variable so can be reset by calibration adjustment if necessary
    int calFixAttempts = 0;
    
    // Create and initialize the SolutionInfoSet. 
    // This will fetch the markets to solve and update the prices, supplies and demands.
    SolverInfoSet sol( marketplace );
    sol.init( period );
    sol.updateSolvable( false );
    
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Starting Solution. Solving for " << sol.getNumSolvable() << " markets." << endl;

    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );
    sol.printMarketInfo( "Begin Solve", mCalcCounter->getPeriodCount(), singleLog );
    
    // if no markets to solve, break out of solution.
    if ( sol.getNumSolvable() == 0 ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved with last period's prices." << endl;
        return true;
    }
    else if( period == 0 ){
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Period zero has solvable markets." << endl;
    }

    // Print out extra debugging information.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Solution() Begin. Per " << period << endl;
    solverLog << "Number of Markets: " << sol.getNumSolvable() << endl;
    solverLog << "Solution Information Initialized: Left and Right values are the same." << endl;
    
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << sol << endl;

    // Initialize solver components
    mLogNewtonRaphson->init();
    mBisectAll->init();
    mBisectOne->init();
    if( mLogNewtonRaphsonSaveDeriv.get() ){
        mLogNewtonRaphsonSaveDeriv->init();
    }
    
    // Loop is done at least once.
    do {
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Solution() loop. N: " << mCalcCounter->getPeriodCount() << endl;
        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution before BracketAll: " << endl;
        solverLog << sol << endl;

        // Check if the solution is bracketed.
        if ( !sol.isAllBracketed() ) {
            SolverLibrary::bracketAll( marketplace, world, BRACKET_INTERVAL, SOLUTION_TOLERANCE,
                                       ED_SOLUTION_FLOOR, sol, mCalcCounter.get(), period );
            // If its not all bracketed, jump to the top of the loop.
            // Solve continues to call bracketAll until all solvable markets are bracketed
            if ( !sol.isAllBracketed() ){
                continue;
            }
        }

        // If the solution is not near, use bisect all.
        if( sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) > MIN_ED_FOR_BISECT_ALL ) {
            bool solved = mBisectAll->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ALL, sol, period ) == SolverComponent::SUCCESS ? true : false;
            if( solved ){
                continue;
            }
        }

        // Sequence for calling NR and they try bisecting a single market if not solved.
        NRandSingleBisect( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_REL_ED_FOR_NR, MAX_CALCS_NR, MAX_CALCS_BISECT_ONE, sol, 3, period );
        
        // Repeat this solution sequence once to allow for bisect one to occur and Newton-Raphson to try to solve 
        // again before the top of the loop is reached and potentially bracket_all
        NRandSingleBisect( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_REL_ED_FOR_NR, MAX_CALCS_NR, MAX_CALCS_BISECT_ONE, sol, 1, period );

        // If model has solved, then check to see if calibrated -- if not, run world.calc once to move toward calibration
        // This check is inside main solver loop so that full solver routine is available for this stage. If the attempt at calibration fails, this 
        // returns to the main solution loop to solve -- this way this should always exit solved, even if not calibrated. 
        // Note -- only do this if maximum sol count has not been exceeded. This way the model will still solve, if perhaps not calibrate.

        if( sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR )
            && mCalcCounter->getPeriodCount() < maxTotalCalcs && calFixAttempts < MAX_CAL_ATTEMPTS )
        {
            // Make sure calibration was achieved
            int calInnerLoopCount = 0;
            // As long as solution is solved, but not calibrated, then repeat this loop to try and calibrate
            while( ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ) && sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) )
                    && calInnerLoopCount < CAL_LOOP_LIMIT && calFixAttempts < MAX_CAL_ATTEMPTS ){
                solverLog.setLevel( ILogger::NOTICE );
                ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
                calibrationLog.setLevel( ILogger::NOTICE );
                solverLog << "Repeating solution to finish calibration. N = " << mCalcCounter->getPeriodCount() <<  endl;
                calibrationLog << "Repeating solution to finish calibration. N = " << mCalcCounter->getPeriodCount() <<  endl;

                // While solved, and not calibrated, keep calling world->calc() to try and calibrate. Exit loop if not solved (then need to try and solve), or calibrated.
                static const int MAX_CAL_FIX_LOOP = 10; // Maximum numbers of times to try world->calc(). This max is not likely to be approached since, after a few world calc calls this generally falls out of solution and this loop is exited.
                for( unsigned int i = 0; i < MAX_CAL_FIX_LOOP && sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ); i++ ){
                    marketplace->nullSuppliesAndDemands( period );
                    world->calc( period );
                    sol.updateFromMarkets();
                }
                NRandSingleBisect( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_REL_ED_FOR_NR, MAX_CALCS_NR, MAX_CALCS_BISECT_ONE, sol, 3, period );
                ++calInnerLoopCount;
                
                // If ended up exceeding maximum calculation limit then add one to that so that solver can go back and solve
                if ( mCalcCounter->getPeriodCount() > maxTotalCalcs ) { 
                    maxTotalCalcs = mCalcCounter->getPeriodCount() + 1;
                    calInnerLoopCount = CAL_LOOP_LIMIT; // If past count then exit the calibration loop
                }
            }

            ++calFixAttempts;
            if ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ) ) {
                solverLog << "Model calibration procedure did not succeed, returning to full solution loop." << endl;
            }
        }

        // Reset brackets before next iteration through the loop
        // This method labels all markets as unbracketed
        // Without this step, model enters bisection unbracketed
        // TODO: really just want this to check brackets, not reset
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            sol.checkAndResetBrackets();
        }

        // Determine if the model has solved. 
    } while ( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR )
              && mCalcCounter->getPeriodCount() < maxTotalCalcs );
    
    if ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, true ) ) {
        mainLog.setLevel( ILogger::WARNING );
        solverLog << "Model did not calibrate successfully in period " << period << endl;
        mainLog << "Model did not calibrate successfully in period " << period << endl;
    }

    // Determine whether the model was successful.
    if( sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved normally. Iterations period "<< period << ": " << mCalcCounter->getPeriodCount() << ". Total iterations: "<< mCalcCounter->getTotalCount() << endl;
        return true;
    }
    
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Model did not solve within set iteration " << mCalcCounter->getPeriodCount() << endl;
    solverLog << "Printing solution information after failed attempt to solve." << endl;
    solverLog << sol << endl;

    // Print unsolved markets.
    sol.printUnsolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, mainLog );

    if( Configuration::getInstance()->getBool( "debugFindSD" ) ){
        string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "supply_demand_curves" );
        ILogger& sdLog = ILogger::getLogger( logName );
        sdLog.setLevel( ILogger::WARNING );
        sdLog << "Supply and demand curves for markets that did not solve in period: " << period << endl;
        sol.findAndPrintSD( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, world, marketplace, period, sdLog );
    }
    return false;
}

/*! \brief Paired set of calls to mLogNewtonRaphson and single market bisect.
* \details This pair of calls is used several times, so this is encapsulated. 
* \param solTol The period to solve.
* \param edSolFloor Excess demand solution floor
* \param maxNRRelED Maximum excess demand for NR routine
* \param maxCalcsNR Maximum number of calcs for NR routine
* \param maxCalcsBisectOne Maximum number of calcs for bisect one market routine
* \param sol The solution object
* \param aNumBisectOneCalls Number of times to call the bisect one function.
* \param period The period to solve
* \author Steven Smith
*/
void BisectionNRSolver::NRandSingleBisect( const double solTol, const double edSolFloor, const double maxNRRelED, 
                                             const int maxCalcsNR, const int maxCalcsBisectOne, SolverInfoSet& sol,
                                             const unsigned int aNumBisectOneCalls, const int period) {

    ILogger& solverLog = ILogger::getLogger( "solver_log" );

    // If we are near the solution, call NR.
    if( !sol.isAllSolved( solTol, edSolFloor ) && sol.getMaxRelativeExcessDemand( edSolFloor ) < maxNRRelED ) {
        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution before NewtonRaphson: " << endl;
        solverLog << sol << endl;

        // Call LogNewtonRaphson. Ignoring return code. 
        mLogNewtonRaphson->solve( solTol, edSolFloor, maxCalcsNR, sol, period );

        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution after NewtonRaphson: " << endl;
        solverLog << sol << endl;
    }

    // If not solved OR not calibrated, try bisecting a single market. Added the
    // loop for SGM, see if it can be removed.
    if( !sol.isAllSolved( solTol, edSolFloor ) ){
        for( unsigned int k = 0; k < aNumBisectOneCalls; ++k ){
            mBisectOne->solve( solTol, edSolFloor, maxCalcsBisectOne, sol, period );
        }
    }

}
