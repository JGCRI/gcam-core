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
* markets which should be solved. This solve method first attempts Newton-Raphson on all markets,
* then uses bracketing and bisection on markets that are not solved by Newton-Raphson. Bracketing
* and bisection solves the unsolved markets or brings prices closer to solution for the Newton-Raphson
* to solve in the next attempt.
* \param period The period to solve.
* \return Whether the markets all solved.
* \author Josh Lurz, Sonny Kim
*/
bool BisectionNRSolver::solve( const int period ) {
    const Configuration* conf = Configuration::getInstance();
    // Tolerance for solution criteria.
    static const double SOLUTION_TOLERANCE = conf->getDouble( "SolutionTolerance", 0.001 );
    // Minimum value below which solution is assumed to be found.
    static const double ED_SOLUTION_FLOOR = conf->getDouble( "SolutionFloor", 0.01 );
    // Tolerance for calibrations.
    static const double CALIBRATION_ACCURACY = conf->getDouble( "CALIBRATION_TOLERANCE", 0.01 );
    // Maximum number of calculations for the solution algorithm.
    static const int MAX_PERIOD_CALCULATIONS = conf->getInt( "MAX_PERIOD_CALCULATIONS", 1000 );

    // Open all log files.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    mainLog.setLevel( ILogger::DEBUG );
    solverLog.setLevel( ILogger::NOTICE );
    singleLog.setLevel( ILogger::DEBUG );
    solverLog << endl << "Solution() Begin. Per " << period << endl;

    // Create and initialize the solution set.
    // This will fetch the markets to solve and update the prices, supplies and demands.
    SolverInfoSet solution_set( marketplace );
    solution_set.init( period ); // determines solvable and unsolvable markets
    solution_set.updateSolvable( false ); // updates solvable and unsolvable markets

    mainLog << "Starting Solution. Solving for " << solution_set.getNumSolvable()
            << " markets." << endl;
    solution_set.printMarketInfo( "Begin Solve", mCalcCounter->getPeriodCount(), singleLog );

    // If no markets to solve, break out of solution.
    if ( solution_set.getNumSolvable() == 0 ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved with last period's prices." << endl;
        return true;
    }
    else if( period == 0 ){
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Period zero has solvable markets." << endl;
    }

    solverLog << "Number of Markets: " << solution_set.getNumSolvable() << endl;
    solverLog << "Solution Information Initialized: Left and Right values are the same." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << solution_set << endl;

    // Initialize solver components
    mLogNewtonRaphson->init();
    mBisectAll->init();
    if( mLogNewtonRaphsonSaveDeriv.get() ){
        mLogNewtonRaphsonSaveDeriv->init();
    }

    // Loop is done at least once.
    do {
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Solution() loop. N: " << mCalcCounter->getPeriodCount() << endl;
        solverLog.setLevel( ILogger::DEBUG );

        // Call Newton Rahpson approach first. Will return right away if all solved.
        static const int MAX_NR_ITER = conf->getInt( "MAX_NR_ITERATIONS", 4 );
        mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_NR_ITER, solution_set, period );

        // If not all solved, then call bracketing and bisection routines.
        if( !solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) )
        {
            // Get only unsolved solution set from the complete set.
            SolverInfoSet unsolved_solution_set( solution_set.getUnsolvedSet( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) );

            unsolved_solution_set.resetBrackets();
            solverLog << "Unsolved solution set before Bracket: " << endl << unsolved_solution_set << endl;
            // Currently attempts to bracket but does not necessarily bracket all markets.
            static const double BRACKET_INTERVAL = conf->getDouble( "bracket-interval", 0.5 );
            SolverLibrary::bracket( marketplace, world, BRACKET_INTERVAL, SOLUTION_TOLERANCE,
                           ED_SOLUTION_FLOOR, unsolved_solution_set, mCalcCounter.get(), period );

            // Attempts bisection on unsolved solution set.
            static const int MAX_BISECT_ITER = conf->getInt( "MAX_BISECTION_ITERATIONS", 20 );
            bool solved = mBisectAll->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_BISECT_ITER,
                          unsolved_solution_set, period ) == SolverComponent::SUCCESS ? true : false;
            solverLog << "Solution after bisect all of unsolved set: " << endl << unsolved_solution_set << endl;

            //solution_set.merge( unsolved_solution_set.getSolvableSet() );
            unsolved_solution_set.updateToMarkets();  // Update new prices to marketplace.
            // Update solvable and unsolvable markets for complete solution set.
            solution_set.updateSolvable( false );
            solution_set.updateFromMarkets(); // Update solution set prices from marketplace.

            solverLog << "Solution after bisect all and update: " << endl << solution_set << endl;
        }

        /*********** Begin: For Calibration Only **************/
        if( Configuration::getInstance()->getBool( "CalibrationActive" ) )
        {
            // If model has solved, then check to see if calibrated -- if not, run world.calc once to move toward calibration
            // This check is inside main solver loop so that full solver routine is available for this stage. If the attempt at calibration fails, this 
            // returns to the main solution loop to solve -- this way this should always exit solved, even if not calibrated. 
            // Note -- only do this if maximum solution_set count has not been exceeded. This way the model will still solve, if perhaps not calibrate.

            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
            calibrationLog.setLevel( ILogger::NOTICE );
            solverLog.setLevel( ILogger::NOTICE );
            // Number of times to try to fix calibration
            static const int MAX_CAL_ATTEMPTS = 10;
            int calFixAttempts = 1;
            if( solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR )
                && mCalcCounter->getPeriodCount() < MAX_PERIOD_CALCULATIONS
                && calFixAttempts <= MAX_CAL_ATTEMPTS )
            {
                solverLog << "***** Starting calibration mode after solving all markets. N = "
                          << mCalcCounter->getPeriodCount() << endl;
                solverLog << ",Calibration fixed attempt: " << calFixAttempts << endl;
                calibrationLog << "***** Starting calibration mode after solving all markets. N = "
                               << mCalcCounter->getPeriodCount() << endl;

                // As long as solution is solved, but not calibrated, then repeat this loop to 
                // try and calibrate.
                // Max number of iterations to allow inner loop to run for calibration fix
                // (this does not have to be terribly small, since loop will exit once model
                // does not solve).
                static const int CAL_LOOP_LIMIT = 10;
                int calInnerLoopCount = 1;
                while( ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false )
                         && solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) )
                         && calInnerLoopCount <= CAL_LOOP_LIMIT
                         && calFixAttempts < MAX_CAL_ATTEMPTS )
                {
                    // While solved, and not calibrated, keep calling world->calc() to try and calibrate
                    // Exit loop if not solved (then need to try and solve), or calibrated.
                    // Maximum numbers of times to try world->calc(). This max is not likely to be approached 
                    // since, after a few world calc calls this generally falls out of solution and this loop is exited.
                    solverLog << ",Calling calibration inner loop (solved but not calibrated): " << calInnerLoopCount << endl;
                    calibrationLog << ",Calling calibration inner loop (solved but not calibrated): " << calInnerLoopCount << endl;
                    static const int MAX_CAL_FIX_LOOP = 10;
                    for( unsigned int i = 1; i <= MAX_CAL_FIX_LOOP 
                        && solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) 
                        && !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ); i++ ){
                        marketplace->nullSuppliesAndDemands( period );
                        world->calc( period );
                        solution_set.updateFromMarkets();
                        solverLog << ",,Calling calibration fix loop (solved but not calibrated): " << i << endl;
                        calibrationLog << ",,Calling calibration fix loop (solved but not calibrated): " << i << endl;
                    }

                    mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_NR_ITER, solution_set, period );
                    ++calInnerLoopCount;
                }

                ++calFixAttempts;
                if ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ) ) {
                    solverLog << "Calibration failed. " << "Returning to full solution loop." << endl;
                }
            }
        }
        /*********** End: For Calibration Only **************/

        // Determine if the model has solved. 
    } while ( !solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR )
              && mCalcCounter->getPeriodCount() < MAX_PERIOD_CALCULATIONS );
    
    if ( Configuration::getInstance()->getBool( "CalibrationActive" )
         && !world->isAllCalibrated( period, CALIBRATION_ACCURACY, true ) ) {
        mainLog.setLevel( ILogger::WARNING );
        solverLog << "Model did not calibrate successfully in period " << period << endl;
        mainLog << "Model did not calibrate successfully in period " << period << endl;
    }

    // Determine whether the model was successful.
    if( solution_set.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved normally. Iterations period "<< period << ": "
                << mCalcCounter->getPeriodCount() << ". Total iterations: "
                << mCalcCounter->getTotalCount() << endl;
        return true;
    }
    
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Model did not solve within set iteration " << mCalcCounter->getPeriodCount() << endl;
    solverLog << "Printing solution information after failed attempt to solve." << endl;
    solverLog << solution_set << endl;

    // Print unsolved markets.
    solution_set.printUnsolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, mainLog );

    if( Configuration::getInstance()->getBool( "debugFindSD" ) ){
        string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "supply_demand_curves" );
        ILogger& sdLog = ILogger::getLogger( logName );
        sdLog.setLevel( ILogger::WARNING );
        sdLog << "Supply and demand curves for markets that did not solve in period: " << period << endl;
        solution_set.findAndPrintSD( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, world, marketplace, period, sdLog );
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
            mBisectOneWorst->solve( solTol, edSolFloor, maxCalcsBisectOne, sol, period );
        }
    }

}
