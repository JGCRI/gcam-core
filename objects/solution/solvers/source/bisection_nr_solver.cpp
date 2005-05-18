/*! 
* \file bisection_nr_solver.cpp
* \ingroup objects
* \brief BisectionNRSolver class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
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
* the prices into the range of the solution, and then uses Newton-Rhaphson to clear the markets.
* \param period The period to solve.
* \return Whether the markets all solved.
*/
bool BisectionNRSolver::solve( const int period ) {
    // Constants. Make these configuration variables.
    // relative tolerance for solution criteria
    const Configuration* conf = Configuration::getInstance();
    static const double SOLUTION_TOLERANCE = conf->getDouble( "SolutionTolerance", 0.001 );
    
    // relative tolerance for calibrations
    const double CALIBRATION_ACCURACY = SOLUTION_TOLERANCE * 4 ;
    // Adds significant time if CALIBRATION_ACCURACY = SOLUTION_TOLERANCE. If CALIBRATION_ACCURACY > SOLUTION_TOLERANCE then speeds things up quite a bit.
    // The calibration numbers are generally not good to this accuracy in any event, so having a slightly higher CALIBRATION_ACCURACY should be ok.
    
    // minimum value below which solution is assumed to be found.
    static const double ED_SOLUTION_FLOOR = conf->getDouble( "SolutionFloor", 0.01 );
    
    // Read in the bracket interval until MiniCAM and SGM can consistently solve
    // with the same one.
    static const double BRACKET_INTERVAL = conf->getDouble( "bracket-interval", 0.5 );
    static const double MAX_REL_ED_FOR_NR = 10000;
    static const double MIN_ED_FOR_BISECT_ALL = 1;
    static const int MAX_CALCS = 1000;
    static const int MAX_CALCS_BISECT_ALL = 25;
    static const int MAX_CALCS_BISECT_ONE = 35;
    static const int MAX_CALCS_NR = 500; // Should be based on number of markets.
    static const unsigned int CAL_REPEAT_LIMIT = 20;
 
    // Create and initialize the SolutionInfoSet. 
    // This will fetch the markets to solve and update the prices, supplies and demands.
    SolverInfoSet sol( marketplace );
    sol.init( period );
    sol.updateSolvable( false );
    
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::DEBUG );
    mainLog << "Starting Solution. Solving for " << sol.getNumSolvable() << " markets." << endl;

    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    mainLog.setLevel( ILogger::DEBUG );
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
            singleLog << "Begin Bracketing" << mCalcCounter->getPeriodCount() << endl;
            SolverLibrary::bracketAll( marketplace, world, BRACKET_INTERVAL, SOLUTION_TOLERANCE,
                                       ED_SOLUTION_FLOOR, sol, period );
            singleLog << "End Bracketing" << mCalcCounter->getPeriodCount() << endl;
            // If its not all bracketed, jump to the top of the loop.
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

        // If we are near the solution, call NR.
        if( sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) < MAX_REL_ED_FOR_NR ) {
            solverLog.setLevel( ILogger::DEBUG );
            solverLog << "Solution before NewtonRaphson: " << endl;
            solverLog << sol << endl;
            // Call LogNewtonRaphson. Ignoring return code. 
            mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );

            solverLog.setLevel( ILogger::DEBUG );
            solverLog << "Solution after NewtonRaphson: " << endl;
            solverLog << sol << endl;
        }
        
        // Try bisecting a single market. Added the loop for SGM, see if it can be removed.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            for( unsigned int k = 0; k < 3; ++k ){
                mBisectOne->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );
            }
        }
        
        // If we are near the solution, call NR.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) < MAX_REL_ED_FOR_NR ) {
            solverLog.setLevel( ILogger::DEBUG );
            solverLog << "Solution before NewtonRaphson: " << endl;
            solverLog << sol << endl;

            // Call LogNewtonRaphson. Ignoring return code. 
            mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );

            solverLog.setLevel( ILogger::DEBUG );
            solverLog << "Solution after NewtonRaphson: " << endl;
            solverLog << sol << endl;
        }
        
        // Try bisecting a single market.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            mBisectOne->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );
        }
        // Determine if the model has solved. 
    } while ( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && mCalcCounter->getPeriodCount() < MAX_CALCS );
    
    // Only calibrate if the model solved.
    if( sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
        // Make sure calibration was achieved
        unsigned int calCount = 0;
        while( ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, false ) ||
            !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ) && calCount < CAL_REPEAT_LIMIT ){
            solverLog.setLevel( ILogger::NOTICE );
            solverLog << "Repeating to calibrate. N = " << mCalcCounter->getPeriodCount() <<  endl;
            world->calc( period );
            mLogNewtonRaphsonSaveDeriv->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );
            ++calCount;
        }

        mainLog.setLevel( ILogger::ERROR );
        if ( !world->isAllCalibrated( period, CALIBRATION_ACCURACY, true ) ) {
            mainLog << "Model did not calibrate sucesfully in period: " << period << endl;
            return false;
        }
    }
    // Determine whether the model was successful.
    if( sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
        mainLog << "Model solved normally. Iterations period "<< period << ": " << mCalcCounter->getPeriodCount() << ". Total iterations: "<< mCalcCounter->getTotalCount() << endl;
        return true;
    }

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

