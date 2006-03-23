/*! 
* \file bisect_policy_nr_solver.cpp
* \ingroup objects
* \brief BisectPolicyNRSolver class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <memory>
#include "solution/solvers/include/bisect_policy_nr_solver.h"
#include "containers/include/world.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "solution/util/include/solver_library.h"
#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/solver.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "containers/include/world.h"
#include "util/logger/include/ilogger.h"
using namespace std;

extern Scenario* scenario;

const string BisectPolicyNRSolver::SOLVER_NAME = "BisectPolicyNRSolver";

//! Constructor
BisectPolicyNRSolver::BisectPolicyNRSolver( Marketplace* marketplaceIn, World* worldIn ):Solver( marketplaceIn, worldIn ) {
    // Construct components.
    mCalcCounter.reset( new CalcCounter() );
    mLogNewtonRaphson = SolverComponent::getSolverComponent( "LogNewtonRaphson", marketplace, world, mCalcCounter.get() );
    mBisectAll = SolverComponent::getSolverComponent( "BisectAll", marketplace, world, mCalcCounter.get() );
    mBisectOne = SolverComponent::getSolverComponent( "BisectOne", marketplace, world, mCalcCounter.get() );
    mBisectPolicy = SolverComponent::getSolverComponent( "BisectPolicy", marketplace, world, mCalcCounter.get() );
}

//! Destructor
BisectPolicyNRSolver::~BisectPolicyNRSolver() {
}

//! Get the solver name.
const string& BisectPolicyNRSolver::getName(){
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& BisectPolicyNRSolver::getNameStatic() {
    return SOLVER_NAME;
}

//! Initialize the solver at the beginning of the model.
void BisectPolicyNRSolver::init() {
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
bool BisectPolicyNRSolver::solve( const int period ) {
    // Constants. Make these configuration variables.
    // relative tolerance for solution criteria
    static const double SOLUTION_TOLERANCE = Configuration::getInstance()->getDouble( "SolutionTolerance", 0.001 );
    
    // minimum value below which solution is assumed to be found.
    static const double ED_SOLUTION_FLOOR = Configuration::getInstance()->getDouble( "SolutionFloor", 0.01 );
    static const int MAX_CALCS = 5000;
    static const int MAX_CALCS_BISECT_ONE = 35;
    static const int MAX_CALCS_NR = 1000; // Should be based on number of markets.
    static unsigned int MAX_BISECT_ONE_MARKETS = 2;
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

    // Loop is done at least once.
    do {
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Solution() loop. N: " << mCalcCounter->getPeriodCount() << endl;
        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution before BisectPolicy: " << endl;
        solverLog << sol << endl;
        
        // Bisect the policy market or the worst market if the policy market is non-existant.
        sol.unsetBisectedFlag();
        mBisectPolicy->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );

        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution before NewtonRaphson: " << endl;
        solverLog << sol << endl;

        // Call mLogNewtonRaphson. Ignore return code because it may have skipped singular markets.
        mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );

        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "After NewtonRaphson " << mCalcCounter->getPeriodCount() << endl;
        solverLog << sol << endl;

        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            unsigned int count = 0;
            while( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && count < MAX_BISECT_ONE_MARKETS && ( sol.hasSingularUnsolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) 
                || sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) > 1 ) ){
                // Try bisecting a single market.
                mBisectOne->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );
                ++count;
                }
                cout << "Exiting Bisect-One loop. Count: " << count << " HasSingularUnsolved: " << sol.hasSingularUnsolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) << endl;
        }

        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution before NewtonRaphson: " << endl;
        solverLog << sol << endl;
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            // Call mLogNewtonRaphson. Ignore return code because it may have skipped singular markets.
            mLogNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );
        }
        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "After NewtonRaphson " << mCalcCounter->getPeriodCount() << endl;
        solverLog << sol << endl;
    // Determine if the model has solved. 
    } while ( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && mCalcCounter->getPeriodCount() < MAX_CALCS );
    
    mainLog.setLevel( ILogger::NOTICE );
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

