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
#include "util/logger/include/logger.h"
#include "util/logger/include/logger_factory.h"

using namespace std;

extern Scenario* scenario;
extern ofstream bugoutfile, logfile;

//! Constructor
BisectionNRSolver::BisectionNRSolver( Marketplace* marketplaceIn, World* worldIn ):Solver( marketplaceIn, worldIn ) {
    // Setup configuration options.
    const Configuration* conf = Configuration::getInstance();
    bugTracking = conf->getBool( "bugTracking" );
    bugMinimal = conf->getBool( "bugMinimal" );
    trackED = conf->getBool( "trackMaxED" );

    // Construct components.
    logNewtonRaphson = SolverComponent::getSolverComponent( "LogNewtonRaphson", marketplace, world, &calcCounter );
    bisectAll = SolverComponent::getSolverComponent( "BisectAll", marketplace, world, &calcCounter );
    bisectOne = SolverComponent::getSolverComponent( "BisectOne", marketplace, world, &calcCounter );
}

//! Destructor
BisectionNRSolver::~BisectionNRSolver() {
}

//! Initialize the solver at the beginning of the model.
void BisectionNRSolver::init() {
    // Set the pointer into the world for the CalcCounter so that it is automatically updated.
    world->setCalcCounter( &calcCounter );
}

/*! \brief Solution method to solve all markets for one period.
* \details This is the main solution function called from within the Marketplace. It is called once for each period to clear all 
* markets which should be solved. This solve method first brackets the markets, then uses several iterations of bisection_all to move 
* the prices into the range of the solution, and then uses Newton-Rhaphson to clear the markets.
* \param period The period to solve.
* \return Whether the markets all solved.
*/
bool BisectionNRSolver::solve( const int period ) {
    bool solved = false; // whether the model has solved.
    SolverComponent::ReturnCode code = SolverComponent::ORIGINAL_STATE;

    // Constants. Make these configuration variables.
    static const double SOLUTION_TOLERANCE = 0.001; // tolerance for solution criteria
    static const double ED_SOLUTION_FLOOR = 0.01; // minimum value below which solution is assumed to be found.
    static const double BRACKET_INTERVAL = 0.5;
    static const double MAX_REL_ED_FOR_NR = 10000;
    static const double MIN_ED_FOR_BISECT_ALL = 1;
    static const int MAX_CALCS = 1000;
    static const int MAX_CALCS_BISECT_ALL = 25;
    static const int MAX_CALCS_BISECT_ONE = 35;
    static const int MAX_CALCS_NR = 500; // Should be based on number of markets.

    Configuration* conf = Configuration::getInstance();
    trackED = conf->getBool( "trackMaxED" ); //!< Get parameter to turn on (or not) solution mechanism tracking (to cout)

    // Create and initialize the SolutionInfoSet. 
    // This will fetch the markets to solve and update the prices, supplies and demands.
    SolverInfoSet sol( marketplace );
    sol.init( period );
    sol.updateSolvable( false );

    logfile << ",Starting Solution. Solving for " << sol.getNumSolvable() << " markets." << endl;
    sol.printMarketInfo( "Begin Solve", calcCounter.getPeriodCount() );
    
    // if no markets to solve, break out of solution.
    if ( sol.getNumSolvable() == 0 ) {
        cout << "Model solved with last period's prices"; 
        return true;
    }


    // for debugging
    if ( bugMinimal ) {
        bugoutfile << "Solution() Begin. Per " << period << endl;
        bugoutfile <<"Number of Markets: "<< sol.getNumSolvable() << endl;
        bugoutfile <<"Solution Information Initialized: Left and Right values are same. "<< endl;
        bugoutfile << sol << endl;
    }

    // Loop is done at least once.
    do {
        if ( bugTracking ) {
            bugoutfile << "Solution() loop. N: " << calcCounter.getPeriodCount() << endl;
        }

        // Check if the brackets are correct. If not, reset.
        sol.checkAndResetBrackets();
        if( bugTracking ){
            bugoutfile << "CheckBracket() called: Left and Right sides reinitialized if unbracketed. " << endl;
        }
        if( trackED ){
            cout << "CheckBracket() called. Resetting left and right brackets." << endl;
        }

        // Check if the solution is bracketed.
        if ( !sol.isAllBracketed() ) {
            sol.printMarketInfo( "Begin Bracketing", calcCounter.getPeriodCount() );
            SolverLibrary::bracket( marketplace, world, BRACKET_INTERVAL, sol, period );
            sol.printMarketInfo( "End Bracketing", calcCounter.getPeriodCount() );
            // If its not all bracketed, jump to the top of the loop.
            if ( !sol.isAllBracketed() ){
                continue;
            }
        }
        
        // If the solution is not near, use bisect all.
        if( sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) > MIN_ED_FOR_BISECT_ALL ) {
            solved = bisectAll->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ALL, sol, period ) == SolverComponent::SUCCESS ? true : false;
            if( solved ){
                continue;
            }
        }

        // If we are near the solution, call NR.
        if( sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) < MAX_REL_ED_FOR_NR ) {
            // Debug output
            if ( bugMinimal ) {
                bugoutfile << endl << "Solution before NR_Ron: " << endl;
                bugoutfile << sol << endl;
            }

            // Call LogNewtonRaphson. Ignoring return code. 
            logNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );

            // Debug output
            if ( bugMinimal || bugTracking ) { 
                bugoutfile << "After Ron_NR "<< calcCounter.getPeriodCount() << endl;
            }
            if ( bugMinimal ) {
                bugoutfile << endl << "Solution after NR_Ron: " << endl;
                bugoutfile << sol << endl;
            }
        }
        
        // Try bisecting a single market.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            bisectOne->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );
        }
        
        // If we are near the solution, call NR.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && sol.getMaxRelativeExcessDemand( ED_SOLUTION_FLOOR ) < MAX_REL_ED_FOR_NR ) {
            // Debug output
            if ( bugMinimal ) {
                bugoutfile << endl << "Solution before NR_Ron: " << endl;
                bugoutfile << sol << endl;
            }

            // Call LogNewtonRaphson. Ignoring return code. 
            logNewtonRaphson->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_NR, sol, period );

            // Debug output
            if ( bugMinimal || bugTracking ) { 
                bugoutfile << "After Ron_NR "<< calcCounter.getPeriodCount() << endl;
            }
            if ( bugMinimal ) {
                bugoutfile << endl << "Solution after NR_Ron: " << endl;
                bugoutfile << sol << endl;
            }
        }
        
        // Try bisecting a single market.
        if( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
            bisectOne->solve( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, MAX_CALCS_BISECT_ONE, sol, period );
        }
        // Determine if the model has solved. 
    } while ( !sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) && calcCounter.getPeriodCount() < MAX_CALCS );

    if( sol.isAllSolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR ) ){
        cout << "Model solved normally. Iterations this period: " << calcCounter.getPeriodCount() << ". Total iterations: "<< calcCounter.getTotalCount() << endl;
        logfile << ",Model solved normally: worldCalcCount = " << calcCounter.getPeriodCount() << "; Cumulative = "<< calcCounter.getTotalCount() << endl;
        return true;
    }
    else { // SolverComponent::FAILURE:
        cout << "Model did not solve within set iteration " << calcCounter.getPeriodCount() << endl;
        logfile << ",Model did not solve within set iteration " << calcCounter.getPeriodCount() << endl;
        logfile << sol << endl;

        // Print unsolved markets.
        sol.printUnsolved( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR );

        const Configuration* conf = Configuration::getInstance();
        if( conf->getBool( "debugFindSD" ) ){
            string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "SDCurves.csv" );
            Logger* sdLog = LoggerFactory::getLogger( logName );
            LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and demand curves for markets that did not solve in period: " << period << endl;
            sol.findAndPrintSD( SOLUTION_TOLERANCE, ED_SOLUTION_FLOOR, world, marketplace, period, sdLog );
        }
        return false;
    }
}

