/*! 
* \file bisect_policy.cpp
* \ingroup objects
* \brief BisectPolicy class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_policy.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

using namespace std;

const string BisectPolicy::SOLVER_NAME = "BisectPolicy";

//! Default Constructor. Constructs the base class. 
BisectPolicy::BisectPolicy( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Init method. Currently does nothing.
void BisectPolicy::init() {
}

//! Get the name of the SolverComponent
const string& BisectPolicy::getName() const {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& BisectPolicy::getNameStatic() {
    return SOLVER_NAME;
}

/*! \brief Bisection the worst market.
* \details Bisect the the worst market.
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored
* \param maxIterations Maximum number of iterations the subroutine will periodform. 
* \param solverSet Object which contains a set of objects with information on each market.
* \param period Model periodiod
*/
SolverComponent::ReturnCode BisectPolicy::solve( const double solutionTolerance, const double edSolutionFloor,
                                                 const unsigned int maxIterations, SolverInfoSet& solverSet,
                                                 const int period )
{
    startMethod();

    // Constants.
    const static unsigned int MAX_ITER_NO_IMPROVEMENT = 8; // Maximum number of iterations without improvement.
    const static double BRACKET_INTERVAL = 0.1;
    // Setup logging.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "BisectPolicy function called." << endl;
    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    // Make sure we have all updated information.
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );

    // Select the worst market.
    SolverInfo* worstSol = solverSet.getPolicyOrWorstSolverInfo( solutionTolerance, edSolutionFloor );
    worstSol->setBisectedFlag();
    unsigned int numIterations = 0;
    if( !worstSol->isSolved( solutionTolerance, edSolutionFloor ) ){
        SolverLibrary::bracketOne( marketplace, world, BRACKET_INTERVAL, solutionTolerance,
                                   edSolutionFloor, solverSet, worstSol, period );
        if( !worstSol->isSolved( solutionTolerance, edSolutionFloor ) ){
            do {
                solverSet.printMarketInfo( "BisectPolicy" + worstSol->getName(), calcCounter->getPeriodCount(), singleLog );

                // Move the left bracket in if Supply > Demand
                if ( worstSol->getED() < 0 ) {
                    worstSol->moveLeftBracketToX();
                }
                // Move the right bracket in if Demand >= Supply
                else {
                    worstSol->moveRightBracketToX();
                }
                // Set new trial value to center
                worstSol->setPriceToCenter();

                // price=0 and supply>demand. only true for constraint case
                // other markets cannot have supply>demand as price->0
                // Another condition that should be moved. 
                if ( fabs( worstSol->getPrice() ) < util::getSmallNumber() && worstSol->getED() < 0 ) { 
                    worstSol->setPrice( 0 ); 
                } 

                solverSet.updateToMarkets();
                marketplace->nullSuppliesAndDemands( period );

                world->calc( period );
                solverSet.updateFromMarkets();
                solverSet.updateSolvable( false );
                addIteration( worstSol->getName(), worstSol->getRelativeED( edSolutionFloor ) );
                worstMarketLog << "BisectPolicy-MaxRelED: "  << *worstSol << endl;
            } // end do loop        
            while ( isImproving( MAX_ITER_NO_IMPROVEMENT ) &&
                ( ++numIterations < maxIterations ) &&
                !worstSol->isWithinTolerance( solutionTolerance, edSolutionFloor ) );
        }
    }
    // Report results.
    solverLog.setLevel( ILogger::NOTICE );
    if( numIterations >= maxIterations ){
        solverLog << "Exiting BisectPolicy due to reaching max iterations." << endl;
    }
    else if( !isImproving( MAX_ITER_NO_IMPROVEMENT ) ){
        solverLog << "Exiting BisectPolicy due to lack of improvement." << endl;
    }
    else {
        solverLog << "Exiting BisectPolicy because chosen market is solved." << endl;
    }
    return solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ? SUCCESS: FAILURE_ITER_MAX_REACHED; // WRONG ERROR CODE
}
