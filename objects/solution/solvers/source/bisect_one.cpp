/*! 
* \file bisect_one.cpp
* \ingroup objects
* \brief BisectOne class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_one.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/configuration.h"

using namespace std;

const string BisectOne::SOLVER_NAME = "BisectOne";

//! Default Constructor. Constructs the base class. 
BisectOne::BisectOne( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Init method. Currently does nothing.
void BisectOne::init() {
}

//! Get the name of the SolverComponent
const string& BisectOne::getName() const {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& BisectOne::getNameStatic() {
    return SOLVER_NAME;
}

/*! \brief Bisection the worst market.
* \details Bisect the the worst market.
* \param solutionTolerance Target value for maximum relative solution for worst
*        market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored
* \param maxIterations Maximum number of iterations the subroutine will
*        periodform. 
* \param solverSet Object which contains a set of objects with information on
*        each market.
* \param period Model periodiod
*/
SolverComponent::ReturnCode BisectOne::solve( const double solutionTolerance, const double edSolutionFloor,
                                              const unsigned int maxIterations, SolverInfoSet& solverSet,
                                              const int period )
{
    startMethod();

    // Constants.
     // Maximum number of iterations without improvement.
    const static unsigned int MAX_ITER_NO_IMPROVEMENT = 8;
    // Read in the bracket interval until MiniCAM and SGM can consistently solve
    // with the same one.
    const Configuration* conf = Configuration::getInstance();
    static const double BRACKET_INTERVAL = conf->getDouble( "bracket-interval", 0.5 );
    // Setup logging.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );

    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    // Make sure we have all updated information.
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );

    // Select the worst market.
    // SolverInfo& worstSol = solverSet.getWorstSolverInfoReverse( solutionTolerance, 0, true );
    SolverInfo* worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // worstSol.setBisectedFlag();
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "BisectOne function called on market " << worstSol->getName() << "." << endl;
    unsigned int numIterations = 0;
    SolverLibrary::bracketOne( marketplace, world, BRACKET_INTERVAL, solutionTolerance,
                               edSolutionFloor, solverSet, worstSol, calcCounter, period );
    do {
        solverSet.printMarketInfo( "Bisect One on " + worstSol->getName(), calcCounter->getPeriodCount(), singleLog );

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

        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );

        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        addIteration( worstSol->getName(), worstSol->getRelativeED( edSolutionFloor ) );
        worstMarketLog << "BisectOne-MaxRelED: "  << *worstSol << endl;
    } // end do loop        
    while ( isImproving( MAX_ITER_NO_IMPROVEMENT )
            && ++numIterations < maxIterations 
            && !worstSol->isSolved( solutionTolerance, edSolutionFloor ) );
    // Report results.
    solverLog.setLevel( ILogger::NOTICE );
    if( numIterations >= maxIterations ){
        solverLog << "Exiting BisectOne due to reaching max iterations." << endl;
    }
    else if( !isImproving( MAX_ITER_NO_IMPROVEMENT ) ){
        solverLog << "Exiting BisectOne due to lack of improvement." << endl;
    }
    else {
        solverLog << "Exiting BisectOne because chosen market is solved." << endl;
    }
    return solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ? SUCCESS: FAILURE_ITER_MAX_REACHED; // WRONG ERROR CODE
}
