/*! 
* \file bisect_all.cpp
* \ingroup objects
* \brief BisectAll class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_all.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

using namespace std;

const string BisectAll::SOLVER_NAME = "BisectAll";

//! Default Constructor. Constructs the base class. 
BisectAll::BisectAll( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Init method. Currently does nothing.
void BisectAll::init() {
}

//! Get the name of the SolverComponent
const string& BisectAll::getNameStatic() {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& BisectAll::getName() const {
    return SOLVER_NAME;
}

/*! \brief Bisection Solution Mechanism (all markets)
* \details This solution mechanism bisects all markets at once. 
* \todo Update this documentation.
* Bisection is always periodformed at least a few times. Bisection stops if the maximum 
* relative ED does not change at a rate larger than BREAK_OUT_THRESHOLD.
* If the maximum relative "ED" is larger than BRACKET_THRESHOLD, then the unsolved markets
* are re-bracketed. Then bisection continues. The bracketing interval is smaller than that
* used initially so as to not periodturb trial values too much. If a further re-bracket
* is necessary, the bracketing interval is decreased further. 
*
* Also, the price and demand markets are very prone to move outside their brackets.
* A check for that is periodformed each time and the brackets are adjusted accordingly.
* This check is critical for solution with simultuantey.
*
* Tracking the excess demand is turned on from the logging configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \todo need more general way to reset price and demand market types within bisect
* \todo implement check on price and demand markets within bracket?
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored
* \param maxIterations Maximum number of iterations the subroutine will periodform. 
* \param solverSet Object which contains a set of objects with information on each market.
* \param period Model periodiod
*/
SolverComponent::ReturnCode BisectAll::solve( const double solutionTolerance, const double edSolutionFloor,
                                              const unsigned int maxIterations, SolverInfoSet& solverSet,
                                              const int period )
{
    startMethod();

    unsigned int numIterations = 0; // number of iterations
    ReturnCode code = ORIGINAL_STATE; // code that reports success 1 or failure 0
    const static unsigned int MAX_ITER_NO_IMPROVEMENT = 10; // Maximum number of iterations without improvement.
    
    // Setup Logging.
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    worstMarketLog.setLevel( ILogger::NOTICE );
    solverLog << "Bisection_all routine starting" << endl; 

    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );
    
    // Select the worst market.
    SolverInfo* worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // solve all markets
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    do {
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "BisectionAll " << numIterations << endl;
        solverSet.printMarketInfo( "Bisect All", calcCounter->getPeriodCount(), singleLog );

        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); ++i ) {
            SolverInfo& currSol = solverSet.getSolvable( i );
            if ( !currSol.isWithinTolerance( solutionTolerance, edSolutionFloor ) ) { // if haven't solved
                // Move the left bracket in if Supply > Demand
                if ( currSol.getED() < 0 ) {
                    currSol.moveLeftBracketToX();
                }
                // Move the right bracket in if Demand >= Supply
                else {
                    currSol.moveRightBracketToX();
                }
                // Set new trial value to center
                currSol.setPriceToCenter();
            }	
            // price=0 and supply>demand. only true for constraint case
            // other markets cannot have supply>demand as price->0
            // Another condition that should be moved. 
            if ( fabs( currSol.getPrice() ) < util::getSmallNumber() && currSol.getED() < 0 ) { 
                currSol.setPrice( 0 ); 
            } 
        }

        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );

        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        // The  price and demand markets are very prone to moving beyond their brackets. 
        // So check and adjust if needed. Lines below check if XL < Demand, or XR > Demand, 
        // and move brackets if necessary. A more general  bracket check is below, but this
        // is needed more often and can be done simply.
        solverSet.adjustBrackets();
        
        const SolverInfo* maxSol = solverSet.getWorstSolverInfo( edSolutionFloor );
        addIteration( maxSol->getName(), maxSol->getRelativeED( edSolutionFloor ) );
        worstMarketLog << "BisectAll-maxRelED: " << *maxSol << endl;
    } // end do loop		
    while ( isImproving( MAX_ITER_NO_IMPROVEMENT ) 
            && ++numIterations < maxIterations 
            && !solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) );

    // Set the return code. 
    code = ( solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) < solutionTolerance ? SUCCESS : FAILURE_ITER_MAX_REACHED ); // report success, or failure
    
    // Report exit conditions.
    solverLog.setLevel( ILogger::NOTICE );
    if ( numIterations >= maxIterations ){
        solverLog << "Exiting BisectionAll due to reaching the maximum number of iterations." << endl;
    }
    else if( !isImproving( MAX_ITER_NO_IMPROVEMENT ) ){
        solverLog << "Exiting BisectionAll due to lack of improvement in relative excess demand." << endl;
    }
    else {
        solverLog << "Exiting BisectionAll with model fully solved." << endl;
    }
    return code;
}
