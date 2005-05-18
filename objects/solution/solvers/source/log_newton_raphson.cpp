/*! 
* \file log_newton_raphson.cpp
* \ingroup objects
* \brief LogNewtonRaphson class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/log_newton_raphson.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

#include <mtl/matrix.h>
#include <mtl/mtl.h>
typedef mtl::matrix<double, mtl::rectangle<>, mtl::dense<>, mtl::row_major>::type Matrix;

using namespace std;

const string LogNewtonRaphson::SOLVER_NAME = "LogNewtonRaphson";

//! Default Constructor. Constructs the base class. 
LogNewtonRaphson::LogNewtonRaphson( Marketplace* aMarketplace,
                                    World* aWorld, CalcCounter* aCalcCounter, double aDeltaPrice ):
SolverComponent( aMarketplace, aWorld, aCalcCounter ),
mDeltaPrice( aDeltaPrice )
{
}

//! Init method.
void LogNewtonRaphson::init() {
}

//! Get the name of the SolverComponent
const string& LogNewtonRaphson::getNameStatic() {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& LogNewtonRaphson::getName() const {
    return SOLVER_NAME;
}

/*! \brief Ron's version of the Newton Raphson Solution Mechanism (all markets)
* \details Derivatives are taken once. They are not taken again unless:
* a) The Max Relative ED after calculation is greater than MAXED_FOR_DERIV_RECALC
* b) or 10 NR iterations have occurred.
* As long as Bisection is close, one set of derivatives per period is sufficient.
* Tracking the worst excess demand is turned on from the log configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored 
* \param maxIterations The maximum number of world.calc calls to make before exiting this function.
* \param solverSet An object containing the set of MarketInfo objects representing all markets.
* \param period Model period
*/
SolverComponent::ReturnCode LogNewtonRaphson::solve( const double solutionTolerance, const double edSolutionFloor,
                                                     const unsigned int maxIterations, SolverInfoSet& solverSet,
                                                     const int period ){
    startMethod();
    const unsigned int nrCalcsStart = calcCounter->getMethodCount( SOLVER_NAME );
    ReturnCode code = SolverComponent::ORIGINAL_STATE;
      
    // Constants
    const static double EXIT_VALUE = 100; // Value of Relative Excess Demand above which NR will quit.
    const static unsigned int MAX_ITER_NO_IMPROVEMENT = 7; // Maximum number of iterations without improvement.
    // Update the SolutionVector for the correct markets to solve.
    solverSet.updateFromMarkets();
    SolverInfoSet::UpdateCode solvableChanged = solverSet.updateSolvable( true );

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "LogNewtonRaphson beginning" << endl;
    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    worstMarketLog.setLevel( ILogger::DEBUG );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    if( solverSet.getNumSolvable() == 0 ){
        solverLog << "Exiting newton raphson early. No non-singular markets." << endl;
        return SUCCESS; // Need a new code here.
    }

    // Turn off calibration.
    const bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();


    do {
        singleLog.setLevel( ILogger::DEBUG );
        solverSet.printMarketInfo( "Begin logNR", calcCounter->getPeriodCount(), singleLog );

        // Declare matrices here due to a resize bug.
        Matrix JF( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFDM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFSM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );

        // Calculate derivatives
        code = calculateDerivatives( solverSet, JFSM, JFDM, JF, period );        
        if ( code != SUCCESS ) {
            // Restore calibration status to avoid superfluous errors in latter periods
            if ( calibrationStatus ) { 
                world->turnCalibrationsOn();
            }  
            return code;
        }
        
        // Calculate new prices
        SolverLibrary::calculateNewPricesLogNR( solverSet, JFSM, JFDM, JF );        
       
         // Call world.calc and update supplies and demands. 
        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        solverSet.updateFromMarkets();

        // Add to the iteration list.
        SolverInfo* currWorstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
        addIteration( currWorstSol->getName(), currWorstSol->getRelativeED( edSolutionFloor ) );

        worstMarketLog.setLevel( ILogger::NOTICE );
        worstMarketLog << "NR-maxRelED: " << *currWorstSol << endl;
        solverLog.setLevel( ILogger::DEBUG );
        solverLog << "Solution after " << calcCounter->getMethodCount( SOLVER_NAME ) - nrCalcsStart << " iterations in NR_RON: " << endl;
        solverLog << solverSet << endl;
        
        solverSet.updateSolvable( true );

        solverSet.printMarketInfo( "NR routine ", calcCounter->getPeriodCount(), singleLog );
    } // end do loop	
    while ( isImproving( MAX_ITER_NO_IMPROVEMENT ) && 
        calcCounter->getMethodCount( SOLVER_NAME ) - nrCalcsStart < maxIterations && 
        solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) >= solutionTolerance );

    // Update the return code. 
    code = ( solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) < solutionTolerance ? SUCCESS : FAILURE_ITER_MAX_REACHED );

    // Print if we exited NR because it had solved all the markets.
    solverLog.setLevel( ILogger::NOTICE );
    if( code == SUCCESS && !solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        solverLog << "Newton-Raphson solved all markets except at least one with a singularity." << endl;
        solverSet.printUnsolved( solutionTolerance, edSolutionFloor, solverLog );
    }
    else if( code == SUCCESS && solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        solverLog << "Newton-Raphson solved all markets successfully." << endl;
    }
    else if( calcCounter->getMethodCount( SOLVER_NAME ) - nrCalcsStart > maxIterations ){
        solverLog << "Exiting due to exceeding maximum iterations: " << maxIterations << endl;
    }
    else {
        solverLog << "Exiting NR due to lack of improvement. " << endl;
    }

    if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
        world->turnCalibrationsOn();
    }  
    
    return code;
}

//! Calculate derivatives
SolverComponent::ReturnCode LogNewtonRaphson::calculateDerivatives( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF, int period ) {
    // Calculate derivatives.
    SolverLibrary::derivatives( marketplace, world, solverSet, mDeltaPrice, period ); 
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Derivatives calculated" << endl;

    // Update the JF, JFDM, and JFSM matrices
    SolverLibrary::updateMatrices( solverSet, JFSM, JFDM, JF );
    SolverLibrary::invertMatrix( JF );

    return SUCCESS;
}
