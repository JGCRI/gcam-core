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
* \file log_newton_raphson.cpp
* \ingroup objects
* \brief LogNewtonRaphson class source file.
* \author Josh Lurz
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

using namespace std;

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
    static const string SOLVER_NAME = "LogNewtonRaphson";
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& LogNewtonRaphson::getName() const {
    return getNameStatic();
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
* \param solution_set An object containing the set of MarketInfo objects representing all markets.
* \param period Model period
*/
SolverComponent::ReturnCode LogNewtonRaphson::solve( const double solutionTolerance, const double edSolutionFloor,
                                                     const unsigned int maxIterations, SolverInfoSet& solution_set,
                                                     const int period )
{
    ReturnCode code = SolverComponent::ORIGINAL_STATE;

    // If all markets are solved, then return with succes code.
    if( solution_set.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        return code = SolverComponent::SUCCESS;
    }

    startMethod();
    const unsigned int nrCalcsStart = calcCounter->getMethodCount( getName() );
      
    // Constants
    const static int MAX_ITER_NO_IMPROVEMENT = 7; // Maximum number of iterations without improvement.
    // Update the SolutionVector for the correct markets to solve.
    solution_set.updateFromMarkets();
    // Need to update solvable status before starting solution (Ignore return code)
    solution_set.updateSolvable( true );

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Log-Newton-Raphson Beginning" << endl;
    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    worstMarketLog.setLevel( ILogger::DEBUG );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );

    if( solution_set.getNumSolvable() == 0 ){
        solverLog << "Exiting Newton-Raphson early. No non-singular markets." << endl;
        return SUCCESS; // Need a new code here.
    }

    // Turn off calibration.
    const bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();

    // Setup the solution matrices.
    size_t currSize = solution_set.getNumSolvable();
    Matrix JF( currSize, currSize );
    Matrix JFDM( currSize, currSize );
    Matrix JFSM( currSize, currSize );

    unsigned int number_of_NR_iteration = 1;
    bool success = true;
    do {
        singleLog.setLevel( ILogger::DEBUG );
        solution_set.printMarketInfo( "Begin logNR", calcCounter->getPeriodCount(), singleLog );

        // Calculate derivatives
        code = calculateDerivatives( solution_set, JFSM, JFDM, JF, period );        
        if ( code != SUCCESS ) {
            // Restore calibration status to avoid superfluous errors in latter periods
            if ( calibrationStatus ) { 
                world->turnCalibrationsOn();
            }  
            return code;
        }

        // Calculate new prices
        if( SolverLibrary::calculateNewPricesLogNR( solution_set, JFSM, JFDM, JF ) ){   
            // Call world.calc and update supplies and demands. 
            solution_set.updateToMarkets();
            marketplace->nullSuppliesAndDemands( period );
            solverLog << "Supplies and demands calculated with new prices." << endl;
            world->calc( period );
            solution_set.updateFromMarkets();

            // Add to the iteration list.
            SolverInfo* currWorstSol = solution_set.getWorstSolverInfo( edSolutionFloor );
            addIteration( currWorstSol->getName(), currWorstSol->getRelativeED( edSolutionFloor ) );

            worstMarketLog.setLevel( ILogger::NOTICE );
            worstMarketLog << "NR-maxRelED: " << *currWorstSol << endl;
            solverLog.setLevel( ILogger::DEBUG );
            solverLog << "Solution after " << number_of_NR_iteration << " iterations in NewtonRhapson: " << endl;
            solverLog << solution_set << endl;

            if( solution_set.updateSolvable( true ) != SolverInfoSet::UNCHANGED ){
                size_t newSize = solution_set.getNumSolvable();
                JF.resize( newSize, newSize );
                JFDM.resize( newSize, newSize );
                JFSM.resize( newSize, newSize );
            }

            solution_set.printMarketInfo( "NR routine ", calcCounter->getPeriodCount(), singleLog );
        }
        else {
            success = false;
        }
        ++number_of_NR_iteration;
    } // end do loop    
    while ( success && number_of_NR_iteration <= maxIterations &&
            solution_set.getMaxRelativeExcessDemand( edSolutionFloor ) >= solutionTolerance );

    // Update the return code. 
    code = ( solution_set.getMaxRelativeExcessDemand( edSolutionFloor ) < solutionTolerance ? SUCCESS : FAILURE_ITER_MAX_REACHED );

    // Print if we exited NR because it had solved all the markets.
    solverLog.setLevel( ILogger::NOTICE );
    if( code == SUCCESS && !solution_set.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        solverLog << "Newton-Raphson solved all markets except at least one with a singularity." << endl;
        solution_set.printUnsolved( solutionTolerance, edSolutionFloor, solverLog );
    }
    else if( code == SUCCESS && solution_set.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        solverLog << "Newton-Raphson solved all markets successfully." << endl;
    }
    else if( number_of_NR_iteration > maxIterations ){
        solverLog << "Exiting Newton-Rhapson without solving all markets. Maximum iteration count exceeded: " << maxIterations << endl;
    }
    else {
        solverLog << "Exiting Newton-Rhapson due to lack of improvement. " << endl;
    }

    if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
        world->turnCalibrationsOn();
    }
    
    solverLog << endl; // new line
    
    return code;
}

//! Calculate derivatives
SolverComponent::ReturnCode LogNewtonRaphson::calculateDerivatives( SolverInfoSet& solution_set, Matrix& JFSM, Matrix& JFDM, Matrix& JF, int period ) {
    // Calculate derivatives.
    SolverLibrary::derivatives( marketplace, world, solution_set, mDeltaPrice, period ); 
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Derivatives calculated" << endl;

    // Update the JF, JFDM, and JFSM matrices
    SolverLibrary::updateMatrices( solution_set, JFSM, JFDM, JF );
    bool isSingular = false;
    JF = SolverLibrary::invertMatrix( JF, isSingular );
    if( isSingular ) {
        solverLog.setLevel( ILogger::ERROR );
        solverLog << "Matrix came back as singular, could not invert." << endl;
        solverLog.setLevel( ILogger::NOTICE );
        return FAILURE_SINGULAR_MATRIX;
    } 
    return SUCCESS;
}
