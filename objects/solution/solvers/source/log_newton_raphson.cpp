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
#include <iostream>
#include <fstream>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/log_newton_raphson.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"

#include <mtl/matrix.h>
#include <mtl/mtl.h>
#include <mtl/utils.h>

using namespace std;

const string LogNewtonRaphson::SOLVER_NAME = "LogNewtonRaphson";
extern ofstream logfile, bugoutfile;

//! Default Constructor. Constructs the base class. 
LogNewtonRaphson::LogNewtonRaphson( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn )
:SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Default Destructor. Currently does nothing.
LogNewtonRaphson::~LogNewtonRaphson(){
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
* \detailed Derivatives are taken once. They are not taken again unless:
* a) The Max Relative ED after calculation is greater than MAXED_FOR_DERIV_RECALC
* b) or 10 NR iterations have occurred.
* As long as Bisection is close, one set of derivatives per period is sufficient.
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
* \author Sonny Kim, Josh Lurz, Steve Smith
* \warning Unless stated otherwise, ED values are normalized (i.e., that 10 == 10% difference).
* \param solutionTolerance Target value for maximum relative solution for worst market 
* \param edSolutionFloor *Absolute value* beneath which market is ignored 
* \param maxIterations The maximum number of world.calc calls to make before exiting this function.
* \param solverSet An object containing the set of MarketInfo objects representing all markets.
* \param period Model period
*/
SolverComponent::ReturnCode LogNewtonRaphson::solve( const double solutionTolerance, const double edSolutionFloor, const int maxIterations, SolverInfoSet& solverSet, const int period ){
    startMethod();
    const int nrCalcsStart = calcCounter->getMethodCount( SOLVER_NAME );
    unsigned int numDerivativeCalcs = 0; // count number of times derivatives are calculated
    unsigned int iterInNR = 0;
    ReturnCode code = SolverComponent::ORIGINAL_STATE;
      
    // Constants
    const static unsigned int MAX_DERIVATIVE_CALC = 25; // count number of times derivatives are normally calculated
    const static double MAXED_FOR_DERIV_RECALC = 0; // recalculate derivatives is ED is larger than this 
    const static double EXIT_VALUE = 100; // Value of Relative Excess Demand above which NR will quit.
    const static unsigned int MAX_ITER_BEFORE_DERIV_RECALC = 3; // Number of iterations before recalculating derivatives.
    const Configuration* conf = Configuration::getInstance();
    const bool debugChecking = conf->getBool( "debugChecking" );

    // Update the SolutionVector for the correct markets to solve.
    solverSet.updateFromMarkets();
    SolverInfoSet::UpdateCode solvableChanged = solverSet.updateSolvable( true );
    
    if ( trackED ) { 
        cout << "NR_Ron begin ";
    }
    
    if( solverSet.getNumSolvable() == 0 ){
        if( trackED ){
            cout << "Exiting newton raphson early. No non-singular markets." << endl;
        }
        return SUCCESS; // Need a new code here.
    }

    // Turn off calibration.
    const bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();

    logfile << ",,Ron's version of the Newton-Raphson function called." << endl;
    // Select the worst market.
    SolverInfo& worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // addIteration( worstSol.getName(), worstSol.getRelativeED( edSolutionFloor ) );

    do {
       
        // Declare matrices here due to resize bug.
        Matrix JF( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFDM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFSM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        
        // Calculate derivatives
        code = calculateDerivatives( solverSet, JFSM, JFDM, JF, period );        
        if ( code != SUCCESS ) {
            return code;
        }
        
        // Calculate new prices
        SolverLibrary::calculateNewPricesLogNR( solverSet, JFSM, JFDM, JF );        
       
         // Call world.calc and update supplies and demands. 
        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        solverSet.updateFromMarkets();
        solvableChanged = solverSet.updateSolvable( true );

        // Add to the iteration list.
        SolverInfo& currWorstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
        addIteration( currWorstSol.getName(), currWorstSol.getRelativeED( edSolutionFloor ) );

        solverSet.printMarketInfo( "NR routine ", calcCounter->getPeriodCount() );

        // Debugging output.
        if ( trackED ) {
            cout << "NR-maxRelED: ";
            worstSol.printTrackED();
        }
                
   } // end do loop	

    while ( isImproving( 7 ) && calcCounter->getMethodCount( SOLVER_NAME ) - nrCalcsStart < maxIterations && solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) >= solutionTolerance );	

    // Update the return code. 
    code = ( solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) < solutionTolerance ? SUCCESS : FAILURE_ITER_MAX_REACHED );
    logfile << ",Number of Newton-Raphson iterations: n =" << iterInNR << endl;
    // Print if we exited NR because it had solved all the markets.
    if( trackED && code == SUCCESS && !solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        cout << "Newton-Raphson solved all markets except at least one with a singularity." << endl;
        solverSet.printUnsolved( solutionTolerance, edSolutionFloor );
        cout << endl;
    }
    else if( trackED && code == SUCCESS && solverSet.isAllSolved( solutionTolerance, edSolutionFloor ) ){
        cout << "Newton-Raphson solved all markets successfully." << endl;
    }
    else if( calcCounter->getMethodCount( SOLVER_NAME ) - nrCalcsStart > maxIterations ){
        if( trackED ){
            cout << "Exiting NR due to exceeding maximum iterations: " << maxIterations << endl;
        }
        logfile << "Exiting due to exceeding maximum iterations: " << maxIterations << endl;
    }
    else {
        if( trackED ){
            cout << "Exiting NR due to lack of improvement. " << endl;
        }
        logfile << "Exiting NR due to lack of improvement. " << endl;
    }

    if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
        world->turnCalibrationsOn();
    }  
    
    return code;
}

//! Calculate derivatives
SolverComponent::ReturnCode LogNewtonRaphson::calculateDerivatives( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF, int period ) {
        
      // Always calculate derivatives in this solver component
           
      // Calculate derivatives.
      SolverLibrary::derivatives( marketplace, world, solverSet, period ); 
      // numDerivativeCalcs++;
   
      logfile << ",,,Derivatives calculated" << endl;
      if ( trackED ) {
         cout <<" End Derivatives " << endl;
      }
               
      // Update the JF, JFDM, and JFSM matrices
      SolverLibrary::updateMatrices( solverSet, JFSM, JFDM, JF );
      SolverLibrary::invertMatrix( JF );
                       
      return SUCCESS;
}