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

using namespace std;

const string LogNewtonRaphson::SOLVER_NAME = "LogNewtonRaphson";
extern ofstream logfile, bugoutfile;

//! Default Constructor. Constructs the base class. 
LogNewtonRaphson::LogNewtonRaphson( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Default Destructor. Currently does nothing.
LogNewtonRaphson::~LogNewtonRaphson(){
}

//! Init method. Currently does nothing. 
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

    vector<double> NP; // adjustment value
    vector<double> KD; // k values demand
    vector<double> KS; // k values supply
    vector<double> KDS; // k values demand - supply

    // Turn off calibration.
    const bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();

    logfile << ",,Ron's version of the Newton-Raphson function called." << endl;
    // Select the worst market.
    SolverInfo& worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // addIteration( worstSol.getName(), worstSol.getRelativeED( edSolutionFloor ) );

    do {
        solverSet.printMarketInfo( "Begin logNR", calcCounter->getPeriodCount() );

        // Declare matrices here due to resize bug.
        Matrix JF( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFDM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        Matrix JFSM( solverSet.getNumSolvable(), solverSet.getNumSolvable() );
        
        // control no of times derivatives are calculated
        // if ( ( numDerivativeCalcs < MAX_DERIVATIVE_CALC ) || ( solvableChanged != SolverInfoSet::UNCHANGED ) ) { 
            // Resize vectors to current number of solvable. 
            NP.resize( solverSet.getNumSolvable() );
            KD.resize( solverSet.getNumSolvable() );
            KS.resize( solverSet.getNumSolvable() );
            KDS.resize( solverSet.getNumSolvable() );

            // Calculate derivatives.
            SolverLibrary::derivatives( marketplace, world, solverSet, period ); 
            // numDerivativeCalcs++;

            logfile << ",,,Derivatives calculated" << endl;

            if ( trackED ) {
                cout <<" End Derivatives " << endl;
            }
        // }
        // Update the JF, JFDM, and JFSM matrices
        SolverLibrary::updateMatrices( solverSet, JFSM, JFDM, JF );
        SolverLibrary::invertMatrix( JF );

        // initialize KD and KS as logs of original demand and supply
        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
            KD[ i ] = log( max( solverSet.getSolvable( i ).getDemand(), util::getSmallNumber() ) );
            KS[ i ] = log( max( solverSet.getSolvable( i ).getSupply(), util::getSmallNumber() ) );
        }
    
        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
            for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                double tempValue = log( max( solverSet.getSolvable( j ).getPrice(), util::getSmallNumber() ) );
                KD[ i ] -= tempValue * JFDM[ i ][ j ];
                KS[ i ] -= tempValue * JFSM[ i ][ j ];
                assert( util::isValidNumber( KD[ i ] ) );
                assert( util::isValidNumber( KS[ i ] ) );
            }

            KDS[ i ] = KD[ i ] - KS[ i ];
            assert( util::isValidNumber( KDS[ i ] ) );
        }

        // Calculate new log price based on NR
        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
            NP[ i ] = 0;
            for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                assert( util::isValidNumber( JF[ i ][ j ] ) );
                NP[ i ] += JF[ i ][ j ] * KDS[ j ];
                assert( util::isValidNumber( NP[ i ] ) );
            }
            
            // Set the new price with the exponent of the correction vector.
            solverSet.getSolvable( i ).setPrice( exp( NP[ i ] ) );
            assert( util::isValidNumber( solverSet.getSolvable( i ).getPrice() ) );
            
            // Debugging output.
            if( debugChecking ){
                if ( solverSet.getSolvable( i ).getPrice() > 1e10) {
                    cerr << " Large price in market: " << solverSet.getSolvable( i ).getName() << endl;
                    // first get largest derivitive
                    double maxDerVal = 0; 
                    double maxKDSval = 0;
                    for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                        maxKDSval = max( maxKDSval, fabs( KDS[ j ] ) );
                        maxDerVal = max( maxDerVal, fabs( JF[ i ][ j ]) );
                    }
                    cout << "Max KDS: " << maxKDSval << ", Max Derivitive: " << maxDerVal;
                    cout << "Large derivitives against: " << endl;
                    for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                        if ( fabs( JF[ i ][ j ]) > maxDerVal / 100 ) {
                            cout << "   Market: " << solverSet.getSolvable( j ).getName() << ", Value: "<< JF[ i ][ j ] << endl;
                        }
                    }
                }
            }
        }

        // Call world.calc and update supplies and demands. 
        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        solverSet.updateFromMarkets();
        solvableChanged = solverSet.updateSolvable( true );

        // Add to the iteration list.
        SolverInfo& currWorstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
        addIteration( currWorstSol.getName(), currWorstSol.getRelativeED( edSolutionFloor ) );

        // Debugging output.
        if ( trackED ) {
            cout << "NR-maxRelED: ";
            worstSol.printTrackED();
        }
        
        /*
        // if solution moves in wrong direction
        if( solverSet.getMaxRelativeExcessDemand( edSolutionFloor + 5 ) > EXIT_VALUE ){
            logfile << ",,Exit Newton-Raphson function maxSolVal > " << EXIT_VALUE << endl;
            if ( trackED ) {
                cout << "Exit Newton-Raphson function maxSolVal > " << EXIT_VALUE << endl;
            }
            
            const double maxSolVal = solverSet.getMaxRelativeExcessDemand( edSolutionFloor + 5 );
            for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
                const double relativeED = solverSet.getSolvable( i ).getRelativeED( edSolutionFloor + 5 );
                if ( fabs( relativeED ) > maxSolVal / 20 ) {
                    if ( trackED ) {
                        cout << "RED: (" << solverSet.getSolvable( i ).getName()  << ") - " << relativeED << endl;
                    }
                    logfile << ",,,Due to market " << solverSet.getSolvable( i ).getName() << " - RED: " << relativeED << endl;
                }
            }
            return FAILURE_WRONG_DIRECTION;
        }

        // If have iterated 10 times in NR without solving, then re-do derivatives
        if ( ( iterInNR > 0 ) && ( iterInNR % MAX_ITER_BEFORE_DERIV_RECALC == 0 ) ) {
            numDerivativeCalcs = 0;
            // sjs add this to print out SD curves when has trouble solving
            if ( debugChecking ) {
                marketplace->checkMarketSolution( solutionTolerance, edSolutionFloor, period , false ) ;
            }
        }

        // IF ED is too high then re-calculate derivatives
        if ( solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) > MAXED_FOR_DERIV_RECALC ) {
            numDerivativeCalcs = 0;
        }
         // Debug output
        if ( bugMinimal ) {
            bugoutfile << endl << "Solution after " << iterInNR << " iterations in NR_RON: " << endl;
            bugoutfile << solverSet << endl;
        }
        
        iterInNR++;
        */
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