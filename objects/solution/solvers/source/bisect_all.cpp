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
#include <iostream>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/bisect_all.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"

using namespace std;

const string BisectAll::SOLVER_NAME = "BisectAll";
extern ofstream bugoutfile, logfile;

//! Default Constructor. Constructs the base class. 
BisectAll::BisectAll( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ):SolverComponent( marketplaceIn, worldIn, calcCounterIn ) {
}

//! Default Destructor. Currently does nothing. 
BisectAll::~BisectAll(){
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
* \detailed This solution mechanism bisects all markets at once. 
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
* Useful TrackED writeout to screen is turned on by toggle in configuration file.
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
SolverComponent::ReturnCode BisectAll::solve( const double solutionTolerance, const double edSolutionFloor, const int maxIterations, SolverInfoSet& solverSet, const int period ){
    startMethod();

    int numIterations = 0; // number of iterations
    int interLimitAdd = 0; // Variable to allow more iterations if re-bisect
    int numbRebrackets = 0; // Variable to allow more iterations if re-bisect
    ReturnCode code = ORIGINAL_STATE; // code that reports success 1 or failure 0
    bool breakout;	// var to allow various conditions to exit bisection routine
    const int MAX_REBRACKETS = 3;
    const double BREAK_OUT_THRESHOLD = 0.001; // leave bracketing if not improving by at least this much
    const double BRACKET_THRESHOLD = 10;	// if breakout & relative maxED is > this then try re-bracketing
    double bracketInterval = 0.05; // starting value for re-bracketing interval
    double previousEDvalue = -1;
    SolverInfo previousED = solverSet.getSolvable( 0 );
    bool reBracketingDone = false;

    if ( trackED ) { 
        cout << endl << "Bisection routine starting..." << endl; 
    }
    logfile << ",,Bisection_all function called." << endl;

    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );
    
    // Select the worst market.
    SolverInfo& worstSol = solverSet.getWorstSolverInfo( edSolutionFloor );
    // addIteration( worstSol.getName(), worstSol.getRelativeED( edSolutionFloor ) );
    // solve all markets
    do {
        breakout = false; //default is not to breakout of bisection routine
        if (bugMinimal) {
            bugoutfile << " Bisect " << numIterations;
        }
        solverSet.printMarketInfo( "Begin Bisect All", calcCounter->getPeriodCount() );

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
        marketplace->nullSuppliesAndDemands(period);

        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        // The  price and demand markets are very prone to moving beyond their brackets. 
        // So check and adjust if needed. Lines below check if XL < Demand, or XR > Demand, 
        // and move brackets if necessary. A more general  bracket check is below, but this
        // is needed more often and can be done simply.
        solverSet.adjustBrackets();
        
        const SolverInfo maxSol = solverSet.getWorstSolverInfo( edSolutionFloor );
        addIteration( maxSol.getName(), maxSol.getRelativeED( edSolutionFloor ) );

        // double maxSolValue = maxSol.getRelativeED( edSolutionFloor );

        if ( trackED ) {
            cout << "BisectAll-maxRelED: ";
            maxSol.printTrackED();
        }
        
        /*
        // If the worst ED is not changing too much then breakout of bisection
        if ( numIterations > 5 ) {	// always bisect a few times . MAGIC NUMBER!        
            if ( fabs( maxSolValue - previousEDvalue ) / previousEDvalue < BREAK_OUT_THRESHOLD && maxSol == previousED )  {
                if( trackED ){
                    cout << "Setting breakout flag." << endl;
                }
                breakout = true; 
            }
        }

        previousEDvalue = maxSolValue;
        previousED = maxSol;
        
        // If have not solved, then try bracketing again.
        // This helps when some market has fell out of its bracketing range
        // This helps if the maxED is still somewhat large (otherwise, NR should be fine)
        // This needs to be below maxED printout so that inconsistent TrackED printout does not occur
        if ( breakout && !reBracketingDone && ( fabs( maxSolValue ) > BRACKET_THRESHOLD ) ) {
            if( trackED ){
                cout << "Rebracketing" << endl;
            }
            ++numbRebrackets;

            if ( numbRebrackets > MAX_REBRACKETS ) {
                reBracketingDone = true; // only try this once
            }

            // reset bracket flag for any markets not solved
            for( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
                SolverInfo& currSol = solverSet.getSolvable( i );
                if ( !currSol.isWithinTolerance( solutionTolerance, edSolutionFloor ) ) {
                    currSol.resetBrackets();
                }
                else {
                    currSol.setBracketed();
                }
            }
            logfile << ",";
            SolverLibrary::bracket( marketplace, world, bracketInterval, solverSet, period );

            // Reduce bracket interval for next re-bracket so do not periodturb prices as much
            bracketInterval = bracketInterval/2; 
            breakout = false;
            interLimitAdd = numIterations;
            logfile << ",,,Bisection_all continuing after re-bracketing." << endl;
            if (trackED) {
                cout << "Bisection continuing after re-bracketing" << endl; 
            }
        } // end re-bracket block           
    */
    } // end do loop		
    while ( isImproving( 10 ) && ++numIterations < ( maxIterations+interLimitAdd) && solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) >= solutionTolerance && !breakout );

    // Set the return code. 
    code = ( solverSet.getMaxRelativeExcessDemand( edSolutionFloor ) < solutionTolerance ? SUCCESS : FAILURE_ITER_MAX_REACHED ); // report success, or failure

    if ( numIterations >= ( maxIterations + interLimitAdd ) && !breakout ) {
        if( trackED ){
            cout << "Exiting Bisection all due to reaching the maximum number of iterations." << endl;
        }
        logfile << ",,,Exited Bisection_all. Max number of Iterations exceeded." << endl;
    }
    else if( breakout ){
        if( trackED ){
            cout << "Exiting Bisection all due to breakout flag." << endl;
        }
        logfile << ",,,Exited Bisection_all. Breakout flag set." << endl;
    }
    else if( !isImproving( 10 ) ){
        if( trackED ){
            cout << "Exiting BisectionAll due to lack of improvement in RED." << endl;
        }
    }
    else {
        if( trackED ){
            cout << "Exiting BisectionAll with model fully solved." << endl;
        }
    }
    if (trackED) { cout << endl; }

    return code;
}