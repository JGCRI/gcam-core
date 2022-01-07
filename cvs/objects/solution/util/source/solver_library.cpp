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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*!
* \file solver_library.cpp
* \ingroup Objects
* \brief SolverLibrary class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"

#include <boost/numeric/ublas/vector.hpp>
#include <boost/numeric/ublas/vector_proxy.hpp>
#include <boost/numeric/ublas/matrix.hpp>
#include <boost/numeric/ublas/triangular.hpp>
#include <boost/numeric/ublas/lu.hpp>
#include <boost/numeric/ublas/io.hpp>
#include "util/base/include/definitions.h"
#include <vector>
#include <map>
#include <cmath>
#include <string>
#include <algorithm>
#include "solution/util/include/solver_library.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "solution/util/include/solution_info.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/calc_counter.h"
#include "util/logger/include/ilogger.h"
#include "solution/util/include/ublas-helpers.hpp"
#include "containers/include/iactivity.h"

#include "solution/util/include/edfun.hpp"

using namespace std;

#define NO_REGIONAL_DERIVATIVES 0

/*! \brief Calculate and return a relative excess demand.
* \author Josh Lurz
* \details This function determines the excess demand relative to the demand. This helps to determine which market is truely
* the worst. This function returns 0 if the ED value is below the excessDemandFloor as the market is considered solved.
* Otherwise, relative excess demand is determined to be the absolute value of excess demand divided by demand multiplied by 100.
* \param excessDemand The excess demand.
* \param demand The market demand.
* \param excessDemandFloor Value of ED below which the market should be considered solved.
* \return The relative excess demand, excess demand as a percentage of demand.
*/
double SolverLibrary::getRelativeED( const double excessDemand, const double demand, const double excessDemandFloor ) {

    // Initialize return value
    double retValue = 0;
    double tempDemand = fabs( demand );

    // If demand is 0, set tempDemand to small number to avoid
    // divide by 0.
    if( tempDemand < util::getSmallNumber() ) {
        tempDemand = util::getSmallNumber();
    }

    // Check if the ED is below a minimal value.
    if( fabs( excessDemand ) < excessDemandFloor ) {
        retValue = 0;
    }
    // Find the ratio of excess demand to demand.
    else {
        retValue = fabs( excessDemand ) / tempDemand;
    }

    return retValue;
}

/*! \brief Determine whether a market is within the solution tolerance.
* \author Josh Lurz
* \details This function determines if a market is solved to within the solution tolerance. It does this by checking if the
* relative excess demand is less than the solution tolerance.
* \param excessDemand The excess demand.
* \param demand The market demand.
* \param solutionTolerance The relative excess demand below which a market is considered solved.
* \param excessDemandFloor Absolute value of ED below which the market should be considered solved.
*/
bool SolverLibrary::isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance, const double excessDemandSolutionFloor ) {
    return ( getRelativeED( excessDemand, demand, excessDemandSolutionFloor ) < solutionTolerance );
}

/*! \brief Function to calculate partial derivatives for Newton-Raphson method, NR_Ron()
*
* This function calculates matrices of partial derivatives of supplies and demands for all markets which are currently being solved.
* The function uses the fact that changing a price while holding all other prices constant can only change markets that are affected.
* It must however set the Marketplace mIsDerivativeCalc flag to ensure only changes are noted so the non-affected markets do not need
* to null supplies and demands nor recalculate what they were.
* \param marketplace The marketplace to perform derivative calculations on.
* \param world The world object which is used for calls to World::calc
* \param aSolutionSet The vector of SolutionInfo objects which store the current prices, supplies and demands.
* \param Delta to use when changing prices.
* \param per The current model period.
*/
void SolverLibrary::derivatives( Marketplace* marketplace, World* world, SolutionInfoSet& aSolutionSet,
                                 const double aDeltaPrice, const int per ) {

    // TODO: could fix but why?
    /*ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Starting derivative calculation" << endl;

    // Initial call to world.calc to ensure we have set our baseline to take
    // partials from.
    marketplace->nullSuppliesAndDemands( per );
    world->calc( per );

#if( !NO_REGIONAL_DERIVATIVES )
    // Set the flag to only take supply/demand differences
    marketplace->mIsDerivativeCalc = true;
#endif
    // Retain the original values.
    aSolutionSet.storeValues();

    // Calculate derivatives for each market.
    for ( unsigned int j = 0; j < aSolutionSet.getNumSolvable(); j++ ) {
        double currDeltaPrice = aSolutionSet.getSolvable( j ).getDeltaPrice( aDeltaPrice );
        aSolutionSet.getSolvable( j ).increaseX( currDeltaPrice, currDeltaPrice );

#if( NO_REGIONAL_DERIVATIVES )
        marketplace->nullSuppliesAndDemands( per );
        world->calc( per );
#else

        // Determine which calculations are affected.
        const vector<IActivity*>& affectedItems = aSolutionSet.getSolvable( j ).getDependencies();

        /*! \invariant There is at least one item to calculate. * /
        assert( !affectedItems.empty() );

        world->calc( per, affectedItems );
        
        // Set the stale flag to indicate that demand calculations may need to
        // recalculate it's corresponding price calculation.  Setting this flag
        // allows this to be lazy.
        for( size_t partialIndex = 0 ; partialIndex < affectedItems.size(); ++partialIndex ) {
            affectedItems[ partialIndex ]->setStale();
        }
#endif

        aSolutionSet.getSolvable( j ).calcDemandElas( aSolutionSet );
        aSolutionSet.getSolvable( j ).calcSupplyElas( aSolutionSet );
        aSolutionSet.restoreValues();
    }
#if( !NO_REGIONAL_DERIVATIVES )
    // Reset the derivative flag.
    marketplace->mIsDerivativeCalc = false;
#endif
*/
}

/* \brief Calculate the JFDM, JFSM, and JF matrices from the derivatives stored in the SolutionInfo. */
void SolverLibrary::updateMatrices( SolutionInfoSet& aSolutionSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF ){
    for( unsigned int j = 0; j < aSolutionSet.getNumSolvable(); ++j ){
        for( unsigned int i = 0; i < aSolutionSet.getNumSolvable(); ++i ){
            JFDM( i , j ) = aSolutionSet.getSolvable( j ).getDemandElasWithRespectTo( i );
            JFSM( i , j ) = aSolutionSet.getSolvable( j ).getSupplyElasWithRespectTo( i );
            JF( i , j ) = JFSM( i , j ) - JFDM( i , j );
            assert( util::isValidNumber( JF( i , j) ) );
        }
    }

}

/*! \brief Calculate and set new market prices based on Log NR  mechanism
 * \param aSolutionSet An object containing the set of SolutionInfo objects representing all markets.
 * \param JFLUFactorized LU factored JF matrix
 * \param aPermMatrix Permutation matrix used to factorize JF
 * \param aDefaultMaxPriceJump The default factor used to limit the price changes from this algorithm.
 * \return Whether prices were set successfully.
 */
bool SolverLibrary::calculateNewPricesLogNR( SolutionInfoSet& aSolutionSet, Matrix& JFLUFactorized,
                                             PermutationMatrix& aPermMatrix, const double aDefaultMaxPriceJump )
{
    using namespace boost::numeric::ublas;
    boost::numeric::ublas::vector<double> KDS( aSolutionSet.getNumSolvable() ); // k values demand - supply
    
    // initialize KDS as logs of original demand - supply or -F(Xn)
    for ( unsigned int i = 0; i < aSolutionSet.getNumSolvable(); i++ ) {
        KDS[ i ] = log( max( aSolutionSet.getSolvable( i ).getDemand(), util::getSmallNumber() ) )
            - log( max( aSolutionSet.getSolvable( i ).getSupply(), util::getSmallNumber() ) );
    }
    
    // Store the solution set prices so that they can be restored if NR fails to
    // generate valid prices.
    std::vector<double> storedPrices = storePrices( aSolutionSet );
    boost::numeric::ublas::vector<double> price(aSolutionSet.getNumSolvable());
    std::copy(storedPrices.begin(),storedPrices.begin()+price.size(),price.begin());

    ILogger &solverLog = ILogger::getLogger("solver_log");
    solverLog.setLevel(ILogger::DEBUG);
    solverLog << "x: " << price << "\nfx: " << KDS << "\n";
    
    
    // to solve JF(Xn) * ( Xn+1 - Xn ) = -F(Xn) we use lu substitution which
    // is favorable to calculating the inverse of JF.  lu_substitue will leave
    // us with ( Xn+1 - Xn ) stored in KDS
    lu_substitute( JFLUFactorized, aPermMatrix, KDS );
    solverLog << "dx: " << KDS << "\n";
    
    // To calculate the new price Xn+1 we do Xn+1 = Xn + KDS
    // then take the e^Xn+1 to get the prices back in normal terms
    for ( unsigned int i = 0; i < aSolutionSet.getNumSolvable(); i++ ) {
        // we must take the log of the current prices before adding it to KDS
        // to put them in the same domain
        double newPrice = exp( log( aSolutionSet.getSolvable( i ).getPrice() ) + KDS[ i ] );
        if( util::isValidNumber( newPrice ) ) {
            // determine a max price change to allow
            SolutionInfo& currSolutionInfo = aSolutionSet.getSolvable( i );
            double maxNRStep = currSolutionInfo.getMaxNRPriceJump( aDefaultMaxPriceJump );
            
            // limit the price change to avoid diverging from the solution
            // when a derivative was taken at a discontinous point
            if ( newPrice > currSolutionInfo.getPrice() * maxNRStep ){
                ILogger& solverLog = ILogger::getLogger( "solver_log" );
                solverLog.setLevel( ILogger::DEBUG );
                solverLog << currSolutionInfo.getName() << " hit max price change, " << currSolutionInfo.getPrice() << ','
                    << newPrice << ',' << currSolutionInfo.getPrice() * maxNRStep << ','
                    << newPrice / currSolutionInfo.getPrice()  << endl;
                newPrice = currSolutionInfo.getPrice() * maxNRStep;
            }
            else if ( newPrice < currSolutionInfo.getPrice() / maxNRStep ){
                ILogger& solverLog = ILogger::getLogger( "solver_log" );
                solverLog.setLevel( ILogger::DEBUG );
                solverLog << currSolutionInfo.getName() << " hit max price change, " << currSolutionInfo.getPrice() << ','
                    << newPrice << ',' << currSolutionInfo.getPrice() / maxNRStep << ','
                    << currSolutionInfo.getPrice() / newPrice << endl;
                newPrice = currSolutionInfo.getPrice() / maxNRStep;
            }
            
            currSolutionInfo.setPrice( newPrice );
        }
        else {
            ILogger& solverLog = ILogger::getLogger( "solver_log" );
            solverLog.setLevel( ILogger::ERROR );
            solverLog << "Correcting invalid price generated by Newton-Raphson in market "
                << aSolutionSet.getSolvable( i ).getName() << ".  Price was "
                << newPrice << endl;
            // Restore prices and return failure.
            restorePrices( aSolutionSet, storedPrices );
            return false;
        }
    }
    return true;
}

/*! \brief Calculate the LU factorization of a matrix using partial pivoting.
 * \author Pralit Patel
 * \details This function changes an input matrix to an LU factorizated version and also
 *          produces a permutation matrix. This can be used in conjunction with lu_substitue
 *          to solve a set of equations of the form Ax=b and is less computationaly intensive
 *          than finding the inverse of A and multiplying that by b.
 *          This function also checks if the matrix is singular in which case garbage may
 *          be returned since no inverse exists.  The return value should always be
 *          checked after a call to this method.
 * \param aInputMatrix Matrix that will be LU factorized.
 * \param aPermMatrix A permutation matrix that will hold the permutations used for LU decomposition.
 * \return Whether the factorization was succesful.  It may not be successful if the input matrix is
 *          is singular in which case the contents of the matrix and permutation matrix will be garbage.
 */
bool SolverLibrary::luFactorizeMatrix( Matrix& aInputMatrix, PermutationMatrix& aPermMatrix ) {
    using namespace boost::numeric::ublas;
    
    // need to reset the permutation matrix
    PermutationMatrix tempPerm( aInputMatrix.size1() );
    aPermMatrix.assign( tempPerm );
    
    // do the LU-factorization, the return value is the singular row + 1
    // and so if the return value is 0 there were no singularities
    return lu_factorize( aInputMatrix, aPermMatrix ) != 0;
}

/*! \brief Bracket a set of markets.
* \details Function finds bracket interval for each market and puts this
*          information into solution set vector
* \author Sonny Kim, Josh Lurz, Steve Smith, Kate Calvin
* \param aMarketplace Marketplace reference.
* \param aWorld World reference.
* \param aDefaultBracketInterval The default bracket interval by which trial values are moved
*                                which may be overriden by a SolutionInfo.  The bracket interval
*                                is used as the multiplier to expand trial brackets.
* \param aMaxIterations The maximum iterations allowed to find a brackets.
* \param aSolutionSet Vector of market solution information
* \param aCalcCounter The calculation counter.
* \param aPeriod Model period
* \return Whether bracketing of all markets completed successfully.
*/
bool SolverLibrary::bracket( Marketplace* aMarketplace, World* aWorld, const double aDefaultBracketInterval,
                             const unsigned int aMaxIterations, SolutionInfoSet& aSolutionSet, CalcCounter* aCalcCounter,
                             const ISolutionInfoFilter* aSolutionInfoFilter, const bool aUseSecantBracket, const int aPeriod )
{
    bool code = false;

    // Make sure the markets are up to date before starting.
    aMarketplace->nullSuppliesAndDemands( aPeriod );
#if GCAM_PARALLEL_ENABLED
    aWorld->calc( aPeriod, aWorld->getGlobalFlowGraph() );
#else
    aWorld->calc( aPeriod );
#endif
    aSolutionSet.updateSolvable( aSolutionInfoFilter );
    aSolutionSet.resetBrackets();

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Entering bracketing" << endl;
    
    if( aSolutionSet.getNumSolvable() == 0 ) {
        solverLog << "Exiting bracketing early due to empty solvable set." << endl;
        return true;
    }
    
    // Set up the EDFun wrapper which we will use to do the model evaluations.
    // This way we can re-use the same concepts to backtrack on a bracket step
    // similar to how we do it in the linesearch algorithm used in NR.
    LogEDFun edFun(aSolutionSet, aWorld, aMarketplace, aPeriod, false);
    UBVECTOR fx(aSolutionSet.getNumSolvable());
    UBVECTOR x(aSolutionSet.getNumSolvable());
    UBVECTOR prev_x(aSolutionSet.getNumSolvable());
    UBVECTOR dx(aSolutionSet.getNumSolvable());
    for(int i = 0; i < aSolutionSet.getNumSolvable(); ++i) {
        x[i] = aSolutionSet.getSolvable(i).getPrice();
    }
    edFun.scaleInitInputs(x);
    edFun(x,fx);
    double currFX = fx.dot(fx);
    double prevFX;
    
    // save initial values in case the solution becomes substantially worse
    // and we need to revert to them
    double initialFX = currFX;
    UBVECTOR initialX = x;
    
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << aSolutionSet << endl;
    solverLog << endl << "Initial FX: " << currFX << endl;

    ILogger& singleLog = ILogger::getLogger( "single_market_log" );

    // Loop is done at least once.
    unsigned int iterationCount = 1;
    do {
        prev_x = x;
        prevFX = currFX;
        aSolutionSet.printMarketInfo( "Bracket All", aCalcCounter->getPeriodCount(), singleLog );

        // Iterate through each market.
        for ( unsigned int i = 0; i < aSolutionSet.getNumSolvable(); i++ ) {
            // Fetch the current 
            SolutionInfo& currSol = aSolutionSet.getSolvable( i );
            double currBracketInterval = currSol.getBracketInterval( aDefaultBracketInterval );
            
            // Check for special case where a resource with no supply can become "solved" during
            // the bracketing procedure.
            if( fabs( currSol.getSupply() ) < util::getSmallNumber() &&
                fabs( currSol.getDemand() ) < util::getSmallNumber() )
            {
                currSol.setBracketed();
            }
            if ( !currSol.isBracketed() ) {
                // If a market is not bracketed, then EDL and EDR have the same sign
                // Check if ED has the same sign as EDL and EDR.
                if ( util::sign( currSol.getED() ) == util::sign( currSol.getEDLeft() ) ) {
                    // If Supply > Demand at point X, then we want to decrease x to increase demand
                    // If ED is negative, then so are EDL and EDR
                    // So, X, XL, and XR are all greater than the solution price
                    if ( currSol.getED() < 0 ) {
                        currSol.takeBracketStep(false, currBracketInterval, aUseSecantBracket);
                    } // END: if statement testing if ED < 0
                    // If Supply <= Demand. Price needs to increase so demand decreases
                    // If ED is positive, then so are EDL and EDR
                    // So, X, XL, and XR are all less than the solution price
                    else {
                        currSol.takeBracketStep(true, currBracketInterval, aUseSecantBracket);
                    } // END: if statement testing if ED > 0
                } // END: if statement testing if ED and EDL have the same sign
                // If market is unbracketed, EDL and EDR have the same sign
                // ED has the opposite sign of EDL and EDR
                else {
                    // If ED < 0, then EDL > 0 and EDR > 0
                    // To bracket we just need to move XR to X
                    if ( currSol.getED() < 0 ) {
                        currSol.moveRightBracketToX();
                    } // END: if statement testing if ED < 0
                    // If ED > 0, then EDL < 0 and EDR < 0
                    // To bracket, we just need to move XL to X
                    else {
                        currSol.moveLeftBracketToX();
                    } // END: if statement testing if ED > 0
                } // END: if statement testing if ED and EDL have opposite signs

                // Check if current marketed is now bracketed
                // If so, set bracketed to true
                if( currSol.isCurrentlyBracketed() ){
                    currSol.setBracketed();
                }

            } // END: if statement testing if bracketed
            // If bracketed, but left and right prices are equal
            else if ( currSol.getBracketSize() == 0 ){
                // If XL and XR are equal, market is not bracketed
                // If ED, EDL and EDR all have same sign, set bracketed to false
                if ( util::sign( currSol.getED() ) == util::sign( currSol.getEDLeft() ) ) {
                    currSol.resetBrackets();
                }
                // if ED < EDL, then X > XL
                // if we move XR to X, then market will be bracketed
                else if ( currSol.getED() < currSol.getEDLeft() ) {
                    currSol.moveRightBracketToX();

                    // Check if market is currently bracketed
                    // If so, set bracketed to true
                    if( currSol.isCurrentlyBracketed() ){
                        currSol.setBracketed();
                    }
                }
                // if ED > EDL, then X < XL
                // if we move XL to X, then market will be bracketed
                else {
                    currSol.moveLeftBracketToX();

                    // Check if market is currently bracketed
                    // If so, set bracketed to true
                    if( currSol.isCurrentlyBracketed() ){
                        currSol.setBracketed();
                    }
                }
            } // END: if statement testing if currSol is bracketed with XL == XR
            x[i] = currSol.getPrice();
        } // end for loop

        // Rescale prices to be normalized then run an iteration
        edFun.scaleInitInputs(x);
        edFun(x, fx);
        currFX = fx.dot(fx);
        solverLog << "Current FX: " << currFX << endl;
        
        // Check if this bracket step has increased the "error" F dot F by more than the
        // allowable threshold and walk back the step by half until it no longer does.
        const double FX_INCREASE_THRESHOLD = 10.0;
        double stepMult = 1.0;
        dx = x - prev_x;
        while(currFX > (prevFX * FX_INCREASE_THRESHOLD)) {
            stepMult /= 2.0;
            x = prev_x + dx * stepMult;
            edFun(x, fx);
            currFX = fx.dot(fx);
            solverLog << "Walked back: " << stepMult << ", Current FX: " << currFX << endl;
        }
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracket: " << iterationCount << endl;
        solverLog << aSolutionSet << endl;
    } while ( ++iterationCount <= aMaxIterations && !aSolutionSet.isAllBracketed() );

    code = ( aSolutionSet.isAllBracketed() ? true : false );
    
    // do not allow the overall solution to get worse by three orders of magnitude
    if(currFX > (initialFX * 1e3)) {
        solverLog.setLevel( ILogger::WARNING );
        solverLog << "Final FX: " << currFX << " increased from: " << initialFX<< " beyond thge allowable limit." << endl;
        solverLog << "Brackets may be unreliable, reseting to initial prices and marking failure." << endl;
        // reset brackets and go back to the original price vector
        aSolutionSet.resetBrackets();
        edFun(initialX, fx);
        code = false;
    }

    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution Info Set before leaving bracket: " << endl;
    solverLog << aSolutionSet << endl;

    aSolutionSet.printMarketInfo( "End Bracketing Attempt", 0, singleLog );

    return code;
}

/*
 * \brief Function finds bracket interval for a single market.
 * \author Josh Lurz
 * \param aMarketplace Marketplace reference.
 * \param aWorld World reference.
 * \param aDefaultBracketInterval The default bracket interval by which trial values are moved
 *                                which may be overriden by a SolutionInfo.  The bracket interval
 *                                is used as the multiplier to expand trial brackets.
 * \param aMaxIterations The maximum iterations allowed to find a brackets.
 * \param aSolSet The solution info set.
 * \param aSol The solution info to bracket.
 * \param aCalcCounter The calculation counter.
 * \param period Model period
 * \return Whether the market was successfully bracketed.
 */
bool SolverLibrary::bracketOne( Marketplace* aMarketplace, World* aWorld, const double aDefaultBracketInterval,
                                const unsigned int aMaxIterations, SolutionInfoSet& aSolSet, SolutionInfo* aSol,
                                CalcCounter* aCalcCounter, const ISolutionInfoFilter* aSolutionInfoFilter, const int aPeriod )
{
    aSolSet.updateSolvable( aSolutionInfoFilter );
    double bracketInterval = aSol->getBracketInterval( aDefaultBracketInterval );

    // Logging
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Entering single market bracketing for market " << aSol->getName() << "." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution info set prior to bracket one." << endl;
    solverLog << aSolSet << endl;

    ILogger& singleLog = ILogger::getLogger( "single_market_log" );

    aSol->resetBrackets();
    unsigned int numIterations = 0;

    // Loop is done at least once.
    do {
        aSolSet.printMarketInfo( "Bracket One on " + aSol->getName(),
                                 aCalcCounter->getPeriodCount(),
                                 singleLog );

        // If ED at X and L are the same sign.
        if ( util::sign( aSol->getED() ) == util::sign( aSol->getEDLeft() ) ) {
            // If Supply > Demand at point X.
            if ( aSol->getED() < 0 ) {
                aSol->moveRightBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->decreaseX( bracketInterval );
                }
                else {
                    aSol->setBracketed();
                }
            }
            else { // If Supply <= Demand. Price needs to increase.
                aSol->moveLeftBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->increaseX( bracketInterval );
                }
                else {
                    aSol->setBracketed();
                }
            }
        }
        else {  // ED at X and R are the same sign.
            if ( aSol->getED() < 0 ) { // If Supply > Demand at X.
                aSol->moveRightBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->decreaseX( bracketInterval );
                }
                else {
                    aSol->setBracketed();
                }
            }
            else { // If Supply <= Demand at X. Prices need to increase.
                aSol->moveLeftBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->increaseX( bracketInterval );
                }
                else {
                    aSol->setBracketed();
                }
            }
        }
        // Check if the market is actually solved.
        if( aSol->isSolved() ){
            aSol->setBracketed();
            aSol->moveLeftBracketToX();
            aSol->moveRightBracketToX();
        }

        aMarketplace->nullSuppliesAndDemands( aPeriod );
        aWorld->calc( aPeriod );
        aSolSet.updateSolvable( aSolutionInfoFilter );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracketOne." << endl;
        solverLog << *aSol << endl;
    } while ( ++numIterations < aMaxIterations && !aSol->isBracketed() );

    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Exiting single market bracketing." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution info set after bracket one." << endl;
    solverLog << aSolSet << endl;

    return aSol->isBracketed();
}

/*! \brief Store the current prices in the solver set in a vector.
* \param aSolutionSet Solution set.
* \return Vector of prices currently in the solver set.
*/
vector<double> SolverLibrary::storePrices( const SolutionInfoSet& aSolutionSet ){
    vector<double> storedPrices;
    for( unsigned int i = 0; i < aSolutionSet.getNumTotal(); ++i ){
        storedPrices.push_back( aSolutionSet.getAny( i ).getPrice() );
    }
    return storedPrices;
}

/*! \brief Restore prices in the solver set to the values of a given vector.
* \param aSolutionSet Solution set.
* \param aPrices Prices to set into the solver set.
*/
void SolverLibrary::restorePrices( SolutionInfoSet& aSolutionSet, const vector<double>& aPrices ){
    /*! \pre One price per market. */
    assert( aSolutionSet.getNumTotal() == aPrices.size() );
    for( unsigned int i = 0; i < aSolutionSet.getNumTotal(); ++i ){
        aSolutionSet.getAny( i ).setPrice( aPrices[ i ] );
    }
}

std::ostream & operator<<(std::ostream &ostrm, const UBVECTOR &v) {
    ostrm << "(";
    for(size_t i=0; i<v.size(); ++i) {
        if(i>0) {
            // print dividers to make the thing easier to read
            if(i%50 == 0)
                ostrm << "\n" << i << ":\t";
            else if(i%10 == 0)
                ostrm << "\n\t";
            else ostrm << " ";
        }
        ostrm << v[i];
    }
    ostrm << ")";
    return ostrm;
}


std::ostream & operator<<(std::ostream &ostrm, const UBMATRIX &M) {
    int m = M.rows();
    int n = M.cols();
    
    for(int i=0;i<m;++i) {
        ostrm << i << ":   ";
        for(int j=0;j<n;++j) {
            if(j>0 && j%50==0) ostrm << "|";
            if(j>0 && j%10==0) ostrm << "| ";
            ostrm << M(i,j) << " ";
        }
        ostrm << "\n";
    }
    return ostrm;
}
