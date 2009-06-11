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
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/calc_counter.h"
#include "util/logger/include/ilogger.h"

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

    // This case should not occur (demand null but with supply),
    // but check and return 0.
    if( demand == 0 && excessDemand <= util::getSmallNumber() ){
        retValue = 0;
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
* The function uses the fact that changing a regional price while holding all other prices constant can only change markets within that region.
* It first creates matrices of supply and demand which contain the amount of supply and demand added to each market by each region
* using the unchanged prices.
* To do this, the function steps through World::calc(), calling it separately for each region.
* Once these matrices are completed, when calculating a derivative, supplies and demands at the base price for the regions within the market
* for which derivatives are being calculated are subtracted from the saved global totals. Then the price of the market is perturbed,
* and World::calc() is only called on regions contained in the market, thus adding back in supplies and demands for regions within the market
* using the perturbed market price. Cross-derivatives are then calculated for the market and original prices, supplies and demands reset.
* Here is a summary of the steps.
* <ol>
*  <li>Create matrices of additional supplies and additional demands added to each market by each region at the base prices.</li>
*  <li>Save the original supplies and demands.</li>
*  <li>Iterate over each market performing the following steps:
*  <ol>
*        <li>Perturb the market price by multiplying by 1 + DELTA.</li>
*        <li>Iterate over all regions contained by the market and remove supplies and demands added to each market by each region, using the additive matrices.</li>
* .      <li>Call World::calc() on only the regions contained by the market.</li>
*        <li>Calculate cross-derivatives of demand and supply.</li>
*        <li>Reset original prices, supplies and demands.</li>
*        </ol></li>
* <li> Return the partial demand matrices of supply and demand. </li>
* </ol>
*
* \param marketplace The marketplace to perform derivative calculations on.
* \param world The world object which is used for calls to World::calc
* \param solverSet The vector of SolutionInfo objects which store the current prices, supplies and demands.
* \param Delta to use when changing prices.
* \param per The current model period.
* \todo Move this function into SolverInfoSet.
*/

void SolverLibrary::derivatives( Marketplace* marketplace, World* world, SolverInfoSet& solverSet,
                                 const double aDeltaPrice, const int per ) {

    const bool doDebugChecks = Configuration::getInstance()->getBool( "debugChecking" );
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Starting derivative calculation" << endl;

    // Initial call to world.calc to fix problems with calibration.
    marketplace->nullSuppliesAndDemands( per );
    world->calc( per );
    solverSet.updateFromMarkets();

    // If updateSolvable changes the size of the solvable set, then the 
    // Jacobian matrix will not be the right size. This will cause problems
    // later in the solver code. KVC doesn't think this update is necessary.
    // SolverInfoSet::UpdateCode solvableChanged = solverSet.updateSolvable( true );

    // If the size of the matrix changed then the inversion will eventually fail.
    // assert( solvableChanged == SolverInfoSet::UNCHANGED );

#if( !NO_REGIONAL_DERIVATIVES )
    // Save original global supplies and demands for error checking.
    const vector<double>& originalSupplies = solverSet.getSupplies();
    const vector<double>& originalDemands = solverSet.getDemands();

    const RegionalSDDifferences& sdDifferences = calcRegionalSDDifferences( marketplace, world, solverSet, per );

    // This code will sum up the additive value for each market over all regions.
    // These sums are then checked against the original global supplies and demands.
    if( doDebugChecks ) {
        doRegionalValuesSum( sdDifferences.supplies, originalSupplies );
        doRegionalValuesSum( sdDifferences.demands, originalDemands );
    }
#endif
    // Retain the original values.
    solverSet.storeValues();

    // Calculate derivatives for each market.
    for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
        solverSet.getSolvable( j ).increaseX( aDeltaPrice, aDeltaPrice );
        solverSet.updateToMarkets();

#if( NO_REGIONAL_DERIVATIVES )
        marketplace->nullSuppliesAndDemands( per );
        world->calc( per );
#else

        // Now remove additive supplies and demands.
        const vector<const objects::Atom*> containedRegions = solverSet.getSolvable( j ).getContainedRegions();

        /*! \invariant There is at least one contained region. */
        assert( !containedRegions.empty() );

        // Iterate over all regions within the market.
        typedef std::vector<const objects::Atom*>::const_iterator RegionIterator;
        for ( RegionIterator regionIter = containedRegions.begin(); regionIter != containedRegions.end(); ++regionIter ) {
            // Find the vectors contains supply and demand reductions for this region.
            /*! \invariant There is a vector of supply additions for this region. */
            assert( util::hasValue( sdDifferences.supplies, *regionIter ) );
            const vector<double> supplyReductions = util::searchForValue( sdDifferences.supplies, *regionIter );

            /*! \invariant There is a vector of demand additions for this region. */
            assert( util::hasValue( sdDifferences.demands, *regionIter ) );
            const vector<double> demandReductions = util::searchForValue( sdDifferences.demands, *regionIter );

            // Iterate through each market and remove its supply and demand.
            for( unsigned int k = 0; k < solverSet.getNumTotal(); ++k ){
                solverSet.getAny( k ).removeFromRawSupply( supplyReductions.at( k ) );
                solverSet.getAny( k ).removeFromRawDemand( demandReductions.at( k ) );
            }
        }

        world->calc( per, containedRegions );
#endif
        solverSet.updateFromMarkets();

        solverSet.getSolvable( j ).calcDemandElas( solverSet );
        solverSet.getSolvable( j ).calcSupplyElas( solverSet );
        solverSet.restoreValues();
    }
}

/* \brief Calculate the JFDM, JFSM, and JF matrices from the derivatives stored in the SolverInfo. */
void SolverLibrary::updateMatrices( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF ){
    for( unsigned int j = 0; j < solverSet.getNumSolvable(); ++j ){
        for( unsigned int i = 0; i < solverSet.getNumSolvable(); ++i ){
            JFDM( i , j ) = solverSet.getSolvable( j ).getDemandElasWithRespectTo( i );
            JFSM( i , j ) = solverSet.getSolvable( j ).getSupplyElasWithRespectTo( i );
            JF( i , j ) = JFSM( i , j ) - JFDM( i , j );
            assert( util::isValidNumber( JF( i , j) ) );
        }
    }

}

/*! \brief Calculate and set new market prices based on Log NR  mechanism

* \param JF matrix
* \param JFDM matrix
* \param JFSM matrix
* \param KS k values supply
* \param KD k values demand
* \param solverSet An object containing the set of MarketInfo objects representing all markets.
* \param period Model period
* \return Whether prices were set successfully.
*/
bool SolverLibrary::calculateNewPricesLogNR( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF ){
    const Configuration* conf = Configuration::getInstance();
    const bool debugChecking = conf->getBool( "debugChecking" );

    vector<double> NP; // adjustment value
    vector<double> KD; // k values demand
    vector<double> KS; // k values supply
    vector<double> KDS; // k values demand - supply

    KD.resize( solverSet.getNumSolvable() );
    KS.resize( solverSet.getNumSolvable() );
    NP.resize( solverSet.getNumSolvable() );
    KDS.resize( solverSet.getNumSolvable() );

    // initialize KD and KS as logs of original demand and supply
    for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
        KD[ i ] = log( max( solverSet.getSolvable( i ).getDemand(), util::getSmallNumber() ) );
        KS[ i ] = log( max( solverSet.getSolvable( i ).getSupply(), util::getSmallNumber() ) );
    }

    for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
        for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
            double tempValue = log( max( solverSet.getSolvable( j ).getPrice(), util::getSmallNumber() ) );
            KD[ i ] -= tempValue * JFDM( i , j );
            KS[ i ] -= tempValue * JFSM( i , j );
            assert( util::isValidNumber( KD[ i ] ) );
            assert( util::isValidNumber( KS[ i ] ) );
        }

        KDS[ i ] = KD[ i ] - KS[ i ];
        assert( util::isValidNumber( KDS[ i ] ) );
    }

    // Store the solver set prices so that they can be restored if NR fails to
    // generate valid prices.
    vector<double> storedPrices = storePrices( solverSet );

    // Calculate new log price based on NR
    for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
        NP[ i ] = 0;
        for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
            // An error at this assert means that something happened to the JF matrix during inversion (since there is an assert checking for valid numbers when JF is created)
            // This is probably a singular matrix where some market has zero derivative for supply and demand
            assert( util::isValidNumber( JF( i , j ) ) );
            NP[ i ] += JF( i , j ) * KDS[ j ];
            assert( util::isValidNumber( NP[ i ] ) );
        }

        // Set the new price with the exponent of the correction vector.
        double newPrice = exp( NP[ i ] );
        if( util::isValidNumber( newPrice ) ){
            static const double MAX_NR_STEP = conf->getDouble( "MAX_NR_STEP", 0.2 );
           
            if ( newPrice > solverSet.getSolvable( i ).getPrice() * (1+MAX_NR_STEP) ){
                newPrice = solverSet.getSolvable( i ).getPrice() * (1+MAX_NR_STEP);
            }
            else if ( newPrice < solverSet.getSolvable( i ).getPrice() / (1+MAX_NR_STEP) ){
                newPrice = solverSet.getSolvable( i ).getPrice() / (1+MAX_NR_STEP);
            }
            
            solverSet.getSolvable( i ).setPrice( newPrice );
        }
        else {
            ILogger& solverLog = ILogger::getLogger( "solver_log" );
            solverLog.setLevel( ILogger::ERROR );
            solverLog << "Correcting invalid price generated by Newton-Raphson in market "
                << solverSet.getSolvable( i ).getName() << "." << endl;
            // Restore prices and return failure.
            restorePrices( solverSet, storedPrices );
            return false;
        }

        // Debugging output.
        if( debugChecking ){
            if ( solverSet.getSolvable( i ).getPrice() > 1e10 ) {
                ILogger& solverLog = ILogger::getLogger( "solver_log" );
                solverLog.setLevel( ILogger::WARNING );
                solverLog << " Large price in market: " << solverSet.getSolvable( i ).getName() << endl;
                // first get largest derivitive
                double maxDerVal = 0;
                double maxKDSval = 0;
                for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                    maxKDSval = max( maxKDSval, fabs( KDS[ j ] ) );
                    maxDerVal = max( maxDerVal, fabs( JF( i , j ) ) );
                }
                solverLog << "Max KDS: " << maxKDSval << ", Max Derivitive: " << maxDerVal;
                solverLog << "Large derivitives against: " << endl;
                for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                    if ( fabs( JF( i , j ) ) > maxDerVal / 100 ) {
                        solverLog << "   Market: " << solverSet.getSolvable( j ).getName() << ", Value: " << JF( i , j ) << endl;
                    }
                }
            }
        }
    }
    return true;
}

/*! \brief Calculate the inverse of a matrix using Gauss-Jordan partial pivoting.
* \author Pralit Patel
* \author Josh Lurz
* \details This function uses Gauss-Jordan partial pivoting to determine the inverse
*          of the matrix. The matrix which was passed in can then be set to its inverse.
*          This function also checks if the matrix is singular in which case garbage may
*          be returned since no inverse exists.  The singular attrubute should always be
*          checked after a call to this method.
* \param aInputMatrix Matrix to invert.
* \param singular An out parameter which is used to determine if the matrix is singular.
*/
Matrix SolverLibrary::invertMatrix( const Matrix& aInputMatrix, bool& aIsSingular ) {
    using namespace boost::numeric::ublas;

    const int size = aInputMatrix.size1();

    // Cannot invert if non-square matrix or 0x0 matrix.
    // Report it as singular in these cases, and return
    // a 0x0 matrix.
    if ( size != aInputMatrix.size2() || size == 0 ) {
        aIsSingular = true;
        Matrix A( 0, 0 );
        return A;
    }

    // Handle 1x1 matrix edge case as general purpose
    // inverter below requires 2x2 to function properly.
    if ( size == 1 ) {
        Matrix A(1, 1);
        if ( aInputMatrix(0,0) == 0.0 ) {
            aIsSingular = true;
            return A;
        }
        aIsSingular = false;
        A(0,0) = 1/aInputMatrix( 0, 0 );
        return A;
    }

    // Create an augmented matrix A to invert. Assign the
    // matrix to be inverted to the left hand side and an
    // identity matrix to the right hand side.
    Matrix A( size, 2*size );
    matrix_range< Matrix > Aleft( A, range( 0, size ), range( 0, size ) );
    Aleft = aInputMatrix;
    matrix_range< Matrix > Aright( A, range( 0, size ), range( size, 2*size ) );
    Aright = identity_matrix<double>( size );

    // Swap rows to eliminate zero diagonal elements.
    for (int k = 0; k < size; k++) {
        if ( A( k, k ) == 0 ) // TODO: test for "small" instead
        {
            // Find a row(l) to swap with row(k)
            int l = -1;
            for ( int i = k+1; i < size; i++ ) {
                if ( A( i,k ) != 0 ) {
                    l = i;
                    break;
                }
            }

            // Swap the rows if found
            if ( l < 0 ) {
                aIsSingular = true;
                return Aleft;
            }
            else {
                matrix_row< Matrix > rowk( A, k );
                matrix_row< Matrix > rowl( A, l );
                rowk.swap( rowl );
            }
        }
    }

    // Doing partial pivot
    for (int k = 0; k < size; k++)
    {
        // normalize the current row
        for ( int j = k+1; j < 2*size; j++ ) {
            A( k, j ) /= A( k, k );
        }
        A( k, k ) = 1;

        // normalize other rows
        for ( int i = 0; i < size; i++ ) {
            if ( i != k ) {
                if ( A( i, k ) != 0 ) {
                    for ( int j = k+1; j < 2*size; j++ ) {
                        A( i, j ) -= A( k, j ) * A( i, k );
                    }
                    A( i, k ) = 0;
                }
            }
        }
    }

    aIsSingular = false;
    return Aright;
}

//! Check if regional values sum to a world total.
bool SolverLibrary::doRegionalValuesSum( const RegionalMarketValues& regionalValues, const vector<double>& worldTotals ){
    const double ERROR_LIMIT = 1E-5;
    // First check to make sure worldTotals isn't a vector of zeros.
    // While possible, this is likely an error and we should warn.
    SolverLibrary::ApproxEqual approxEq( 0, util::getSmallNumber() );
    if( count_if( worldTotals.begin(), worldTotals.end(), approxEq ) == worldTotals.size() ){
        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::WARNING );
        solverLog << "World totals vector is all zeros." << endl;
    }

    // Check and make sure the worldTotals vector isn't empty.
    if( worldTotals.empty() ){
        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::WARNING );
        solverLog << "World totals size is zero." << endl;
    }

    // Compute the sum of the regional values.
    typedef RegionalMarketValues::const_iterator RegionValueIterator;
    vector<double> regionalSums( worldTotals.size() );

    // Loop through the list of regions and add
    for ( RegionValueIterator checkIter = regionalValues.begin(); checkIter != regionalValues.end(); ++checkIter ) {
        assert( checkIter->second.size() == worldTotals.size() );
        for ( unsigned int currMarkIter = 0; currMarkIter < checkIter->second.size(); ++currMarkIter ) {
            regionalSums[ currMarkIter ] += checkIter->second.at( currMarkIter );
        }
    }

    // Check if the sums adds up to the total.
    unsigned int errorCount = 0;
    for( unsigned int marketCheckIter = 0; marketCheckIter < worldTotals.size(); ++marketCheckIter ) {
        if( fabs( worldTotals.at( marketCheckIter ) - regionalSums.at( marketCheckIter ) ) > ERROR_LIMIT ){
            errorCount++;
            ILogger& solverLog = ILogger::getLogger( "solver_log" );
            solverLog.setLevel( ILogger::WARNING );
            solverLog << "Difference between world totals and regional sums of " << worldTotals[ marketCheckIter ] - regionalSums[ marketCheckIter ] << endl;
        }
    }
    if ( errorCount > 0 ) {
        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::WARNING );
        solverLog << errorCount << " sums not equal in derivative calculation." << endl;
        return false;
    }
    return true;
}

//! Calculate regional supplies and demand adders. Should this be in the solverSet or market object itself? Turn region names into map? TODO
// Further thought: Any world.calc call could update regional map, maybe flag for speed. Could check whether region was actually
// allowed to add to the market as well.
const SolverLibrary::RegionalSDDifferences SolverLibrary::calcRegionalSDDifferences( Marketplace* marketplace, World* world, SolverInfoSet& solverSet, const int per ) {

    // Create additive matrices.
    // Get the region IDs from the world.
    const vector<const objects::Atom*>& regionNames = world->getRegionIDs();

    /*! \invariant The region IDs vector is not-empty. */
    assert( !regionNames.empty() );

    // Additive matrices, indexed by region name and market number.
    RegionalSDDifferences regionalDifferences;

    // Supplies and demands saved from previous region during calculation of additive matrices.
    vector<double> prevSupplies( solverSet.getNumTotal() );
    vector<double> prevDemands( solverSet.getNumTotal() );

    // clear demands and supplies.
    marketplace->nullSuppliesAndDemands( per );

    // Declare vectors outside loop to avoid repetive construction.
    vector<double> diffInSupplies( solverSet.getNumTotal() );
    vector<double> diffInDemands( solverSet.getNumTotal() );
    vector<const objects::Atom*> singleRegion( 1 );

    // iterate over regions and calculate additional supplies and demands for each region for the current prices.
    typedef std::vector<const objects::Atom*>::const_iterator RegionIterator;
    for ( RegionIterator regionIter = regionNames.begin(); regionIter != regionNames.end(); ++regionIter ) {

        // Call world->calc() for this region only.
        singleRegion[ 0 ] = *regionIter;
        world->calc( per, singleRegion );

        // determine current supplies and demands.
        solverSet.updateFromMarkets();
        const vector<double> currSupplies = solverSet.getSupplies();
        const vector<double> currDemands = solverSet.getDemands();

        // calculate differences between previous supply and supply after calculating supply and demand for this region.
        for( unsigned int k = 0; k < solverSet.getNumTotal(); k++ ) {
            diffInSupplies.at( k ) = currSupplies.at( k ) - prevSupplies.at( k );
            diffInDemands.at( k ) = currDemands.at( k ) - prevDemands.at( k );
        }

        // save the current supplies and demands.
        prevSupplies = currSupplies;
        prevDemands = currDemands;

        // Insert this regions additional supplies and demands into the additive matrices.
        regionalDifferences.supplies[ *regionIter ] = diffInSupplies;
        regionalDifferences.demands[ *regionIter ] = diffInDemands;
    }

    return regionalDifferences;
}

/*! \brief Bracket a set of markets.
* \details Function finds bracket interval for each market and puts this
*          information into solution set vector
* \author Sonny Kim, Josh Lurz, Steve Smith, Kate Calvin
* \param aSolutionTolerance Target value for maximum relative solution for worst
*        market
* \param aSolutionFloor Absolute value beneath which market is ignored
* \param bracketInterval Relative interval by which trial values are moved.
* \param solverSet Vector of market solution information
* \param aCalcCounter The calculation counter.
* \param period Model period
* \return Whether bracketing of all markets completed successfully.
*/
bool SolverLibrary::bracket( Marketplace* marketplace, World* world, const double aBracketInterval,
                             const double aSolutionTolerance, const double aSolutionFloor,
                             SolverInfoSet& solverSet, CalcCounter* aCalcCounter, const int period ) {
    bool code = false;
    // Return with code true if all markets are bracketed.
    if( solverSet.isAllBracketed() ){
        return code = true;
    }

    static const double LOWER_BOUND = util::getVerySmallNumber();
    static const int MAX_ITERATIONS = Configuration::getInstance()->
                                      getInt( "MAX_BRACKET_ITERATIONS", 40 );

    // Make sure the markets are up to date before starting.
    solverSet.updateToMarkets();
    marketplace->nullSuppliesAndDemands( period );
    world->calc( period );
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Entering bracketing" << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << solverSet << endl;

    ILogger& singleLog = ILogger::getLogger( "single_market_log" );

    // Loop is done at least once.
    unsigned int iterationCount = 1;
    do {
        solverSet.printMarketInfo( "Bracket All", aCalcCounter->getPeriodCount(), singleLog );

        // Iterate through each market.
        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
            // Fetch the current 
            SolverInfo& currSol = solverSet.getSolvable( i );
            
            // We know current market is not solved. Check if it is bracketed
            if ( !currSol.isBracketed() ) {
                // If a market is not bracketed, then EDL and EDR have the same sign
                // Check if ED has the same sign as EDL and EDR.
                if ( util::sign( currSol.getED() ) == util::sign( currSol.getEDLeft() ) ) {
                    // If Supply > Demand at point X, then we want to decrease x to increase demand
                    // If ED is negative, then so are EDL and EDR
                    // So, X, XL, and XR are all greater than the solution price
                    if ( currSol.getED() < 0 ) {
                        currSol.moveRightBracketToX();
                        currSol.decreaseX( aBracketInterval, LOWER_BOUND );
                    } // END: if statement testing if ED < 0
                    // If Supply <= Demand. Price needs to increase so demand decreases
                    // If ED is positive, then so are EDL and EDR
                    // So, X, XL, and XR are all less than the solution price
                    else {
                        currSol.moveLeftBracketToX();
                        currSol.increaseX( aBracketInterval, LOWER_BOUND );
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

                // Check if the market is unbracketable. Move this check into updateSolvable.-JPL
                if( ( currSol.getPrice() < util::getVerySmallNumber() ) && ( currSol.getED() < 0 ) ) {
                    currSol.setPrice( 0 );
                    currSol.resetBrackets();
                    currSol.setBracketed();
                } // END: if statement testing if the market is unbracketable

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
        } // end for loop

        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracket: " << iterationCount << endl;
        solverLog << solverSet << endl;
    } while ( ++iterationCount <= MAX_ITERATIONS && !solverSet.isAllBracketed() );

    code = ( solverSet.isAllBracketed() ? true : false );

    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution Info Set before leaving bracket: " << endl;
    solverLog << solverSet << endl;

    solverSet.printMarketInfo( "End Bracketing Attempt", 0, singleLog );

    return code;
}

/*
 * \brief Function finds bracket interval for a single market.
 * \author Josh Lurz
 * \param aBracketInterval Multiplier to use when adjusting the bracket size.
 * \param aSolutionTolerance Solution Tolerance
 * \param aSolutionFloor Solution Floor
 * \param aSolSet The solver info set.
 * \param aSol The solver info to bracket.
 * \param aCalcCounter The calculation counter.
 * \param period Model period
 * \return Whether the market was successfully bracketed.
 */
bool SolverLibrary::bracketOne( Marketplace* marketplace, World* world, const double aBracketInterval,
                                const double aSolutionTolerance, const double aSolutionFloor,
                                SolverInfoSet& aSolSet, SolverInfo* aSol, CalcCounter* aCalcCounter,
                                const int period )
{
    static const double LOWER_BOUND = util::getSmallNumber();

    // Constants.
    static const unsigned int MAX_ITERATIONS = 100;
    //  world->calc( period );
    aSolSet.updateFromMarkets();
    aSolSet.updateSolvable( false );

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
                    aSol->decreaseX( aBracketInterval, LOWER_BOUND );
                }
                else {
                    aSol->setBracketed();
                }
            }
            else { // If Supply <= Demand. Price needs to increase.
                aSol->moveLeftBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->increaseX( aBracketInterval, LOWER_BOUND );
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
                    aSol->decreaseX( aBracketInterval, LOWER_BOUND );
                }
                else {
                    aSol->setBracketed();
                }
            }
            else { // If Supply <= Demand at X. Prices need to increase.
                aSol->moveLeftBracketToX();
                if( !aSol->isCurrentlyBracketed() ){
                    aSol->increaseX( aBracketInterval, LOWER_BOUND );
                }
                else {
                    aSol->setBracketed();
                }
            }
        }
        // Check if the market is actually solved.
        if( aSol->isSolved( aSolutionTolerance, aSolutionFloor ) ){
            aSol->setBracketed();
            aSol->moveLeftBracketToX();
            aSol->moveRightBracketToX();
        }

        aSolSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        aSolSet.updateFromMarkets();
        aSolSet.updateSolvable( false );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracketOne." << endl;
        solverLog << *aSol << endl;
    } while ( ++numIterations < MAX_ITERATIONS && !aSol->isBracketed() );

    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Exiting single market bracketing." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution info set after bracket one." << endl;
    solverLog << aSolSet << endl;

    return aSol->isBracketed();
}

/*! \brief Store the current prices in the solver set in a vector.
* \param aSolverSet Solver set.
* \return Vector of prices currently in the solver set.
*/
vector<double> SolverLibrary::storePrices( const SolverInfoSet& aSolverSet ){
    vector<double> storedPrices;
    for( unsigned int i = 0; i < aSolverSet.getNumTotal(); ++i ){
        storedPrices.push_back( aSolverSet.getAny( i ).getPrice() );
    }
    return storedPrices;
}

/*! \brief Restore prices in the solver set to the values of a given vector.
* \param aSolverSet Solver set.
* \param aPrices Prices to set into the solver set.
*/
void SolverLibrary::restorePrices( SolverInfoSet& aSolverSet, const vector<double>& aPrices ){
    /*! \pre One price per market. */
    assert( aSolverSet.getNumTotal() == aPrices.size() );
    for( unsigned int i = 0; i < aSolverSet.getNumTotal(); ++i ){
        aSolverSet.getAny( i ).setPrice( aPrices[ i ] );
    }
}
