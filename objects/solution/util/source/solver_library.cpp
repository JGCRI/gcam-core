/*! 
* \file solver_library.cpp
* \ingroup objects
* \brief SolverLibrary class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <mtl/matrix.h>
#include <mtl/mtl.h>
#include <mtl/utils.h>
#include <mtl/lu.h>
#include "util/base/include/definitions.h"
#include <vector>
#include <map>
#include <cmath>
#include <string>
#include <algorithm>
#include <iostream>
#include "solution/util/include/solver_library.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_info_set.h"
#include "util/logger/include/ilogger.h"

#include <mtl/matrix.h>
#include <mtl/mtl.h>
#include <mtl/utils.h>

using namespace std;
using namespace mtl;

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

    double retValue;
    double tempDemand = demand;

    if( tempDemand < util::getSmallNumber() ) {
        tempDemand = util::getSmallNumber();
    }

    // Check if the ED is below a minimal value. 
    if( fabs( excessDemand ) < excessDemandFloor ) {
        retValue = 0;
    }

    // Find the ratio of excess demand to demand.
    else {
        retValue = fabs( excessDemand ) / tempDemand * 100;
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

/*! \brief Function to calculate partial derivatives for Newton-Rhaphson method, NR_Ron()
*
* This function calculates matrices of partial derivatives of supplies and demands for all markets which are currently being solved.
* The function uses the fact that changing a regional price while holding all other prices constant can only change markets within that region.
* It first creates matrices of supply and demand which contain the amount of supply and demand added to each market by each region
* using the unchanged prices.
* To do this, the function steps through World::calc(), calling it seperately for each region.
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
* \param JFDM A matrix of partial derivatives of demands. This matrix is modified by the function and returned by reference.
* \param JFSM A matrix of partial derivatives of supplies. This matrix is modified by the function and returned by reference.
* \param worldCalcCount The current number of iterations of World::calc. This value is modified by the function and returned by reference.
* \param per The current model period.
* \todo Move this function into SolverInfoSet.
*/

void SolverLibrary::derivatives( Marketplace* marketplace, World* world, SolverInfoSet& solverSet, const int per ) {

    const double DELTAP = 1e-5;
    const bool doDebugChecks = Configuration::getInstance()->getBool( "debugChecking" );
    ILogger& mainLog = ILogger::getLogger( "solver_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Starting derivative calculation" << endl;

    // Initial call to world.calc to fix problems with calibration.
    marketplace->nullSuppliesAndDemands( per );
    world->calc( per );
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( true );

#if( !NO_REGIONAL_DERIVATIVES )
    // Save original global supplies and demands for error checking.
    const vector<double>& originalSupplies = solverSet.getSupplies();
    const vector<double>& originalDemands = solverSet.getDemands();

    const RegionalSDDifferences& sdDifferences = calcRegionalSDDifferences( marketplace, world, solverSet, per );
    // This code will sum up the additive value for each market over all regions.
    // These sums are then checked against the original global supplies and demands.
    if( doDebugChecks ) {
        doRegionalValuesSum( sdDifferences.supplies , originalSupplies, true );
        doRegionalValuesSum( sdDifferences.demands, originalDemands, true );
    }
#endif
    // Retain the original values. 
    solverSet.storeValues();

    // Calculate derivatives for each market.
    for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
        solverSet.getSolvable( j ).increaseX( DELTAP, DELTAP );
        solverSet.updateToMarkets();

#if( NO_REGIONAL_DERIVATIVES )
        marketplace->nullSuppliesAndDemands( per );
        world->calc( per );
#else

        // Now remove additive supplies and demands.
        // Iterate over all regions within the market.
        vector<string> containedRegions = solverSet.getSolvable( j ).getContainedRegions();
        for ( RegionIterator regionIter = containedRegions.begin(); regionIter != containedRegions.end(); ++regionIter ) {
            // Find the vectors contains supply and demand reductions for this region.
            const vector<double> supplyReductions = util::searchForValue( sdDifferences.supplies, *regionIter );
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
            JFDM[ i ][ j ] = solverSet.getSolvable( j ).getDemandElasWithRespectTo( i );
            JFSM[ i ][ j ] = solverSet.getSolvable( j ).getSupplyElasWithRespectTo( i );
            JF[ i ][ j ] = JFSM[ i ][ j ] - JFDM[ i ][ j ];
            assert( util::isValidNumber( JF[ i ][ j ] ) );
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
*/
void SolverLibrary::calculateNewPricesLogNR( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF ){
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
        double newPrice = exp( NP[ i ] );
        if( !util::isValidNumber( newPrice ) ){
            ILogger& solverLog = ILogger::getLogger( "solver_log" );
            solverLog.setLevel( ILogger::ERROR );
            solverLog << "Correcting invalid price generated by Newton-Raphson in market " 
                << solverSet.getSolvable( i ).getName() << "." << endl;
            solverSet.getSolvable( i ).setPrice( 1 );
        }
        else {
            solverSet.getSolvable( i ).setPrice( newPrice );
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
                    maxDerVal = max( maxDerVal, fabs( JF[ i ][ j ]) );
                }
                solverLog << "Max KDS: " << maxKDSval << ", Max Derivitive: " << maxDerVal;
                solverLog << "Large derivitives against: " << endl;
                for ( unsigned int j = 0; j < solverSet.getNumSolvable(); j++ ) {
                    if ( fabs( JF[ i ][ j ]) > maxDerVal / 100 ) {
                        solverLog << "   Market: " << solverSet.getSolvable( j ).getName() << ", Value: " << JF[ i ][ j ] << endl;
                    }
                }
            }
        }
    }
}

/*! \brief Calculate the inverse of a matrix using an LU factorization.
* \author Josh Lurz
* \details This function uses an LU decomposition to determine the inverse of the matrix. The matrix which was passed in
* is then set to its inverse.
* \param A matrix to invert.
*/
void SolverLibrary::invertMatrix( Matrix& A ) {

    // create LU decomposition
    Matrix LU( A.nrows(), A.ncols() );

    dense1D<int> pvector( A.nrows() );

    copy(A, LU);
    lu_factor( LU, pvector );

    // solve
    lu_inverse( LU, pvector, A );
}

//! Check if regional values sum to a world total.
bool SolverLibrary::doRegionalValuesSum( const RegionalMarketValues& regionalValues, const vector<double>& worldTotals, const bool doPrint ){
    // First check to make sure worldTotals isn't a vector of zeros.
    // While possible, this is likely an error and we should warn.
    SolverLibrary::ApproxEqual approxEq( 0, util::getSmallNumber() );
    if( count_if( worldTotals.begin(), worldTotals.end(), approxEq ) == worldTotals.size() ){
        cout << "Warning: World totals vector is all zeros." << endl;
    }

    // Check and make sure the worldTotals vector isn't empty.
    if( worldTotals.size() == 0 ){
        cout << "Warning: World totals size is zero." << endl;
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
        if( fabs( worldTotals.at( marketCheckIter ) - regionalSums.at( marketCheckIter ) ) > 1E-5 ){
            errorCount++;
            if( doPrint ){
                cout << "Difference between world totals and regional sums of " << worldTotals[ marketCheckIter ] - regionalSums[ marketCheckIter ] << endl;
            }
        }
    } 
    if ( errorCount > 0 ) {
        if( doPrint ){
            cout << "Warning - " << errorCount << " sums not equal in derivative calc." << endl;
        }
        return false;
    }
    return true;
}

//! Calculate regional supplies and demand adders. Should this be in the solverSet or market object itself? Turn region names into map? TODO
// Further thought: Any world.calc call could update regional map, maybe flag for speed. Could check whether region was actually 
// allowed to add to the market as well.
const SolverLibrary::RegionalSDDifferences SolverLibrary::calcRegionalSDDifferences( Marketplace* marketplace, World* world, SolverInfoSet& solverSet, const int per ) {

    // Create additive matrices.
    // Get the region names from the world.
    const vector<string>& regionNames = world->getRegionVector();

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
    vector<string> singleRegion( 1 );

    // iterate over regions and calculate additional supplies and demands for each region for the current prices. 
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

//! Bracketing function only
/* Function finds bracket interval for each market and puts this information into solverSet vector
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param SOLUTION_TOLERANCE Target value for maximum relative solution for worst market 
* \param ED_SOLUTION_FLOOR *Absolute value* beneath which market is ignored 
* \param bracketInterval Relative multipliciatve interval by which trail values are moved
* \param solverSet Vector of market solution information 
* \param allbracketed Boolean that holds bracketing state 
* \param firsttime Boolean that marks first time bracket is performed 
* \param worldCalcCount Counter for number of worldcalc model calls 
* \param per Model period
*/
bool SolverLibrary::bracket( Marketplace* marketplace, World* world, const double bracketInterval, SolverInfoSet& solverSet, const int period ) {
    int numIterations = 0;
    bool code = false;
    bool calibrationStatus = world->getCalibrationSetting();
    static const double LOWER_BOUND = util::getSmallNumber();
    static const int MAX_ITERATIONS = 30;
    // Make sure the markets are up to date before starting.
    solverSet.updateToMarkets();
    marketplace->nullSuppliesAndDemands( period );
    world->calc( period );
    solverSet.updateFromMarkets();
    solverSet.updateSolvable( false );
    solverSet.checkAndResetBrackets();

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Entering bracketing" << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << solverSet << endl;

    // sjs -- turn off calibration to let bracketing operate faster. Let calibrations happen in Bisection
    world->turnCalibrationsOff();    

    // Loop is done at least once.
    do {        
        // Iterate through each market.
        for ( unsigned int i = 0; i < solverSet.getNumSolvable(); i++ ) {
            // Fetch the current 
            SolverInfo& currSol = solverSet.getSolvable( i );
            // If the market is not bracketed.
            if ( !currSol.isBracketed() ) {
                // If ED at X and L are the same sign.
                if ( util::sign( currSol.getED() ) == util::sign( currSol.getEDLeft() ) ) {
                    // If Supply > Demand at point X.
                    if ( currSol.getED() < 0 ) { 
                        currSol.moveLeftBracketToX();
                        if( !currSol.isCurrentlyBracketed() ){
                            currSol.decreaseX( bracketInterval, LOWER_BOUND );
                        } 
                        else {
                            currSol.setBracketed();
                        }
                    }
                    else { // If Supply <= Demand. Price needs to increase.
                        currSol.moveRightBracketToX();
                        if( !currSol.isCurrentlyBracketed() ){
                            currSol.increaseX( bracketInterval, LOWER_BOUND );
                        }
                        else {
                            currSol.setBracketed();
                        }
                    }
                }
                else {  // ED at X and R are the same sign.
                    if ( currSol.getED() < 0 ) { // If Supply > Demand at X. 
                        currSol.moveLeftBracketToX();
                        if( !currSol.isCurrentlyBracketed() ){
                            currSol.decreaseX( bracketInterval, LOWER_BOUND );
                        }
                        else {
                            currSol.setBracketed();
                        }
                    }
                    else { // If Supply <= Demand at X. Prices need to increase.
                        currSol.moveRightBracketToX();
                        if( !currSol.isCurrentlyBracketed() ){
                            currSol.increaseX( bracketInterval, LOWER_BOUND );
                        }
                        else {
                            currSol.setBracketed();
                        }
                    }
                }
            } 
            // Check if the market is actually solved.
            // Small number here and below should actually be the solution tolerance or floor.
            if( currSol.isWithinTolerance( util::getVerySmallNumber(), util::getVerySmallNumber() ) ){
                currSol.setBracketed();
            }
            // Check if the market is unbracketable. Move this check into updateSolvable.-JPL
            else if( ( currSol.getPrice() < util::getVerySmallNumber() ) && ( currSol.getED() < 0 ) ) {
                currSol.setPrice( 0 );
                currSol.resetBrackets();
                currSol.setBracketed();
            }
        } // for 

        solverSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        solverSet.updateFromMarkets();
        solverSet.updateSolvable( false );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracket." << endl;
    } while ( ++numIterations < MAX_ITERATIONS && !solverSet.isAllBracketed() );
    code = ( solverSet.isAllBracketed() ? true : false );	

    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution Info Set before leaving bracket: " << endl;
    solverLog << solverSet << endl;

    if ( calibrationStatus ) {
        world->turnCalibrationsOn();	// sjs -- turn calibration back on if it was on before
    }
    return code;
}

//! Bracketing function only
/* Function finds bracket interval for a single market. 
* \author Josh Lurz
* \param per Model period
*/
bool SolverLibrary::bracketOne( Marketplace* marketplace, World* world, SolverInfoSet& aSolSet, SolverInfo& aSol, const int period ) {
    // Turn off calibration.
    const bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();    

    // Constants.
    static const unsigned int MAX_ITERATIONS = 100;
    const double LOWER_BOUND = 0.5;
    const double BRACKET_MULT = 0.5;
    //  world->calc( period );
    aSolSet.updateFromMarkets();
    aSolSet.updateSolvable( false );

    // Logging
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Entering single market bracketing for market " << aSol.getName() << "." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution info set prior to bracket one." << endl;
    solverLog << aSolSet << endl;

    aSol.resetBrackets();
    unsigned int numIterations = 0;

    // Loop is done at least once.
    do {        
        // If ED at X and L are the same sign.
        if ( util::sign( aSol.getED() ) == util::sign( aSol.getEDLeft() ) ) {
            // If Supply > Demand at point X.
            if ( aSol.getED() < 0 ) { 
                aSol.moveLeftBracketToX();
                if( !aSol.isCurrentlyBracketed() ){
                    aSol.decreaseX( BRACKET_MULT, LOWER_BOUND );
                } 
                else {
                    aSol.setBracketed();
                }
            }
            else { // If Supply <= Demand. Price needs to increase.
                aSol.moveRightBracketToX();
                if( !aSol.isCurrentlyBracketed() ){
                    aSol.increaseX( BRACKET_MULT, LOWER_BOUND );
                }
                else {
                    aSol.setBracketed();
                }
            }
        }
        else {  // ED at X and R are the same sign.
            if ( aSol.getED() < 0 ) { // If Supply > Demand at X. 
                aSol.moveLeftBracketToX();
                if( !aSol.isCurrentlyBracketed() ){
                    aSol.decreaseX( BRACKET_MULT, LOWER_BOUND );
                }
                else {
                    aSol.setBracketed();
                }
            }
            else { // If Supply <= Demand at X. Prices need to increase.
                aSol.moveRightBracketToX();
                if( !aSol.isCurrentlyBracketed() ){
                    aSol.increaseX( BRACKET_MULT, LOWER_BOUND );
                }
                else {
                    aSol.setBracketed();
                }
            }
        }
        // Check if the market is actually solved.
        // Small number here and below should actually be the solution tolerance or floor.
        if( aSol.isWithinTolerance( util::getVerySmallNumber(), util::getVerySmallNumber() ) ){
            aSol.setBracketed();
        }
        // Check if the market is unbracketable. Move this check into updateSolvable.-JPL
        else if( ( aSol.getPrice() < util::getVerySmallNumber() ) && ( aSol.getED() < 0 ) ) {
            // aSol.setPrice( 0 );
            // aSol.resetBrackets();
            aSol.setBracketed();
        }

        aSolSet.updateToMarkets();
        marketplace->nullSuppliesAndDemands( period );
        world->calc( period );
        aSolSet.updateFromMarkets();
        aSolSet.updateSolvable( false );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Completed an iteration of bracketOne." << endl;
        solverLog << aSol << endl;
    } while ( ++numIterations < MAX_ITERATIONS && !aSol.isBracketed() );

    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Exiting single market bracketing." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << "Solution info set after bracket one." << endl;
    solverLog << aSolSet << endl;

    // turn calibration back on if it was on before
    if ( calibrationStatus ) {
        world->turnCalibrationsOn();
    }
    return aSol.isBracketed();
}
