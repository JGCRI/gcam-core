/*! 
* \file SolverLibrary.cpp
* \ingroup CIAM
* \brief SolverLibrary class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#if defined(_MSC_VER)
#pragma warning( disable: 4275 )
#pragma warning( disable: 4786 )
#endif

#include <mtl/matrix.h>
#include <mtl/mtl.h>
#include <mtl/utils.h>
#include <mtl/lu.h>
#include "Definitions.h"
#include <vector>
#include <map>
#include <cmath>
#include <string>
#include "SolverLibrary.h"
#include "Marketplace.h"
#include "World.h"
#include "Configuration.h"
#include "Util.h"

using namespace std;
using namespace mtl;

//! Constructor.
SolverLibrary::SolutionInfo::SolutionInfo( const string& marketNameIn, const string& marketGoodIn ) : marketName( marketNameIn ), marketGood( marketGoodIn ) {
      X = 0;
      ED = 0;
      demand = 0;
      supply = 0;
      dX = 0;
      XL = 0;
      XR = 0;
      EDL = 0;
      EDR = 0;
      bracketed = false;
}

//! Return name for convenience.
string SolverLibrary::SolutionInfo::getName() const {
   return ( marketName + marketGood );
}

//! Creates and returns a solution vector of markets that require solving. 
vector<SolverLibrary::SolutionInfo> SolverLibrary::getMarketsToSolve( const Marketplace* marketplace, const int period, const bool isNR ) {
   
   vector<SolverLibrary::SolutionInfo> solutionVector;

   // Get the markets to solve from the marketplace.
   const vector< pair< string,string > > marketsToSolve = marketplace->getMarketsToSolve( period, isNR );
   
   // new create the solution structs.
   for ( vector< pair< string, string > >::const_iterator iter = marketsToSolve.begin(); iter != marketsToSolve.end(); iter++ ) {
      
      // Create the new SolutionInfo object. 
      SolutionInfo newSol( iter->first, iter->second );

      // Add it to the new solution vector.
      solutionVector.push_back( newSol );
   }

   // return the new solution vector to the solver.
   return solutionVector;
}

//! Sets the prices contained in the solution vector into their corresponding markets. 
void SolverLibrary::setPricesToMarkets( Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& solutionVector, const int period ) {

   for ( vector<SolverLibrary::SolutionInfo>::const_iterator iter = solutionVector.begin(); iter != solutionVector.end(); iter++ ) {
      marketplace->setRawPrice( iter->marketName, iter->marketGood, iter->X, period );
   }
}

//! Gets the demands, supplies, prices and excess demands from the markets and sets them into their corresponding places in the solution vector.
void SolverLibrary::update( Marketplace* marketplace, vector<SolverLibrary::SolutionInfo>& solutionVector, const int period ) {
 
   for ( vector<SolverLibrary::SolutionInfo>::iterator iter = solutionVector.begin(); iter != solutionVector.end(); iter++ ) {
      iter->ED = marketplace->getRawDemand( iter->marketName, iter->marketGood, period ) - marketplace->getRawSupply( iter->marketName, iter->marketGood, period );
      iter->demand = marketplace->getRawDemand( iter->marketName, iter->marketGood, period );
      iter->supply = marketplace->getRawSupply( iter->marketName, iter->marketGood, period );
      iter->X = marketplace->getRawPrice( iter->marketName, iter->marketGood, period );
   }
}

//! Determines if any price or demand markets have been unbracketed and attempts to restore them to a bracketed state. 
void SolverLibrary::adjustPriceAndDemandMarkets( const Marketplace* marketplace, vector<SolverLibrary::SolutionInfo>& solutionVector, const int period ) {
   for ( vector<SolverLibrary::SolutionInfo>::iterator iter =  solutionVector.begin(); iter != solutionVector.end(); iter++ ) {
      if( marketplace->isPriceOrDemandMarket( iter->marketName, iter->marketGood, period ) ) {
         double rawDemand = marketplace->getRawDemand( iter->marketName, iter->marketGood, period );
         
         if( iter->XL < rawDemand ) {
            iter->XL = rawDemand * 1.5;
         }
         if( iter->XR > rawDemand ) {
            iter->XR = rawDemand / 1.5;
         }
      }
   }
}

//! Finds and returns the maximum excess demand in a solution set.
double SolverLibrary::findMaxExcessDemand( const vector<SolverLibrary::SolutionInfo>& solutionVector, const double excessDemandSolutionFloor, int& worstMarketIndex, const int period ) {
   
   worstMarketIndex = 0;
   double largest = 0;

   for ( int i = 0; i < static_cast<int>( solutionVector.size() ); i++ ) {

      const double relativeED = getRelativeED( solutionVector[ i ].ED, solutionVector[ i ].demand, excessDemandSolutionFloor );
      
      if ( ( fabs( solutionVector[ i ].X ) > util::getSmallNumber() ) && ( relativeED > largest ) ) {
         worstMarketIndex = i;
         largest = relativeED;
      }
   }
   return largest;
}

//! Calculates and returns a relative excess demand.
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

bool SolverLibrary::isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance, const double excessDemandSolutionFloor ) {
   return ( getRelativeED( excessDemand, demand, excessDemandSolutionFloor ) < solutionTolerance );
}

//! Calculate demand elasticities
const vector<double> SolverLibrary::calcDemandElas( const Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& sol, const int marketSolutionNumber, const int per ) {
   
   double ddemand;
   double dprice;
   vector<double> JFD( sol.size() );
   
   for ( int i = 0; i < static_cast<int>( sol.size() ); i++ ) {
      
      ddemand = getLogChangeInRawDemand( marketplace, sol, i, per );
      dprice = getLogChangeInRawPrice( marketplace, sol, marketSolutionNumber, per );
      
      if( dprice == 0 ){
         dprice = util::getSmallNumber();
      }
      
      JFD[ i ] = ddemand / dprice;
      assert( util::isValidNumber( JFD[ i ] ) );
   }
   
   return JFD;
}

//! Calculate supply elasticities
const vector<double> SolverLibrary::calcSupplyElas( const Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& sol, const int marketSolutionNumber, const int per ) {
   
   double dsupply;
   double dprice;
   
   vector<double> JFS( sol.size() );
   
   for ( int i = 0; i < static_cast<int>( sol.size() ); i++ ) {
      
      dsupply = getLogChangeInRawSupply( marketplace, sol, i, per );
      dprice = getLogChangeInRawPrice( marketplace, sol, marketSolutionNumber, per );
      
      if( dprice == 0 ){
         dprice = util::getSmallNumber();
      }
      
      JFS[ i ] = dsupply / dprice;
      assert( util::isValidNumber( JFS[ i ] ) );
   }
   return JFS;
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
* \param sol The vector of SolutionInfo objects which store the current prices, supplies and demands. 
* \param JFDM A matrix of partial derivatives of demands. This matrix is modified by the function and returned by reference.
* \param JFSM A matrix of partial derivatives of supplies. This matrix is modified by the function and returned by reference.
* \param worldCalcCount The current number of iterations of World::calc. This value is modified by the function and returned by reference.
* \param per The current model period.
* \sa NR_Ron
*/
void SolverLibrary::derivatives( Marketplace* marketplace, World* world, vector<SolverLibrary::SolutionInfo>& sol, Matrix& JFDM, Matrix& JFSM, double& worldCalcCount, const int per ) {
   
   cout << endl << "Begin derivative calculation..." << endl;
   const int marketsToSolve = static_cast<int>( sol.size() );
   const double DELTAP = 1e-10; // Orginal, What is the proper value for delta?
   vector<double> tmpJFD( marketsToSolve );
   vector<double> tmpJFS( marketsToSolve );
   
   // Create additive matrices.
   
   // Get the region names from the world.
   const vector<string> regionNames = world->getRegionVector();
   
   // Save original global supplies and demands for error checking.
   vector<double> originalSupplies = marketplace->getSupplies( per );
   vector<double> originalDemands = marketplace->getDemands( per );
   int numMarkets = static_cast<int>( originalSupplies.size() );

   // Additive matrices, indexed by region name and market number.
   map< string, vector< double > > additionalSupplies;
   map< string, vector< double > > additionalDemands;
   
   // Supplies and demands saved from previous region during calculation of additive matrices. 
   vector<double> prevSupplies( numMarkets, 0.0 );
   vector<double> prevDemands( numMarkets, 0.0 );
   
   // clear demands and supplies.
   marketplace->nullDemands( per );
   marketplace->nullSupplies( per );
   
   // iterate over regions and calculate additional supplies and demands for each region for the current prices. 
   for ( vector<string>::const_iterator regionIter = regionNames.begin(); regionIter != regionNames.end(); regionIter++ ) {
      vector<double> currSupplies;
      vector<double> currDemands;
      vector<double> diffInSupplies( numMarkets, 0.0 );
      vector<double> diffInDemands( numMarkets, 0.0 );
      
      // Call world->calc() for this region only. 
      world->calc( per, vector<string>( 1, *regionIter ) );
      worldCalcCount += ( 1.0 / static_cast<double> ( regionNames.size() ) );
      
      currSupplies = marketplace->getSupplies( per );
      currDemands = marketplace->getDemands( per );
      
      // calculate differences between previous supply and supply after calculating supply and demand for this region.
      for( int k = 0; k < numMarkets; k++ ) {
         diffInSupplies[ k ] = currSupplies[ k ] - prevSupplies[ k ];
         diffInDemands[ k ] = currDemands[ k ] - prevDemands[ k ];
      }
      
      // save the current supplies and demands.
      prevSupplies = currSupplies;
      prevDemands = currDemands;
      
      // Insert this regions additional supplies and demands into the additive matrices. 
      additionalSupplies[ *regionIter ] = diffInSupplies;
      additionalDemands[ *regionIter ] = diffInDemands;
   }
   
   marketplace->storeinfo( per ); // store original market info before perturbing price
   
   // Perform optional error checking.
   // This code will sum up the additive value for each market over all regions.
   // These sums are then checked against the original global supplies and demands.
   if( Configuration::getInstance()->getBool( "debugChecking" ) ) {
      // cout << "Checking sums..." << endl;
      // Compute the sum of the regional supplies and demands.
      vector<double> suppliesSum( numMarkets, 0.0 );
      vector<double> demandsSum( numMarkets, 0.0 );
      
      for ( vector<string>::const_iterator checkIter = regionNames.begin(); checkIter != regionNames.end(); checkIter++ ) {
         for ( int currMarkIter = 0; currMarkIter < numMarkets; currMarkIter++ ) {
            suppliesSum[ currMarkIter ] += additionalSupplies[ *checkIter ][ currMarkIter ];
            demandsSum[ currMarkIter ] += additionalDemands[ *checkIter ][ currMarkIter ];
         }
      }
      
      double ErrorCount = 0;
      // Check if the sum adds up to the total. 
      for( int marketCheckIter = 0; marketCheckIter < numMarkets; marketCheckIter++ ) {
         if( fabs( originalSupplies[ marketCheckIter ] - suppliesSum[ marketCheckIter ] ) > 1E-5 ){
            if ( ErrorCount < 0 ) {
               cout << "Error in derivative Calc. Unequal sums: ";
               cout << " S Difference: " << originalSupplies[ marketCheckIter ] - suppliesSum[ marketCheckIter ];
               cout << endl;
            }
            ErrorCount += 1;
         }
         if ( fabs( originalDemands[ marketCheckIter ] - demandsSum[ marketCheckIter ] ) > 1E-5 ) {
            if ( ErrorCount < 0 ) {
               cout << "Error in derivative Calc. Unequal sums: ";
               cout << " D Difference: " << originalDemands[ marketCheckIter ] - demandsSum[ marketCheckIter ];
               cout << endl;
            }
            ErrorCount += 1;
         }
       } 
      if ( ErrorCount > 0 ) {
         cout << "Warning - " << ErrorCount << " supply & demand sums not equal in derivative Calc. " << endl;
      }
   }

   // Sums checking complete.
   // Done creating additive matrices.
   // Now calculate derivatives for each market.
   
   update( marketplace, sol, per );
   for ( int j = 0; j < marketsToSolve; j++ ) {	// j is column index
      
      // Store the original price.
      double storedPrice = sol[ j ].X;

      // Price is near zero.
      if( sol[ j ].X < DELTAP ) {
         sol[ j ].X = DELTAP;
      }
      
      // Price is positive.
      else {
         sol[ j ].X *= ( 1 + DELTAP ); // add price times deltap
      }
      
      setPricesToMarkets( marketplace, sol, per ); // set new price for one market
      
      // Now remove additive supplies and demands.
      // Iterate over all regions within the market.
      const vector<string> containedRegions = marketplace->getContainedRegions( sol[ j ].marketName, sol[ j ].marketGood, per );

      for ( vector<string>::const_iterator regionIter2 = containedRegions.begin(); regionIter2 != containedRegions.end(); regionIter2++ ) {
         // Remove supply.
         marketplace->removeFromRawSupplies( additionalSupplies[ *regionIter2 ], per );
         // Remove demand
         marketplace->removeFromRawDemands( additionalDemands[ *regionIter2 ], per );
      }
      
      world->calc( per, containedRegions );
      worldCalcCount += ( static_cast<double>( containedRegions.size() ) / static_cast<double> ( regionNames.size() ) );
      
      tmpJFD =  calcDemandElas( marketplace, sol, j, per ); // calculate demand elasticities
      tmpJFS =  calcSupplyElas( marketplace, sol, j, per ); // calculate supply elasticities
      
      for ( int i = 0; i < marketsToSolve; i++ ) {// copy column vector to Jacobian Matrix
         JFDM[ i ][ j ] = tmpJFD[ i ]; // i is row index
         JFSM[ i ][ j ] = tmpJFS[ i ]; // i is row index
      }
      
      marketplace->restoreinfo( per ); // restore market supplies and demands.
      sol[ j ].X = storedPrice; //  restore perturbed market price
   }
}

//! Calculate the inverse of a matrix using an LU factorization.
void SolverLibrary::invertMatrix( Matrix& A ) {
   
   // create LU decomposition
   Matrix LU( A.nrows(), A.ncols() );
   
   dense1D<int> pvector( A.nrows() );
   
   copy(A, LU);
   lu_factor( LU, pvector );
   
   // solve
   lu_inverse( LU, pvector, A );
}

//! function to check bracketing -- ???(see details)
/*! if not solving and bracketed prices are converging,
???? what does this mean?*/
void SolverLibrary::checkBracket( const double solutionTolerance, const double excessDemandSolutionFloor, vector<SolverLibrary::SolutionInfo>& sol, bool& allbracketed ){
   
   const int numCurrMarkets = sol.size(); // number of markets to solve
   // try rebracketing by setting bracketed array to false
   
   for( int i = 0; i < numCurrMarkets; i++ ) {
      if ( fabs( sol[ i ].dX ) < util::getSmallNumber() ) {
         allbracketed = false;
         sol[ i ].bracketed = false;
         sol[ i ].XL = sol[ i ].XR = sol[ i ].X; 
         sol[ i ].EDL = sol[ i ].EDR = sol[ i ].ED; 
      }
   }
}

//! Calculate the log change in raw demand for an index in the sol vector.
double SolverLibrary::getLogChangeInRawDemand( const Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& sol, const int solNumber, const int per ) {
   
   double storedDemand = marketplace->getStoredRawDemand( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double demand = marketplace->getRawDemand( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double change;

   // Case 1: Demand or Previous Demand is zero.
   if( storedDemand == 0 || demand == 0 ) {
      change = util::getVerySmallNumber();
   }
   
   // Case 2: Demand and Previous Demand are both positive.
   else if( ( demand > 0 ) && ( storedDemand > 0 ) ) {
      change = log( demand ) - log( storedDemand );
   }
   
   // Case 3: Demand or Previous Demand is negative. This should not occur.
   else {
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   
   return change;
}

//! Calculate the log change in raw supply for an index in the sol vector.
double SolverLibrary::getLogChangeInRawSupply( const Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& sol, const int solNumber, const int per ) {
   
   double storedSupply = marketplace->getStoredRawSupply( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double supply = marketplace->getRawSupply( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double change;

   // Case 1: supply or Previous supply is zero.
   if( storedSupply == 0 || supply == 0 ) {
      change = util::getVerySmallNumber();
   }
   
   // Case 2: supply and Previous supply are both positive.
   else if( ( supply > 0 ) && ( storedSupply > 0 ) ) {
      change = log( supply ) - log( storedSupply );
   }
   
   // Case 3: supply or Previous supply is negative. This should not occur.
   else {
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   
   return change;
}

//! Calculate the log change in raw price for an index in the sol vector.
double SolverLibrary::getLogChangeInRawPrice( const Marketplace* marketplace, const vector<SolverLibrary::SolutionInfo>& sol, const int solNumber, const int per ) {
   
   double storedPrice = marketplace->getStoredRawPrice( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double price = marketplace->getRawPrice( sol[ solNumber ].marketName, sol[ solNumber ].marketGood, per );
   double change;

   // Case 1: price or Previous price is zero.
   if( storedPrice == 0 || price == 0 ) {
      change = util::getVerySmallNumber();
   }
   
   // Case 2: price and Previous price are both positive.
   else if( ( price > 0 ) && ( storedPrice > 0 ) ) {
      change = log( price ) - log( storedPrice );
   }
   
   // Case 3: price or Previous price is negative. This should not occur.
   else {
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   
   return change;
}