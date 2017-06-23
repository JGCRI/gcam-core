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
* \file supply_demand_curve.cpp
* \ingroup Objects
* \brief SupplyDemandCurve class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cassert>
#include <algorithm>
#include <cmath>
#include "util/base/include/supply_demand_curve.h"
#include "marketplace/include/market.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/world.h"
#include "marketplace/include/marketplace.h"
#include "solution/util/include/edfun.hpp"
#include "containers/include/scenario.h"
#include "util/base/include/manage_state_variables.hpp"

extern Scenario* scenario;

using namespace std;

//! Constructor
SupplyDemandCurve::SupplyDemandCurve( int aMarketNumber, const string& aMarketName ):
mMarketNumber( aMarketNumber ),
mMarketName(aMarketName )
{
}

//! Destructor
SupplyDemandCurve::~SupplyDemandCurve() {
    for ( vector<SupplyDemandPoint*>::iterator i = mPoints.begin(); i != mPoints.end(); i++ ) {
        delete *i;
    }
}

/*! \brief Calculate given number of supply and demand points.
*
* This function first determines a series of price ratios to use to determine the prices to 
* create SupplyDemandPoints for. It then saves the original marketplace information, and perturbs the price
* as specified by the price ratios. Using this new perturbed price, it call World::Calc to determine supply and 
* demand for the market. It saves that point for printing later, and continues to perform this process for each price.
* Finally it restores the original market information.
*
* \param aNumPoints The number of points to calculate.
* \param aSolnSet The solution set to interact with markets through.
* \param aWorld The World object to use for World::calc
* \param aMarketplace The marketplace to use to store and restore information.
* \param aPeriod The period to perform the calculations on.
* \todo Un-hardcode the prices. 
*/

void SupplyDemandCurve::calculatePoints( const int aNumPoints, SolutionInfoSet& aSolnSet, World* aWorld,
                                         Marketplace* aMarketplace, const int aPeriod )
{
    vector<double> priceMults;

    // Determine price ratios.
    const int middle = static_cast<int>( floor( double( aNumPoints ) / double( 2 ) ) );

    for( int pointNumber = 0; pointNumber < aNumPoints; pointNumber++ ) {

        if( pointNumber < middle ) {
            priceMults.push_back( 1 - double( 1 ) / double( middle - abs( middle - pointNumber ) + 2 ) );
        }
        else if( pointNumber == middle ) {
            priceMults.push_back( 1 );
        }
        else {
            priceMults.push_back( 1 + double( 1 ) / double( middle - abs( middle - pointNumber ) + 2 ) );
        }
    }
    
    size_t nsolv = aSolnSet.getNumSolvable();
    using UBVECTOR = boost::numeric::ublas::vector<double>;
    UBVECTOR x( nsolv ), fx( nsolv );
    
    for( size_t i = 0; i < nsolv; ++i ) {
        x[i] = aSolnSet.getSolvable( i ).getPrice();
    }
    
    // double basePrice = x[ mMarketNumber ];

    // This is the closure that will evaluate the ED function
    LogEDFun F(aSolnSet, aWorld, aMarketplace, aPeriod, false);
    F.scaleInitInputs( x );
    
    // Call F( x ), store the result in fx
    F(x,fx);
    // Have the state manage save the current state as a "clean" state.
    scenario->getManageStateVariables()->setPartialDeriv(true);

    // iterate through the points and determine supply and demand.
    for ( int pointNumber2 = 0; pointNumber2 < aNumPoints; pointNumber2++ ) {
        
        F.partial(mMarketNumber);
        // Note x is a scaled price
        // x[ mMarketNumber ] = priceMults[ pointNumber2 ] * basePrice;
        x[ mMarketNumber ] = pointNumber2 * ( double( 10 ) / double( aNumPoints - 1 ) );
        F(x, fx, mMarketNumber);

        mPoints.push_back( new SupplyDemandPoint( aSolnSet.getSolvable( mMarketNumber ).getPrice(),
                                                  aSolnSet.getSolvable( mMarketNumber ).getDemand(),
                                                  aSolnSet.getSolvable( mMarketNumber ).getSupply(),
                                                  fx[ mMarketNumber ] ) );

    } // Completed iterating through all price points.

    // restore state information for summary.
    F.partial(-1);
}

/*! \brief Print the supply demand curve.
*
* This function prints the vector of SupplyDemandPoints created during the calculatePoints function.
* It creates a copy of points and sorts them before printing them. This was done so that the printing function
* could remain constant. The points are printed to the Logger passed as an argument.
*
* \param sdLog Logger to print the points to.
*/
void SupplyDemandCurve::print( ILogger& aSDLog ) const {
    aSDLog << "Supply and Demand curves for: " << mMarketName << endl;
    aSDLog << "Price,Demand,Supply,Fx" << endl;

    // Create a copy of the points vector so that we can sort it while keeping the print function constant.
    // Since the vector contains pointers to SupplyDemandPoints, this is relatively inexpensive.
    vector<SupplyDemandPoint*> pointsCopy( mPoints );

    // Sort the SupplyDemandPoint object pointers in the pointsCopy vector by increasing price by using the LesserPrice binary operator. 
    sort( pointsCopy.begin(), pointsCopy.end(), SupplyDemandPoint::LesserPrice() );
    for ( vector<SupplyDemandPoint*>::const_iterator i = pointsCopy.begin(); i != pointsCopy.end(); i++ ) {
        ( *i )->print( aSDLog );
    }

    aSDLog << endl;
}

//! Constructor
SupplyDemandCurve::SupplyDemandPoint::SupplyDemandPoint( const double aPrice, const double aDemand, const double aSupply, const double aFx )
: mPrice( aPrice ), mDemand( aDemand ), mSupply( aSupply ), mFx( aFx )
{
}

/*! \brief Get the price.
*
* Return the price contained in the point. This is needed for sorting.
*
* \return The price saved within the point.
*/
double SupplyDemandCurve::SupplyDemandPoint::getPrice() const {
    return mPrice;
}

/*! \brief Print the point in a csv format.
*
* Print the point in a csv format to the specified logger.
*
* \param sdLog The Logger to print to.
*/
void SupplyDemandCurve::SupplyDemandPoint::print( ILogger& aSDLog ) const {
    aSDLog << mPrice << "," << mDemand << "," << mSupply << "," << mFx << endl;
}

