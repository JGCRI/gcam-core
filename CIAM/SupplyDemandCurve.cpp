/*! 
* \file SupplyDemandCurve.cpp
* \ingroup CIAM
* \brief SupplyDemandCurve class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <cassert>
#include <algorithm>
#include <cmath>
#include "SupplyDemandCurve.h"
#include "Market.h"
#include "Logger.h"
#include "World.h"
#include "Marketplace.h"

using namespace std;

//! Constructor
SupplyDemandCurve::SupplyDemandCurve( Market* marketIn ) {
    // Make sure the pointer is non-null.
    assert( marketIn );
    market = marketIn;
}

//! Destructor
SupplyDemandCurve::~SupplyDemandCurve() {
    for ( vector<SupplyDemandPoint*>::iterator i = points.begin(); i != points.end(); i++ ) {
        delete *i;
    }
}

/*! \brief Calculate given number of supply and demand points.
* \todo It would be good if this used a similiar logic to SolverLibrary::derivatives to save time.
*/

void SupplyDemandCurve::calculatePoints( const int numPoints, World* world, Marketplace* marketplace, const int period ) {
    
    bool calibrationStatus = world->getCalibrationSetting();
    world->turnCalibrationsOff();

    vector<double> priceMults;

    // Determine price ratios.
    const int middle = static_cast<int>( floor( double( numPoints ) / double( 2 ) ) );

    for( int pointNumber = 0; pointNumber < numPoints; pointNumber++ ) {

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

    // Store the market info.
    marketplace->storeinfo( period );

    // get the base price of the market.
    const double basePrice = market->getRawPrice();

    // Save the original point as price 1.
    // ( *iter )->createSDPoint();

    // iterate through the points and determine supply and demand.
    for ( int pointNumber2 = 0; pointNumber2 < numPoints; pointNumber2++ ) {

        // clear demands and supplies.
        marketplace->nullDemands( period );
        marketplace->nullSupplies( period );

        // set the price to the current point.
        // market->setRawPrice( priceMults[ pointNumber2 ] * basePrice );
        market->setRawPrice( pointNumber2 * ( double( 10 ) / double( numPoints - 1 ) ) );

        // calculate the world.
        world->calc( period );

        points.push_back( new SupplyDemandPoint(  market->getRawPrice(), market->getRawDemand(), market->getRawSupply() ) );

    } // Completed iterating through all price points.

    marketplace->restoreinfo( period );

    if ( calibrationStatus ) { // turn end-use calibrations back on if were on originally
        world->turnCalibrationsOn();
    }  
}

//! Print the curve.
void SupplyDemandCurve::print( Logger* sdLog ) const {
    LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and Demand curves for: " << market->getName() << endl;
    LOG( sdLog, Logger::WARNING_LEVEL ) << "Price,Demand,Supply," << endl;

    // Create a copy of the points vector so that we can sort it while keeping the print function constant.
    // Since the vector contains pointers to SupplyDemandPoints, this is relatively inexpensive.
    vector<SupplyDemandPoint*> pointsCopy( points );

    // Sort the SupplyDemandPoint object pointers in the pointsCopy vector by increasing price by using the LesserPrice binary operator. 
    sort( pointsCopy.begin(), pointsCopy.end(), SupplyDemandPoint::LesserPrice() );
    for ( vector<SupplyDemandPoint*>::const_iterator i = pointsCopy.begin(); i != pointsCopy.end(); i++ ) {
        ( *i )->print( sdLog );
    }

    LOG( sdLog, Logger::WARNING_LEVEL ) << endl;
}

//! Constructor
SupplyDemandCurve::SupplyDemandPoint::SupplyDemandPoint( const double priceIn, const double demandIn, const double supplyIn ) 
: price( priceIn ), demand( demandIn ), supply( supplyIn ){
}

//! Get the price
double SupplyDemandCurve::SupplyDemandPoint::getPrice() const {
    return price;
}

// Print the point in a csv format.
void SupplyDemandCurve::SupplyDemandPoint::print( Logger* sdLog ) const {
    LOG( sdLog, Logger::WARNING_LEVEL ) << price << "," << demand << "," << supply << endl;
}

