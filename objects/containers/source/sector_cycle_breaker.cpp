/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file sector_cycle_breaker.cpp
 * \ingroup objects
 * \brief The SectorCycleBreaker source file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"
#include <list>
#include <stack>
#include <algorithm>
#include "containers/include/sector_cycle_breaker.h"
#include "containers/include/dependency_finder.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/marketplace.h"

using namespace std;

SectorCycleBreaker::SectorCycleBreaker( Marketplace* aMarketPlace,
                                        const string& aRegionName):
ICycleBreaker(),
mRegionName( aRegionName),
mMarketplace( aMarketPlace )
{
}

//! Virtual destructor.
SectorCycleBreaker::~SectorCycleBreaker(){
}

void SectorCycleBreaker::breakCycle( DependencyFinder &aDependencyFinder,
                                     const size_t aFirstSector,
                                     const size_t aSecondSector ){
    // Notify that we are removing a cycle.
    ILogger& depFinderLog = ILogger::getLogger( "dependency_finder_log" );
    depFinderLog.setLevel( ILogger::DEBUG );
    depFinderLog << "Breaking cycle between " 
                 << aDependencyFinder.getNameFromIndex( aFirstSector ) << " and "
                 << aDependencyFinder.getNameFromIndex( aSecondSector ) << "." << endl;

    // Add simul markets to remove the dependency. Note that one of these
    // sectors may already have a simul market setup for it, the marketplace
    // will ignore the request to convert the market in that case.
    mMarketplace->resetToPriceMarket( aDependencyFinder.getNameFromIndex( aFirstSector ),
                                      mRegionName );
    mMarketplace->resetToPriceMarket( aDependencyFinder.getNameFromIndex( aSecondSector ),
                                      mRegionName );
    
    // Remove the cycle from the graph by removing both edges.
    aDependencyFinder.removeDependency( aFirstSector, aSecondSector );
    aDependencyFinder.removeDependency( aSecondSector, aFirstSector );
}


