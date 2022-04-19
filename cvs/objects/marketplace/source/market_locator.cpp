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
* \file market_locator.cpp
* \ingroup Objects
* \brief MarketLocator class source file.
* \author Josh Lurz
*/

//#include "util/base/include/definitions.h"
#include <cassert>
//#include <string>

#include "marketplace/include/market_locator.h"
//#include "util/base/include/hash_map.h"

/*#define PERFORM_TIMING 0
#if PERFORM_TIMING
#include <iostream>
#include "util/base/include/timer.h"
// Static variables used for timing. Static variables are automatically
// initialized to zero.
static double gTotalLookupTime;
static int gNumLookups;
#endif*/




using namespace std;




/*! \brief Constructor */
MarketLocator::MarketLocator()
//:mLastRegionLookup( mMarketList.end() )
{
    mRegionList.max_load_factor(0.5);
    /*const unsigned int MARKET_REGION_LIST_SIZE = 71;
    mRegionList.reset( new RegionMarketList( MARKET_REGION_LIST_SIZE ) );
    mMarketList.reset( new RegionMarketList( MARKET_REGION_LIST_SIZE ) );*/
}

//! Destructor
MarketLocator::~MarketLocator(){
/*#if PERFORM_TIMING
    cout << "Total time spent in lookups: " << gTotalLookupTime << " seconds." << endl
         << "Average time per lookup: " << gTotalLookupTime / static_cast<double>( gNumLookups )
         << " seconds." << endl;
#endif*/
}

/*! \brief Add a market to the locator.
* \details This function adds a market's position to the MarketLocator.
* \param aMarket The name of the market area to which this market is being
*        added.
* \param aRegion The name of the region containing the new market.
* \param aGoodName The name of the Good this market number represents.
* \param aUniqueNumber A unique market index to use if the market does not
*        already exist.
* \return The market number that was either already existing or added.
*/
int MarketLocator::addMarket( const gcamstr& aMarket,
                              const gcamstr& aRegion,
                              const gcamstr& aGoodName,
                              const int aUniqueNumber )
{
    // Check if the market area exists in the market area list.
    pair<gcamstr, gcamstr> marketKey(aMarket, aGoodName);
    RegionMarketList::iterator iter = mMarketList.find( marketKey );
    
    int goodNumber;
    // The market area does not exist. Create a new entry.
    if( iter == mMarketList.end() ){
        /*boost::shared_ptr<RegionOrMarketNode> newMarketNode( new RegionOrMarketNode( aMarket ) );
        // Add the node to the hashmap.
        mMarketList->insert( make_pair( aMarket, newMarketNode ) );

        // Add the item to it.
        goodNumber = newMarketNode->addGood( aGoodName, aUniqueNumber );*/
        mMarketList[marketKey] = aUniqueNumber;
        goodNumber = aUniqueNumber;
    }
    else {
        goodNumber = (*iter).second;
        // The market area already exists. Add the item to it.
        //goodNumber = iter->second->addGood( aGoodName, aUniqueNumber );
    }
    
    pair<gcamstr, gcamstr> regionKey(aRegion, aGoodName);
    mRegionList[regionKey] = goodNumber;

    // Check if the region exists in the region list.
    /*iter = mRegionList->find( aRegion );

    // The region does not exist. Create a new entry.
    if( iter == mRegionList->end() ){
        boost::shared_ptr<RegionOrMarketNode> newRegionNode( new RegionOrMarketNode( aRegion ) );
        // Add the new region to the region list.
        mRegionList->insert( make_pair( aRegion, newRegionNode ) );
        
        // Add the item to the region list.
        newRegionNode->addGood( aGoodName, goodNumber );
    }
    else {
        // The region already exists. Add the item to it.
        iter->second->addGood( aGoodName, goodNumber );
    }*/

    // Return the good number used.
    return goodNumber;
}

/*! \brief Find the market number for a given region and good.
* \param aRegion Region for which to search.
* \param aGoodName Good for which to search.
* \return The market number or MARKET_NOT_FOUND if it is not present.
*/
int MarketLocator::getMarketNumber( const gcamstr& aRegion, const gcamstr& aGoodName ) const {
    // Compile in extra timing. Note that timing causes significant overhead, so
    // timed runs will take longer. The result is useful to compare across timed
    // runs, not vs non-timed runs.
/*#if PERFORM_TIMING
    Timer timer;
    timer.start();

    // Lookup the market in the marketList.
    int marketNumber = getMarketNumberInternal( aRegion, aGoodName );
    timer.stop();
    gTotalLookupTime += timer.getTimeDifference();
    ++gNumLookups;
    return marketNumber;
#else
    RegionMarketList::const_iterator iter = 
    //return getMarketNumberInternal( aRegion, aGoodName );
#endif*/
    RegionMarketList::const_iterator iter = mRegionList.find(make_pair(aRegion, aGoodName));
    return iter == mRegionList.end() ? MARKET_NOT_FOUND : (*iter).second;
}

