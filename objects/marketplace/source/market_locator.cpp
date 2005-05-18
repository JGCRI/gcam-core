/*! 
* \file market_locator.cpp
* \ingroup Objects
* \brief MarketLocator class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <algorithm>
#include <iostream>

#include "marketplace/include/market_locator.h"
#define PERFORM_TIMING 0
#if PERFORM_TIMING
#include "util/base/include/timer.h"
// Static variables used for timing. Static variables are automatically initialized to zero.
static double gTotalLookupTime;
static int gNumLookups;
#endif

using namespace std;

/*! \brief Default constructor */
MarketLocator::MarketLocator():
mMarketList( new RegionOrMarketList() ),
mRegionOrMarketList( new RegionOrMarketList() )
{
}

//! Destructor
MarketLocator::~MarketLocator(){
#if PERFORM_TIMING
    cout << "Total time spent in lookups: " << gTotalLookupTime << " seconds." << endl;
    cout << "Average time per lookup: " << gTotalLookupTime / static_cast<double>( gNumLookups ) << " seconds." << endl;
#endif
}

/*! \brief Add a market to the locator.
* \details This function adds a market's position to the MarketLocator.
* \param aMarket The name of the market area this market is being added to.
* \param aRegion The name of the region containing the new market.
* \param aGoodName The name of the Good this market number represents.
* \param aUniqueNumber A unique market index to use if the market does not already exist.
* \return The market number that was either already existing or added.
*/
int MarketLocator::addMarket( const string& aMarket, const string& aRegion, const string& aGoodName,
                              const int aUniqueNumber )
{
    // First try to add the market list.
    const int goodNumber = mMarketList->addToList( aMarket, aGoodName, aUniqueNumber );

    // Now add to the region list.
    mRegionOrMarketList->addToList( aRegion, aGoodName, goodNumber );

    // Return the Good number used.
    return goodNumber;
}

/*! \brief Find the market number for a given region and good.
* \param aRegion Region to search for.
* \param aGoodName Good to search for.
* \return The market number or MARKET_NOT_FOUND if it is not present.
*/
int MarketLocator::getMarketNumber( const string& aRegion, const string& aGoodName ) const {
    // Compile in extra timing. Note that timing causes signifigant overhead, so timed runs will 
    // take longer. The result is useful to compare across timed runs, not vs untimed runs.
#if PERFORM_TIMING
    Timer timer;
    timer.start();
    const int marketNumber = mRegionOrMarketList->getMarketNumber( aRegion, aGoodName );
    timer.stop();
    gTotalLookupTime += timer.getTimeDifference();
    ++gNumLookups;
    return marketNumber;
#else
    return mRegionOrMarketList->getMarketNumber( aRegion, aGoodName );
#endif
}

//! Constructor
MarketLocator::RegionOrMarketList::RegionOrMarketList():mCachedPosition( 0 ){
}

//! Destructor
MarketLocator::RegionOrMarketList::~RegionOrMarketList(){
    for( vector<RegionOrMarketNode*>::iterator item = mList.begin(); item != mList.end(); ++item ){
        delete *item;
    }
}

/*! \brief Add an item to a RegionOrMarketList.
* \param aName Name of the region or market to add if it does not already exist.
* \param aGoodName Name of the good to add to this region or market.
* \param aUniqueNumber Number to add for the good if it does not already exist.
* \return The number of the market if it already exists, aUniqueNumber otherwise.
*/
int MarketLocator::RegionOrMarketList::addToList( const string& aName, const string& aGoodName, int aUniqueNumber ){
    // Search linearly to find the existing node or insert a new one.
    // Special case for an empty list.
    if( mList.empty() ){
        RegionOrMarketNode* newNode = new RegionOrMarketNode( aName );
        mList.insert( mList.begin(), newNode );
        return newNode->addGood( aGoodName, aUniqueNumber );
    }

    // This searches through the alphabetical list of regions or markets forward, stopping
    // when it finds a name after or equal to the name of the item we are inserting. If it finds an 
    // equal name, it adds the sector name to that nodes list of sectors. If it finds a node after the
    // item's region name, it inserts the new region before that location. If the loop is exiting
    // before finding a node with a name greater than the name of the region we are inserting, it adds
    // the new node to the end of the list.
    for( vector<RegionOrMarketNode*>::iterator item = mList.begin(); item != mList.end(); ++item ){
        // Check if the new region name is after the current node.
        if( (*item)->getName() > aName ){
            // Insert a new node before the current.
            RegionOrMarketNode* newNode = new RegionOrMarketNode( aName );
            // insert inserts the new item before the current item.
            mList.insert( item, newNode );
            // Add the sector to the new node.
            return newNode->addGood( aGoodName, aUniqueNumber );
        }
        // Check if it is a match. If this is true, than the region or market already exists and 
        // the sector node is added to the existing region or market node.
        if( (*item)->getName() == aName ){
            return (*item)->addGood( aGoodName, aUniqueNumber );
        }
    }

    // If we reached here the node should be after the last node in the list.
    RegionOrMarketNode* newNode = new RegionOrMarketNode( aName );
    mList.insert( mList.end(), newNode );
    return newNode->addGood( aGoodName, aUniqueNumber );
}

/*! \brief Find the position of a good within the region or market's list.
* \details This function performs a binary search for the market or region, and then searches into the found
* region or market's list.
* \param aName Name of the region or market to search for.
* \param aGoodName The name of the good to search for.
* \return The market number, MARKET_NOT_FOUND if it does not exist.
*/
int MarketLocator::RegionOrMarketList::getMarketNumber( const string& aName, const string& aGoodName ) const {
    // Check the cached region or market position.
    if( mCachedPosition && mCachedPosition->getName() == aName ){
        return mCachedPosition->getMarketNumber( aGoodName );
    }

    // Check if the list is empty.
    if( mList.empty() ){
        return MARKET_NOT_FOUND;
    }
    // Do a binary search through the list.
    // Need to use ints to avoid unsigned rollover.
    int firstPosition = 0;
    int lastPosition = static_cast<int>( mList.size() - 1 );

    while( firstPosition <= lastPosition ){
        // Note: Bitwise shift to the right(>>) is the same as dividing by two but much faster.
        int middle = ( firstPosition + lastPosition ) >> 1;
        if( mList[ middle ]->getName() == aName ){
            mCachedPosition = mList[ middle ];
            return mList[ middle ]->getMarketNumber( aGoodName );
        }
        if( mList[ middle ]->getName() > aName ){
            lastPosition = middle - 1;
        }
        else {
            firstPosition = middle + 1;
        }
    }
    return MARKET_NOT_FOUND;
}

//! Constructor
MarketLocator::RegionOrMarketNode::RegionOrMarketNode( const string& aName ):
mName( aName )
{
}

//! Destructor
MarketLocator::RegionOrMarketNode::~RegionOrMarketNode(){
    for( GoodList::iterator good = mGoodList.begin(); good != mGoodList.end(); ++good ){
        delete *good;
    }
}

/*! \brief Add a Good to the RegionOrMarketNode.
* \param aGoodName Name of the Good to add.
* \param aUniqueNumber A unique market location to use if the good is added to the list.
* \return aUniqueNumber if the good was added to the list, the market number if it already existed.
*/
int MarketLocator::RegionOrMarketNode::addGood( const string& aGoodName, const int aUniqueNumber ){
    // Search forwards for the Good.
    for( GoodList::iterator curr = mGoodList.begin(); curr != mGoodList.end(); ++curr ){
        if( (*curr)->mName == aGoodName ){
            return (*curr)->mNumber;
        }
    }
    // Add the new market node to the end.
    mGoodList.push_back( new GoodNode( aGoodName, aUniqueNumber ) );
    return aUniqueNumber;
}

/*! \brief Find a market number given a Good name.
* \param aGoodName The name of the Good to search for.
* \return The market number for the Good, MARKET_NOT_FOUND otherwise.
*/
int MarketLocator::RegionOrMarketNode::getMarketNumber( const string& aGoodName ) const {
    // Search linearly through the list.
    for( GoodList::iterator curr = mGoodList.begin(); curr != mGoodList.end(); ++curr ){
        if( (*curr)->mName == aGoodName ){
            // If it is not the first item in the list, move it up one position.
            // This allows frequently requested Goods to be earlier in the list.
            // Testing shows a speed increase of approximately 40 percent per lookup.
            if( curr != mGoodList.begin() ){
                // Postfix iterator returns the current position and then decrements.
                GoodList::iterator prev = curr--;
                // Swap the pointers contained in prev and curr.
                swap( *prev, *curr );
            }
            return (*curr)->mNumber;
        }
    }
    // The good does not exist.
    return MARKET_NOT_FOUND;
}

//! Constructor
MarketLocator::GoodNode::GoodNode( const string& aName, const int aMarketNumber ):
mName( aName ),
mNumber( aMarketNumber )
{
}
