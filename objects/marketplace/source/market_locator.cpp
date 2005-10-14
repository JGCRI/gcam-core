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

#include "marketplace/include/market_locator.h"
#include "util/base/include/hash_map.h"

#define PERFORM_TIMING 0
#if PERFORM_TIMING
#include <iostream>
#include "util/base/include/timer.h"
// Static variables used for timing. Static variables are automatically
// initialized to zero.
static double gTotalLookupTime;
static int gNumLookups;
#endif

using namespace std;



/*! \briefConstructor */
MarketLocator::MarketLocator(){
    const unsigned int MARKET_REGION_LIST_SIZE = 71;
    mRegionList.reset( new RegionMarketList( MARKET_REGION_LIST_SIZE ) );
    mMarketList.reset( new RegionMarketList( MARKET_REGION_LIST_SIZE ) );
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
* \param aMarket The name of the market area to which this market is being
*        added.
* \param aRegion The name of the region containing the new market.
* \param aGoodName The name of the Good this market number represents.
* \param aUniqueNumber A unique market index to use if the market does not
*        already exist.
* \return The market number that was either already existing or added.
*/
int MarketLocator::addMarket( const string& aMarket, const string& aRegion, const string& aGoodName,
							  const int aUniqueNumber )
{
	// Check if it exists in the market list.
	RegionMarketList::iterator iter = mMarketList->find( aMarket );
	boost::shared_ptr<RegionOrMarketNode> marketNode = ( iter != mMarketList->end() ) ? iter->second : boost::shared_ptr<RegionOrMarketNode>();
	if( !marketNode.get() ){
		// Create the market node if it does not exist.
		marketNode.reset( new RegionOrMarketNode( aMarket ) );
		// Add the node to the hashmap.
		mMarketList->insert( make_pair( aMarket, marketNode ) );
	}

	// Add the item to it.
	const int goodNumber = marketNode->addGood( aGoodName, aUniqueNumber );

	// Check if the region exists in the region list.
	iter = mRegionList->find( aRegion );
	boost::shared_ptr<RegionOrMarketNode> regionNode = ( iter != mRegionList->end() ) ? iter->second : boost::shared_ptr<RegionOrMarketNode>();
	if( !regionNode.get() ){
		// Create the region node if it does not exist.
		regionNode.reset( new RegionOrMarketNode( aRegion ) );
		// Add the node to the hashmap.
		mRegionList->insert( make_pair( aRegion, regionNode ) );
	}

	// Now add the item to the region list.
	regionNode->addGood( aGoodName, goodNumber );

	// Return the good number used.
	return goodNumber;
}

/*! \brief Find the market number for a given region and good.
* \param aRegion Region for which to search.
* \param aGoodName Good for which to search.
* \return The market number or MARKET_NOT_FOUND if it is not present.
*/
int MarketLocator::getMarketNumber( const string& aRegion, const string& aGoodName ) const {
	// Compile in extra timing. Note that timing causes signifigant overhead, so
	// timed runs will take longer. The result is useful to compare across timed
    // runs, not vs untimed runs.
#if PERFORM_TIMING
	Timer timer;
	timer.start();

	// Lookup the market in the marketList.
	int marketNumber = getMarketNumberInternal( aRegion, aGoodName );
	timer.stop();
	gTotalLookupTime += timer.getTimeDifference();
	++gNumLookups;
	return marketNumber;
#else
	return getMarketNumberInternal( aRegion, aGoodName );
#endif
}

/*! \brief Internal calculation which determines the market number from a region
*          and good name.
* \details Performs the calculation which determines the market number from a
*          region and good name.
* \param aRegion Region for which to search.
* \param aGoodName Good for which to search.
* \return The number of the associated market or MARKET_NOT_FOUND if it does not
*         exist.
*/
int MarketLocator::getMarketNumberInternal( const string& aRegion, const string& aGoodName ) const {
	boost::shared_ptr<const RegionOrMarketNode> region;
	if( mLastRegionLookup.get() && mLastRegionLookup->getName() == aRegion ){
		region = mLastRegionLookup;
	}
	else {
		RegionMarketList::const_iterator iter = mRegionList->find( aRegion );
		mLastRegionLookup = region = ( iter != mRegionList->end() ) ? iter->second : boost::shared_ptr<RegionOrMarketNode>();
	}
	return region.get() ? region->getMarketNumber( aGoodName ) : MARKET_NOT_FOUND;
}

//! Constructor
MarketLocator::RegionOrMarketNode::RegionOrMarketNode( const string& aName ):
mName( aName ){
    const unsigned int SECTOR_LIST_SIZE = 51;
    mSectorNodeList.reset( new SectorNodeList( SECTOR_LIST_SIZE ) );
}

//! Destructor
MarketLocator::RegionOrMarketNode::~RegionOrMarketNode(){
}

/*! \brief Add a Good to the RegionOrMarketNode.
* \param aGoodName Name of the Good to add.
* \param aUniqueNumber A unique market location to use if the good is added to
*        the list.
* \return aUniqueNumber if the good was added to the list, the market number if
*         it already existed.
*/
int MarketLocator::RegionOrMarketNode::addGood( const string& aGoodName, const int aUniqueNumber ){
	// Check if it exists in the good list.
	SectorNodeList::iterator iter = mSectorNodeList->find( aGoodName );
	boost::shared_ptr<GoodNode> goodNode;
	if( iter != mSectorNodeList->end() ){
		goodNode = iter->second;
	}

	if( !goodNode.get() ){
		// Create the good node if it does not exist.
		goodNode.reset( new GoodNode( aGoodName, aUniqueNumber ) );

		// Add the node to the hashmap.
		mSectorNodeList->insert( make_pair( aGoodName, goodNode ) );
	}
	// Return the good number.
	return goodNode->mNumber;
}

/*! \brief Find a market number given a Good name.
* \param aGoodName The name of the Good to search for.
* \return The market number for the Good, MARKET_NOT_FOUND otherwise.
*/
int MarketLocator::RegionOrMarketNode::getMarketNumber( const string& aGoodName ) const {
	// Check if it exists in the good list.
	SectorNodeList::const_iterator iter = mSectorNodeList->find( aGoodName );
	boost::shared_ptr<const GoodNode> goodNode = ( iter != mSectorNodeList->end() ) ? iter->second : boost::shared_ptr<GoodNode>();
	return goodNode.get() ? goodNode->mNumber : MARKET_NOT_FOUND;
}

//! Constructor
MarketLocator::GoodNode::GoodNode( const string& aName, const int aMarketNumber ):
mName( aName ),
mNumber( aMarketNumber )
{
}
