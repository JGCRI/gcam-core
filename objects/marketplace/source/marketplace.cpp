/*! 
* \file marketplace.cpp
* \ingroup CIAM
* \brief Marketplace class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#include <iostream>
#include <algorithm>
#include <fstream>
#include <vector>
#include <map>

#include "containers/include/scenario.h"
#include "marketplace/include/market.h"
#include "marketplace/include/price_market.h"
#include "marketplace/include/demand_market.h"
#include "marketplace/include/calibration_market.h"
#include "marketplace/include/ghg_market.h"
#include "marketplace/include/normal_market.h"
#include "marketplace/include/marketplace.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "containers/include/world.h"
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/base/include/supply_demand_curve.h"

using namespace std;

extern ofstream logfile;
extern Scenario* scenario;

/*! \brief Default constructor
*
* The default constructor for the Marketplace which initializes several datamembers and
* creates an instance of the selected solver.
*
* \todo Improve error checking and handling. 
* \todo Make a static method which returns a new BisectionNRSolver for further encapsulation.
* \todo Marketplace might be better as a singleton.
*/
Marketplace::Marketplace() {
    uniqueNo = 0;
    numMarkets = 0;
}

/*! \brief Destructor
*
* Destructor for the marketplace which deletes all the markets.
*/
Marketplace::~Marketplace() {

    // Clean up the markets.
    for ( vector< vector< Market* > >::iterator outerIter = markets.begin(); outerIter != markets.end(); outerIter++ ) {
        for( vector< Market* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            delete *innerIter;
        }
    }
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called hierarchically from the main loop to write out 
* the current state of the model at a given time period. All member variables relevant to the running 
* of the model are written out to assist with debugging. It writes to the output stream
* passed as an argument in an XML format. 
* 
* \warning Currently to limit the size of the file which is written this method is only called for the
* US region.
* \param period The period for which to print the debugging information.
* \param out The output stream to which to print.
* \author Josh Lurz
*/
void Marketplace::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    // write the beginning tag.
    tabs->writeTabs( out );
    out << "<Marketplace>" << endl;

    // increase the indent.
    tabs->increaseIndent();

    // write the xml for the class members.
    XMLWriteElement( numMarkets, "numberOfMarkets", out, tabs );

    // First write out the individual markets
    for( int i = 0; i < static_cast<int>( markets.size() ); i++ ){
        markets[ i ][ period ]->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.

    // decrease the indent.
    tabs->decreaseIndent();

    // write the closing tag.
    tabs->writeTabs( out );
    out << "</Marketplace>" << endl;
}

/*! \brief Function to create market key from a region name and a good name.
* 
* This convenience function was added to consistently create the name of the market from the region name and
* good name.
*
* \author Josh Lurz
* \param marketName The market region.
* \param goodName The market good.
*/
string Marketplace::createMarketKey( const string& marketName, const string& goodName ) {
    return ( marketName + goodName );
}

/*! \brief Function to find the market number from the market name and good name.
*
* This function uses the market region and and good name to determine the market. It performs a lookup
* on the key created by createMarketKey using the marketMap to determine the market. 
*
* \author Josh Lurz
* \param marketName The market region of the market.
* \param goodName The good of the market. 
* \return The market number of the market, -1 if the lookup fails.
*/
int Marketplace::getMarketNumberFromNameAndGood( const string& marketName, const string& goodName ) const {

    string key = createMarketKey( goodName, marketName );

    const map<string,int>::const_iterator marketKey = marketMap.find( key );

    if ( marketKey != marketMap.end() ) {
        return marketKey->second;
    }
    else {
        cout << "Warning: Market not found: " << key << "! " << endl;
        return -1;
    }
}

/*! \brief Function to set the raw price from the marketName and goodName.
*
* This function first finds a market based on its marketName and goodName. It then
* sets the value of Market::price to the passed in value.
*
* \author Josh Lurz
* \warning MarketName is NOT the same as RegionName.
* \param marketName The marketRegion of the market.
* \param goodName The name of the good the market contains. 
* \param priceIn The value to set Market::price to.
* \param period The period in which to set the new price. 
*/
void Marketplace::setRawPrice( const string& marketName, const string& goodName, const double priceIn, const int period ) {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Set the market's raw price.
    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ period ]->setRawPrice( priceIn );
    }
    if ( priceIn > 1e10 ) {
        cerr << "Error: " << goodName << " price in "<< marketName <<" is being set to: " << priceIn << endl;
    }
}

/*! \brief Return the market's raw demand. 
*
* This function uses a market type independent function to find the raw or true demand for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This function is used by the solution mechanism, which needs access to the raw demand. 
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw demand is needed.
* \param marketName The market for which a raw demand is needed.
* \param period The period for which to return the raw demand.
* \return The market's raw demand.
*/
double Marketplace::getRawDemand( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw demand.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getRawDemand();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief Return the market's stored raw demand. 
*
* This function uses a market type independent function to find the stored raw or true demand for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This function is used by the solution mechanism, which needs access to the stored raw demand in
* order to calculate derivatives. 
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw stored demand is needed.
* \param regionName The region for which a raw stored demand is needed.
* \return The market's raw stored demand.
*/
double Marketplace::getStoredRawDemand( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw demand.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getStoredRawDemand();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief This function returns true if the given market is a price or demand market.  
*
* This function is used to determine if a given market is a price or demand market. Currently 
* these markets have special behavior within the solution mechanism, so the solution mechanism
* must have access to this information.
*
* \note This is not particularly good OO programming.
* \todo Find a way to avoid leaking this information.
* \warning marketName is not the same as regionName.
* \param goodName The good of the market. 
* \param regionName The region of the market.
* \return If a market is a price or demand market. 
*/
bool Marketplace::isPriceOrDemandMarket( const string& marketName, const string& goodName, const int period ) const {

    bool retValue = false;
    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw demand.
    if ( marketNumber != -1 ) {
        string marketType = markets[ marketNumber ][ period ]->getType();
        if( marketType == "PriceMarket" || marketType == "DemandMarket" ) {
            retValue = true;
        }
    }
    return retValue;
}

/*! \brief Return the market's raw supply. 
*
* This function uses a market type independent function to find the raw or true supply for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This function is used by the solution mechanism, which needs access to the raw supply. 
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw supply is needed.
* \param regionName The region for which a raw supply is needed.
* \return The market's raw supply.
*/
double Marketplace::getRawSupply( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw supply.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getRawSupply();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief Return the market's stored raw supply. 
*
* This function uses a market type independent function to find the stored raw or true supply for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This function is used by the solution mechanism, which needs access to the stored raw supply in
* order to calculate derivatives. 
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw stored supply is needed.
* \param regionName The region for which a raw stored supply is needed.
* \return The market's raw stored supply.
*/
double Marketplace::getStoredRawSupply( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw supply.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getStoredRawSupply();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief Return the market's raw price. 
*
* This function uses a market type independent function to find the raw or true price for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This behavior is different than getPrice becuase this function is only called from within
* the solution mechanism, which should never be searching for a non-existant market.
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw price is needed.
* \param regionName The region for which a raw price is needed.
* \return The market's raw price.
*/
double Marketplace::getRawPrice( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw price.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getRawPrice();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief Return the market's stored raw price. 
*
* This function uses a market type independent function to find the stored raw or true price for a market determined by the goodName and regionName.
* For non-existant markets, this function returns 0. This function is used by the solution mechanism, which needs access to the stored raw price in
* order to calculate derivatives. 
*
* \warning marketName is not the same as regionName.
* \param goodName The good for which a raw stored price is needed.
* \param regionName The region for which a raw stored price is needed.
* \return The market's raw stored price.
*/
double Marketplace::getStoredRawPrice( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the raw price.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getStoredRawPrice();
    }
    else {
        cout << "Error market not found!" << endl;
        return 0;
    }
}

/*! \brief Remove an amount specified for each market from each market's raw demand.
*
* This function is passed a vector of raw demands of the same size and same ordering as the market vector. 
* Each vector item is the amount of demand to remove from the associated market in the marketplace.
* This method uses the Market::removeFromRawDemand method, so that the Market::demand variable is directly reduced,
* in a type independent manner. This method is needed for calculating regional derivatives. 
*
* \author Josh Lurz
* \todo Relying on the ordering of the markets is probably not a good idea. 
* \param rawDemands Vector in the same order as the markets in the marketplace containing an amount of demand to remove for each market.
* \param period Period in which to remove the demand. 
*/
void Marketplace::removeFromRawDemands( const vector<double>& rawDemands, const int period ) {

    // First check vector sizes match.
    assert( rawDemands.size() == markets.size() );

    // Iterate through the markets and remove raw demands.
    for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
        markets[ i ][ period ]->removeFromRawDemand( rawDemands[ i ] );
    }
}

/*! \brief Remove an amount specified for each market from each market's raw supply.
*
* This function is passed a vector of raw supplies of the same size and same ordering as the market vector. 
* Each vector item is the amount of supply to remove from the associated market in the marketplace.
* This method uses the Market::removeFromRawSupply method, so that the Market::supply variable is directly reduced,
* in a type independent manner. This method is needed for calculating regional derivatives. 
*
* \author Josh Lurz
* \todo Relying on the ordering of the markets is probably not a good idea. 
* \param rawSupplies Vector in the same order as the markets in the marketplace containing an amount of supply to remove for each market.
* \param period Period in which to remove the supply. 
*/
void Marketplace::removeFromRawSupplies( const vector<double>& rawSupplies, const int period ) {

    // First check vector sizes match.
    assert( rawSupplies.size() == markets.size() );

    // Iterate through the markets and remove raw demands.
    for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
        markets[ i ][ period ]->removeFromRawSupply( rawSupplies[ i ] );
    }
}

/*! \brief Get the list of contained regions from a market.
*
* Each market contains a list of regions that are within it. This represents regions who feed into and take out of this market.
* This function returns that listing.
*
* \author Josh Lurz
* \param marketName The name of the market region.
* \param goodName The good of the market.
* \param period The period of the market to return the contained regions.
* \return A vector of region names contained by the market.
*/
const vector<string> Marketplace::getContainedRegions( const string& marketName, const string& goodName, const int period ) const {

    // Get the market number.
    const int marketNumber = getMarketNumberFromNameAndGood( marketName, goodName );

    // Get the contained regions.
    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getContainedRegions();
    }
    else {
        return vector<string>();
    }
}

/*! \brief This function creates a market of the specified type for a given market region and good if it does not already exist.
*
* This function first checks if a market exists for a given market name and good name. If it does, the function add the region 
* to the contained regions of the market and add the region and good name to the regionToMarketMap, then finally returns false.
* Otherwise, it creates a market of the specified type, add the region to the contained region list, and adds keys to the marketMap and regionToMarketMap.
* The key to the marketMap is based on the marketName and goodName and the key to the regionToMarketMap is based on the regionName and goodName.
*
* \todo A factory method should be used so all market classes don't have to be included by this class. 
* \warning There is an important distinction here between the region name vs the market name. The key to the market is the goodName + market name.
* \param regionName The region of the sector for which to create a market.
* \param marketName The market region for which to create a market. This varies from the regionName, it can be global, a multi-region market, or the same as the region name.
* \param goodName The good for which to create a market.
* \param typeIn The type of market to create.
* \return Whether a market was created.
*/
bool Marketplace::createMarket( const string& regionName, const string& marketName, const string& goodName, const NewMarketType typeIn ) {
    const Modeltime* modeltime = scenario->getModeltime();
    int marketsNo;
    bool retValue;

    // create unique markets from distinct good and market region names
    string key = createMarketKey( goodName, marketName );

    if ( marketMap.find( key ) != marketMap.end() ) { // market exists, no unique number
        retValue = false;
        marketsNo = marketMap[ key ];

        // add the additional region to the contained region names.
        for( int i = 0; i < static_cast<int> ( markets[ marketsNo ].size() ); i++ ) {
            markets[ marketsNo ][ i ]->addRegion( regionName );
        }
    } 
    else { // market does not exist, give unique number
        retValue = true;
        marketMap[ key ] = uniqueNo;

        // create a vector of market objects, one for each period
        vector<Market*> tempVector( modeltime->getmaxper() );

        for( int i = 0; i < static_cast<int>( tempVector.size() ); i++ ){
            if ( typeIn == NORMAL ){
                tempVector[ i ] = new NormalMarket( goodName, marketName, i );
            }
            else if ( typeIn == GHG ) {
                tempVector[ i ] = new GHGMarket( goodName, marketName, i );
            }
            else if ( typeIn == CALIBRATION ) {
                tempVector[ i ] = new CalibrationMarket( goodName, marketName, i );
            }
            else if ( typeIn == DEMAND ) {
                tempVector[ i ] = new DemandMarket( goodName, marketName, i );
            }
            else {
                cerr << "Invalid market type: " << typeIn << endl;
                assert( false );
            }

            tempVector[ i ]->addRegion( regionName );
        }

        markets.push_back( tempVector ); // create individual markets
        //markets is 2 dimentional
        marketsNo = uniqueNo;
        uniqueNo++;

        // increment market sizes
        numMarkets++;
    }
    // market lookup from good and region names
    // marketsNo may not be unique
    regionToMarketMap[ goodName + regionName ] = marketsNo;
    return retValue;
}

/*! \brief Returns the market number of a market given a goodName and regionName.
*
* This function uses the regionToMarketMap to lookup a market based on the passed goodName and regionName.
*
* \param goodName The good of the market.
* \param regionName The region for which a market is needed.
* \return The market number if the lookup is successful, -1 otherwise.
*/
int Marketplace::getMarketNumber( const string& goodName, const string& regionName ) const {
    map <string, int> :: const_iterator findIter = regionToMarketMap.find( goodName + regionName );

    if( findIter == regionToMarketMap.end() ) {
        return -1;
    }
    else {
        return findIter->second;
    }
}

/*! \brief Restructures a market to account for simultaneities.
*
* Changes the named market to a price market, which suplies a trial price for a secondary good.
* It also adds a corresponding demand market that provides a trial value for demand.
* Markets are subclassed to allow Market::addToDemand, Market::getDemand, Market::getPrice, etc. to act 
* differently for PriceMarket and DemandMarket so that these changes are transparent to the 
* rest of the code.
* 
* \author Steve Smith
* \todo This currently will not work for global markets. 
* \param goodName The name of the good of the market to be restructured.
* \param regionName The region of the market to be restructured.
*/
void Marketplace::resetToPriceMarket( const string& goodName, const string& regionName ) {

    Configuration* conf = Configuration::getInstance();
    Market* newPriceMarket = 0;
    Market* newDemandMarket = 0;
    const int marketNumber = getMarketNumber( goodName, regionName );
    int demandMarketNumber = 0;
    string marketName;
    string demandGoodName;
    const bool setNewMarkets = conf->getBool( "simulActive" ); // for debugging, set this to false to turn this off

    if( marketNumber == -1 ) {
        cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
    }

    else if( !setNewMarkets ) {
        return;
    }

    else if ( markets[ marketNumber ][ 0 ]->getType() == "NormalMarket" ) {

        // Setup the coresponding demand markets
        marketName = markets[ marketNumber ][ 0 ]->getRegionName();
        demandGoodName = goodName + "Demand_int";
        createMarket( regionName, marketName, demandGoodName, DEMAND );
        demandMarketNumber = getMarketNumber( demandGoodName, regionName );

        // loop through time periods            
        for( int per = 1; per < static_cast<int>( markets[ marketNumber ].size() ); per++ ){

            // Get the pointer of the new demand market.
            newDemandMarket = markets[ demandMarketNumber ][ per ];
            assert( newDemandMarket );

            // Create a new price market from the old market.
            newPriceMarket = new PriceMarket( *markets[ marketNumber ][ per ], newDemandMarket );

            // Delete the old market.
            delete markets[ marketNumber ][ per ];

            // Insert the new price market. 
            markets[ marketNumber ][ per ] = newPriceMarket;

            // Set both markets to solve.
            setMarketToSolve( goodName, regionName, per );
            setMarketToSolve( demandGoodName, regionName, per );
            // this assumes that all markets have the same number of periods
        }
    }
}

/*! \brief Set the prices by period of a market from a vector.
*
* This function sets the price for each period of a market according to the corresponding value in the prices vector.
* The function checks so that it will not fail if the prices vector is a different size than the markets vector. 
*
* \author Josh Lurz
* \param goodName The goodName of the market for which to set new prices.
* \param regionName The regionName to use for the lookup to determine the correct market.
* \param prices A vector containing prices to set into the market. 
*/
void Marketplace::setPriceVector( const string& goodName, const string& regionName, const vector<double>& prices ){

    // determine what market the region and good are in.
    const int marketNumber = regionToMarketMap[ goodName + regionName ];

    for( int i = 0; i < static_cast<int>( markets[ marketNumber ].size() ) && i < static_cast<int>( prices.size() ); i++ ){
        markets[ marketNumber ][ i ]->setRawPrice( prices[ i ] );
    }
}

/*! \brief Initialize prices for all markets. 
*
* Supply and demand sector prices should always get set somewhere else except for in the first period.
* However, no such guarantee exists for GHG markets. The function initializes prices for all periods 
* in all markets just to be safe if they are not already set. 
* Initialization also occurs for supply and demand markets that have prices read-in via routine:
* setPriceVector in sector as supply and demand markets are created.
* This has no effect for future periods as these prices are overwritten by 
* Marketplace::init_to_last except for CalibrationMarkets.
*/
void Marketplace::initPrices(){

    // initialize supply and demand sector market prices to 1.
    for ( int i = 0; i < static_cast<int>( markets.size() ); i++ ){               
        for( int j = 0; j < static_cast<int>( markets[ i ].size() ); j++ ){
            markets[ i ][ j ]->initPrice();
        }
    }
}

/*! \brief Set the solve flag for this market for the given period, or all periods if per argument is undefined.
*
* This function determines a market from a good and region and sets the market to solve for the period passed to the function.
* If this period is -1, the default value, the function sets the solve flag for all periods of the market. This solve flag 
* determines whether the market is solved by the solution mechanism, except for cases where the market does not pass certain
* other criteria related to singularities. If this flag is set to false, as is the default, the market will never be solved. 
*
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should be solved, -1 is the default values and tells the function to set the flag for all periods.
*/
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName, const int per ) {

    const int marketNumber = getMarketNumber( goodName, regionName );

    // If the market exists.
    if ( marketNumber != -1 ) {
        // If by default we are setting all time periods to solve.
        if( per == -1 ) {
            for( int per = 0; per < static_cast<int>( markets[ marketNumber ].size() ); per++ ){                 
                markets[ marketNumber ][ per ]->setSolveMarket( true );
            }
        }
        // Otherwise set the individual market to solve.
        else {
            markets[ marketNumber ][ per ]->setSolveMarket( true );
        }
    }
    else {
        cout << "Error: Market cannot be set to solve as it does not exist: " << goodName << " " << regionName << endl;
    }
}

/*! \brief Unset the solve flag for this market for the given period, or all periods if per argument is undefined.
* \todo Decide a)If this is a good name b)If this function should be combined with the above using a parameter. c)Overloaded function for period?
* This function determines a market from a good and region and unsets the market to solve for the period passed to the function.
* If this period is -1, the default value, the function unsets the solve flag for all periods of the market. This solve flag 
* determines whether the market is solved by the solution mechanism, except for cases where the market does not pass certain
* other criteria related to singularities. If this flag is set to false, as is the default, the market will never be solved.
* This function also clears supply and demand for the market at the same time. 
*
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should not be solved, -1 is the default values and tells the function to set the flag for all periods.
*/
void Marketplace::unsetMarketToSolve ( const string& goodName, const string& regionName, const int per ) {

    const int marketNumber = getMarketNumber( goodName, regionName );

    // If the market exists.
    if ( marketNumber != -1 ) {
        // If by default we are setting all time periods to solve.
        if( per == -1 ) {
            for( int per = 0; per < static_cast<int>( markets[ marketNumber ].size() ); per++ ){                 
                markets[ marketNumber ][ per ]->setSolveMarket( false );
                markets[ marketNumber ][ per ]->nullSupply();
                markets[ marketNumber ][ per ]->nullDemand();
            }
        }
        // Otherwise set the individual market to solve.
        else {
            markets[ marketNumber ][ per ]->setSolveMarket( false );
            markets[ marketNumber ][ per ]->nullSupply();
            markets[ marketNumber ][ per ]->nullDemand();
        }
    }
    else {
        cout << "Error: Market cannot be set not to solve as it does not exist: " << goodName << " " << regionName << endl;
    }
}

/*! \brief Initialize all market demands for the given period.
* 
* This function iterates through the markets and nulls the demand of each market
* in the given period.
*
* \param period Period in which to null the demands. 
*/
void Marketplace::nullDemands( const int period ) {
    for ( int i = 0; i < numMarkets; i++ ) {
        markets[ i ][ period ]->nullDemand();
    }
}

/*! \brief Initialize all market supplies for the given period.
* 
* This function iterates through the markets and nulls the supply of each market
* in the given period.
*
* \param period Period in which to null the supplies. 
*/
void Marketplace::nullSupplies( const int period ) {
    for ( int i = 0; i < numMarkets; i++ ) {
        markets[ i ][ period ]->nullSupply();
    }
}

/*! \brief Set the market price.
*
* This function uses the type dependent method Market::setPrice to set the passed in value into the market.
*
* \param goodName The good of the market.
* \param regionName The region setting the price.
* \param value The value to which to set price.
* \param per The period in which to set the price.
*/
void Marketplace::setPrice( const string& goodName, const string& regionName, const double value, const int per ){
    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ per ]->setPrice( value );
    }
}

/*! \brief Add to the supply for this market.
*
* This function increments the supply for a market determined by the goodName and regionName
* by a given value. This function is used throughout the model to add supply to markets. 
*
* \param goodName Name of the good for which to add supply.
* \param regionName Name of the region in which supply should be added for the market.
* \param value Amount of supply to add.
* \param per Period in which to add supply.
*/
void Marketplace::addToSupply( const string& goodName, const string& regionName, const double value, const int per ){

    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ per ]->addToSupply( value );
    }
}

/*! \brief Add to the demand for this market.
*
* This function increments the demand for a market determined by the goodName and regionName
* by a given value. This function is used throughout the model to add demand to markets. 
*
* \param goodName Name of the good for which to add demand.
* \param regionName Name of the region in which demand should be added for the market.
* \param value Amount of demand to add.
* \param per Period in which to add demand.
*/
void Marketplace::addToDemand( const string& goodName, const string& regionName, const double value, const int per ){
    const int marketNumber = getMarketNumber( goodName, regionName );

    if (value < 0 && goodName != "CO2" ) {
        cerr << "ERROR in addToDemand: Demand value < 0 for market " << goodName << " in region " << regionName << endl;
    }

    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ per ]->addToDemand( value );
    }
}

/*! \brief Return the market price. 
*
* This function uses a market type dependent function to find the price for a market determined by the goodName and regionName.
* This price is not always the raw or true price. For non-existant markets, this function returns a near infinite price, except for renewable
* markets that do not exist, for which it returns 0.
*
* \todo do something else about "renewable"
* \param goodName The good for which a price is needed.
* \param regionName The region for which a price is needed.
* \param per The period to return the market price for. 
* \return The market price.
*/
double Marketplace::getPrice( const string& goodName, const string& regionName, const int per ) const {

    const int marketNumber = getMarketNumber( goodName, regionName );
	// if market number is not found, marketNumber is -1
    if( marketNumber == -1 ) {
        // the "renewable" fuel depends on the returned price being zero
        if ( goodName == "renewable" ) {
            return 0;
        } 
		else {
            logfile << "Market not found for " << goodName << " in region " << regionName << endl;
            cerr << "Market not found for " << goodName << " in region " << regionName << endl;
            return 1e12;
        }
    }
    else {
        return markets[ marketNumber ][ per ]->getPrice();
    }

}

/*! \brief Return the market supply. 
*
* This function uses a market type dependent function to find the supply for a market determined by the goodName and regionName.
* This supply is not always the raw or true supply. For non-existant markets, this function returns 0.
*
* \param goodName The good for which a supply is needed.
* \param regionName The region for which a supply is needed.
* \return The market supply.
*/
double Marketplace::getSupply( const string& goodName, const string& regionName, const int per ) const {
    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ per ]->getSupply();
    }
    else {
        cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
        return 0;
    }
}

/* UNUSED!
//! return supply for use in checking solution, including Raw supply for demand market
/*! Not used now, could be used for debugging at some point*/
double Marketplace::checkSupply( const string& goodName, const string& regionName, const int per ) const {

    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ per ]->getSupplyForChecking();
    }
    else {
        cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
        return 0;
    }
}

/*! \brief Return the market demand. 
*
* This function uses a market type dependent function to find the demand for a market determined by the goodName and regionName.
* This demand is not always the raw or true demand. For non-existant markets, this function returns 0.
*
* \param goodName The good for which a demand is needed.
* \param regionName The region for which a demand is needed.
* \param per The period to return the market demand for.
* \return The market demand.
*/
double Marketplace::getDemand(  const string& goodName, const string& regionName, const int per ) const {
    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ per ]->getDemand();
    }
    else {
        cerr << "ERROR: Called for demand of non-existant market "<< goodName << " in " << regionName << endl;
        return 0;
    }
}

/*! \brief This function is used to check whether all markets cleared. It also prints supply-demand curves for unsolved markets.
*
* This function checks each market's raw supply and raw demand against each other and compares them to the solutionTolerance.
* If they are not within the tolerance, it adds them to the unsolved market vector. This vector is then printed out, and if the option 
* is set, supply demand curves are created for the worst n of these markets, where n is a configuration setting. 
*
* \param solTolerance The tolerance value to use when checking if a market cleared.
* \param excessDemandSolutionFloor An absolute value of excess demand below which markets are considered cleared.
* \param period Period in which to check the solution.
* \return Whether or not the markets all cleared.
*/
bool Marketplace::checkMarketSolution( const double solTolerance, const double excessDemandSolutionFloor, const int period, const bool notSolved ) {

    const Configuration* conf = Configuration::getInstance();
    const bool debugChecking = conf->getBool( "debugChecking" );
    const bool debugFindSD = conf->getBool( "debugFindSD" );

    bool solvedOK = true;
    vector<Market*> unsolved;

    for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {
        if ( !SolverLibrary::isWithinTolerance( markets[ i ][ period ]->getRawDemand() - markets[ i ][ period ]->getRawSupply(), markets[ i ][ period ]->getRawDemand(), solTolerance, excessDemandSolutionFloor ) ) {
            unsolved.push_back( markets[ i ][ period ] );
        }
    }

    if( !unsolved.empty() ) {
        solvedOK = false;
        if ( notSolved ) {
				cout << "Unsolved markets: " << endl;
		  } else {
				cout << "Markets that had difficulty solving: " << endl;
		  }
    }

    for ( vector<Market*>::const_iterator iter = unsolved.begin(); iter != unsolved.end(); iter++ ) {
        cout << "Market (" << ( *iter )->getName() << ") S: "<< ( *iter )->getRawSupply() << " D: " << ( *iter )->getRawDemand();
        cout << " P: " << ( *iter )->getRawPrice() << " Solve: " << ( *iter )->shouldSolve() << " SolveNR: " << ( *iter )->shouldSolveNR() << endl;
    }

    if ( debugFindSD && !unsolved.empty() ) {
        string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "SDCurves.csv" );
        Logger* sdLog = LoggerFactory::getLogger( logName );
        if ( notSolved ) {
				LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and demand curves for markets that did not solve in period: " << period << endl;
		  } else {
				LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and demand curves for markets that had difficulty solving in period: " << period << endl;
		  }
        findAndPrintSD( unsolved, period );
    }

    return solvedOK;
}

/*! \brief Find and print supply-demand curves for unsolved markets.
*
* This function determines the n worst markets, where n is defined by the configuration file, 
* and creates a SupplyDemandCurve for each. It then instructs the SupplyDemandCurve to calculate the 
* supply and demand at a series of prices, and to print the resulting curve.
*
* \author Josh Lurz
* \param unsolved Vector of unsolved markets.
* \param period Period for which to print supply-demand curves.
*/
void Marketplace::findAndPrintSD( vector<Market*>& unsolved, const int period ) {

    const Configuration* conf = Configuration::getInstance();
    const int numMarketsToFindSD = conf->getInt( "numMarketsToFindSD", 5 );
    const int numPointsForSD = conf->getInt( "numPointsForSD", 5 );
    string logName = Configuration::getInstance()->getFile( "supplyDemandOutputFileName", "SDCurves.csv" );
    Logger* sdLog = LoggerFactory::getLogger( logName );
    World* world = scenario->getWorld();

    // Remove any markets that should not be solved. 
    for( vector<Market*>::iterator removeIter = unsolved.begin(); removeIter != unsolved.end(); ) {
        if ( !( *removeIter )->shouldSolve() ) {
            removeIter = unsolved.erase( removeIter );
        }
        else {
            removeIter++;
        }
    }

    // If there aren't any markets left return.
    if( unsolved.empty() ) {
        return;
    }

    // Sort the vector so the worst markets are first and remove the rest.
    sort( unsolved.begin(), unsolved.end(), Market::greaterRelativeExcessDemand() );

    // Truncate vector if neccessary.
    if( static_cast<int>( unsolved.size() ) > numMarketsToFindSD ) { 
        unsolved.resize( numMarketsToFindSD );
    }

    // Now determine supply and demand curves for each.
    for ( vector<Market*>::iterator iter = unsolved.begin(); iter != unsolved.end(); iter++ ) {
        SupplyDemandCurve sdCurve( *iter );
        sdCurve.calculatePoints( numPointsForSD, world, this, period );
        sdCurve.print( sdLog );
    }

}

/*! \brief This function iterates through the markets and returns the set that should be solved.
*
* The function uses each market's Market::shouldSolve or Market::shouldSolveNR to determine if the market should 
* be solved. If it should be, then it inserts into the vector the regionName and goodName pair which is unique
* and so can later be used by the solution mechanism to reference an individual market.
*
* \author Josh Lurz
* \param period Period for which to find markets to solve.
* \param isNR Whether or not the markets are for Newton-Rhaphson.
* \return A vector of pairs of market regions and good names. 
*/
vector< pair< string, string > > Marketplace::getMarketsToSolve( const int period, const bool isNR ) const {

    vector< pair< string, string > > marketsToSolve;

    for( int i = 0; i < static_cast<int>( markets.size() ); i++ ) {

        if ( !isNR ) {
            if ( markets[ i ][ period ]->shouldSolve() ) {
                marketsToSolve.push_back( pair<string,string>( markets[ i ][ period ]->getRegionName(), markets[ i ][ period ]->getGoodName() ) );
            }
        }
        else {
            // Add markets to NR solution list
            if ( markets[ i ][ period ]->shouldSolveNR() ) {
                marketsToSolve.push_back( pair<string,string>( markets[ i ][ period ]->getRegionName(), markets[ i ][ period ]->getGoodName() ) );
            }
        }
    }
    return marketsToSolve;
}

/*! \brief Initializes the market prices to the market prices from the previous period.
* 
* This function initializes each periods raw price with the raw price from the previous period.
* This only occurs for periods greater than 0.
* 
* \author Sonny Kim
* \param period Period for which to initialize prices.
*/
void Marketplace::init_to_last( const int period ) { 
    // only after the starting period
    if ( period > 0 ) {
        for ( int i = 0; i < numMarkets; i++ ) {
            markets[ i ][ period ]->setPriceFromLast( markets[ i ][ period - 1 ]->getRawPrice() );
        }
    }
}

/*! \brief Sets the values of the stored variables to the raw values from the previous period.
*
* Is this actually neccessary??
* 
* \todo Determine if this is neccessary. 
* \param period Period for which to set the stored values.
*/
void Marketplace::storeto_last( const int period ) {
    // only after the starting period
    if ( period > 0 ) {
        for ( int i = 0; i < numMarkets; i++ ) {
            markets[ i ][ period ]->storeInfoFromLast( markets[ i ][ period - 1 ]->getRawDemand(), markets[ i ][ period - 1 ]->getRawSupply(), markets[ i ][ period - 1 ]->getRawPrice() );
        }
    }
}

/*! \brief Store the demand, supply and price for each market. 
*
* This function called the Market::storenfo function on each market in the marketplace,
* which causes them to set their stored demand, supply, and price to the respective current values
* for those variables.
*
* \author Sonny Kim
* \param period Period for which to store demands, supplies and prices.
*/
void Marketplace::storeinfo( const int period ) {
    for ( int i = 0; i  < numMarkets; i++ ) {
        markets[ i ][ period ]->storeInfo();
    }
}

/*! \brief Restore the stored demand, supply and price for each market. 
*
* This function called the Market::restoreInfo function on each market in the marketplace,
* which causes them to set their demand, supply, and price to the respective stored values
* for those variables.
*
* \param period Period for which to restore demands, supplies, and prices.
*/
void Marketplace::restoreinfo( const int period) {
    for ( int i = 0; i < numMarkets; i++ ) {
        markets[ i ][ period ]->restoreInfo();
    }
}

/*! \brief Adds an information value and an associated key to the market for the
* specified goodName, regionName and period.
* \details This functions adds to the Market extra information which needs to be
* explicitally associated with a Market. 
* \author Josh Lurz
* \warning The function is UNRELATED to storeinfo and restoreinfo.
* \param goodName The good of the market to add the information for.
* \param regionName The region used to find the market to add the information to.
* \param period The period the information is associated with. 
* \param itemName The string to use as the key for this information value.
* \param itemValue The value to be associated with this key. 
*/
void Marketplace::setMarketInfo( const string& goodName, const string& regionName, const int period, const string itemName, const double itemValue ){
    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ period ]->setMarketInfo( itemName, itemValue );
    }
    else {
        cerr << "ERROR: Called setMarketInfo for non-existant market " << goodName << " in " << regionName << endl;
    }
}

/*! \brief Gets an information value for associated key from the market for the
* specified goodName, regionName and period.
* \details This functions gets extra Market information which needs to be
* explicitally associated with a Market. 
* \author Josh Lurz
* \warning The function is UNRELATED to storeinfo and restoreinfo.
* \param goodName The good of the market to get the information for.
* \param regionName The region used to find the market to get the information from.
* \param period The period the information is associated with. 
* \param itemName The string to use as the key to query the market.
*/
double Marketplace::getMarketInfo( const string& goodName, const string& regionName, const int period, const string& itemName ) const{
    const int marketNumber = getMarketNumber( goodName, regionName );

    if ( marketNumber != -1 ) {
        return markets[ marketNumber ][ period ]->getMarketInfo( itemName );
    }
    else {
        cerr << "ERROR: Called getMarketInfo for non-existant market "<< goodName << " in " << regionName << endl;
        return 0;
    }
}

/*! \brief Returns whether a Market specified by a goodName and regionName currently exists.
* \author Josh Lurz
* \param goodName The good name to use to search for a market.
* \param regionName The region name to use to search for a market.
* \param period The period to use to search for a market. This is currently not used but inserted for future expansion.
* \return Whether the market exists.
*/
bool Marketplace::doesMarketExist( const std::string& goodName, const std::string& regionName, const int period ) const {
    bool retValue;
    if( getMarketNumber( goodName, regionName ) == -1 ) {
        retValue = false;
    }
    else {
        retValue = true;
    }
    return retValue;
}


/*! \brief Returns a vector of raw market demands for all markets.
*
* This method returns a vector of raw demands in the order the markets are contained.
* This function is used to help with calculation of derivatives of regional markets.
* 
* \todo This ordering is bad. Should maybe use a map?
* \param per Period for which to return demands.
* \return A vector of raw demands, one for each market. 
*/
const vector<double> Marketplace::getDemands( const int per ) const {

    vector<double> demands( numMarkets );

    for ( int i = 0; i < numMarkets; i++ ) {
        demands[ i ] = markets[ i ][ per ]->getRawDemand();
    }
    return demands;
}

/*! \brief Returns a vector of raw market supplies for all markets.
*
* This method returns a vector of raw supplies in the order the markets are contained.
* This function is used to help with calculation of derivatives of regional markets.
* 
* \todo This ordering is bad. Should maybe use a map?
* \param per Period for which to return supplies.
* \return A vector of raw supplies, one for each market. 
*/
const vector<double> Marketplace::getSupplies( const int per ) const {

    vector<double> supplies( numMarkets );

    for ( int i = 0; i < numMarkets; i++ ) {
        supplies[ i ] = markets[ i ][ per ]->getRawSupply();
    }
    return supplies;
}


/*! \brief Write out the market information to the database.
*
* This function is used to perform a data writeout to the database.
*
* \note This will be replaced by toXMLOutput
*/
void Marketplace::MCoutput() const {

    const Modeltime* modeltime = scenario->getModeltime();

    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp(maxPeriod);
    int j;
    // write market prices, supply and demand
    for (int i=0;i< static_cast<int>( markets.size() );i++) {
		string tempRegName = markets[i][0]->getRegionName();
		string tempGoodName = markets[i][0]->getGoodName();
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawPrice();
        }
        dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"1_price","$/GJ",temp);
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawSupply();
        }
        dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"2_supply","EJ",temp);
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawDemand();
        }
        dboutput4(markets[i][0]->getRegionName(),"Market",markets[i][0]->getGoodName(),"3_demand","EJ",temp);
    }
}

/*! \brief Write out the market information to a file.
*
* This function is used to perform a data writeout to a plain text file.
*
* \note This will be replaced by toXMLOutput
*/
void Marketplace::outputfile() const {

    const Modeltime* modeltime = scenario->getModeltime();

    // function protocol
    void fileoutput2(string var1name,string var2name,string var3name,
        string var4name,string var5name,vector<double> dout,string uname);
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    const int maxPeriod = modeltime->getmaxper();
    vector<double> temp(maxPeriod);
    int j;

    // Function arguments are variable name, double array, db name, and
    // table name.
    // The function writes all years.

    // write market prices and supply (or demand)
    for (int i=0;i<numMarkets;i++) {
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawPrice();
        }
        fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","price","$/GJ",temp);
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawSupply();
        }
        fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","supply","EJ",temp);
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawDemand();
        }
        fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","demand","EJ",temp);
    }
}
