/*! 
* \file marketplace.cpp
* \ingroup CIAM
* \brief Marketplace class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"

#include <vector>
#include <map>

#include "marketplace/include/marketplace.h"
#include "marketplace/include/market.h"
#include "marketplace/include/imarket_type.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/price_market.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Default constructor 
*
* The default constructor for the Marketplace which initializes several datamembers and
* creates an instance of the selected solver.
*
* \todo Improve error checking and handling. 
* \todo Marketplace might be better as a singleton.
*/
Marketplace::Marketplace():
uniqueNo( 0 ),
numMarkets( 0 ){
}

/*! \brief Destructor
*
* Destructor for the marketplace which deletes all the markets.
*/
Marketplace::~Marketplace() {
    // Clean up the markets.
    for ( vector<vector<Market*> >::iterator outerIter = markets.begin(); outerIter != markets.end(); outerIter++ ) {
        for( vector<Market*>::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
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

    XMLWriteOpeningTag( "Marketplace", out, tabs );

    // write the xml for the class members.
    XMLWriteElement( numMarkets, "numberOfMarkets", out, tabs );

    // Write out the individual markets
    for( unsigned int i = 0; i < markets.size(); i++ ){
        markets[ i ][ period ]->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.
	XMLWriteClosingTag( "Marketplace", out, tabs );
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
* \param aTy[e The type of market to create.
* \return Whether a market was created.
*/
bool Marketplace::createMarket( const string& regionName, const string& marketName, const string& goodName, const IMarketType::Type aType ) {
    int marketsNo;
    bool retValue;

    // create unique markets from distinct good and market region names
    string key = createMarketKey( goodName, marketName );

    if ( marketMap.find( key ) != marketMap.end() ) { // market exists, no unique number
        retValue = false;
        marketsNo = marketMap[ key ];

        // add the additional region to the contained region names.
        for( unsigned int i = 0; i < markets[ marketsNo ].size(); i++ ) {
            markets[ marketsNo ][ i ]->addRegion( regionName );
        }
    } 
    else { // market does not exist, give unique number
        retValue = true;
        marketMap[ key ] = uniqueNo;

        // create a vector of market objects, one for each period
        const Modeltime* modeltime = scenario->getModeltime();
        vector<Market*> tempVector( modeltime->getmaxper() );

        for( unsigned int i = 0; i < tempVector.size(); i++ ){
            tempVector[ i ] = Market::createMarket( aType, goodName, marketName, i ).release();
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
    // If simuls are off, return immediately. 
    if( !Configuration::getInstance()->getBool( "simulActive" ) ){
        return;
    }

    const int marketNumber = getMarketNumber( goodName, regionName );

    if( marketNumber == -1 ) {
        ILogger& mainLog = ILogger::getLogger( "mainLog" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Cannot reset Market "<< goodName << " to a price market because it does not exist."  << endl;
    }
    else if( markets[ marketNumber][ 0 ]->getType() != "NormalMarket" ){
        ILogger& mainLog = ILogger::getLogger( "mainLog" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Cannot reset market type other than normal to a price market.";
    }
    else {
        // Setup the coresponding demand markets
        string marketName = markets[ marketNumber ][ 0 ]->getRegionName();
        string demandGoodName = goodName + "Demand_int";
        createMarket( regionName, marketName, demandGoodName, IMarketType::DEMAND );
        int demandMarketNumber = getMarketNumber( demandGoodName, regionName );

        // loop through time periods            
        for( unsigned int per = 1; per < markets[ marketNumber ].size(); per++ ){

            // Get the pointer of the new demand market.
            Market* newDemandMarket = markets[ demandMarketNumber ][ per ];
            assert( newDemandMarket );

            // Create a new price market from the old market.
            Market* newPriceMarket = new PriceMarket( *markets[ marketNumber ][ per ], newDemandMarket );

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
    for ( unsigned int i = 0; i < markets.size(); ++i ){               
        for( unsigned int j = 0; j < markets[ i ].size(); ++j ){
            markets[ i ][ j ]->initPrice();
        }
    }
}

/*! \brief Set the solve flag for this market for the given period, or all periods if per argument is undefined.
*
* This function determines a market from a good and region and sets the market to solve for the period passed to the function.
* This solve flag determines whether the market is solved by the solution mechanism, except for cases where the market does not pass certain
* other criteria related to singularities. If this flag is set to false, as is the default, the market will never be solved. 
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should be solved.
*/
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName, const int per ) {
    const int marketNumber = getMarketNumber( goodName, regionName );

    // If the market exists.
    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ per ]->setSolveMarket( true );
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market cannot be set to solve as it does not exist: " << goodName << " " << regionName << endl;
    }
}

/*! \brief Unset the solve flag for this market for the given period, or all periods if per argument is undefined.
* This function determines a market from a good and region and unsets the market to solve for the period passed to the function.
* This solve flag determines whether the market is solved by the solution mechanism, except for cases where the market does not pass certain
* other criteria related to singularities. If this flag is set to false, as is the default, the market will never be solved.
* This function also clears supply and demand for the market at the same time. 
*
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should not be solved.
*/
void Marketplace::unsetMarketToSolve ( const string& goodName, const string& regionName, const int per ) {

    const int marketNumber = getMarketNumber( goodName, regionName );

    // If the market exists.
    if ( marketNumber != -1 ) {
        markets[ marketNumber ][ per ]->setSolveMarket( false );
        markets[ marketNumber ][ per ]->nullSupply();
        markets[ marketNumber ][ per ]->nullDemand();
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market cannot be unset not to solve as it does not exist: " << goodName << " " << regionName << endl;
    }
}

/*! \brief Clear all market supplies and demands for the given period.
* 
* This function iterates through the markets and nulls the supply and demand 
* of each market in the given period.
*
* \param period Period in which to null the supplies and demands. 
*/
void Marketplace::nullSuppliesAndDemands( const int period ) {
    for ( unsigned int i = 0; i < numMarkets; i++ ) {
        markets[ i ][ period ]->nullDemand();
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

    if ( value < 0 && goodName != "CO2" ) {
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
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Called for price of non-existant market " << goodName << " in region " << regionName << endl;
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
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Called for supply of non-existant market " << goodName << " in " << regionName << endl;
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
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        cerr << "Called for demand of non-existant market " << goodName << " in " << regionName << endl;
        return 0;
    }
}

//! Returns a set of pointers to each market for the period.
vector<Market*> Marketplace::getMarketsToSolve( const int period ) const {
    vector<Market*> toSolve;
    
    // Loop through the markets and add all markets.
    for( unsigned int i = 0; i < markets.size(); ++i ){
        toSolve.push_back( markets[ i ][ period ] );
    }
    return toSolve;
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
        for ( unsigned int i = 0; i < numMarkets; i++ ) {
            markets[ i ][ period ]->setPriceFromLast( markets[ i ][ period - 1 ]->getRawPrice() );
        }
    }
}

/*! \brief Sets the values of the stored variables to the raw values from the previous period.
* 
* \todo Determine if this is neccessary. 
* \param period Period for which to set the stored values.
*/
void Marketplace::storeto_last( const int period ) {
    // only after the starting period
    if ( period > 0 ) {
        for ( unsigned int i = 0; i < numMarkets; i++ ) {
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
    for ( unsigned int i = 0; i  < numMarkets; i++ ) {
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
    for ( unsigned int i = 0; i < numMarkets; i++ ) {
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
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Called setMarketInfo for non-existant market " << goodName << " in " << regionName << endl;
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
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Called getMarketInfo for non-existant market " << goodName << " in " << regionName << endl;
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
    if( getMarketNumber( goodName, regionName ) == -1 ) {
        return false;
    }
    return true;
}

/*! \brief Write out the market information to the database.
*
* This function is used to perform a data writeout to the database.
*
* \note This will be replaced by toXMLOutput
*/
void Marketplace::dbOutput() const {

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

/*! \brief Write out market information to a file.
*
* This function is used to perform a data writeout to a plain text file.
*
* \note This will be replaced by toXMLOutput
*/
void Marketplace::csvOutputFile( string marketsToPrint ) const {

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
    for ( unsigned int i=0;i<numMarkets;i++) {
      if ( marketsToPrint == "" || markets[i][0]->getRegionName() == marketsToPrint ) {
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawPrice();
        }
        fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","price","$/GJ",temp);
        for (j=0;j<maxPeriod;j++) {
            temp[j] = markets[i][j]->getRawSupply();
        }
        fileoutput3(markets[i][0]->getRegionName(),"market",markets[i][0]->getGoodName()," ","supply","EJ",temp);
      }
    }
}
