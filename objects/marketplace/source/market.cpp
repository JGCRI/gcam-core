/*! 
* \file market.cpp
* \ingroup CIAM
* \brief Market class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cassert>

#include "util/base/include/model_time.h" 
#include "util/base/include/xml_helper.h"
#include "marketplace/include/market.h"
#include "marketplace/include/imarket_type.h"
#include "marketplace/include/market_info.h"
#include "containers/include/scenario.h"
#include "marketplace/include/price_market.h"
#include "marketplace/include/demand_market.h"
#include "marketplace/include/calibration_market.h"
#include "marketplace/include/ghg_market.h"
#include "marketplace/include/normal_market.h"
#include "marketplace/include/trial_value_market.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string Market::XML_NAME = "market";

/*! \brief Default constructor
* 
* This is the constructor for the market class. No default constructor exists
* to prevent the creation of empty markets. 
*
* \warning The arguments are required to define the good name, region name and model period. These values are invariants.
* \param goodNameIn The good or fuel name for the item in the market.
* \param regionNameIn The region which this market covers. It may include several model regions.
* \param periodIn The period the market exists in.
*/
Market::Market( const string& goodNameIn, const string& regionNameIn, const int periodIn )
: good( goodNameIn ), 
region( regionNameIn ), 
period( periodIn ),
price( 0 ),
storedPrice( 0 ),
demand( 0 ),
storedDemand( 0 ),
supply( 0 ),
storedSupply( 0 ),
solveMarket( false ){
}

//! Destructor
Market::~Market(){
}

/*! \brief Protected copy constructor
* \details This copy constructor is needed because auto_ptr held memory cannot be copied automatically.
* The copy constructor is protected because it should only be accessed by the PriceMarket derived class.
* \param aMarket The market to copy.
* \author Josh Lurz
*/
Market::Market( const Market& aMarket ): 
good( aMarket.good ),
region( aMarket.region ),
solveMarket( aMarket.solveMarket ),
period( aMarket.period ),
price( aMarket.price ),
storedPrice( aMarket.storedPrice ),
demand( aMarket.demand ),
storedDemand( aMarket.storedDemand ),
supply( aMarket.supply ),
storedSupply( aMarket.storedSupply ),
containedRegionNames( aMarket.containedRegionNames ){
    // Only copy the MarketInfo if it has been allocated.
    if( aMarket.mMarketInfo.get() ){
        mMarketInfo.reset( new MarketInfo( *aMarket.mMarketInfo.get() ) );
    }
}

/*! \brief Static factory method to create a market based on its type.
* \details
* \param aGoodName The good or fuel name for the item in the market to create.
* \param aRegionName The region which the market to create covers.
* \param aPeriod The period the market to create exists in.
* \param aType Type of market to create.
* \return A pointer to the newly allocated market, null if the type did not exist. 
*/
auto_ptr<Market> Market::createMarket( const IMarketType::Type aType, const std::string& aGoodName, const std::string& aRegionName, const int aPeriod ) {
    auto_ptr<Market> rNewMarket;
    if ( aType == IMarketType::NORMAL ){
        rNewMarket.reset( new NormalMarket( aGoodName, aRegionName, aPeriod ) );
    }
    else if ( aType == IMarketType::GHG ) {
        rNewMarket.reset( new GHGMarket( aGoodName, aRegionName, aPeriod ) );
    }
    else if ( aType == IMarketType::CALIBRATION ) {
        rNewMarket.reset( new CalibrationMarket( aGoodName, aRegionName, aPeriod ) );
    }
    else if ( aType == IMarketType::DEMAND ) {
        rNewMarket.reset( new DemandMarket( aGoodName, aRegionName, aPeriod ) );
    }
    else if ( aType == IMarketType::TRIAL_VALUE ) {
        rNewMarket.reset( new TrialValueMarket( aGoodName, aRegionName, aPeriod ) );
    }
    else {
        cerr << "Invalid market type: " << aType << endl;
    }
    return rNewMarket;
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called by the Marketplace::toDebugXML method to write out information for each individual market.
* It prints the current state of all internal variables. It also calls a derived method which prints derived class 
* specific information.
*
* \param period Model period for which to print information.
* \param out Output stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void Market::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
   const Modeltime* modeltime = scenario->getModeltime();
   XMLWriteOpeningTag( getXMLName(), out, tabs, getName(), modeltime->getper_to_yr( period ) , getType() );
   XMLWriteElement( good, "MarketGoodOrFuel", out, tabs );
   XMLWriteElement( region, "MarketRegion", out, tabs );
   XMLWriteElement( price, "price", out, tabs );
   XMLWriteElement( storedPrice, "storedPrice", out, tabs );
   XMLWriteElement( demand, "demand", out, tabs );
   XMLWriteElement( storedDemand, "storedDemand", out, tabs );
   XMLWriteElement( supply, "supply", out, tabs );
   XMLWriteElement( storedSupply, "storedSupply", out, tabs );
   
   for( vector<string>::const_iterator i = containedRegionNames.begin(); i != containedRegionNames.end(); i++ ) {
      XMLWriteElement( *i, "ContainedRegion", out, tabs );
   }
   if( mMarketInfo.get() ){
       mMarketInfo->toDebugXML( out, tabs );
   }
   derivedToDebugXML( out, tabs );
   
   // finished writing xml for the class members.
   XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Market::getXMLName() const {
	return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& Market::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Add a region to the list of contained regions.
*
* This function is used to add a region to the list of model regions which are contained in the market region.
* If the region already exists in the vector it is not added.
*
* \param regionNameIn The name of the region to add.
*/
void Market::addRegion( const string& regionNameIn ) {
   if ( std::find( containedRegionNames.begin(), containedRegionNames.end(), regionNameIn ) == containedRegionNames.end()	) {
      containedRegionNames.push_back( regionNameIn );
   }
}

/*! \brief Get the vector of contained regions.
*
* This function when called returns the containedRegions vector. This vector consists of the names of all regions
* within this market.
*
* \return The vector of contained regions.
*/
const vector<string> Market::getContainedRegions() const {
   return containedRegionNames;
}

/*! \brief Set an initial price for the market.
* 
* This function checks if the price of the market is less than util::getSmallNumber
* and if it is sets the price to 1. This is done because the model needs a non-zero 
* starting price, but should not overwrite read-in prices. 
*
* \warning Prices for periods greater than zero will have their read-in prices overridden 
* by default when prices are initialized from the last period unless that method is overridden.
*/
void Market::initPrice() {
   if ( price < util::getSmallNumber() ) {
      price = 1;	
   }
}

/*! \brief Sets the price variable to the value specified.
*
* This method is used when it is neccessary to set the price variable to a value regardless of the type of the market.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \warning This function is not virtual.
* \author Josh Lurz
* \param priceIn The value to which to set the price member variable.
* \sa setPrice
* \sa setPriceToLast
*/
void Market::setRawPrice( const double priceIn ) {
   price = priceIn;
}

/*! \brief Set the price of the market based on the type.
*
* This method is used throughout the model to set a new price into a market. 
* But this is not used by the solution mechanism.
*
* \param priceIn The new price to set the market price to.
* \sa setRawPrice
* \sa setPriceToLast
*/
void Market::setPrice( const double priceIn ) {
   price = priceIn;
}

/*! \brief Set the market price using the price from the last period.
*
* This function is used when setting the price for a market to the value from the last period.
* The reason setRawPrice is not used is so that this method can be overridden to be a 
* no-op. This is because for CalibrationMarket the initial price is read in and should not be set from the last period.
* 
* \warning Use this instead of setRawPrice when setting the price to the price from the last period.
* \param lastPrice Price from the last period to set the market price to.
* \sa setRawPrice
* \sa setPrice
*/
void Market::setPriceFromLast( const double lastPrice ) {
   price = lastPrice;
}

/*! \brief Get the market price. 
*
* This method is used to get the price out of a Market.
*
* \return The price for the Market.
* \sa getRawPrice
*/
double Market::getPrice() const {
   return price;
}

/*! \brief Get the Raw price.
*
* This method is used to get the true value of the price variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \return The true value of the price variable.
* \sa getPrice
*/
double Market::getRawPrice() const {
   return price;
}

/*! \brief Get the storedPrice.
*
* This method is used to get the value of the storedPrice variable in the Market. It is often used in the solution mechanism.
* This is used when calculating the derivative of a market, so that the price can be changed and the solution
* mechanism can determine the difference in price, supply, and demand.
* \return The value of the storedPrice variable.
* \sa getPrice
*/
double Market::getStoredRawPrice() const {
   return storedPrice;
}

/*! \brief Null the demand.
* This function sets demand to zero. 
*/
void Market::nullDemand() {
   demand = 0;
}

/*! \brief Set the Raw demand.
*
* This method is used to set the true value of the demand variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \param value The new value to set the demand variable to. 
* \sa setDemand
*/
void Market::setRawDemand( const double value ) {
   demand = value;
}

/*! \brief Add to the the Market an amount of demand in a method based on the Market's type.
* This method is used throughout the model to add demand to a market. 
*
* \param demandIn The new demand to add to the current demand.
* \sa setRawDemand
*/
void Market::addToDemand( const double demandIn ) {
   demand += demandIn;
}

/*! \brief Remove an amount of demand from the raw demand.
*
* This function is used by the solution mechanism to subtract out an amount of demand.
* This method was needed because addToDemand is virtual, and this function needs to always change
* the raw demand. 
*
* \param demandIn Amount of demand to remove.
*/
void Market::removeFromRawDemand( const double demandIn ) {
   demand -= demandIn;
}

/*! \brief Get the Raw demand.
*
* This method is used to get the true value of the demand variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \return The true value of the demand variable.
* \sa getDemand
*/
double Market::getRawDemand() const {
   return demand;
}

/*! \brief Get the storedDemand.
*
* This method is used to get the value of the storedDemand variable in the Market. It is often used in the solution mechanism.
* This is used when calculating the derivative of a market, so that the price can be changed and the solution
* mechanism can determine the difference in price, supply, and demand.
* \return The value of the storedDemand variable.
* \sa getPrice
*/
double Market::getStoredRawDemand() const {
   return storedDemand;
}

/*! \brief Get the demand.
*
* Get the demand out of the market.
* 
* \return Market demand.
*/
double Market::getDemand() const {
   return demand;
}

/*! \brief Null the supply.
* This function sets supply to zero. 
*/
void Market::nullSupply() {
   supply = 0;
}

/*! \brief Get the raw supply.
*
* This method is used to get the true value of the supply variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \return The true value of the supply variable.
* \sa getSupply
*/
double Market::getRawSupply() const {
   return supply;
}

/*! \brief Get the storedSupply.
*
* This method is used to get the value of the storedSupply variable in the Market. It is often used in the solution mechanism.
* This is used when calculating the derivative of a market, so that the price can be changed and the solution
* mechanism can determine the difference in price, supply, and demand.
*
* \return The value of the storedSupply variable.
* \sa getSupply
*/
double Market::getStoredRawSupply() const {
   return storedSupply;
}

/*! \brief Set the Raw supply.
*
* This method is used to set the true value of the supply variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \param supplyIn The new value to set the supply variable to. 
* \sa setSupply
*/
void Market::setRawSupply( const double supplyIn ) {
   supply = supplyIn;
}

/*! \brief Get the supply.
*
* Get the supply out of the market.
* 
* \return Market supply
*/
double Market::getSupply() const {
   return supply;
}

/*! \brief Get the supply for use in checking solution.
*
* \return The value of the supply for use in checking the solution.
*/
double Market::getSupplyForChecking() const {
   return supply;
}

/*! \brief Add to the the Market an amount of supply in a method based on the Market's type.
*
* This method is used throughout the model to add supply to a market. 
*
* \param supplyIn The new demand to add to the current demand.
* \sa setRawSupply
*/
void Market::addToSupply( const double supplyIn ) {
   supply += supplyIn;
}

/*! \brief Remove an amount of supply from the raw supply.
*
* This function is used by the solution mechanism to subtract out an amount of supply.
* This method was needed because addToSupply is virtual, and this function needs to always change
* the raw supply. 
*
* \param supplyIn Amount of supply to remove.
* \return void
*/
void Market::removeFromRawSupply( const double supplyIn ) {
    supply -= supplyIn;
}

/*! \brief Return the market name.
* 
* This function returns the name of the market, as defined by region name plus good name.
*
* \return The market name
*/
string Market::getName() const {
   return region + good;
}

/*! \brief Return the market region.
* 
* This method returns the region of the market. This may not be one of the miniCAM
* regions, as a market region can contain several regions.
*
* \return The market region.
*/
string Market::getRegionName() const {
   return region;
}

/*! \brief Return the market good name.
* 
* This function returns the good that the market represents. 
*
* \return The market good.
*/
string Market::getGoodName() const {
   return good;
}

/*! \brief Set a name and value for a piece of information related to the market.
* \details This function will add the item and value to the MarketInfo object,
* which if itemName already exists will reset the current value, otherwise it will
* create a new key value pair. 
* \author Josh Lurz
* \param itemName The string to use as the key for this information value.
* \param itemValue The value to be associated with this key. 
*/
void Market::setMarketInfo( const std::string& itemName, const double itemValue ){
    
    // Lazily allocate the MarketInfo object.
    if( !mMarketInfo.get() ){
        mMarketInfo.reset( new MarketInfo() );
    }
    mMarketInfo->addItem( itemName, itemValue );
}

/*! \brief Get the value of the information stored with itemName as the key.
* \details This function will query the market's MarketInfo object for the value 
* associated with the key itemName. If the itemName does not exist, it will return 0.
* The MarketInfo object will also emit a warning if this occurs. 
* \author Josh Lurz
* \param itemName The key for the value to be queried.
* \return The value associated with itemName if it exists, 0 otherwise.
* \todo Is zero the best return value for a non-existant key?
*/
double Market::getMarketInfo( const std::string& itemName ) const {
    if( mMarketInfo.get() ){
        return mMarketInfo->getItemValue( itemName );
    }
    return 0;
}

/*! \brief Store info from last period into the market's stored variables.
*
* This function takes a demand, supply and price from the last period and sets those values
* to the markets respective stored variables.
*
* \todo Is this neccessary??
* \param lastDemand Demand from the last period.
* \param lastSupply Supply from the last period.
* \param lastPrice Price from the last period.
*/
void Market::storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice ) {
   storedDemand = lastDemand;
   storedSupply = lastSupply;
   storedPrice = lastPrice;
}

/*! \brief Store the current demand, supply, and price.
*
* This function stores the current values of demand, supply and price into their
* respective stored variables. 
*/
void Market::storeInfo() {
   storedDemand = demand;
   storedSupply = supply;
   storedPrice = price;
}

/*! \brief Restore the previous demand, supply, and price.
*
* This function sets the market's demand, supply and price to the stored
* values of those variables. 
*/
void Market::restoreInfo() {
   demand = storedDemand;
   supply = storedSupply;
   price = storedPrice;
}

/*! \brief Set that the market should be solved by the solution mechanism.
*
* This function sets a flag within the market telling the solution mechanism whether it should
* solve it, given that it satifies whatever conditions are set out in the shouldSolve
* and shouldSolveNR functions.
*
* \param doSolve A flag representing whether or not to solve the market.
*/
void Market::setSolveMarket( const bool doSolve ) {
   solveMarket = doSolve;
}

/*! \brief Determine if a market should be solved.
*
* This function returns whether a Solver should attempt to solve this market.
*
* \return Whether to attempt to solve the market. 
*/
bool Market::shouldSolve() const {
   return solveMarket;
}

/*! \brief Determine if a market should be solved for Newton-Rhapson.
*
* This function returns whether or not a Newton-Rhaphon solution mechanism should
* attempt to solve this market. This function checks that supply and demand are greater than zero.
*
* \warning This function could cause the solution mechanism to not solve a market that should because
* the market could be removed from set of markets to solve when its supply or demand temporarily became
* less than zero. If another market were adjusted however, supply or demand could become positive again.
* \todo This is not the best design right now, this should be contained in the solution mechanism.
* \return Whether or not to solve the market for Newton-Rhaphson.
*/
bool Market::shouldSolveNR() const {
   return ( solveMarket && demand > util::getSmallNumber() && supply > util::getSmallNumber() );
}

/*! \brief Return whether a market is solved according to market type specific conditions.
* \return Whether the market meets special solution criteria.
* \author Josh Lurz
*/
bool Market::meetsSpecialSolutionCriteria() const {
    return false;
}



