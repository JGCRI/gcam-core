/*! 
* \file Market.cpp
* \ingroup CIAM
* \brief Market class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <vector>
#include <algorithm>
#include <cmath>
#include <cassert>

#include "modeltime.h" 
#include "XMLHelper.h"
#include "Market.h"

using namespace std;

/*! \brief Default constructor
* 
* This is the single constructor for the market class.
*
* \warning The arguments are required to define the good name, region name and model period. These values are invariants.
* \param goodNameIn The good or fuel name for the item in the market.
* \param regionNameIn The region which this market covers. It may include several model regions.
* \param periodIn The period of the market.
*/
Market::Market( const string& goodNameIn, const string& regionNameIn, const int periodIn ){
   
   good = goodNameIn;
   region = regionNameIn;
   period = periodIn;
   price = 0;
   storedPrice = 0;
   demand = 0;
   storedDemand = 0;
   supply = 0;
   storedSupply = 0;
   solveMarket = false;
}

//! Destructor
Market::~Market() {
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called by the Marketplace::toDebugXML method to write out information for each individual market.
* It prints the current state of all internal variables. It also calls a derived method which prints derived class 
* specific information.
*
* \param period Model period for which to print information.
* \param out Output stream to print to.
* \return void
*/
void Market::toDebugXML( const int period, ostream& out ) const {
   
   
   // write the beginning tag.
   Tabs::writeTabs( out );
   out << "<Market name=\""<< getName() << "\" type=\"" << getType() << "\">" << endl;
   
   // increase the indent.
   Tabs::increaseIndent();
   
   XMLWriteElement( good, "MarketGoodOrFuel", out );
   XMLWriteElement( region, "MarketRegion", out );
   XMLWriteElement( period, "period", out );
   XMLWriteElement( price, "price", out );
   XMLWriteElement( storedPrice, "storedPrice", out );
   XMLWriteElement( demand, "demand", out );
   XMLWriteElement( storedDemand, "storedDemand", out );
   XMLWriteElement( supply, "supply", out );
   XMLWriteElement( storedSupply, "storedSupply", out );
   
   for( vector<string>::const_iterator i = containedRegionNames.begin(); i != containedRegionNames.end(); i++ ) {
      XMLWriteElement( *i, "ContainedRegion", out );
   }

   derivedToDebugXML( out );
   
   // finished writing xml for the class members.
   
   // decrease the indent.
   Tabs::decreaseIndent();
   
   // write the closing tag.
   Tabs::writeTabs( out );
   out << "</Market>" << endl;
}

/*! \brief Add a region to the contained regions.
*
* This function is used to add a region to the list of model regions which are contained in the market region.
* If the region already exists in the vector it is not added.
*
* \param regionNameIn The name of the region to add.
* \return void
*/
void Market::addRegion( const string& regionNameIn ) {
   if ( std::find( containedRegionNames.begin(), containedRegionNames.end(), regionNameIn ) == containedRegionNames.end()	) {
      containedRegionNames.push_back( regionNameIn );
   }
}

/*! \brief Get the vector of contained regions.
* This function when called returns the containedRegions vector. This vector consists of the names of all regions
* within this market.
*
* \return The vector of contained regions.
*/
const vector<string>& Market::getContainedRegions() {
   return containedRegionNames;
}

/*! \brief Set an internal pointer to the companion price or demand market.
*
* This function exists in the NormalMarket to avoid dynamic casting and the use of RTTI. 
* In the base class, this function is a no-op. If it is called the program terminates as this is an error.
*
* \param pointerIn A pointer to the companion market. 
*/
void Market::setCompanionMarketPointer( Market* pointerIn ){
    assert( false );
}

/*! \brief Set an initial price for the market.
*
* \ return void
*/
void Market::initPrice() {
   // Generally want a non-zero value as starting value.
   if ( price < util::getSmallNumber() ) {	// don't set if already set by read-in
      price = 1;	
   }
}

/*! \brief Set the price to zero.
*
* \return void
*/
void Market::nullPrice() {
   price = 0;
}

/*! \brief Sets the price variable to the value specified.
*
* This method is used when it is neccessary to set the price variable to an exact value regardless of the type of the market.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \warning This function is not virtual.
* \param priceIn The value to which to set the price member variable.
* \return void
* \sa setPrice
* \sa setPriceToLast
*/
void Market::setRawPrice( const double priceIn ) {
   price = priceIn;
}

/*! \brief Set the price of the market based on the type.
*
* This method is used throughout the model to set a new price into a market. 
*
* \warning This function is virtual and its behavior is different for PriceMarket and DemandMarket.
* \param priceIn The new price to set the price variable to.
* \return void
* \sa setRawPrice
* \sa setPriceToLast
*/
void Market::setPrice( const double priceIn ) {
   price = priceIn;
}

/*! \brief Set the price using the price from the last period.
*
* This function is used when setting the price for a market initially to the value from the last period.
* The reason setRawPrice is not used is so that in CalibrationMarket this method can be overridden to be a 
* no-op. This is because for CalibrationMarket the initial price is read in and should not be set from the last period.
* 
* \warning Use this instead of setRawPrice when setting the price to the price from the last period.
* \param lastPrice Price from the last period to set the price variable to.
* \return void
* \sa setRawPrice
* \sa setPrice
*/
void Market::setPriceFromLast( const double lastPrice ) {
   price = lastPrice;
}

/*! \brief Get the price out of the market is a method dependent on the Market type.
*
* This method is used to get the price out of a Market. It's behavior is different for the derived DemandMarket and PriceMarket.
*
* \warning This function is virtual and its behavior is changed in inherited classes.
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
* \warning This function is not virtual.
* \return The true value of the price variable.
* \sa getPrice
*/
double Market::getRawPrice() const {
   return price;
}

/*! \brief Get the storedPrice.
*
* This method is used to get the value of the storedPrice variable in the Market. It is often used in the solution mechanism.
*
* \return The value of the storedPrice variable.
* \sa getPrice
*/
double Market::getStoredRawPrice() const {
   return storedPrice;
}

/*! \brief Null the demand.
* This function sets demand to zero. 
* 
* \return void
*/
void Market::nullDemand() {
   demand = 0;
}

/*! \brief Set the Raw demand.
* This method is used to set the true value of the demand variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \warning This function is not virtual.
* \param value the new value to set demand to. 
* \return void
* \sa setDemand
*/
void Market::setRawDemand( const double value ) {
   demand = value;
}

/*! \brief Add to the amount of demand in the Market in a method based on the Market's type.
* This method is used throughout the model to add demand to a market. 
*
* \warning This function is virtual and its behavior is different for PriceMarket and DemandMarket.
* \param demandIn The new demand to add to the current demand.
* \return void
* \sa setRawDemand
*/
void Market::addToDemand( const double demandIn ) {
   demand += demandIn;
}

/*! \brief Remove an amount of demand from the raw demand.
* This function is used by the solution mechanism to subtract out an amount of demand.
*
* \warning This function is not virtual.
* \param demandIn Amount of demand to remove.
* \return void
*/
void Market::removeFromRawDemand( const double demandIn ) {
   demand -= demandIn;
}

/*! \brief Get the Raw demand.
* This method is used to get the true value of the demand variable in the Market. It is often used in the solution mechanism.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \warning This function is not virtual.
* \return The true value of the demand variable.
* \sa getDemand
*/
double Market::getRawDemand() const {
   return demand;
}

/*! \brief Get the storedDemand.
*
* This method is used to get the value of the storedDemand variable in the Market. It is often used in the solution mechanism.
*
* \return The value of the storedDemand variable.
* \sa getPrice
*/
double Market::getStoredRawDemand() const {
   return storedDemand;
}

//! Get the demand.
double Market::getDemand() const {
   return demand;
}

//! Null the supply.
void Market::nullSupply() {
   supply = 0;
}

//! Get the Raw supply.
double Market::getRawSupply() const {
   return supply;
}

/*! \brief Get the storedSupply.
*
* This method is used to get the value of the storedSupply variable in the Market. It is often used in the solution mechanism.
*
* \return The value of the storedSupply variable.
* \sa getPrice
*/
double Market::getStoredRawSupply() const {
   return storedSupply;
}

//! Set the Raw supply.
void Market::setRawSupply( const double supplyIn ) {
   supply = supplyIn;
}

//! Get the supply. 
double Market::getSupply() const {
   return supply;
}

//! Get the supply for use in checking solution.
double Market::getSupplyForChecking() const {
   return supply;
}

//! Set the supply.
void Market::addToSupply( const double supplyIn ) {
   supply += supplyIn;
}

/*! \brief Remove an amount of supply from the raw supply.
* This function is used by the solution mechanism to subtract out an amount of supply.
*
* \warning This function is not virtual.
* \param supplyIn Amount of supply to remove.
* \return void
*/
void Market::removeFromRawSupply( const double supplyIn ) {
   supply -= supplyIn;
}

//! Return the market name.
string Market::getName() const {
   return region + good;
}

//! Return the market good name.
string Market::getGoodName() const {
   return good;
}

//! Return the market region.
string Market::getRegionName() const {
   return region;
}

//! Store info from last period.
void Market::storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice ) {
   storedDemand = lastDemand;
   storedSupply = lastSupply;
   storedPrice = lastPrice;
}

//! Store the current demand, supply, and price.
void Market::storeInfo() {
   storedDemand = demand;
   storedSupply = supply;
   storedPrice = price;
}

//! Restore the previous demand, supply, and price.
void Market::restoreInfo() {
   demand = storedDemand;
   supply = storedSupply;
   price = storedPrice;
}

//! Set the solveMarket flag.
void Market::setSolveMarket( const bool doSolve ) {
   solveMarket = doSolve;
}

//! Determine if a market should be solved.
bool Market::shouldSolve() const {
   return solveMarket;
}

//! Determine if a market should be solved for Newton-Rhapson.
bool Market::shouldSolveNR() const {
   return ( solveMarket && demand > util::getSmallNumber() && supply > util::getSmallNumber() );
}

//! Print the market to the given output stream.
void Market::print( ostream& out ) const {
   
   out << "Market Period: " << period << endl;
   out << "Market Name: " << good << endl;
   out << "Market Region: " << region << endl;
   out << "Market Price: " << price << endl;
   out << "Market Supply: " << supply << endl;
   out << "Market Demand: " << demand << endl << endl;
}


//! Get the relative excess demand, used for sorting markets.
double Market::getRelativeExcessDemand() const {
   
   if( demand < util::getSmallNumber() ) {
      return 0;
   }

   else {
      return ( demand - supply ) / demand;
   }
}

// Other markets here temp









