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
* This is the constructor for the market class. No default constructor exists
* to prevent the creation of empty markets. 
*
* \warning The arguments are required to define the good name, region name and model period. These values are invariants.
* \param goodNameIn The good or fuel name for the item in the market.
* \param regionNameIn The region which this market covers. It may include several model regions.
* \param periodIn The period the market exists in.
*/
Market::Market( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
good( goodNameIn ), region( regionNameIn ), period( periodIn ) {
   price = 0;
   storedPrice = 0;
   demand = 0;
   storedDemand = 0;
   supply = 0;
   storedSupply = 0;
   solveMarket = false;
}

//! Empty destructor defined for future use. 
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

/*! \brief Set the price to zero.
*/
void Market::nullPrice() {
   price = 0;
}

/*! \brief Sets the price variable to the value specified.
*
* This method is used when it is neccessary to set the price variable to a value regardless of the type of the market.
* Note that all the functions with "Raw" in the name have this behavior.
*
* \warning This function is not virtual.
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

/*! \brief Return the market good name.
* 
* This function returns the good that the market represents. 
*
* \return The market good.
*/
string Market::getGoodName() const {
   return good;
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

/*! \brief Get the relative excess demand.
*
* This function is used to determine a markets relative excess demand
* so that a set of markets can be sorted based on that criteria. 
* 
* \todo Might not be the best place for this function.
* \return The relative excess demand of the market.
*/
double Market::getRelativeExcessDemand() const {
   
   if( demand < util::getSmallNumber() ) {
      return 0;
   }
   else {
      return ( demand - supply ) / demand;
   }
}









