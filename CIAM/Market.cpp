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
#include "SavePoint.h"
#include "Logger.h"

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
   excessDemand = 0;
   logOfExcessDemand = 0;
   storedExcessDemand = 0;
   derivativeOfExcessDemand = 0;
   logOfDemand = 0;
   logOfSupply = 0;
   solveMarket = false;
}

//! Destructor
Market::~Market() {

   for ( vector<SavePoint*>::iterator iter2 = sdPoints.begin(); iter2 != sdPoints.end(); iter2++ ) {
      delete *iter2;
   }
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
   XMLWriteElement( excessDemand, "excessDemand", out );
   XMLWriteElement( logOfExcessDemand, "logOfExcessDemand", out );
   XMLWriteElement( storedExcessDemand, "storedExcessDemand", out );
   XMLWriteElement( derivativeOfExcessDemand, "derivativeOfExcessDemand", out );
   XMLWriteElement( logOfDemand, "logOfDemand", out );
   XMLWriteElement( logOfSupply, "logOfSupply", out );
   
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

/*! \brief Add additional information to the debug xml stream for derived classes.
*
* This method is inherited from by derived class if they which to add any additional information to the printout of the class.
* This method is not abstract, but for the base class it is simply an empty method. Thus it is optional to derive this method.
*
* \param out Output stream to print to.
* \return void
*/
void Market::derivedToDebugXML( ostream& out ) const {
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

/*! \brief Set an internal pointer to the companion price or demand market.
*
* This function exists in the NormalMarket to avoid dynamic casting and the use of RTTI. 
* In the base class, this function is a no-op. If it is called the program terminates as this is an error.
*
* \param pointerIn A pointer to the companion market. 
* \return void
*/

/*! \brief Get the vector of contained regions.
* This function when called returns the containedRegions vector. This vector consists of the names of all regions
* within this market.
*
* \return The vector of contained regions.
*/
const vector<string>& Market::getContainedRegions() {
   return containedRegionNames;
}

void Market::setCompanionMarketPointer( Market* pointerIn ) {
   
   //! \pre This should never be called for the base Market class.
   assert( false );
}

/*! \brief Set an initial price for the market.
*
* \ return void
*/
void Market::initPrice() {
   // Generally want a non-zero value as starting value.
   if ( price < 1E-6 ) {	// don't set if already set by read-in
      price = 1;	
   }
}

/*! \brief Set the price to zero.
*
* \return void
*/
void Market::nullPrice() {
   price = 0.0;
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
* \param priceIn Price from the last period to set the price variable to.
* \return void
* \sa setRawPrice
* \sa setPrice
*/
void Market::setPriceToLast( const double lastPrice ) {
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

/*! \brief Restore the previous price.
*
* This function resets the price to the stored price. It is often used in the solution algorithms.
*
* \return void
*/
void Market::restorePrice() {
   price = storedPrice;
}

/*! \brief Get the change in price relative to the previous price.
*
* This function calculates the change from the current value of price to the stored value of price.
* It then divides by the stored value of price. 
*
* \warning If the stored value of price is zero, the function will return zero.
* \return void
*/
double Market::getChangeInPrice() const {
   double change;
   
   if( storedPrice == 0  ){ 
      change = 0;
   }
   else {
      change = ( price - storedPrice ) / storedPrice;
   }
   return change;
}

/*! \brief Get the natural log of the change in price relative to the stored price.
* This function calculates the difference between the natural log of the stored price minus the natural log of the 
* current price. If either price is zero, the function will return the small constant passed to it. If either price is
* negative an assert will trigger an error.
*
* \param SMALL_NUM A constant small number to return if either price is zero.
* \return The change in the natural log of the prices.
*/

double Market::getLogChangeInPrice( const double SMALL_NUM ) const {
   // Should 0 be replaced with SMALL_NUM?
   double change;
   // Case 1: Price or Previous price is 0.
   if( ( storedPrice == 0 ) || ( price == 0 ) ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Price and Previous Price are both positive. 
   else if( ( storedPrice > 0 ) && ( price > 0 ) ) {
      change = log( price ) - log( storedPrice );
   }
   
   // Case 3: Price or Previous Price are negative. This should not occur.
   else {
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   return change;
}

/*! \brief Null the demand.
* This function sets demand to zero. 
* 
* \return void
*/
void Market::nullDemand() {
   demand = 0.0;
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
void Market::setDemand( const double demandIn ) {
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

//! Get the demand.
double Market::getDemand() const {
   return demand;
}

//! Calculate the natural log of the demand.
void Market::calcLogDemand( const double SMALL_NUM ) {
   logOfDemand = log( max( demand, SMALL_NUM ) );
}

//! Return the natural log of the demand.
double Market::getLogDemand() const {
   return logOfDemand;
}

//! Return the change in demand relative to the previous demand.
double Market::getChangeInDemand() const {
   
   double change;
   
   if( storedDemand == 0 ){ 
      change = 0;
   }
   else { 
      change = ( demand - storedDemand ) / storedDemand;
   }
   return change;
}

//! Get the log of the change in demand relative to the previous demand.
double Market::getLogChangeInDemand( const double SMALL_NUM ) const {
   
   double change;
   // Case 1: Demand or Previous Demand is zero.
   if( storedDemand == 0 || demand == 0 ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Demand and Previous Demand are both positive.
   else if( ( demand > 0 ) && ( storedDemand > 0 ) ) {
      change = log( demand ) - log( storedDemand );
   }
   
   // Case 3: Demand or Previous Demand is negative. This should not occur.
   else {
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   
   return change;
}

//! Null the supply.
void Market::nullSupply() {
   supply = 0.0;
}

//! Get the Raw supply.
double Market::getRawSupply() const {
   return supply;
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
void Market::setSupply( const double supplyIn ) {
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

//! Calculate the natural log of the supply.
void Market::calcLogSupply( const double SMALL_NUM ) {
   logOfSupply = log( max( supply, SMALL_NUM ) );
}

//! Get the natural log of the supply.
double Market::getLogSupply() const {
   return logOfSupply;
}

//! Get the change in the supply relative to the previous supply.
double Market::getChangeInSupply() const {
   
   double change;
   
   if( storedSupply == 0 ){ 
      change = 0;
   }
   else {
      change = ( supply - storedSupply ) / storedSupply;
   }
   return change;
}

//! Get the log of the change in supply relative to the previous supply.
double Market::getLogChangeInSupply( const double SMALL_NUM ) const {
   
   double change;
   // Case 1: Supply or Previous Supply is zero.
   if( storedSupply == 0 || supply == 0 ) {
      change = SMALL_NUM;
   }
   
   // Case 2: Supply and Previous Supply are both positive.
   else if( ( supply > 0 ) && ( storedSupply > 0 ) ) {
      change = log( supply ) - log( storedSupply );
   }
   
   // Case 3: Supply or Previous Supply is negative. This should not occur.
   else {
      cerr << "Error negative supply in market: " << getName() << "! Price: " << price << " Supply: " << supply << " Previous supply: " << storedSupply << endl;
      change = 0; // This avoids a compiler warning.
      assert( false );
   }
   
   return change;
}

//! Calculate the excess demand, setting the excessDemand variable.
void Market::calcExcessDemand() {
   excessDemand = demand - supply;
}

//! Return the excess demand.
double Market::getExcessDemand() const {
   return excessDemand;
}

//! Calculate the log base 10 of the excess demand, setting the logOfExcessDemand variable.
void Market::calcLogExcessDemand( const double SMALL_NUM ) {
   if ( demand < SMALL_NUM && supply < SMALL_NUM ) {
      logOfExcessDemand = 0; // logs may not be calculated exactly
   }
   else {
      logOfExcessDemand = log10( max( demand, SMALL_NUM ) ) - log10( max( supply, SMALL_NUM ) ); 
   }
}

//! Return the log base 10 of the excess demand.
double Market::getLogExcessDemand() const {
   return logOfExcessDemand;
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

//! Determine if a market should be solved for NR.
bool Market::shouldSolveNR( const double SMALL_NUM ) const {
   return ( solveMarket && demand > SMALL_NUM && supply > SMALL_NUM );
}

//! Return the type of the market.
string Market::getType() const {
   return "NormalMarket";
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

//! Create create stored Supply-Demand point which can be printed later.
void Market::createSDPoint() {
   SavePoint* newSavePoint = new SavePoint( price, demand, supply );
   sdPoints.push_back( newSavePoint );
}

//! Clear the current vector of SD points.
void Market::clearSDPoints() {

   for ( vector<SavePoint*>::iterator iter = sdPoints.begin(); iter != sdPoints.end(); iter++ ) {
      delete *iter;
   }

   sdPoints.clear();
}

//! Print supply and demand curved created for debugging.
void Market::printSupplyDemandDebuggingCurves( Logger* sdLog ) {

   LOG( sdLog, Logger::WARNING_LEVEL ) << "Supply and Demand curves for: " << getName() << endl;
   LOG( sdLog, Logger::WARNING_LEVEL ) << "Price,Demand,Supply," << endl;
   
   // Sort the vector and print it.
   sort( sdPoints.begin(), sdPoints.end(), std::less<SavePoint*>() );
   for ( vector<SavePoint*>::const_iterator i = sdPoints.begin(); i != sdPoints.end(); i++ ) {
      ( *i )->print( sdLog );
   }

   LOG( sdLog, Logger::WARNING_LEVEL ) << endl;
}

//! Get the relative excess demand, used for sorting markets.
double Market::getRelativeExcessDemand() const {
   
   const double SMALL_NUM = 1E-6;
   
   if( demand < SMALL_NUM ) {
      return 0;
   }

   else {
      return ( demand - supply ) / demand;
   }
}

// Other markets here temp

//! Constructor
PriceMarket::PriceMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
   priceMultiplier = 1;
   demandMarketPointer = 0;
}

//! Copy Constructor.
PriceMarket::PriceMarket( const Market& marketIn ) : Market( marketIn ) {
   priceMultiplier = 1;
   demandMarketPointer = 0;
}

void PriceMarket::setCompanionMarketPointer( Market* pointerIn ) {
   assert( pointerIn );
   demandMarketPointer = pointerIn;
}

void PriceMarket::derivedToDebugXML( ostream& out ) const {
   XMLWriteElement( priceMultiplier, "PriceMultiplier", out );
   
   if ( demandMarketPointer != 0 ) {
      XMLWriteElement( demandMarketPointer->getName(), "LinkedDemandMarket", out );
   }
}

string PriceMarket::getType() const {
   return "PriceMarket";
}

void PriceMarket::setPrice( const double priceIn ) {
   
   // Note, can't use function sestoredSupply here since that would be re-directed
   // reversed (the other order does not work)
   demand = priceIn * priceMultiplier;
   supply = price * priceMultiplier;
}

double PriceMarket::getPrice() const {
   return price / priceMultiplier; 
}

void PriceMarket::setSupply( const double supplyIn ) {
   // Pass the supply to the underlying supply market.
   assert( demandMarketPointer );
   demandMarketPointer->setSupply( supplyIn );
}

double PriceMarket::getSupply() const {
   assert( demandMarketPointer );
   return demandMarketPointer->getSupply();
}

void PriceMarket::setDemand( const double demandIn ) {
   assert( demandMarketPointer );
   demandMarketPointer->setDemand( demandIn );
}

double PriceMarket::getDemand() const {
   assert( demandMarketPointer );
   return demandMarketPointer->getDemand();
}

//! Constructor
DemandMarket::DemandMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
   demMktSupply = 0;
   priceMarketPointer = 0;
}

void DemandMarket::derivedToDebugXML( ostream& out ) const {
   XMLWriteElement( demMktSupply, "DemandMarketSupply", out );
   if ( priceMarketPointer != 0 ) {
      XMLWriteElement( priceMarketPointer->getName(), "LinkedPriceMarket", out );
   }
}

string DemandMarket::getType() const {
   return "DemandMarket";
}

double DemandMarket::getDemand() const {
   return price;
}

double DemandMarket::getSupplyForChecking() const {
   return demMktSupply;
}

void DemandMarket::setSupply( const double supplyIn ) {
   supply = price; 
   demMktSupply += supplyIn; // Store Raw supply value to check later
}

void DemandMarket::nullSupply() {
   supply = 0;
   demMktSupply = 0;
}

void DemandMarket::setCompanionMarketPointer( Market* pointerIn ) {
   assert( pointerIn );
   priceMarketPointer = pointerIn;
}

//! Constructor
GHGMarket::GHGMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

string GHGMarket::getType() const {
   return "GHGMarket";
}

void GHGMarket::initPrice() {
   price = 1;
}

bool GHGMarket::shouldSolve() const {

   // Don't solve if  there is no constraint
   return ( solveMarket && supply > 1E-6 );
}

bool GHGMarket::shouldSolveNR( const double SMALL_NUM ) const {
   bool doSolveMarket = false;
   // Check if this market is supposed to be solved & if a significant demand exists
   if ( solveMarket && demand > SMALL_NUM ) {
      doSolveMarket = true;
   }
   
   if ( ( supply < SMALL_NUM ) ||  ( price < SMALL_NUM && excessDemand < SMALL_NUM ) || ( price < SMALL_NUM ) ) {
      doSolveMarket = false; 
   }

   return doSolveMarket;
}

//! Constructor
CalibrationMarket::CalibrationMarket( const string& goodNameIn, const string& regionNameIn, const int periodIn ) :
Market( goodNameIn, regionNameIn, periodIn ) {
}

string CalibrationMarket::getType() const {
   return "CalibrationMarket";
}

void CalibrationMarket::setPriceToLast( const double lastPrice ) {
   // Do nothing, as in a calibration market the initial values for each period are set from the XML.
}