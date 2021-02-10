/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy ( DOE ). NEITHER THE GOVERNMENT NOR THE
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
* \file market.cpp
* \ingroup Objects
* \brief Market class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <vector>
#include <algorithm>
#include <functional>
#include <cmath>
#include <cassert>

#include "util/base/include/model_time.h" 
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"
#include "marketplace/include/market.h"
#include "marketplace/include/imarket_type.h"
#include "marketplace/include/market_container.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/info_factory.h"
#include "util/base/include/atom_registry.h"
#include "util/base/include/atom.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace objects;

extern Scenario* scenario; 


/*! \brief Constructor
 * \details This is the constructor for the market class. No default constructor
 *          exists to prevent the creation of empty markets.
 * \param aContainer The pointer to the containing MarketContainer that holds some
 *                   markets parameters that are shared accross Market objects since
 *                   they are constant over time.
 */
Market::Market( const MarketContainer* aContainer )
: mContainer( aContainer ),
mMarketInfo( InfoFactory::constructInfo( 0, aContainer->getName() ) )
{
    mSolveMarket = false;
    mPrice = 0.0;
    mSupply = 0.0;
    mDemand = 0.0;
    mForecastPrice = 0.0;
    mForecastDemand = 0.0;
    mOriginal_price = 0.0;
}

//! Destructor. This is needed because of the auto_ptr.
Market::~Market(){
}

/*! \brief Copy all data members from the given market object.
* \param aMarket The market to copy.
* \author Josh Lurz
*/
void Market::copy( const Market& aMarket ) {
    mSolveMarket = aMarket.mSolveMarket;
    mPrice = aMarket.mPrice;
    mSupply = aMarket.mSupply;
    mDemand = aMarket.mDemand;
    mForecastPrice = aMarket.mForecastPrice;
    mForecastDemand = aMarket.mForecastDemand;
    mOriginal_price = aMarket.mOriginal_price;
    mYear = aMarket.mYear;
}

/*! \brief Write out XML for debugging purposes.
* \details This method is called by the Marketplace::toDebugXML method to write
*          out information for each individual market. It prints the current
*          state of all internal variables. It also calls a derived method which
*          prints derived class specific information.
* \param period Model period for which to print information.
* \param out Output stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void Market::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs, getName(), mYear, convert_type_to_string( getType() ) );
    XMLWriteElement( mSolveMarket, "solved_Market_Flag", out, tabs );
    XMLWriteElement( mContainer->getGoodName(), "MarketGoodOrFuel", out, tabs );
    XMLWriteElement( mContainer->getRegionName(), "MarketRegion", out, tabs );
    XMLWriteElement( mPrice, "price", out, tabs );
    XMLWriteElement( getRawDemand(), "demand", out, tabs );
    XMLWriteElement( getRawSupply(), "supply", out, tabs );

    const vector<const Atom*>& containedRegions = getContainedRegions();
    for( vector<const Atom*>::const_iterator i = containedRegions.begin(); i != containedRegions.end(); ++i ) {
        XMLWriteElement( (*i)->getID(), "ContainedRegion", out, tabs );
    }

    mMarketInfo->toDebugXML( period, tabs, out );

    toDebugXMLDerived( out, tabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way the tag is always consistent for both read-in and output and
*          can be easily changed. The "==" operator that is used when parsing,
*          required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& Market::getXMLNameStatic() {
    const static string XML_NAME = "market";
    return XML_NAME;
}

/*! \brief Get the IDs of all regions contained by this market.
 * \details Return the list of contained regions implemented as a vector of
 *          constant Atoms. This vector consists of the IDs of all regions within
 *          this market.
 * \return The IDs of all regions contained by this market.
 */
const vector<const Atom*>& Market::getContainedRegions() const {
    return mContainer->getContainedRegions();
}

/*! \brief Set an initial price for the market.
* \details This function checks if the price of the market is zero
*          in which case it resets the price to 1. This is done
*          because the model needs a non-zero starting price, but should not
*          overwrite read-in prices.
* \warning Prices for periods greater than zero will have their read-in prices
*          overridden by default when prices are initialized from the last
*          period unless that method is overridden.
*/
void Market::initPrice() {
    if ( mPrice == 0 ) {
        mPrice = 1;
    }
}

/*! \brief Sets the price variable to the value specified.
* \details This method is used when it is necessary to set the price variable
*          to a value regardless of the type of the market. Note that all the
*          functions with "Raw" in the name have this behavior.
* \warning This function is not virtual.
* \author Josh Lurz
* \param priceIn The value to which to set the price member variable.
* \sa setPrice
* \sa setPriceToLast
*/
void Market::setRawPrice( const double priceIn ) {
    mPrice = priceIn;
    // trigger special actions in price, supply, and trial value markets
}

/*! \brief Set the price of the market based on the type.
* \details This method is used throughout the model to set a new price into a
*          market, but this is not used by the solution mechanism.
* \param priceIn The new price to set the market price to.
* \sa setRawPrice
* \sa setPriceToLast
*/
void Market::setPrice( const double priceIn ) {
    mPrice = priceIn;
}

/*! \brief Set the market price using the price from the last period.
* \details This function is used when setting the price for a market to the
*          value from the last period. The reason setRawPrice is not used is so
*          that this method can be overridden to be a no-op. This is because for
*          CalibrationMarket the initial price is read in and should not be set
*          from the last period.
* \todo Markets never override their read-in price now so this function can
*       become non-virtual.
* \warning Use this instead of setRawPrice when setting the price to the price
*          from the last period.
* \param lastPrice Price from the last period to set the market price to.
* \sa setRawPrice
* \sa setPrice
*/
void Market::set_price_to_last_if_default( const double lastPrice ) {
    // Only initialize the price from last period's price if the price is set to
    // the default. This prevents overwriting read-in initial prices.
    if( mPrice == 1 ){
        mPrice = lastPrice;
    }
    // If last period price is null, reset to small number so that solver
    // has a value to start with.
    else if( mPrice == 0 ){
        mPrice = util::getSmallNumber();
    }
}

/*! \brief Set the market price using the price from the last period.
* \details This function is used when setting the price for a market to the
*          value from the last period. The reason setRawPrice is not used is so
*          that this method can be overridden to be a no-op. This is because for
*          CalibrationMarket the initial price is read in and should not be set
*          from the last period.
* \todo Markets never override their read-in price now so this function can
*       become non-virtual.
* \warning Use this instead of setRawPrice when setting the price to the price
*          from the last period.
* \param lastPrice Price from the last period to set the market price to.
* \sa setRawPrice
* \sa setPrice
*/
void Market::set_price_to_last( const double lastPrice ) {
    // Initialize the price from last period's price.
    // This resets all prices to last.
    if( mPrice > 0 ){
        mPrice = lastPrice;
    }
    // If last period price is null, reset to small number so that solver
    // has a value to start with.
    else if( mPrice == 0 ){
        mPrice = util::getSmallNumber();
    }
}

/*!
 * \brief Sets a forecasted price for this market intended to give the solution
 *        algorithm a better starting point.
 * \details The forecasted price may not be used in all circumstances.
 * \sa Marketplace::init_to_last
 * \param aForecastPrice The forecasted price to set.
 */
void Market::setForecastPrice( double aForecastPrice ) {
    if( aForecastPrice != 0.0 ) {
        mForecastPrice = aForecastPrice;
    }
}

/*!
 * \brief Get the forecasted price for this market.
 * \return The forecasted price for this market.
 */
double Market::getForecastPrice() const {
    return mForecastPrice;
}

/*!
 * \brief Sets a forecasted demand for this market which may be used in some
 *        solution algorithms to rescale supplies and demands such that all markets
 *        are in a similar scale.
 * \param aForecastDemand The forecasted demand to set.
 */
void Market::setForecastDemand( double aForecastDemand ) {
    if( aForecastDemand != 0.0 ) {
        mForecastDemand = aForecastDemand;
    }
}

/*!
 * \brief Get the forecasted demand for this market.
 * \return The forecasted demand for this market.
 */
double Market::getForecastDemand() const {
    return mForecastDemand;
}

/*! \brief Get the market price.
* \details This method is used to get the price out of a Market.
* \return The price for the Market.
* \sa getRawPrice
*/
double Market::getPrice() const {
    return mPrice;
}

/*! \brief Get the raw price.
* \details This method is used to get the true value of the price variable in
*          the Market. It is often used in the solution mechanism. Note that all
*          the functions with "Raw" in the name have this behavior.
* \return The true value of the price variable.
* \sa getPrice
*/
double Market::getRawPrice() const {
    return mPrice;
}

/*! \brief Null the demand.
* This function stores the demand and resets demand to zero. 
*/
void Market::nullDemand() {
    mDemand = 0;
}

/*! \brief Add to the the Market an amount of demand in a method based on the
*          Market's type.
* \details This method is used throughout the model to add demand to a market. 
* \param demandIn The new demand to add to the current demand.
* \sa setRawDemand
*/
void Market::addToDemand( const double demandIn ) {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock writeLock( mDemandMutex, true );
        mDemand += demandIn;
    }
    else {
        mDemand += demandIn;
    }
#else
    mDemand += demandIn;
#endif
}

/*! \brief Get the raw demand.
* \details This method is used to get the true value of the demand variable in
*          the Market. It is often used in the solution mechanism. Note that all
*          the functions with "Raw" in the name have this behavior.
* \return The true value of the demand variable.
* \sa getDemand
*/
double Market::getRawDemand() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mDemandMutex, false );
        return mDemand;
    }
    else {
        return mDemand;
    }
#else
    return mDemand;
#endif
}

/*! \brief Get the demand used in the solver.
 * \details This method can be overridden in subclasses to produce better
 *          behavior in the solver (i.e., by mitigating known numerical issues).
 *          By default, it just returns the raw demand.
 * \return Demand value to be used in the solver
 * \sa getRawDemand
 */
double Market::getSolverDemand() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mDemandMutex, false );
        return mDemand;
    }
    else {
        return mDemand;
    }
#else
    return mDemand;
#endif
}

/*! \brief Get the demand.
* \details Get the demand out of the market.
* \return Market demand.
*/
double Market::getDemand() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mDemandMutex, false );
        return mDemand;
    }
    else {
        return mDemand;
    }
#else
    return mDemand;
#endif
}

/*! \brief Null the supply.
* \details This function stores the supply and resets supply to zero. 
*/
void Market::nullSupply() {
    mSupply = 0;
}

/*! \brief Get the raw supply.
* \details This method is used to get the true value of the supply variable in
*          the Market. It is often used in the solution mechanism. Note that all
*          the functions with "Raw" in the name have this behavior.
* \return The true value of the supply variable.
* \sa getSupply
*/
double Market::getRawSupply() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mSupplyMutex, false );
        return mSupply;
    }
    else {
        return mSupply;
    }
#else
    return mSupply;
#endif
}

/*! \brief Get the supply value to be used in the solver
* \details This method can be overridden in subclasses to produce better
*          behavior in the solver (i.e., by mitigating known numerical issues
*          -- see the market_RES override for an example of this).
*          By default, it just returns the raw supply.
* \return Supply value to be used in the solver.
* \sa getRawSupply
*/
double Market::getSolverSupply() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mSupplyMutex, false );
        return mSupply;
    }
    else {
        return mSupply;
    }
#else
    return mSupply;
#endif
}

/*! \brief Get the supply.
* \details Get the supply out of the market.
* \return Market supply
*/
double Market::getSupply() const {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock readLock( mSupplyMutex, false );
        return mSupply;
    }
    else {
        return mSupply;
    }
#else
    return mSupply;
#endif
}

/*! \brief Add to the the Market an amount of supply in a method based on the
*          Market's type.
* \details This method is used throughout the model to add supply to a market. 
* \param supplyIn The new supply to add to the current supply.
* \sa setRawSupply
*/
void Market::addToSupply( const double supplyIn ) {
#if GCAM_PARALLEL_ENABLED
    if( !Marketplace::mIsDerivativeCalc ) {
        Mutex::scoped_lock writeLock( mSupplyMutex, true );
        mSupply += supplyIn;
    }
    else {
        mSupply += supplyIn;
    }
#else
    mSupply += supplyIn;
#endif
}

/*! \brief Return the market name.
 * \details This function returns the name of the market, as defined by region
 *          name plus good name.
 * \return The market name
 */
const string& Market::getName() const {
    return mContainer->getName();
}

/*! \brief Return the market region.
 * \details This method returns the region of the market. This may not be one of
 *          the miniCAM regions, as a market region can contain several regions.
 * \return The market region.
 */
const string& Market::getRegionName() const {
    return mContainer->getRegionName();
}

/*! \brief Return the market good name.
 * \details This function returns the good that the market represents.
 * \return The market good.
 */
const string& Market::getGoodName() const {
    return mContainer->getGoodName();
}

/*! \brief Get the information object for this market which can then be used to
*          query for specific values.
* \details This function returns the internal IInfo object of this market which
*          represents a set of pairings of information name to value. The
*          information object is allocated in the constructor and so cannot be
*          null. This specific function returns a constant pointer to the
*          information object, so values can be queried but not added or
*          modified.
* \note This version of the function is required so that it can be called in
*       constant functions. A second version is available which returns a
*       mutable pointer.
* \return A constant pointer to the market information object.
* \author Josh Lurz
*/
const IInfo* Market::getMarketInfo() const {
    return mMarketInfo.get();
}

/*! \brief Get the information object for this market which can then be used to
*          query, add, or modify specific values.
* \details This function returns the internal IInfo object of this market which
*          represents a set of pairings of information name to value. The
*          information object is allocated in the constructor and so cannot be
*          null. This specific function returns a mutable pointer to the
*          information object, so values can be queried, added and modified.
* \note This function returns a mutable pointer to the information object so it
*       cannot be called from constant function.
* \return A mutable pointer to the market information object.
* \author Josh Lurz
*/
IInfo* Market::getMarketInfo() {
    return mMarketInfo.get();
}

/*! \brief Store the original price.
*/
void Market::store_original_price() {
    mOriginal_price = mPrice;
}

/*! \brief Store the original price.
*/
void Market::restore_original_price() {
    mPrice = mOriginal_price;
}

/*! \brief Set that the market should be solved by the solution mechanism.
* \details This function sets a flag within the market telling the solution
*          mechanism whether it should solve it, given that it satifies whatever
*          conditions are set out in the shouldSolve and shouldSolveNR
*          functions.
* \param doSolve A flag representing whether or not to solve the market.
*/
void Market::setSolveMarket( const bool doSolve ) {
    mSolveMarket = doSolve;
}

/*! \brief Determine if a market should be solved.
* \details This function returns whether a Solver should attempt to solve this market.
* \return Whether to attempt to solve the market. 
*/
bool Market::shouldSolve() const {
    return mSolveMarket;
}

/*! \brief Determine if a market should be solved for Newton-Rhapson.
* \details This function returns whether or not a Newton-Rhaphon solution
*          mechanism should attempt to solve this market. This function checks
*          that supply and demand are greater than zero.
* \warning This function could cause the solution mechanism to not solve a
*          market that should because the market could be removed from set of
*          markets to solve when its supply or demand temporarily became less
*          than zero. If another market were adjusted however, supply or demand
*          could become positive again. 
* \warning With the introduction of the linear price NR family of solvers I
*          have removed the price constraint.  That means that if you
*          are using the log price version, you must explicitly supply
*          a price-greater-than filter to prevent the solver trying to
*          take the log of a negative number. 
* \todo This is not the best design right now, this should be contained in the
*       solution mechanism.
* \return Whether or not to solve the market for Newton-Rhaphson.
*/
bool Market::shouldSolveNR() const {
    // Solves all solvable markets where there is nonzero supply.
    return mSolveMarket && ( getRawSupply() != 0.0 || getRawDemand() >= util::getVerySmallNumber() );
}

/*! \brief Return whether a market is solved according to market type specific
*          conditions.
* \return Whether the market meets special solution criteria.
* \author Josh Lurz
*/
bool Market::meetsSpecialSolutionCriteria() const {
    // This is a normal market which should not be solved in the base period
    // unless the solve flag is set.
    return ( !mSolveMarket && mYear == scenario->getModeltime()->getStartYear() );
}

/*!
 * \brief Get the year of this market.
 * \return The year of this market.
 */
int Market::getYear() const {
    return mYear;
}

/*!
 * \brief Set the year of this market.
 * \param aYear The new year to set.
 */
void Market::setYear( const int aYear ) {
    mYear = aYear;
}

/*!
 * \brief Get this market's serial number.
 */
int Market::getSerialNumber() const {
    return mContainer->getSerialNumber();
}

/*! \brief Update an output container with information from a Market for a given period.
* \param aVisitor The output container to update.
* \param aPeriod The period for which to update.
*/
void Market::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitMarket( this, aPeriod );
    aVisitor->endVisitMarket( this, aPeriod );
}

/*!
 * \brief Helper method to convert a type into a string for output.
 * \param aType Market type to convert to a string.
 * \return Market type as a string.
 * \todo Move this to a MarketUtils class.
 */
const string& Market::convert_type_to_string( const IMarketType::Type aType ) {
    // Check that the type is legal.
    assert( aType < IMarketType::END );

    // Setup a static array of the types of markets. If you add a type to the
    // IMarketType array make sure to add to this array as well and keep this in
    // the same order as the IMarketType enum.
    static const string types[] = { "Normal", "Calibration", "Inverse-Calibration",
                                    "Tax", "RES", "Subsidy", "Trial-Value",
                                    "Demand", "Price", "Linked" };

    // Check that the types array is up to date.
    assert( sizeof( types ) / sizeof( types[ 0 ] ) == IMarketType::END );

    return types[ aType ];
}

/*!
 * \brief Returns if the flag which indicate that this market should be solved is set.
 * \details This method should not be used by a solver to determine if it should
 *          attempt to solve this market.  Instead it should use Market::shouldSolve().
 * \return The current value of the solveMarket flag.
 */
bool Market::isSolvable() const {
    return mSolveMarket;
}

/*!
 * \brief Relinquishes control of the market info object.
 * \details This becomes useful for instance when a market gets replaced with a
 *          price market.
 * \return The pointer to this market's info object for which the caller will
 *         subsequently be responsible for.
 */
IInfo* Market::releaseMarketInfo() {
    return mMarketInfo.release();
}
