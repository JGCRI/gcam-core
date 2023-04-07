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
* \file marketplace.cpp
* \ingroup Objects
* \brief Marketplace class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"

#include <vector>
#include <iomanip>

#if GCAM_PARALLEL_ENABLED
#include <tbb/parallel_for.h>
#endif

#include "marketplace/include/marketplace.h"
#include "marketplace/include/market.h"
#include "marketplace/include/market_container.h"
#include "marketplace/include/imarket_type.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"
#include "util/base/include/fltcmp.hpp"
#include "util/base/include/time_vector.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/market_locator.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/cached_market.h"
#include "containers/include/market_dependency_finder.h"
#include "solution/util/include/ublas-helpers.hpp"

using namespace std;

extern Scenario* scenario;
const double Marketplace::NO_MARKET_PRICE = util::getLargeNumber();
bool Marketplace::mIsDerivativeCalc = false;

/*! \brief Default constructor 
*
* The default constructor for the Marketplace which initializes several datamembers and
* creates an instance of the selected solver.
*
* \todo Marketplace might be better as a singleton.
*/
Marketplace::Marketplace():
mMarketLocator( new MarketLocator() ),
mDependencyFinder( new MarketDependencyFinder( this ) )
{
}

/*! \brief Destructor
*
* Destructor for the marketplace which deletes all the markets.
*/
Marketplace::~Marketplace() {
    // Clean up the markets.
    for( auto marketContainer : mMarkets ) {
        delete marketContainer;
    }
    mMarkets.clear();
}

/*! \brief Get the XML node name in static form for outputting XML.
* \author Josh Lurz
* \return The constant XML element name string as a static.
*/
const string& Marketplace::getXMLNameStatic() {
    const static string XML_NAME = "Marketplace";
    return XML_NAME;
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
* \param tabs Tabs object used to track the number of tabs to print.
* \author Josh Lurz
*/
void Marketplace::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLNameStatic(), out, tabs );

    // write the xml for the class members.
    XMLWriteElement( static_cast<int>( mMarkets.size() ), "numberOfMarkets", out, tabs );

    // Write out the individual markets
    const static string debugRegion =
        Configuration::getInstance()->getString( "debug-region", "USA" );

    for( unsigned int i = 0; i < mMarkets.size(); i++ ){
        // TODO: This isn't quite right. This should search the contained
        // region list.
        if( mMarkets[ i ]->getMarket( period )->getRegionName() == debugRegion ||
            mMarkets[ i ]->getMarket( period )->getRegionName() == "global" )
        {
            mMarkets[ i ]->getMarket( period )->toDebugXML( period, out, tabs );
        }
    }

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLNameStatic(), out, tabs );
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
* \param aType The type of market to create.
* \return Whether a market was created.
*/
bool Marketplace::createMarket( const string& regionName, const string& marketName, const string& goodName, const IMarketType::Type aType ) {
    /*! \pre Region name, market name, and sector name must be non null. */
    assert( !regionName.empty() && !marketName.empty() && !goodName.empty() );

    // Create the index within the market locator.
    const int uniqueNumber = static_cast<int>( mMarkets.size() );
    int marketNumber = mMarketLocator->addMarket( marketName, regionName, goodName, uniqueNumber );

    // If the market number is the unique number we passed it, the market did not already exist and 
    // we should create the market objects, one per period.
    const bool isNewMarket = ( marketNumber == uniqueNumber );
    if( isNewMarket ){
        mMarkets.push_back( new MarketContainer( aType, goodName, marketName ) );
    }

    // Add the region onto the market.
    mMarkets[ marketNumber ]->addRegion( regionName );
    // Return whether we were required to create a new market.
    return isNewMarket;
}

/*!
 * \brief Create a market that links to another market.
 * \details This function creates a market using the same symatics as createMarket
 *          however differs in two key ways:
 *          - The market type is always LinkedMarket and the parameter linkedMarket along
 *            with the regionName will be used to look up the good to link to.
 *          - We allow these types of markets to change over time.  That is if the aStartPeriod
 *            parameter is set (non negative value) then this call may potentially override a
 *            previously set market with one that links elsewhere.
 *          Please see LinkedMarket for details on what it means for a market to link to
 *          another market.
 * \param regionName The region of the policy for which to create a market.
 * \param marketName The market region for which to create a market. This varies from the
 *                   regionName, it can be global, a multi-region market, or the same as
 *                   the region name.
 * \param goodName The good for which to create a market.
 * \param linkedGoodName The good which in conjunction with regionName will be used to lookup
 *                       which market is linked to.
 * \param aStartPeriod If a non-negative value is the period in which a linked market may
 *                     override a previously set market to effectively switch markets over time.
 * \return If a new market was created.
 * \sa LinkedMarket
 * \note If user calls to this method with aStartPeriod such that the LINKED market is left
 *       with gaps, then by default those periods will link to no market.  Such that calls
 *       getPrice will return NO_MARKET_PRICE and calls to addToDemand will do nothing.
 */
bool Marketplace::createLinkedMarket( const string& regionName, const string& marketName,
                                      const string& goodName, const string& linkedGoodName,
                                      const int aStartPeriod )
{
    /*! \pre Region name, market name, sector name, and linked good name must be non null. */
    assert( !regionName.empty() && !marketName.empty() && !goodName.empty() && !linkedGoodName.empty() );

    /*!
     * \warning Changing marketName and changing a market over time may have unintended consequences
     *          instead we reccommend using regional markets when a user wants to change the linked
     *          market over time.
     */
    if( aStartPeriod != -1 && regionName != marketName ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Changing regional markets and linking to different markets over time is not reccommend." << endl;
        mainLog << "A safer approach would be to use regional markets in this use case." << endl;
    }

    // Create the index within the market locator.
    const int uniqueNumber = static_cast<int>( mMarkets.size() );
    int marketNumber = mMarketLocator->addMarket( marketName, regionName, goodName, uniqueNumber );
    
    // If the market number is the unique number we passed it, the market did not already exist and 
    // we should create the market objects, one per period.
    const bool isNewMarket = ( marketNumber == uniqueNumber );

    // Find the market to link to.
    const int linkedMarketNumber = mMarketLocator->getMarketNumber( regionName, linkedGoodName );
    if( linkedMarketNumber == MarketLocator::MARKET_NOT_FOUND ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Linked market "<< goodName << " in " << regionName << " could not be linked to " << linkedGoodName << endl;
    }
    // We check if the default value for start year is found (-1).  If this is a new market then
    // we assume the user wanted to link for all period.  Otherwise we must assume this linked policy
    // is simply adding it's region to the market.
    if( isNewMarket ) {
        mMarkets.push_back( new MarketContainer( linkedMarketNumber == MarketLocator::MARKET_NOT_FOUND ? 0
                                                 : mMarkets[ linkedMarketNumber ], goodName, marketName, aStartPeriod ) );
    }
    else {
        mMarkets[ marketNumber ]->changeLinkedMarket( linkedMarketNumber == MarketLocator::MARKET_NOT_FOUND ? 0
                                                      : mMarkets[ linkedMarketNumber ], aStartPeriod );
    }
    
    // Add the region onto the market.
    mMarkets[ marketNumber ]->addRegion( regionName );
    
    // Return whether we were required to create a new market.
    return isNewMarket;
}

/*! \brief Restructures a market to account for simultaneities.
*
* Changes the named market to a price market, which supplies a trial price for a secondary good.
* It also adds a corresponding demand market that provides a trial value for demand.
* Markets are subclassed to allow Market::addToDemand, Market::getDemand, Market::getPrice, etc. to act 
* differently for PriceMarket and DemandMarket so that these changes are transparent to the 
* rest of the code.  Only secondary goods are allowed to restructure their markets;
* any markets set to solve will be ignored.
* 
* \author Steve Smith
* \param aMarketNumber The market by number to reset.
*/
int Marketplace::resetToPriceMarket( const int aMarketNumber ) {
    // If simuls are off, return immediately. 
    if( !Configuration::getInstance()->getBool( "simulActive" ) ){
        return -1;
    }

    if( aMarketNumber == MarketLocator::MARKET_NOT_FOUND ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Cannot reset Market "<< aMarketNumber << " to a price market because it does not exist."  << endl;
    }
    else if( mMarkets[ aMarketNumber]->getMarket( 0 )->getType() != IMarketType::NORMAL ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Cannot reset market type other than normal to a price market." << endl;
    }
    else if( mMarkets[ aMarketNumber ]->getMarket( 1 )->isSolvable() ) {
        // Solved markets do not need to be split.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Solved markets do not need trial price/demand markets." << endl;
    }
    else {
        // Setup the corresponding demand markets
        string marketName = mMarkets[ aMarketNumber ]->getRegionName();
        string goodName = mMarkets[ aMarketNumber ]->getGoodName();
        string demandGoodName = goodName + "Demand_int";
        string regionName = mMarkets[ aMarketNumber ]->getRegionName();
        createMarket( regionName, marketName, demandGoodName, IMarketType::DEMAND );
        // Add units of the corresponding NORMAL market to the DEMAND market info object.
        IInfo* marketInfoFrom = getMarketInfo( goodName, regionName, 0, true );
        IInfo* marketInfo = getMarketInfo( demandGoodName, regionName, 0, true );
        marketInfo->setString( "price-unit", marketInfoFrom->getString( "price-unit", true ) );
        marketInfo->setString( "output-unit", marketInfoFrom->getString( "output-unit", true ) );

        int demandMarketNumber = mMarketLocator->getMarketNumber( regionName, demandGoodName );
        assert( demandMarketNumber != MarketLocator::MARKET_NOT_FOUND );
        mMarkets[ aMarketNumber ]->resetToPriceMarket( mMarkets[ demandMarketNumber ] );
        return demandMarketNumber;
    }
    return -1;
}

/*! \brief Set the prices by period of a market from a vector.
*
* This function sets the price for each period of a market according to the corresponding value in 
* the prices vector.
*
* \author Josh Lurz
* \param goodName The goodName of the market for which to set new prices.
* \param regionName The regionName to use for the lookup to determine the correct market.
* \param prices A vector containing prices to set into the market. 
*/
void Marketplace::setPriceVector( const string& goodName, const string& regionName,
                                 const objects::PeriodVector<Value>& prices ){
    // determine what market the region and good are in.
    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );
    if( marketNumber == MarketLocator::MARKET_NOT_FOUND ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market price vector cannot be set the market does not exist: " 
            << goodName << " " << regionName << endl;
    }
    else {
        for( unsigned int i = 0; i < mMarkets[ marketNumber ]->size() && i < prices.size(); i++ ){
            mMarkets[ marketNumber ]->getMarket( i )->setPrice( prices[ i ] );
        }
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
* Marketplace::init_to_last except for calibration markets.
*/
void Marketplace::initPrices(){
    // initialize supply and demand sector market prices to 1.
    for ( unsigned int i = 0; i < mMarkets.size(); ++i ){
        for( unsigned int j = 0; j < mMarkets[ i ]->size(); ++j ){
            mMarkets[ i ]->getMarket( j )->initPrice();
        }
        // no forecast in period 0
        mMarkets[i]->getMarket( 0 )->setForecastPrice(mMarkets[i]->getMarket( 0 )->getRawPrice());
    }
}

/*! \brief Set the solve flag for this market for the given period, or all periods if per argument 
* is undefined.
*
* This function determines a market from a good and region and sets the market to solve for the 
* period passed to the function.  This solve flag determines whether the market is solved by the
* solution mechanism, except for cases where the market does not pass certain other criteria 
* related to singularities. If this flag is set to false, as is the default, the market will never be solved. 
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should be solved.
*/
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName, const int per ) {
    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );

    // If the market exists.
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        mMarkets[ marketNumber ]->getMarket( per )->setSolveMarket( true );
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market cannot be set to solve as it does not exist: " << goodName << " " 
            << regionName << endl;
    }
}

/*! \brief Unset the solve flag for this market for the given period, or all periods if per argument
* is undefined.  This function determines a market from a good and region and unsets the market to
* solve for the period passed to the function.  This solve flag determines whether the market is
* solved by the solution mechanism, except for cases where the market does not pass certain
* other criteria related to singularities. If this flag is set to false, as is the default, the
* market will never be solved.  This function also clears supply and demand for the market at the
* same time. 
* \todo This function is only used in one place.
* \param goodName The name of the good of the market.
* \param regionName The region name of the market.
* \param per The period for which the market should not be solved.
*/
void Marketplace::unsetMarketToSolve ( const string& goodName, const string& regionName, const int per ) {

    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );

    // If the market exists.
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        mMarkets[ marketNumber ]->getMarket( per )->setSolveMarket( false );
        mMarkets[ marketNumber ]->getMarket( per )->nullSupply();
        mMarkets[ marketNumber ]->getMarket( per )->nullDemand();
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market cannot be unset not to solve as it does not exist: " << goodName << " " 
            << regionName << endl;
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
#if GCAM_PARALLEL_ENABLED
    tbb::parallel_for( tbb::blocked_range<int>( 0, mMarkets.size() ), [this, period]( const tbb::blocked_range<int>& aRange) {
        for( int marketIndex = aRange.begin(); marketIndex != aRange.end(); ++marketIndex ) {
            this->mMarkets[ marketIndex ]->getMarket( period )->nullDemand();
            this->mMarkets[ marketIndex ]->getMarket( period )->nullSupply();
        }
    });
#else
    for ( unsigned int i = 0; i < mMarkets.size(); i++ ) {
        mMarkets[ i ]->getMarket( period )->nullDemand();
        mMarkets[ i ]->getMarket( period )->nullSupply();
    }
#endif
}

/*! \brief Assign a serial number to each market we are attempting to solve
 *
 * \details Iterate over the entire list of markets and assign a
 *          serial number (in fact, the market's index in the master
 *          array of market structures) to each one.
 *
 *          Another side effect of this function is that it writes the
 *          names and serial numbers of the markets to the
 *          solver-data-key at log level DEBUG.  The format of this
 *          log is three comma-separated columns: period, serial
 *          number, market name.
 */
void Marketplace::assignMarketSerialNumbers( int aPeriod )
{
    ILogger &solverDataKey = ILogger::getLogger("solver-data-key");
    solverDataKey.setLevel(ILogger::DEBUG);

    // Give markets being solved the lower numbers
    int id=1;
    for(unsigned i=0; i<mMarkets.size(); ++i) {
        if(mMarkets[i]->getMarket(aPeriod)->shouldSolve()) {
            mMarkets[i]->assignSerialNumber( id );
            solverDataKey << aPeriod << ", " << id << ", " << mMarkets[i]->getMarket(aPeriod)->getName() << "\n";
            id++;
        }
    }

    // Give markets not being solved the higher numbers
    for(unsigned i=0; i<mMarkets.size(); ++i) {
        if(!mMarkets[i]->getMarket(aPeriod)->shouldSolve()) {
            mMarkets[i]->assignSerialNumber( id );
            solverDataKey << aPeriod << ", " << id << ", " << mMarkets[i]->getMarket(aPeriod)->getName() << "\n";
            id++;
        }
    }
}

/*! \brief Set the market price.
*
* This function uses the type dependent method Market::setPrice to set the passed in value into
* the market.
*
* \param goodName The good of the market.
* \param regionName The region setting the price.
* \param value The value to which to set price.
* \param per The period in which to set the price.
* \param aMustExist Whether it is an error for the market not to exist.
*/
void Marketplace::setPrice( const string& goodName, const string& regionName, const double value,
                            const int per, bool aMustExist )
{
    // Print a warning message if the new price is not a finite number.
    if ( !util::isValidNumber( value ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Error setting price in marketplace for: " << goodName << ", value: " << value << endl;
        return;
    }

    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        mMarkets[ marketNumber ]->getMarket( per )->setPrice( value );
    }
    else if( aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Cannot set price for market as it does not exist: " << goodName << " " 
            << regionName << endl;
    }
}

/*! \brief Add to the supply for this market.
*
* This function increments the supply for a market determined by the goodName and regionName
* by a given value. This function is used throughout the model to add supply to markets.
* In order to add to supply a caller must provide a "state" backed Value class this is
* due to when calculating partial derivatives we are interesed in adding the difference
* from the current value and the "base" value to supply.
*
* \param goodName Name of the good for which to add supply.
* \param regionName Name of the region in which supply should be added for the market.
* \param value Amount of supply to add.
* \param per Period in which to add supply.
* \param aMustExist Whether it is an error for the market not to exist.
*/
void Marketplace::addToSupply( const string& goodName, const string& regionName, const Value& value,
                               const int per, bool aMustExist )
{
    // Print a warning message when adding infinity values to the supply.
    if ( !util::isValidNumber( value ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Error adding to supply in marketplace for: " << goodName << ", region: " << regionName << ", value: " << value << endl;
        return;
    }

    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );

    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        mMarkets[ marketNumber ]->getMarket( per )->addToSupply( mIsDerivativeCalc ? value.getDiff() : value.get() );
    }
    else if( aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Cannot add to supply for market as it does not exist: " << goodName << " " 
            << regionName << endl;
    }
}

/*! \brief Add to the demand for this market.
*
* This function increments the demand for a market determined by the goodName and regionName
* by a given value. This function is used throughout the model to add demand to markets. 
* In order to add to demand a caller must provide a "state" backed Value class this is
* due to when calculating partial derivatives we are interesed in adding the difference
* from the current value and the "base" value to demand.
*
* \param goodName Name of the good for which to add demand.
* \param regionName Name of the region in which demand should be added for the market.
* \param value Amount of demand to add.
* \param per Period in which to add demand.
* \param aMustExist Whether it is an error for the market not to exist.
*/
void Marketplace::addToDemand( const string& goodName, const string& regionName, const Value& value,
                               const int per, bool aMustExist )
{
    // Print a warning message when adding infinity values to the demand
    if ( !util::isValidNumber( value ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Error adding to demand in marketplace for: " << goodName << ", region: " << regionName << ", value: " << value << endl;
        return;
    }

    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        mMarkets[ marketNumber ]->getMarket( per )->addToDemand( mIsDerivativeCalc ? value.getDiff() : value.get() );
    }
    else if( aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Cannot add to demand for market as it does not exist: " << goodName << " " 
            << regionName << endl;
    }
}

/*! \brief Return the market price. 
*
* This function uses a market type dependent function to find the price for a market determined by
* the goodName and regionName.  This price is not always the raw or true price. For non-existant
* markets, this function returns a near infinite price, except for renewable
* markets that do not exist, for which it returns 0.
*
* \param goodName The good for which a price is needed.
* \param regionName The region for which a price is needed.
* \param per The period to return the market price for.
* \param aMustExist Whether it is an error for the market not to exist.
* \return The market price.
*/  
double Marketplace::getPrice( const string& goodName, const string& regionName, const int per,
                             bool aMustExist ) const {
    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );
    
    if( marketNumber != MarketLocator::MARKET_NOT_FOUND ){
        return mMarkets[ marketNumber ]->getMarket( per )->getPrice();
    }

    if( aMustExist ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Called for price of non-existant market " << goodName << " in region " 
            << regionName << endl;
    }
    return NO_MARKET_PRICE;
}

/*! \brief Return the market supply. 
*
* This function uses a market type dependent function to find the supply for a market determined
* by the goodName and regionName.  This supply is not always the raw or true supply.
* For non-existant markets, this function returns 0.
*
* \param goodName The good for which a supply is needed.
* \param regionName The region for which a supply is needed.
* \param per Period to get the supply for.
* \return The market supply.
*/
double Marketplace::getSupply( const string& goodName, const string& regionName, const int per ) const {
    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );

    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        return mMarkets[ marketNumber ]->getMarket( per )->getSupply();
    }

    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Called for supply of non-existant market " << goodName << " in " << regionName << endl;
    return 0;
}

/*! \brief Return the market demand. 
*
* This function uses a market type dependent function to find the demand for a market determined
* by the goodName and regionName.  This demand is not always the raw or true demand.
* For non-existant markets, this function returns 0.
*
* \param goodName The good for which a demand is needed.
* \param regionName The region for which a demand is needed.
* \param per The period to return the market demand for.
* \return The market demand.
*/
double Marketplace::getDemand(  const string& goodName, const string& regionName, const int per ) const {
    const int marketNumber = mMarketLocator->getMarketNumber( regionName, goodName );

    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        return mMarkets[ marketNumber ]->getMarket( per )->getDemand();
    }

    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << "Called for demand of non-existant market " << goodName << " in " << regionName << endl;
    return 0;
}

//! Returns a set of pointers to each market for the period.
vector<Market*> Marketplace::getMarketsToSolve( const int period ) const {
    vector<Market*> toSolve;
    
    // Loop through the markets and add all markets.
    for( unsigned int i = 0; i < mMarkets.size(); ++i ){
        toSolve.push_back( mMarkets[ i ]->getMarket( period ) );
    }
    return toSolve;
}

/*!
 * \brief Conditionally initializes the market prices to the market prices from
 *        the previous period.
 * \details Keep existing prices for the calibration years as some of those prices
 *          are calibrated prices and should not be reset. After calibration we
 *          attempt to use the trend in prices to forecast prices ( and demands )
 *          to come up with a good guess that would be closer to the solution.
 *          This only occurs for periods greater than 0.
 * \author Sonny Kim
 * \param period Period for which to initialize prices.
 */
void Marketplace::init_to_last( const int period ) { 
    // Get the last period to allow using parsed prices, which is the
    // final calibration period.
    const int finalCalPeriod = scenario->getModeltime()->getFinalCalibrationPeriod();

    if( period == 0 ) {
        for( unsigned i = 0; i < mMarkets.size(); ++i ) {
            mMarkets[ i ]->getMarket( period )->setForecastPrice( 1.0 );
            mMarkets[ i ]->getMarket( period )->setForecastDemand( 1.0 );
        }
    }
    else if ( period > 0 && period <= finalCalPeriod ) {
        for ( unsigned int i = 0; i < mMarkets.size(); i++ ) {
            double forecastedPrice = mMarkets[ i ]->forecastPrice( period );
            mMarkets[ i ]->getMarket( period )->set_price_to_last_if_default( forecastedPrice );
            mMarkets[ i ]->forecastDemand( period );
        }
    }
    else {
        for ( unsigned int i = 0; i < mMarkets.size(); i++ ) {
            double forecastedPrice = mMarkets[ i ]->forecastPrice( period );
            double lastPeriodPrice = mMarkets[ i ]->getMarket( period - 1 )->getPrice();
            // Only use the forecast price if it is reliable.
            if( (forecastedPrice < 0.0 && lastPeriodPrice > 0.0) ||
                abs( forecastedPrice ) > 5.0 * abs( lastPeriodPrice ) )
            {
                mMarkets[ i ]->getMarket( period )->set_price_to_last( lastPeriodPrice );
            }
            else {
                mMarkets[ i ]->getMarket( period )->set_price_to_last( forecastedPrice );
            }
            // forecast function stores demand forecast in the market.
            // We don't need to do anything further with it here.
            mMarkets[ i ]->forecastDemand( period );
        }
    }
}

/*! \brief Store market prices for policy cost caluclation.
*
*
* \author Sonny Kim
*/
void Marketplace::store_prices_for_cost_calculation()
{
    for ( unsigned int i = 0; i < mMarkets.size(); ++i ){
        for( unsigned int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ){
            mMarkets[ i ]->getMarket( period )->store_original_price();
        }
    }
}

/*! \brief Restore market prices for policy cost caluclation.
*
*
* \author Sonny Kim
*/
void Marketplace::restore_prices_for_cost_calculation()
{
    for ( unsigned int i = 0; i < mMarkets.size(); ++i ){
        for( unsigned int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ){
            mMarkets[ i ]->getMarket( period )->restore_original_price();
        }
    }
}

/*! \brief Get the information object for the specified market and period which
*          can then be used to query for specific values.
* \details Returns the internal IInfo object of the specified market and period
*          which represents a set of pairings of information name to value. This
*          specific function returns a constant pointer to the information
*          object, so values can be queried but not added or modified.
* \author Josh Lurz
* \warning Null will be returned if the market does not exist.
* \warning The function is UNRELATED to storeinfo and restoreinfo.
* \param aGoodName The good of the market for which to get the information object.
* \param aRegionName The region used to find the market from which to get the
*        information object.
* \param aPeriod The period to fetch for which the information object. 
* \param aMustExist Whether it is an error for the market not to exist.
* \return A constant pointer to the market information object, null if the
*         market does not exist.
* \note This version of the function is required so that it can be called in
*       constant functions. A second version is available which returns a
*       mutable pointer.
*/
const IInfo* Marketplace::getMarketInfo( const string& aGoodName, const string& aRegionName,
                                         const int aPeriod, const bool aMustExist ) const 
{
    const int marketNumber = mMarketLocator->getMarketNumber( aRegionName, aGoodName );
    const IInfo* info = 0;
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        info = mMarkets[ marketNumber ]->getMarket( aPeriod )->getMarketInfo();
        /*! \invariant The market is required to return an information object
        *              that is non-null. 
        */
        assert( info );
    }
    
    // Report the error if requested.
    if( !info && aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market info object cannot be returned because market "
                << aGoodName << " in " << aRegionName << " does not exist." << endl;
    }
    return info;
}

/*! \brief Get the information object for the specified market and period which
*          can then be used to query, add, or modify specific values.
* \details Returns the internal IInfo object of the specified market and period
*          which represents a set of pairings of information name to value. This
*          specific function returns a mutable pointer to the information
*          object, so values can be queried, added or modified.
* \author Josh Lurz
* \warning Null will be returned if the market does not exist.
* \warning The function is UNRELATED to storeinfo and restoreinfo.
* \param aGoodName The good of the market for which to get the information
*        object.
* \param aRegionName The region used to find the market from which to get the
*        information object.
* \param aPeriod The period to fetch for which the information object. 
* \param aMustExist Whether it is an error for the market not to exist.
* \return A mutable pointer to the market information object, null if the market
*         does not exist.
* \note This function returns a mutable pointer to the information object so it
*       cannot be called from constant function.
*/
IInfo* Marketplace::getMarketInfo( const string& aGoodName, const string& aRegionName,
                                   const int aPeriod, const bool aMustExist )
{
    const int marketNumber = mMarketLocator->getMarketNumber( aRegionName, aGoodName );
    IInfo* info = 0;
    if ( marketNumber != MarketLocator::MARKET_NOT_FOUND ) {
        info = mMarkets[ marketNumber ]->getMarket( aPeriod )->getMarketInfo();
        /*! \invariant The market is required to return an information object
        *              that is non-null. 
        */
        assert( info );
    }
    
    // Report the error if requested.
    if( !info && aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Market info object cannot be returned because market " 
                << aGoodName << " in " << aRegionName << " does not exist." << endl;
    }
    return info;
}

/*!
 * \brief Locate the market for the given good, region, and period.
 * \details Returns a CachedMarket that wraps the found Market object so that
 *          only functionality provided through the marketplace will be available.
 *          Note that it is not an error to locate a market which does not exist
 *          and calls to methods of a CachedMarket which did not exist will have the
 *          same behavior as the equivalent method in this class.
 * \param aGoodName The good of the market to locate.
 * \param aRegionName The region of the market to locate.
 * \param aPeriod The period for which to locate.
 * \return A CachedMarket object which wraps the requested market.  This will always
 *         be a valid object regardless of if the market was not found.
 * \see CachedMarket
 */
unique_ptr<CachedMarket> Marketplace::locateMarket( const string& aGoodName, const string& aRegionName,
                                                  const int aPeriod ) const
{
    const int marketNumber = mMarketLocator->getMarketNumber( aRegionName, aGoodName );
    unique_ptr<CachedMarket> locatedMarket( new CachedMarket( aGoodName, aRegionName, aPeriod,
                                                            marketNumber != MarketLocator::MARKET_NOT_FOUND ?
                                                            mMarkets[ marketNumber ]->getMarket( aPeriod ) : 0 ) );
    return locatedMarket;
}

/*! \brief Update an output container for the Marketplace.
* \param aVisitor Output container to update.
* \param aPeriod Period to update.
*/
void Marketplace::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitMarketplace( this, aPeriod );

    // Update from the markets.
    for( unsigned int i = 0; i < mMarkets.size(); i++ ){
        // If the period is -1 this means to update all periods.
        if( aPeriod == -1 ){
            for( unsigned int j = 0; j < mMarkets[ i ]->size(); ++j ){
                mMarkets[ i ]->getMarket( j )->accept( aVisitor, aPeriod );
            }
        }
        // Otherwise only update for the current period.
        else {
            mMarkets[ i ]->getMarket( aPeriod )->accept( aVisitor, aPeriod );
        }
    }

    aVisitor->endVisitMarketplace( this, aPeriod );
}

/*!
 * \brief Get the market based dependency finder.
 * \details All sectors should register their dependencies with this object to 
 *          ensure a proper global ordering.
 * \return The market dependency finder.
 */
MarketDependencyFinder* Marketplace::getDependencyFinder() const {
    return mDependencyFinder.get();
}

/*!
 * \brief Get the full state of the marketplace.
 * \param period The model period.
 * \return A vector in strides of 3 which include price, demand, and supply for
 *         all markets.
 */
std::vector<double> Marketplace::fullstate( int period ) const
{
  std::vector<double> state;
  for(unsigned i=0; i<mMarkets.size(); ++i) {
    state.push_back(mMarkets[i]->getMarket( period )->getRawPrice());
    state.push_back(mMarkets[i]->getMarket( period )->getRawDemand());
    state.push_back(mMarkets[i]->getMarket( period )->getRawSupply());
  }
  return state; 
}

//! Check the input vector (previously returned from fullstate)
//! against the current market state. 
//! \details Errors will be logged to the ostream, if it is provided.
//! tol indicates how loose the comparison should be.  The default is
//! 0ulp, meaning that all values should be bitwise identical.  This
//! is appropriate when testing whether a "restore" operation restores
//! correctly.  When testing calculated values, you should use a
//! looser tolerance.
bool Marketplace::checkstate(int period, const std::vector<double> &ostate, std::ostream *log, unsigned tol) const
{
  bool ok = true;
  std::vector<double> cstate(fullstate( period ));
  for(unsigned i=0; i<ostate.size(); ++i) {
      if(!dblcmp(ostate[i],cstate[i], tol)) {
      ok = false;
      if( log ) {
        int imkt = i/3;
        int j = imkt*3;         // index of price for this market
                                // (irrespective of whether it was p,
                                // s, or d that triggered the
                                // discrepancy)
        (*log) << "Market discrepancy: " << mMarkets[imkt]->getName()
               << "\nPrice:  " << ostate[j] << "\t" << cstate[j];
        (*log) << "\nDemand: " << ostate[j+1] << "\t" << cstate[j+1];
        (*log) << "\nSupply: " << ostate[j+2] << "\t" << cstate[j+2] << "\n";
      }
    }
  }
  return ok;
}

/*!
 * \brief Print a table of the market state.
 * \details Useful for debugging.
 * \param period The model period.
 * \param out The output string to print the state.
 */
void Marketplace::prnmktbl(int period, std::ostream &out) const
{
  out << "Market State\ni\tName\tPrice\tSupply\tDemand\n";
  for(unsigned i=0; i<mMarkets.size(); ++i) {
    out << i << "\t" << mMarkets[i]->getName() << "\t"
        << mMarkets[i]->getMarket( period )->getRawPrice() << "\t" << mMarkets[i]->getMarket( period )->getRawSupply()
        << "\t" << mMarkets[i]->getMarket( period )->getRawDemand() << "\n";
  }
  out << "\n";

}

/*!
 * \brief Log the forecast and actual prices for this period
 */
void Marketplace::logForecastEvaluation( int aPeriod ) const
{
    const double smallval=0.1;  // for cases where the actual price was nearly zero
    double ld2=0.0, fd2=0.0;    // sum of squared differences for last val and forecast val
    double ldmax=-1.0, fdmax=-1.0; // maximum difference
    int limax=0, fimax=0;          // location of maximum differences
    double missmax=0.0;            // worst "miss"
    int mimax=0;                   // location of worst miss
    int nmiss=0, nhit=0;
    using std::setw;
    ILogger &solverlog = ILogger::getLogger("solver_log");
    solverlog.setLevel(ILogger::DEBUG);

    solverlog << "\nPeriod " << aPeriod << " price forecasts and results\n"
              << "last period\tforecast   \tthis period\tlast diff  \tfcst diff  \n"; 
    for(unsigned i=0; i<mMarkets.size(); ++i) {
        double lprice = mMarkets[i]->getMarket( aPeriod-1 )->getRawPrice();
        double fcst   = mMarkets[i]->getMarket( aPeriod )->getForecastPrice();
        double cprice = mMarkets[i]->getMarket( aPeriod )->getRawPrice();
        double ldiff = (lprice-cprice)/(fabs(cprice)+smallval);
        double fdiff = (fcst-cprice)/(fabs(cprice)+smallval);
        char marker = fabs(fdiff)<=fabs(ldiff) ? '+' : ' ';
        if(fdiff > 0.1) {
            marker = '!';
        }

        ld2 += ldiff*ldiff;
        fd2 += fdiff*fdiff;

        if(mMarkets[i]->getMarket( aPeriod )->shouldSolve()) {
            // only compute statistics on solvable markets
            if(fabs( ldiff ) > ldmax) {
                ldmax = fabs( ldiff );
                limax = i;
            }
            if(fabs( fdiff ) > fdmax) {
                fdmax = fabs( fdiff );
                fimax = i;
            }

            if(fabs( fdiff ) > fabs( ldiff )) {
                // this forecast was a miss.  Magnitude is the difference
                // in delta-price divided by the delta-price for the
                // persistence forecast
                double miss=(fabs(fcst-cprice)-fabs(lprice-cprice)) / (fabs(lprice-cprice)+smallval);
                if(miss>missmax) {
                    missmax=miss;
                    mimax = i;
                }
                nmiss++;
            }
            else {
                nhit++;
            }
        }
        
        solverlog << setw( 11 ) << lprice << "\t"
                  << setw( 11 ) << fcst << "\t"
                  << setw( 11 ) << cprice << "\t"
                  << setw( 11 ) << ldiff << "\t"
                  << setw( 11 ) << fdiff << "\t"
                  << marker << "  > " // angle marker makes it easier to grep for these lines in the log.
                  << mMarkets[i]->getName() << "\n";
    }

    double fac= (nhit+nmiss > 0) ? 1.0/(nhit+nmiss) : 1.0;
    
    solverlog << "\nhit %= " << fac*nhit << "  miss %= " << fac*nmiss
              << "\nworst miss= " << missmax << " in market= "
              << mMarkets[mimax]->getName();
    solverlog << "\nMax ldiff= " << ldmax << " in market= "
              << mMarkets[limax]->getName();
    solverlog << "\nMax fdiff= " << fdmax << " in market= "
              << mMarkets[fimax]->getName();
    solverlog << "\nRMS initial guess differences:";
    solverlog << "\nlast:     \t" << sqrt(fac*ld2)
              << "\nforecast: \t" << sqrt(fac*fd2)
              << "\n\n";


    // Same thing for demand.  Should probably refactor all this crap.  Later.
    nmiss = nhit;
    mimax = 0;
    missmax = 0.0; 
    solverlog << "\nPeriod " << aPeriod << " demand forecasts and results\n"
              << "last period\tforecast   \tthis period\tlast diff  \tfcst diff  \n";
    for(unsigned i=0; i<mMarkets.size(); ++i) {
        // take absolute value of actuals, since we are only trying to
        // forecast magnitude of demand, not actual value.  Also,
        // values < 1 are forced to 1.
        double ldemand      = std::max(fabs(mMarkets[i]->getMarket( aPeriod-1 )->getSolverDemand()), 1.0);
        double fcstdemand   = mMarkets[i]->getMarket( aPeriod )->getForecastDemand();
        double cdemand      = std::max(fabs(mMarkets[i]->getMarket( aPeriod )->getSolverDemand()), 1.0);
        double lddiff       = (ldemand-cdemand)/(fabs(cdemand)+smallval);
        double fddiff       = (fcstdemand-cdemand)/(fabs(cdemand)+smallval);
        char marker = fabs(fddiff)<=fabs(lddiff) ? '+' : ' ';
        if(fddiff > 0.1) {
            marker = '!';
        }

        ld2 += lddiff*lddiff;
        fd2 += fddiff*fddiff;

        
        if(mMarkets[i]->getMarket( aPeriod )->shouldSolve()) {
            // only compute statistics on solvable markets
            if(fabs( lddiff ) > ldmax) {
                ldmax = fabs( lddiff );
                limax = i;
            }
            if(fabs( fddiff ) > fdmax) {
                fdmax = fabs( fddiff );
                fimax = i;
            }

            if(fabs( fddiff ) > fabs( lddiff )) {
                // this forecast was a miss.  Magnitude is the difference
                // in delta-price divided by the delta-price for the
                // persistence forecast
                double miss=(fabs(fcstdemand-cdemand)-fabs(ldemand-cdemand)) / (fabs(ldemand-cdemand)+smallval);
                if(miss>missmax) {
                    missmax=miss;
                    mimax = i;
                }
                nmiss++;
            }
            else {
                nhit++;
            }
        }
        
        solverlog << setw( 11 ) << ldemand << "\t"
                  << setw( 11 ) << fcstdemand << "\t"
                  << setw( 11 ) << cdemand << "\t"
                  << setw( 11 ) << lddiff << "\t"
                  << setw( 11 ) << fddiff << "\t"
                  << marker << "  > " // angle marker makes it easier to grep for these lines in the log.
                  << mMarkets[i]->getName() << "\n";
    }

    fac= (nhit+nmiss > 0) ? 1.0/(nhit+nmiss) : 1.0;
    
    solverlog << "\nhit %= " << fac*nhit << "  miss %= " << fac*nmiss
              << "\nworst miss= " << missmax << " in market= "
              << mMarkets[mimax]->getName();
    solverlog << "\nMax lddiff= " << ldmax << " in market= "
              << mMarkets[limax]->getName();
    solverlog << "\nMax fddiff= " << fdmax << " in market= "
              << mMarkets[fimax]->getName();
    solverlog << "\nRMS initial guess differences:";
    solverlog << "\nlast:     \t" << sqrt(fac*ld2)
              << "\nforecast: \t" << sqrt(fac*fd2)
              << "\n\n"; 
}
