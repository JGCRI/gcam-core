/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
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
* \file ag_supply_sector.cpp
* \ingroup Objects
* \brief AgSupplySector class source file.
* \author Marshall Wise, Kate Calvin
*/

#include "util/base/include/definitions.h"
#include <string>
#include "util/base/include/xml_helper.h"

#include "sectors/include/ag_supply_sector.h"
#include "sectors/include/ag_supply_subsector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
AgSupplySector::AgSupplySector(): SupplySector(),
   mCalPrice( -1.0 ),
   mSubsidy( 0.0 ),
   // The default is to allow very negative profit rates, which implies to
   // never give a subsidy.
   mCalMinProfitRate( -util::getLargeNumber() )
{
}

//! Default destructor
AgSupplySector::~AgSupplySector( ) {
}

void AgSupplySector::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    SupplySector::toDebugXMLDerived( period, out, tabs );
    XMLWriteElement( scenario->getMarketplace()->getPrice( mName, mRegionName, 1, true ), "calPrice", out, tabs );
    XMLWriteElement( mCalMinProfitRate, "cal-min-profit-rate", out, tabs );
    XMLWriteElement( mMarketName, "market", out, tabs );
}

/*! \brief Perform any initializations specific to this sector
* \author James Blackwood
*/
void AgSupplySector::completeInit( const IInfo* aRegionInfo,
                                   ILandAllocator* aLandAllocator )
{
    if ( !aLandAllocator ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "LandAllocator not read in." << endl;
    }

    if( mMarketName.empty() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Market name for sector " << mName << " was not set. Defaulting to regional market." << endl;
        mMarketName = mRegionName;
    }
	
    SupplySector::completeInit( aRegionInfo, aLandAllocator );
	
	// Store subsidies in the marketplace so technology has access to them.
	Marketplace* marketplace = scenario->getMarketplace();
	const Modeltime* modeltime = scenario->getModeltime();
	for( int per = 0; per < modeltime->getmaxper(); ++per ){
		marketplace->getMarketInfo( mName, mRegionName, per, true )->setDouble( mRegionName + "subsidy", mSubsidy[ per ] );
	}
    
    // make available the minimum calibration profit rate to the technology through the info object
    mSectorInfo->setDouble( "cal-min-profit-rate", mCalMinProfitRate );
}

/*! \brief Calculate the sector price.
* \details AgSupplySectors are solved markets, so this function is overridden
*          to use the market price instead of an average subsector price.
* \param aGDP Regional GDP container.
* \param aPeriod model period.
* \return The sector price.
*/
double AgSupplySector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    return scenario->getMarketplace()->getPrice( mName, mRegionName, aPeriod, true );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& AgSupplySector::getXMLName() const {
    return getXMLNameStatic();
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
const string& AgSupplySector::getXMLNameStatic() {
    const static string XML_NAME = "AgSupplySector";
    return XML_NAME;
}

void AgSupplySector::supply( const GDP* aGDP, const int aPeriod ) {
    // The demand value passed to setOutput does not matter as the 
    // supply and demand will be made equal by the market.
	/* for agSupplySectors, output is summed from technology output rather than shared
	  like it is for default GCAM sector */
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        // set subsector output from Sector demand
        mSubsectors[ i ]->setOutput( 1, 1, aGDP, aPeriod );
    }  
}

//! Create markets
void AgSupplySector::setMarket() {
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();

    /* Since agSupplySectors are solved, they can be in multiregional markets */
    if ( marketplace->createMarket( mRegionName, mMarketName, mName, IMarketType::NORMAL ) ) {
        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( mName, mRegionName, 0, true );
        marketInfo->setString( "price-unit", mPriceUnit );
        marketInfo->setString( "output-unit", mOutputUnit );

        // Set market prices to initial price vector
        marketplace->setPriceVector( mName, mRegionName, mPrice );
        // Reset base period price to mCalPrice
        marketplace->setPrice( mName, mRegionName, mCalPrice, 0, true );

        for( int per = 1; per < modeltime->getmaxper(); ++per ){
            marketplace->setMarketToSolve( mName, mRegionName, per );
        }
        // Don't set calPrice or indicate the market is fully calibrated if mCalPrice is not valid.
        if ( mCalPrice > 0 ) {
            for( int per = 0; per < modeltime->getmaxper(); ++per ){
                IInfo* marketInfo = marketplace->getMarketInfo( mName, mRegionName, per, true );
                marketInfo->setDouble( "calPrice", mCalPrice );
                marketInfo->setBoolean( "fully-calibrated", true );
            }
        }
    }
}


