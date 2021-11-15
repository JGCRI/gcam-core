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
* \file supply_sector.cpp
* \ingroup Objects
* \brief SupplySector class source file.
* \author James Blackwood, Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include "util/base/include/xml_helper.h"
#include "sectors/include/supply_sector.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/subsector.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/imarket_type.h"
#include "util/base/include/configuration.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

/* \brief Constructor
*/
SupplySector::SupplySector():
Sector()
{
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& SupplySector::getXMLName() const {
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
const std::string& SupplySector::getXMLNameStatic() {
    static const string XML_NAME = "supplysector";
    return XML_NAME;
}

/*! \brief XML debugging output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML.
* This function is called by toDebugXML in the base Sector class.
*
* \author Steve Smith, Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void SupplySector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
}

/*! \brief Complete the initialization of the supply sector.
* \param aRegionInfo Regional information object.
* \param ILandAllocator Regional land allocator.
*/
void SupplySector::completeInit( const IInfo* aRegionInfo,
                                 ILandAllocator* aLandAllocator )
{
	// default unit to EJ
	if ( mOutputUnit.empty() ) {
		mOutputUnit = "EJ"; 
	}
	// default unit to EJ
	if ( mInputUnit.empty() ) {
		mInputUnit = "EJ"; 
	}
	// default unit to $/GJ
	if ( mPriceUnit.empty() ) {
		mPriceUnit = "75$/GJ"; 
	}
	Sector::completeInit( aRegionInfo, aLandAllocator );	
    setMarket();
}

/*! \brief Create new market for this Sector
*
* Sets up the appropriate market within the marketplace for this Sector. Note that the type of market is NORMAL -- 
* signifying that this market is a normal market that is solved (if necessary).
*
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
void SupplySector::setMarket() {    
    Marketplace* marketplace = scenario->getMarketplace();
    // Creates a regional market. MiniCAM supply sectors are not independent and 
    // cannot be members of multi-region markets.
    if( marketplace->createMarket( mRegionName, mRegionName, mName, IMarketType::NORMAL ) ) {
        // Initialize prices for markets
        marketplace->setPriceVector( mName, mRegionName, mPrice );

        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( mName, mRegionName, 0, true );
        marketInfo->setString( "price-unit", mPriceUnit );
        marketInfo->setString( "output-unit", mOutputUnit );
    }
}

/*! \brief Initialize the SupplySector.
* \details Currently only calls the base class initCalc.
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the SupplySector.
*/
void SupplySector::initCalc( NationalAccount* aNationalAccount,
                            const Demographic* aDemographics,
                            const int aPeriod )
{
    Sector::initCalc( aNationalAccount, aDemographics, aPeriod );
}

/*! \brief returns Sector output.
*
* Returns the total amount of the SupplySector. 
*
* \author Sonny Kim
* \param period Model period
* \todo make year 1975 regular model year so that logic below can be removed
* \return total output
*/
double SupplySector::getOutput( const int aPeriod ) const {
    double output = 0;
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ) {
        double subsecOutput = mSubsectors[ i ]->getOutput( aPeriod );
        // error check.
        if ( !util::isValidNumber( subsecOutput ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Output for subsector " << mSubsectors[ i ]->getName() << " in Sector " << mName
                << " in region " << mRegionName <<" is not valid." << endl;
            continue;
        }
        output += subsecOutput;
    }

    return output;
}

/*! \brief Return the price of the SupplySector.
* \details The price of a SupplySector is the weighted average subsector price.
* \param aPeriod Model period.
* \return Price.
* \todo Move entire calculation here once demand sectors are rewritten.
*/
double SupplySector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    return Sector::getPrice( aGDP, aPeriod );
}

/*! \brief Calculate the final supply price.
* \details Calculates shares for the sector and price for the supply sector, and
*          then sets the price of the good into the marketplace.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void SupplySector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
    // Instruct all subsectors to calculate their costs. This must be done
    // before prices can be calculated.
    calcCosts( aPeriod );

    // Set the price into the market.
    Marketplace* marketplace = scenario->getMarketplace();

    double avgMarginalPrice = getPrice( aGDP, aPeriod );

    marketplace->setPrice( mName, mRegionName, avgMarginalPrice, aPeriod, true );
}

/*! \brief Set supply Sector output
* \details This routine takes the market demand and propagates that through the
*          supply subsectors where it is shared out (and subsequently passed to
*          the technology level within each sub-Sector to be shared out).
* \author Sonny Kim
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period
*/
void SupplySector::supply( const GDP* aGDP, const int aPeriod ) {
	Marketplace* marketplace = scenario->getMarketplace();
	// demand for the good produced by this Sector
	double marketDemand = max( marketplace->getDemand( mName, mRegionName, aPeriod ), 0.0 );

	// Determine if fixed output must be scaled because fixed supply
	// exceeded demand.
	double fixedOutput = getFixedOutput( aPeriod );
	double scaleFactor = SectorUtils::calcFixedOutputScaleFactor( marketDemand, fixedOutput );

	// Calculate the demand for new investment.
	double newInvestment = max( marketDemand - fixedOutput, 0.0 );
	const vector<double>& subsecShares = calcSubsectorShares( aGDP, aPeriod );

	// This is where subsector and technology outputs are set
	for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
		// set subsector output from Sector demand
		mSubsectors[ i ]->setOutput( subsecShares[ i ] * newInvestment, scaleFactor, aGDP, aPeriod );
	}

	const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
	if ( debugChecking ) {
		// If the model is working correctly this should never give an error
		// An error here means that the supply summed up from the supply sectors 
		// is not equal to the demand that was passed in 
		double mrksupply = getOutput( aPeriod );

		// if demand identically = 1 then must be in initial iteration so is not an error
		if ( aPeriod > 0 && fabs(mrksupply - marketDemand ) > 0.01 && marketDemand != 1 ) {
			ILogger& mainLog = ILogger::getLogger( "main_log" );
			mainLog.setLevel( ILogger::WARNING );
			mainLog << mRegionName << " Market "<<  mName << " demand and derived supply are not equal by: ";
			mainLog << fabs( mrksupply - marketDemand ) << ": ";
			mainLog << "S: " << mrksupply << " D: " << marketDemand << " Fixed-Supply: " << getFixedOutput( aPeriod ) << endl;
		}
	}
}

/*!
 * \brief Get the energy input for the SupplySector.
 * \todo If there is a DemandSupplySector, move this there.
 * \param aPeriod Period.
 * \return Total energy input.
 */
double SupplySector::getEnergyInput( const int aPeriod ) const {
    double totalEnergy = 0;
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        totalEnergy += mSubsectors[ i ]->getEnergyInput( aPeriod );
    }
    return totalEnergy;
}
