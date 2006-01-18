/*! 
* \file supply_sector.cpp
* \ingroup Objects
* \brief SupplySector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"
#include "sectors/include/supply_sector.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/subsector.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/imarket_type.h"
#include "util/base/include/configuration.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string SupplySector::XML_NAME = "supplysector";

/* \brief Constructor
* \param aRegionName The name of the region.
*/
SupplySector::SupplySector( const string& aRegionName ) : Sector ( aRegionName ) {
}

/*! \brief Initialize the SupplySector.
* \details Currently only calls the base class initCalc.
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the SupplySector.
*/
void SupplySector::initCalc( NationalAccount& aNationalAccount,
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
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        double subsecOutput = subsec[ i ]->getOutput( aPeriod );
        // error check.
        if ( !util::isValidNumber( subsecOutput ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Output for subsector " << subsec[ i ]->getName() << " in Sector " << name 
                    << " in region " << regionName <<" is not valid." << endl;
            continue;
        }
        output += subsecOutput;
    }
    
    // In the base period return a read in output if there is none.
    if( aPeriod == 0 && output == 0 ){
        return mBaseOutput;
    }
    return output;
}

/*! \brief Return the price of the SupplySector.
* \details The price of a SupplySector is the weighted average subsector price.
* \param aPeriod Model period.
* \return Price.
* \todo Move entire calculation here once demand sectors are rewritten.
*/
double SupplySector::getPrice( const int aPeriod ) const {
    return Sector::getPrice( aPeriod );
}

/*! \brief Calculate the final supply price.
* \details Calculates shares for the sector and price for the supply sector, and then sets the price of the good
*          into the marketplace.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void SupplySector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
    calcShare( aPeriod, aGDP );
	
	// Set the price into the market.
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->setPrice( name, regionName, getPrice( aPeriod ), aPeriod, true );
}

/*! \brief Set supply Sector output
* \details This routine takes the market demand and propagates that through the
*          supply sub-sectors where it is shared out (and subsequently passed to
*          the technology level within each sub-Sector to be shared out).
*          Routine also calls adjustForFixedOutput which adjusts shares, if
*          necessary, for any fixed output sub-sectors.
* \author Sonny Kim
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period
*/
void SupplySector::supply( const GDP* aGDP, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();

	 // demand for the good produced by this Sector
    double marketDemand = marketplace->getDemand( name, regionName, aPeriod );

    if ( marketDemand < 0 ) {
		ILogger& mainLog = ILogger::getLogger( "main_log" );
		mainLog.setLevel( ILogger::ERROR );
		mainLog << "Demand value < 0 for good " << name << " in region " << regionName << endl;
		marketDemand = 0;
    }

    // Adjust shares for fixed supply
    if ( anyFixedCapacity ) {
        adjustForFixedOutput( marketDemand, aPeriod );
    }

    // This is where subsector and technology outputs are set
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // set subsector output from Sector demand
        subsec[ i ]->setOutput( marketDemand, aGDP, aPeriod );
    }    
    
    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    if ( debugChecking ) {
        // If the model is working correctly this should never give an error
        // An error here means that the supply summed up from the supply sectors 
        // is not equal to the demand that was passed in 
        double marketSupply = getOutput( aPeriod );

        // if demand identically = 1 then must be in initial iteration so is not an error
        if ( aPeriod > 0 && fabs( marketSupply - marketDemand ) > 0.01 && marketDemand != 1 ) {
			ILogger& mainLog = ILogger::getLogger( "main_log" );
			mainLog.setLevel( ILogger::WARNING );
            mainLog << regionName << " Market " <<  name << " demand and derived supply are not equal by: "
				    << fabs( marketSupply - marketDemand ) << ": "
					<< "S: " << marketSupply << "  D: " << marketDemand << endl;
        }
    }
}

/*! \brief Complete the initialization of the supply sector.
* \param aRegionInfo Regional information object.
* \param aDependencyFinder Regional dependency finder.
*/
void SupplySector::completeInit( const IInfo* aRegionInfo,
                                 DependencyFinder* aDependencyFinder,
                                 ILandAllocator* aLandAllocator )
{
    Sector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator );
    setMarket();
}

/*! \brief Set the calibrated supply info into the marketplace.
* \details This routine adds calibrated supply and demand values to the
*          associated marketInfo variables. Values are added only if all outputs
*          for that variable are fixed or calibrated.
* \todo -- Steve to fix potential inconsistency with global markets.
* \param aPeriod Period to add the information for.
*/
void SupplySector::setCalibratedSupplyInfo( const int aPeriod ) const {
    const double MKT_NOT_ALL_FIXED = -1;
    Marketplace* marketplace = scenario->getMarketplace();
	IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( name, regionName, aPeriod, true );
	
	/*! \invariant Supply sector should always have an associated market and
    *              market info. 
    */
	assert( marketInfo );
    
    // If this market is global, the second check is to see if some other region
    // has flaged this as all inputs not fixed.
    if ( outputsAllFixed( aPeriod ) && marketInfo->getDouble( "calSupply", false ) != MKT_NOT_ALL_FIXED ) {
        // If supply of this good has not been elimiated from the search and
        // output is fixed then add to fixed supply value.
        double calSupply = getCalOutput( aPeriod );  // total calibrated output
        calSupply += getFixedOutput( aPeriod );  // total fixed output

		double existingCalSupply = max( marketInfo->getDouble( "calSupply", false ), 0.0 );
		marketInfo->setDouble( "calSupply", existingCalSupply + calSupply );
    } 
    else {
        // If supply of this good is not fixed then set flag to eliminate this from other searches
        marketInfo->setDouble( "calSupply", MKT_NOT_ALL_FIXED );
    }
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& SupplySector::getXMLName() const {
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
const std::string& SupplySector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool SupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
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
    if( marketplace->createMarket( regionName, regionName, name, IMarketType::NORMAL ) ) {
		// Initilize base year price
        marketplace->setPrice( name, regionName, mBasePrice, true );
    }

    // Check if this sector has any fixed capacity. If it does, add a price
    // market because resolution of fixed capacity requires a trial value for
    // demand. When markets become dynamic this can be optimized by moving it to
    // initCalc.
	for( int per = 0; per < scenario->getModeltime()->getmaxper(); ++per ){
		if ( getFixedOutput( per ) > 0 ) {
			marketplace->resetToPriceMarket( name, regionName );
			// Resetting a market to price markets applies to all periods, so
            // quit the search for fixed output.
			break;
		} 
	}
}

/*! \brief Adjust shares to be consistant with fixed supply
* \details This routine determines the total amount of fixed supply in this
*          Sector and adjusts other shares to be consistant with the fixed
*          supply. If fixed supply exceeds demand then the fixed supply is
*          reduced. An internal variable with the Sector share of fixed supply
*          for each sub-Sector is set so that this information is available to
*          other routines.
* \author Steve Smith
* \param aMarketDemand demand for the good produced by this Sector
* \param aPeriod Model period
* \warning fixed supply must be > 0 (to obtain 0 supply, set share weight to
*          zero)
*/
void SupplySector::adjustForFixedOutput( const double aMarketDemand, const int aPeriod ) {
    // set output from technologies that have fixed outputs such as hydro electricity
    // Determine total fixed production and total var shares
    // Need to change the exog_supply function once new, general fixed supply method is available
    double totalfixedOutput = 0;
	double variableShares = 0; // original sum of shares of non-fixed subsectors   

    for( unsigned int i = 0; i < subsec.size(); ++i ){
        double fixedOutput = 0;
        subsec[ i ]->resetFixedOutput( aPeriod );
        fixedOutput = subsec[ i ]->getFixedOutput( aPeriod );

        // initialize property to zero every time just in case fixed share property changes 
        // (shouldn't at the moment, but that could allways change)
        subsec[ i ]->setFixedShare( aPeriod, 0 ); 

        // add up subsector shares without fixed output
        // sjs -- Tried treating capacity limited sub-sectors differently, here and in adjShares,
        //     -- but that didn't give capacity limits exactly.
        if ( fixedOutput == 0 ) { 
            variableShares += subsec[ i ]->getShare( aPeriod );
        } 
		else {
            if ( aMarketDemand != 0 ) {
                double shareVal = fixedOutput / aMarketDemand;
                if ( shareVal > 1 ) { 
                    shareVal = 1; // Eliminates warning message since this conditionshould be fixed below
                } 
                subsec[ i ]->setFixedShare( aPeriod, shareVal ); // set fixed share property
            }
        }
        totalfixedOutput += fixedOutput;
    }

    // Scale down fixed output if its greater than actual demand
    if ( totalfixedOutput > aMarketDemand ) {
        for( unsigned int i = 0; i < subsec.size(); ++i ){
            subsec[ i ]->scaleFixedOutput( aMarketDemand / totalfixedOutput, aPeriod ); 
        }
        totalfixedOutput = aMarketDemand;
    }

    // Adjust shares for any fixed output
    if (totalfixedOutput > 0) {
		double variableSharesNew = 0;
        if (totalfixedOutput > aMarketDemand ) {            
            variableSharesNew = 0; // should be no variable shares in this case
        }
        else {
            assert( aMarketDemand != 0); // check for 0 so that variableSharesNew does not blow up
            variableSharesNew = 1 - (totalfixedOutput/ aMarketDemand );
        }
		
		double shareRatio; // ratio for adjusting shares of non-fixed subsectors
        if (variableShares == 0) {
            shareRatio = 0; // in case all subsectors are fixed output, unlikely
        }
        else {
            shareRatio = variableSharesNew/variableShares;
        }

        // now that parameters are set, adjust shares for all sub-sectors
        for( unsigned int i = 0; i < subsec.size(); ++i ){
            // shareRatio = 0 is okay, sets all non-fixed shares to 0
            subsec[ i ]->adjShares( aMarketDemand, shareRatio, totalfixedOutput, aPeriod ); 
        }
    }
}
