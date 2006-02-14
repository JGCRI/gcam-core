/*! 
* \file export_sector.cpp
* \ingroup Objects
* \brief ExportSector class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "sectors/include/export_sector.h"
#include "sectors/include/subsector.h"
#include "marketplace/include/marketplace.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/imarket_type.h"
#include "util/base/include/configuration.h"
#include "containers/include/dependency_finder.h"
#include "containers/include/scenario.h" // for marketplace

using namespace std;
using namespace xercesc;

extern Scenario* scenario; // for marketplace

/* \brief Constructor
* \param aRegionName The region name.
*/
ExportSector::ExportSector ( const string& aRegionName ) : SupplySector ( aRegionName ) {
    mFixedPrices.resize( scenario->getModeltime()->getmaxper() );
}

/*! \brief Return the export sector price.
* \details Export sectors currently have fixed prices, so return the read-in
*          fixed price.
* \param aPeriod Model period.
* \return Price.
*/
double ExportSector::getPrice( const int aPeriod ) const {
    return mFixedPrices[ aPeriod ];
}

/*! \brief Calculate the final supply price for the ExportSector, which will
*          leave the international price unchanged.
* \details Currently this function does not calculate or set a price into the
*          marketplace, so that the read-in price is preserved.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void ExportSector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
}

/*! \brief Set the ExportSector output.
* \details Export sectors currently have only fixed outputs so this function
*          simply calculates the entire amount of fixed output and distributes
*          that amount to the subsectors.
* \author Josh Lurz
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period
*/
void ExportSector::supply( const GDP* aGDP, const int aPeriod ) {
	// Set the demand for the good produced by this sector to only be the level of fixed output.
    double marketDemand = getFixedOutput( aPeriod );

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

/*! \brief Get the XML node name for output to XML using a virtual method so the
*          correct name is always returned.
* \return The name of the object's associated XML element name.
*/
const std::string& ExportSector::getXMLName() const {
	return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \return The name of the object's associated XML element name.
*/
const std::string& ExportSector::getXMLNameStatic() {
	const static string XML_NAME = "export-sector";
	return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes
* \details Method parses any input data from child nodes that are specific to
*          the classes derived from this class. Since Sector is the generic base
*          class, there are no values here.
* \param aNodeName name of current node
* \param aCurr pointer to the current node in the XML input tree
*/
bool ExportSector::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
    if( aNodeName == "market" ){
		mMarketName = XMLHelper<string>::getValueString( aCurr );
	}
    else if( aNodeName == "sectorprice" ){
        XMLHelper<double>::insertValueIntoVector( aCurr, mFixedPrices, scenario->getModeltime() );
    }
	else {
		return false;
	}
	return true;
}

/*! \brief Write out derived class specific class members as an input file.
* \param aOut The output stream to which to write.
* \param aTabs Object responsible for writing tabs.
*/
void ExportSector::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {  
    // write out the market string.
    XMLWriteElement( mMarketName, "market", aOut, aTabs );
    XMLWriteVector( mFixedPrices, "sectorprice", aOut, aTabs, scenario->getModeltime(), 0.0 );
}	

/*! \brief Write out derived class specific class members for debugging.
* \param aPeriod The period in which to write debugging information.
* \param aOut The output stream to which to write.
* \param aTabs Object responsible for writing tabs.
*/
void ExportSector::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write out the market string.
    XMLWriteElement( mMarketName, "market", aOut, aTabs );
    XMLWriteElement( mFixedPrices[ aPeriod ], "sectorprice", aOut, aTabs );
}

/*! \brief Create new market for the ExportSector.
* \details Sets up the appropriate market within the marketplace for this
*          ExportSector. The market is not solved currently and prices are fixed
*          to read-in values.
*/
void ExportSector::setMarket() {
	// Check if the market name was not read-in or is the same as the region name.
	if( mMarketName.empty() || mMarketName == regionName ){
		ILogger& mainLog = ILogger::getLogger( "main_log" );
		mainLog.setLevel( ILogger::WARNING );
		mainLog << "Market name for export sector was unset or set to the region name." << endl;
		// Is there anything logical to do here?
	}

    Marketplace* marketplace = scenario->getMarketplace();
	// Creates a regional market. MiniCAM supply sectors are not independent and 
	// cannot be members of multi-region markets.
    if( marketplace->createMarket( regionName, mMarketName, name, IMarketType::NORMAL ) ) {
        // Set the base year price which the sector reads in, into the mFixedPrices vector.
        // TODO: Seperate SupplySector so this is not needed.
        mFixedPrices[ 0 ] = mBasePrice;

		// Initializes prices with any values that are read-in. 
        marketplace->setPriceVector( name, regionName, mFixedPrices );
    }
}
