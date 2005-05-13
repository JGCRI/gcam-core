/*! 
* \file building_heating_dmd_technology.cpp
* \ingroup CIAM
* \brief The building heating service demand technology.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>

// User headers
#include "technologies/include/building_heating_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/market_info.h"

using namespace std;

extern Scenario* scenario;
// static initialize.
const string BuildingHeatingDmdTechnology::XML_NAME1D = "heatingservice";

// Technology class method definition

//! Default constructor.
BuildingHeatingDmdTechnology::BuildingHeatingDmdTechnology() {
    heatingDegreeDays = 0;
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingHeatingDmdTechnology* BuildingHeatingDmdTechnology::clone() const {
    return new BuildingHeatingDmdTechnology( *this );
}
 
/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingHeatingDmdTechnology::getXMLName1D() const {
	return XML_NAME1D;
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
const std::string& BuildingHeatingDmdTechnology::getXMLNameStatic1D() {
	return XML_NAME1D;
}

/*! \brief Initialize each period
*
* Transfer data that does not change to the technoogy
*
* \author Steve Smith
* \param aSubsectorInfo The subsectorInfo object. 
*/
void BuildingHeatingDmdTechnology::initCalc( const MarketInfo* aSubsectorInfo ) {

    heatingDegreeDays = aSubsectorInfo->getItemValue( "heatingDegreeDays" );
    BuildingHeatCoolDmdTechnology::initCalc( aSubsectorInfo );
}

/*! \brief Determine sign of internal gains
*
* For heating sectors internal gains subtract from demands, so sign is negative
*
* \author Steve Smith
*/
double BuildingHeatingDmdTechnology::getInternalGainsSign() const {

    return -1;    
}
 
//! Demand function prefix.
/*! Defines the demand function, exclusive of demand and share . 
* \author Steve Smith
* \param regionName name of the region
* \param period Model period
*/
double BuildingHeatingDmdTechnology::getDemandFnPrefix( const string& regionName, const int period )  {
Marketplace* marketplace = scenario->getMarketplace();

    double priceRatio = ( period > 1 ) ? 
        marketplace->getPrice( fuelname, regionName, period ) / 
        marketplace->getPrice( fuelname, regionName, period - 1 ) : 1;
    
    double prefixValue =    saturation * aveInsulation * floorToSurfaceArea * 
                            heatingDegreeDays * pow( priceRatio, priceElasticity );
    
    // Make sure and do not return zero
    return ( prefixValue > 0 ) ? prefixValue : 1;
}


