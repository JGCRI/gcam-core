/*! 
* \file building_heating_dmd_technology.cpp
* \ingroup CIAM
* \brief BuildingHeatingDmdTechnology source file
* \author Steve Smith
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>

// User headers
#include "technologies/include/building_heating_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;
// static initialize.
const string BuildingHeatingDmdTechnology::XML_NAME1D = "heatingservice";

// Technology class method definition

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
BuildingHeatingDmdTechnology::BuildingHeatingDmdTechnology( const string& aName, const int aYear )
:BuildingHeatCoolDmdTechnology( aName, aYear ){
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

/*! 
* \brief Perform initializations that only need to be done once per period.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period.
*/
void BuildingHeatingDmdTechnology::initCalc( const string& aRegionName,
                                             const string& aSectorName,
                                             const IInfo* aSubsectorInfo,
                                             const Demographic* aDemographics,
                                             const int aPeriod )
{
	heatingDegreeDays = aSubsectorInfo->getDouble( "heatingDegreeDays", true );
    BuildingHeatCoolDmdTechnology::initCalc( aRegionName, aSectorName,
                                             aSubsectorInfo, aDemographics, aPeriod );        
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
/*! Defines the demand function, exclusive of demand and share. 
* \author Steve Smith
* \param regionName name of the region
* \param period Model period
*/
double BuildingHeatingDmdTechnology::getDemandFnPrefix( const string& regionName, const int period )  {
	Marketplace* marketplace = scenario->getMarketplace();

    double priceRatio = ( period > 1 ) ? 
        marketplace->getPrice( mTechData->getFuelName(), regionName, period ) / 
        marketplace->getPrice( mTechData->getFuelName(), regionName, 1 ) : 1;
    
    double prefixValue = saturation * aveInsulation * floorToSurfaceArea * 
                         heatingDegreeDays * pow( priceRatio, priceElasticity );
    
    // Make sure and do not return zero
    return ( prefixValue > 0 ) ? prefixValue : 1;
}
