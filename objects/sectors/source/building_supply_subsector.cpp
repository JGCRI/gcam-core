/*! 
* \file subsector.cpp
* \ingroup CIAM
* \brief Subsector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/building_supply_subsector.h"
#include "technologies/include/technology.h"

#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplySubSector::XML_NAME = "BuildingSupplySubSector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
const double LOGIT_EXP_DEFAULT = -3;

BuildingSupplySubSector::BuildingSupplySubSector( const string regionName, const string sectorName ) : Subsector( regionName, sectorName ){
}

/*! \brief Default destructor.
*
* deletes all technology objects associated  with this sector.
*
* \author Josh Lurz
*/
BuildingSupplySubSector::~BuildingSupplySubSector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplySubSector::getXMLName() const {
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
const std::string& BuildingSupplySubSector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief calculate Subsector unnormalized shares
*
* Uses normal subSector share calculation.
*
* Then calculates internal loads and passes that information to the appropriate technologies.
*
* \author Steve Smith
* \param regionName region name
* \param period model period
* \param gdp gdp object
* \warning technologies can not independently have fixed outputs
* \warning there is no difference between demand and supply technologies. Control behavior with value of parameter fuelPrefElasticity
*/
void BuildingSupplySubSector::calcShare(const int period, const GDP* gdp ) {
Marketplace* marketplace = scenario->getMarketplace();
   
    Subsector::calcShare( period, gdp );

    // Now that shares are calculated, can calculate internal loads per unit floor space
    double internalLoads; // internal loads per square meter
    for ( int i=0; i<notech; i++ ) {
        string loadName = techs[ i ][ period ]->getFuelName( );
        internalLoads += marketplace->getMarketInfo( loadName, regionName, period, "internalLoadPerFS" );
    }
   // sjsTODO -- impliment sectorInfo Object here to pass this information up to the sector
}
