/*! 
* \file demand_sector.cpp
* \ingroup CIAM
* \brief BuildingSerSupplySector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/building_ser_supply_sector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "sectors/include/subsector.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSerSupplySector::XML_NAME = "buildingsersupply";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
BuildingSerSupplySector::BuildingSerSupplySector( const string regionName ): Sector( regionName ){
}

//! Default destructor
BuildingSerSupplySector::~BuildingSerSupplySector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSerSupplySector::getXMLName() const {
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
const std::string& BuildingSerSupplySector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Perform any sector level calibration data consistancy checks
*
* For the buildings service supply sectors need to supply total calibrated output
* to the building demand sector through use of market info mechanism
*
* \author Steve Smith
* \param period Model period
*/
void BuildingSerSupplySector::checkSectorCalData( const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();

    double calOutput = getCalOutput( period  );
    marketplace->setMarketInfo( name, regionName, period, "calOutput", calOutput );    

}

