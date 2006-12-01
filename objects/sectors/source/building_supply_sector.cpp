/*! 
* \file building_supply_sector.cpp
* \ingroup CIAM
* \brief The BuildingSupplySector class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/building_supply_sector.h"
#include "sectors/include/building_supply_subsector.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplySector::XML_NAME = "buildingservicesupply";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
BuildingSupplySector::BuildingSupplySector( const string regionName ): SupplySector( regionName ){
}

//! Default destructor
BuildingSupplySector::~BuildingSupplySector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplySector::getXMLName() const {
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
const std::string& BuildingSupplySector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
* \return returns true if the node was parsed
*/
bool BuildingSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
            
    if ( SupplySector::XMLDerivedClassParse( nodeName, curr) ) {
    }   // if false, node was not parsed so far so try to parse here
    else if( nodeName == BuildingSupplySubSector::getXMLNameStatic() ){
        parseContainerNode( curr, subsec, subSectorNameMap, new BuildingSupplySubSector( regionName, name ) );
    }
    else {
        return false;
    }
    // If was true somewhere above then noce was parsed
    return true;
}

/*! \brief Perform any sector level calibration data consistancy checks
*
* For the buildings service supply sectors need to supply total calibrated output
* to the building demand sub-sector through use of market info mechanism. (The building
* demand subsector needs this information to calibrate demands)
*
* \author Steve Smith
* \param period Model period
*/
void BuildingSupplySector::initCalc( NationalAccount* aNationalAccount,
                                     const Demographic* aDemographic,
                                     const int aPeriod )
{
    Marketplace* marketplace = scenario->getMarketplace();

    double calOutput = getCalOutput( aPeriod  );
    marketplace->getMarketInfo( name, regionName, aPeriod, true )->setDouble( "calOutput", calOutput );    
    SupplySector::initCalc( aNationalAccount, aDemographic, aPeriod );
}
