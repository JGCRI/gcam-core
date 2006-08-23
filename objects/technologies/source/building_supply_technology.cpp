/*! 
* \file building_supply_technology.cpp
* \ingroup CIAM
* \brief BuildingSupplyTechnology source file
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

// User headers
#include "technologies/include/building_supply_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "sectors/include/building_dmd_subsector.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplyTechnology::XML_NAME1D = "buildingsupplytech";

// Technology class method definition

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
BuildingSupplyTechnology::BuildingSupplyTechnology( const string& aName, const int aYear )
:technology( aName, aYear ){
    internalLoadFraction = 1;
}

//! Destructor
BuildingSupplyTechnology::~BuildingSupplyTechnology() {
}

//! Clone Function. Returns a deep copy of the current technology.
BuildingSupplyTechnology* BuildingSupplyTechnology::clone() const {
    return new BuildingSupplyTechnology( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingSupplyTechnology::getXMLName1D() const {
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
const std::string& BuildingSupplyTechnology::getXMLNameStatic1D() {
	return XML_NAME1D;
}

//! Parses any input variables specific to derived classes
bool BuildingSupplyTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {

    if( nodeName == "internalLoadFraction" ){
        internalLoadFraction = XMLHelper<double>::getValue( curr );
    }
    else {
      return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void BuildingSupplyTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs);
}	

//! Write object to debugging xml output stream.
void BuildingSupplyTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs );
    XMLWriteElement( input * internalLoadFraction, "internalGain", out, tabs );
}	

/*! \brief Calculates the amount of output from the technology.
* \details Calculates the amount of output of the technology based on the share
*          of the subsector demand. This is then used to determine the amount of
*          input used, and emissions created. Building supply technologies in
*          addition add their secondary output, internal heating, to the
*          marketplace.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDemand Subsector demand for output.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void BuildingSupplyTechnology::production( const string& aRegionName,
                                           const string& aSectorName,
                                           const double aDemand,
                                           const GDP* aGDP,
                                           const int aPeriod )
{
    // Call the normal production function
    technology::production( aRegionName, aSectorName, aDemand, aGDP, aPeriod );

    double internalGain = 0;
    Marketplace* marketplace = scenario->getMarketplace();
    const string intGainsMarketName =  marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod, true )->
                 getString(BuildingDemandSubSector::getInternalGainsInfoName(), false ); 
    // If an internal gains market has been set, calculate internal gains and add to that market 
    if ( internalLoadFraction > 0 && !intGainsMarketName.empty() ) {
        marketplace->addToDemand( intGainsMarketName, aRegionName, input * internalLoadFraction, aPeriod );
        internalGain = input * internalLoadFraction;
    }
}
