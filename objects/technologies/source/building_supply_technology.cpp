/*! 
* \file building_supply_technology.cpp
* \ingroup CIAM
* \brief The building supply technology
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

// User headers
#include "technologies/include/building_supply_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string BuildingSupplyTechnology::XML_NAME1D = "buildingsupplytech";

// Technology class method definition

//! Default constructor.
BuildingSupplyTechnology::BuildingSupplyTechnology() {
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
bool BuildingSupplyTechnology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {

    if( nodeName == "internalLoadFraction" ){
        internalLoadFraction = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "intGainsMarketName" ){
        intGainsMarketName = XMLHelper<string>::getValueString( curr );
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
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
}	

//! XML output for viewing.
void BuildingSupplyTechnology::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs);
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
    XMLWriteElement( input * internalLoadFraction, "internalGain", out, tabs );
}

//! Write object to debugging xml output stream.
void BuildingSupplyTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( internalLoadFraction, "internalLoadFraction", out, tabs );
    XMLWriteElement( intGainsMarketName, "intGainsMarketName", out, tabs );
    XMLWriteElement( input * internalLoadFraction, "internalGain", out, tabs );
}	

//! Calculates fuel input and technology output.
/*! Calls the usual technology production function technology::production
* and then adds internal gains, if any, to the appropriate market
*
* \author Steve Smith
* \param regionName name of the region
* \param prodName name of the product for this sector
* \param gdp pointer to gdp object
* \param dmd total demand for this subsector
* \param per Model period
*/
void BuildingSupplyTechnology::production(const string& regionName,const string& prodName,
                            double dmd, const GDP* gdp, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();

    // Call the normal production function
    technology::production( regionName, prodName, dmd, gdp, period );

    double internalGain = 0;
    // Now, if an internal gains market has been set, calculate internal gains and add to that market 
    if ( internalLoadFraction > 0 && !intGainsMarketName.empty() ) {
        marketplace->addToDemand( intGainsMarketName, regionName, input * internalLoadFraction, period );
        internalGain = input * internalLoadFraction;
    }
    
    // TEMPORARY -- add calibrated internal gains to the internal gains market
    // This should be moved to initCalc when possible since this only has to be done once per period.
    double internalGains = marketplace->getMarketInfo( intGainsMarketName, regionName, period, "calInternalGains" );
    internalGains += calInputValue * internalLoadFraction;
    marketplace->setMarketInfo( intGainsMarketName, regionName, period, "calInternalGains", internalGains );
}
