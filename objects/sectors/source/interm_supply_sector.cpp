/*!
* \file interm_supply_sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author Marshall Wise
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "containers/include/scenario.h"
#include "sectors/include/interm_supply_sector.h"
#include "sectors/include/interm_subsector.h"
#include "marketplace/include/market_info.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string IntermittentSupplySector::XML_NAME = "intermittent-supplysector";

IntermittentSupplySector::IntermittentSupplySector ( const string regionNameIn ) : SupplySector ( regionNameIn ) {

    elecReserveMargin = 0;
    aveGridCapacityFactor = 1;
    backupCapacityFactor = 0;
    backupCost = 0;
}


/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& IntermittentSupplySector::getXMLName() const {
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
const std::string& IntermittentSupplySector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Marshall Wise
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool IntermittentSupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // call the supply sector XML parse (one parent level up) to fill supply sector attributes
    if( SupplySector::XMLDerivedClassParse( nodeName, curr ) ){
    }
    else if( nodeName == IntermittentSubsector::getXMLNameStatic() ){
        parseContainerNode( curr, subsec, subSectorNameMap, new IntermittentSubsector( regionName, name ) );
    }
    else if( nodeName == "elecReserveMargin" ){
        elecReserveMargin = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "aveGridCapacityFactor" ){
        aveGridCapacityFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "backupCapacityFactor" ){
        backupCapacityFactor = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "backupCost" ){
        backupCost = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}


/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Marshall Wise
* \param node pointer to the current node in the XML input tree
*/
bool IntermittentSupplySector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    return false;
    // do nothing
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Marshall Wise
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void IntermittentSupplySector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toInputXMLDerived( out, tabs );

    XMLWriteElementCheckDefault( elecReserveMargin, "elecReserveMargin", out, tabs, 0.0);
    XMLWriteElementCheckDefault( aveGridCapacityFactor, "aveGridCapacityFactor", out, tabs, 1.0);
    XMLWriteElementCheckDefault( backupCapacityFactor, "backupCapacityFactor", out, tabs, 0.0);
    XMLWriteElementCheckDefault( backupCost, "backupCost", out, tabs, 0.0);
}	


//! XML output for viewing.
void IntermittentSupplySector::toOutputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toOutputXMLDerived( out, tabs );

    XMLWriteElementCheckDefault( elecReserveMargin, "elecReserveMargin", out, tabs, 0.0);
    XMLWriteElementCheckDefault( aveGridCapacityFactor, "aveGridCapacityFactor", out, tabs, 0.0);
    XMLWriteElementCheckDefault( backupCapacityFactor, "backupCapacityFactor", out, tabs, 1.0);
    XMLWriteElementCheckDefault( backupCost, "backupCost", out, tabs, 0.0);
}	

//! Write object to debugging xml output stream.
void IntermittentSupplySector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toDebugXMLDerived( period, out, tabs );

    XMLWriteElement( elecReserveMargin, "elecReserveMargin", out, tabs );
    XMLWriteElement( aveGridCapacityFactor, "aveGridCapacityFactor", out, tabs );
    XMLWriteElement( backupCapacityFactor, "backupCapacityFactor", out, tabs );
    XMLWriteElement( backupCost, "backupCost", out, tabs );
}



/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calculations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSupplySector::initCalc( const int period, const MarketInfo* aRegionInfo,
                       NationalAccount& nationalAccount, Demographic* aDemographics ) {

    // add items to sectorinfo 
    // must be done before Sector:initCalc() so that information is available to subsector and technology initCalc() routines
    mSectorInfo->addItem( "elecReserveMargin", elecReserveMargin );
    mSectorInfo->addItem( "aveGridCapacityFactor", aveGridCapacityFactor );
    mSectorInfo->addItem( "backupCapacityFactor", backupCapacityFactor );
    mSectorInfo->addItem( "backupCost", backupCost );

    SupplySector::initCalc( period, aRegionInfo, nationalAccount, aDemographics );
}
