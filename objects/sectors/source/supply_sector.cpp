/*! 
* \file supply_sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/xml_helper.h"
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string SupplySector::XML_NAME = "supplysector";

SupplySector::SupplySector ( const string regionNameIn ) : Sector ( regionNameIn ) {
}

//! Default destructor
SupplySector::~SupplySector() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
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


/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param node pointer to the current node in the XML input tree
*/
bool SupplySector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    return false;
}
