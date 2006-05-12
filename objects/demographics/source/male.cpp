/*! 
* \file male.cpp
* \ingroup Objects-SGM
* \brief Male class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include "demographics/include/male.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;
// static initialize
const string Male::XML_NAME = "male";

//! Parse xml file for data
bool Male::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    return false;
}

//! For derived classes to output XML data
void Male::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // nothing
}

//! Output debug info for derived class
void Male::toDebugXMLDerived(ostream& out, Tabs* tabs ) const {
    // nothing
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Male::getXMLName() const{
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
const std::string& Male::getXMLNameStatic(){
    return XML_NAME;
}

/*! \brief Update a visitor with information about a Male.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void Male::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitMale( this, aPeriod );
	// Update the parent class.
	Gender::accept( aVisitor, aPeriod );
	aVisitor->endVisitMale( this, aPeriod );
}
