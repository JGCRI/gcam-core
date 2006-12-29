/*! 
* \file grade.cpp
* \ingroup Objects
* \brief Grade class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/scenario.h"
#include "resources/include/grade.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;


//! Default constructor
Grade::Grade() {
    available = 0;
    extractCost = 0; 
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    totalCost.resize( maxper );
}

//! Initialize data members from XML.
void Grade::XMLParse( const DOMNode* tempNode ) {
    /*! \pre assume we are passed a valid node. */
    assert( tempNode );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( tempNode, "name" );
    DOMNodeList* tempNodeLst = tempNode->getChildNodes();

    for( unsigned int i = 0; i < tempNodeLst->getLength(); ++i ) {
        DOMNode* tNode = tempNodeLst->item( i );
        string tNodeName = XMLHelper<string>::safeTranscode( tNode->getNodeName() );

        if( tNodeName == "#text" ) {
            continue;
        }

        else if( tNodeName == "available" ){
            available = XMLHelper<double>::getValue( tNode );
        }
        else if( tNodeName == "extractioncost" ){
            extractCost = XMLHelper<double>::getValue( tNode );
        }
        else {
            cout << "Unrecognized text string: " << tNodeName << " found while parsing grade." << endl;
        }
    }
}

//! Write datamembers to datastream in XML format for replicating input file.
void Grade::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    XMLWriteElementCheckDefault( available, "available", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( extractCost, "extractioncost", out, tabs, 0.0 );
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write datamembers to debugging datastream in XML format.
void Grade::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    XMLWriteElement( available, "available", out, tabs );
    XMLWriteElement( extractCost, "extractioncost", out, tabs );
    XMLWriteElement( totalCost[period], "totalcost", out, tabs );
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Grade::getXMLName() const {
    return getXMLNameStatic();
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
const std::string& Grade::getXMLNameStatic() {
    const static string XML_NAME = "grade";
    return XML_NAME;
}

//! Total cost of each grade.
void Grade::calcCost( const double tax, const double cumTechChange, const double environCost, const int per ) {
    totalCost[per] = ( extractCost + environCost ) / cumTechChange + tax;
}

//! Return available amount in each Grade.
double Grade::getAvail() const {
    return available;
}

//! Return the total cost.
double Grade::getCost(const int per) const {
    return totalCost[per];
}

//! Return the extraction cost.
double Grade::getExtCost() const {
    return extractCost;
}

//! Get the name.
const string& Grade::getName() const {
    return name;
}

/*! \brief Perform any initializations needed before each period.
* \details Any initializations or calcuations that only need to be done once per
*          period(instead of every iteration) should be placed in this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param aPeriod Model period
*/
void Grade::initCalc( const string& aRegionName, const string& aResourceName, const int aPeriod ) {
    // do nothing
}

/*! \brief Perform any initializations needed after each period.
* \details Any initializations or calcuations that only need to be done once per
*          period after the model is solved should be placed in this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param aPeriod Model period
*/
void Grade::postCalc( const string& aRegionName, const string& aResourceName, const int aPeriod ) {
    // do nothing
}

/*! \brief Update a visitor for a SubResource.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void Grade::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGrade( this, aPeriod );
    aVisitor->endVisitGrade( this, aPeriod );
}
