/*! 
* \file GRADE.CPP
* \ingroup CIAM
* \brief Grade class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include "scenario.h"
#include "Grade.h"
#include "modeltime.h"
#include "xmlHelper.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor
Grade::Grade() {
    initElementalMembers();
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    totalCost.resize( maxper );
}

//! Clear member variables
void Grade::clear() {
    initElementalMembers();
    name = "";
    totalCost.clear();
}

//! Initialize elemental members
void Grade::initElementalMembers() {
    available = 0;
    extractCost = 0; 
}

//! Initialize data members from XML.
void Grade::XMLParse( const DOMNode* tempNode ) {
    DOMNodeList* tempNodeLst;
    DOMNode* tNode = 0;
    string tNodeName;
    
    /*! \pre assume we are passed a valid node. */
    assert( tempNode );
    
    // get the name attribute.
    name = XMLHelper<string>::getAttrString( tempNode, "name" );
    tempNodeLst = tempNode->getChildNodes();
    
    for( int i = 0; i < static_cast<int>( tempNodeLst->getLength() ); i++ ) {
        tNode = tempNodeLst->item( i );
        tNodeName = XMLHelper<string>::safeTranscode( tNode->getNodeName() );
        
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
void Grade::toXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    Tabs::writeTabs( out );
    out << "<grade name=\"" << name << "\">" << endl;
    Tabs::increaseIndent();

    XMLWriteElementCheckDefault( available, "available", out, 0 );
    XMLWriteElementCheckDefault( extractCost, "extractioncost", out, 0 );
        
    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</grade>" << endl;
}

//! Write datamembers to datastream in XML format for outputting results
void Grade::toOutputXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    Tabs::writeTabs( out );
    out << "<grade name=\"" << name << "\">" << endl;
    Tabs::increaseIndent();

    XMLWriteElement( available, "available", out );
    XMLWriteElement( extractCost, "extractioncost", out );
    
    for( int i = 0; i < static_cast<int>( totalCost.size() ); i++ ){
        XMLWriteElement( totalCost[ i ], "totalcost", out, modeltime->getper_to_yr( i ) );
    }
    
    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</grade>" << endl;
}

//! Write datamembers to debugging datastream in XML format.
void Grade::toDebugXML( const int period, ostream& out ) const {
    
    Tabs::writeTabs( out );
    out << "<Grade name=\"" << name << "\">" << endl;
    Tabs::increaseIndent();
    
    XMLWriteElement( available, "available", out );
    XMLWriteElement( extractCost, "extractioncost", out );
    XMLWriteElement( totalCost[period], "totalcost", out );
    
    Tabs::decreaseIndent();
    Tabs::writeTabs( out );
    out << "</Grade>" << endl;
}

//! Total cost of each grade.
void Grade::calcCost(  const double tax, const double cumTechChange, const double environCost, const int per ) {
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
string Grade::getName() const {
    return name;
}


