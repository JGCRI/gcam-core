/*! 
* \file xy_data_point.cpp
* \ingroup Util
* \brief XYDataPoint class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/curves/include/data_point.h"
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/xml_helper.h"

using namespace std;

const string XYDataPoint::XML_NAME = "XYDataPoint";

/*! \brief Constructor
* \details This is the default constructor for the XYDataPoint. The parameters are used
* to specify the initial x and y values. They default to 0.
* \param xIn The initial x value. Defaults to 0.
* \param yIn The initial y value. Defaults to 0.
*/
XYDataPoint::XYDataPoint( const double xIn, const double yIn ): x( xIn ), y( yIn ){
}

//! Destructor
XYDataPoint::~XYDataPoint(){
}

//! Clone
DataPoint* XYDataPoint::clone() const {
	return new XYDataPoint( x, y );
}

//! Static function to return the name of the XML element associated with this object.
const string& XYDataPoint::getXMLNameStatic() {
    return XML_NAME;
}

//! Return the name of the XML element associated with this object.
const string& XYDataPoint::getXMLName() const {
    return XML_NAME;
}

//! Equality
bool XYDataPoint::operator==( const XYDataPoint& rhs ) const {
    return( ( x == rhs.x ) && ( y == rhs.y ) );
}

//! Inequality
bool XYDataPoint::operator!=( const XYDataPoint& rhs ) const {
    return !( *this == rhs );
}

//! Get the x value.
double XYDataPoint::getX() const {
    return x;
}

//! Get the y value
double XYDataPoint::getY() const {
    return y;
}

//! Set the x value 
void XYDataPoint::setX( const double xValue ){
    x = xValue;
}

//! Set the y value
void XYDataPoint::setY( const double yValue ){
    y = yValue;
}

//! Print the datapoint.
void XYDataPoint::print( ostream& out ) const {
    out << x << "," << y << endl;
}

//! Print the datapoint to an XML stream.
void XYDataPoint::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag( DataPoint::getXMLNameStatic(), out, tabs, "", 0, getXMLName() );
    XMLWriteElement( x, "x", out, tabs );
    XMLWriteElement( y, "y", out, tabs );
    XMLWriteClosingTag( DataPoint::getXMLNameStatic(), out, tabs );
}

//! Parse the XYDataPoint from an XML DOM tree.
void XYDataPoint::XMLParse( const xercesc::DOMNode* node ) {
    
    xercesc::DOMNode* curr = 0;
    xercesc::DOMNodeList* nodeList; 
    string nodeName;

    // assume node is valid.
    assert( node );

    // get all children of the node.
    nodeList = node->getChildNodes();

    // loop through the children
    for ( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<void>::safeTranscode( curr->getNodeName() );

        // select the type of node.
        if( nodeName == "#text" ) {
            continue;
        }
        else if ( nodeName == "x" ){
            x = XMLHelper<double>::getValue( curr );
        }
        else if ( nodeName == "y" ){
            y = XMLHelper<double>::getValue( curr );
        } 
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << endl;
        }
    }
}

//! Switch the X and Y values.
void XYDataPoint::invertAxises(){
    swap( x, y );
}
