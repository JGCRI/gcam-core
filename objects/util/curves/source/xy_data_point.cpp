/*! 
* \file xy_data_point.cpp
* \ingroup Util
* \brief XYDataPoint class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include "util/curves/include/xy_data_point.h"
#include "util/base/include/xml_helper.h"

using namespace std;

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
ExplicitPointSet::DataPoint* XYDataPoint::clone() const {
	return new XYDataPoint( x, y );
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
void XYDataPoint::toXML( ostream& out, Tabs* tabs ) const {
    tabs->writeTabs( out );
    out << "<XYDataPoint>" << endl;
    tabs->increaseIndent();
    XMLWriteElement( x, "x", out, tabs );
    XMLWriteElement( y, "y", out, tabs );
    tabs->decreaseIndent();
    tabs->writeTabs( out );
    out << "</XYDataPoint>" << endl;
}