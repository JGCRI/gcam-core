/*! 
* \file data_point.cpp
* \ingroup Util
* \brief DataPoint class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include "util/base/include/definitions.h"
#include "util/curves/include/data_point.h"
#include "util/curves/include/xy_data_point.h"

using namespace std;

const string DataPoint::XML_NAME = "DataPoint";

//! Constructor
DataPoint::DataPoint() {
}

//! Destructor
DataPoint::~DataPoint(){
}

//! Returns if two datapoints are equal.
bool DataPoint::operator==( const DataPoint& rhs ) const {
    return( ( getX() == rhs.getX() ) && ( ( getY() == rhs.getY() ) ) );
}

//! Returns if two datapoints are not equal.
bool DataPoint::operator!=( const DataPoint& rhs ) const {
    return !( *this == rhs );
}

//! Returns whether one DataPoint is less than another DataPoint, defined by a lesser x value.
bool DataPoint::operator<( const DataPoint& rhs ) const {
    return ( ( this->getX() < rhs.getX() ) || ( ( this->getX() == rhs.getX() ) && ( this->getY() < rhs.getY() ) ) );
}

//! Returns whether one DataPoint is greater than another DataPoint, defined by a greater x value.
bool DataPoint::operator>( const DataPoint& rhs ) const {
    return ( ( this->getX() > rhs.getX() ) || ( ( this->getX() == rhs.getX() ) && ( this->getY() > rhs.getY() ) ) );
}

//! Returns whether one DataPoint is less than or equal to another DataPoint, defined by a lesser or equal x value.
bool DataPoint::operator<=( const DataPoint& rhs ) const {
    return ( ( this->getX() < rhs.getX() ) || ( ( this->getX() == rhs.getX() ) && ( this->getY() <= rhs.getY() ) ) );
}

//! Returns whether one DataPoint is greater than or equal to another DataPoint, defined by a greater or equal x value.
bool DataPoint::operator>=( const DataPoint& rhs ) const {
    return ( ( this->getX() > rhs.getX() ) || ( ( this->getX() == rhs.getX() ) && ( this->getY() >= rhs.getY() ) ) );
}

//! Static function to return the name of the XML element associated with this object.
const string& DataPoint::getXMLNameStatic(){
    return XML_NAME;
}

//! Function to return the name of the XML element associated with this object.
const string& DataPoint::getXMLName() const {
    return XML_NAME;
}

//! Factory method which returns the correct type of DataPoint based on the type string.
DataPoint* DataPoint::getDataPoint( const string& type ) {
    DataPoint* dataPoint;

    if( type == XYDataPoint::getXMLNameStatic() ){
        dataPoint = new XYDataPoint();
    } 
    else {
        cout << "Invalid type of " << getXMLNameStatic() << " requested: " << type << "." << endl;
        dataPoint = 0;
    }
    return dataPoint;
}
