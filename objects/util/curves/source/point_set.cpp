/*! 
* \file point_set.cpp
* \ingroup Util
* \brief PointSet class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include "util/base/include/definitions.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"

using namespace std;

const string PointSet::XML_NAME = "PointSet";

//! Constructor
PointSet::PointSet() {
}

//! Destructor
PointSet::~PointSet(){
}

//! Static function to return the name of the XML element associated with this object.
const string& PointSet::getXMLNameStatic(){
    return XML_NAME;
}

//! Function to return the name of the XML element associated with this object.
const string& PointSet::getXMLName() const {
    return XML_NAME;
}

//! Factory method which returns the correct type of PointSet based on the type string.
PointSet* PointSet::getPointSet( const string& type ) {
    PointSet* pointSet;

    if( type == ExplicitPointSet::getXMLNameStatic() ){
        pointSet = new ExplicitPointSet();
    } 
    else {
        cout << "Invalid type of " << getXMLNameStatic() << " requested: " << type << "." << endl;
        pointSet = 0;
    }
    return pointSet;
}
