/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file curve.cpp
* \ingroup Util
* \brief Curve class source file.
* \author Josh Lurz
*/
#include "util/base/include/definitions.h"
#include <iostream>
#include "util/base/include/xml_helper.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"

using namespace std;

//! Constructor
Curve::Curve(){
    numericalLabel = 0;
}

void Curve::copy( const Curve& aOther ) {
    // nothing is copied?
}

//! Destructor.
Curve::~Curve(){
}

//! Equals operator.
bool Curve::operator==( const Curve& rhs ) const {
    return( getSortedPairs() == rhs.getSortedPairs() );
}

//! Not Equals operator.
bool Curve::operator!=( const Curve& rhs ) const {
    return !( *this == rhs );
}

//! Static function to return the name of the XML element associated with this object.
const string& Curve::getXMLNameStatic() {
    static const string XML_NAME = "Curve";
    return XML_NAME;
}

//! Return the name of the XML element associated with this object.
const string& Curve::getXMLName() const {
    return getXMLNameStatic();
}

//! Static factory method which returns the correct type of Curve based on the type string.
Curve* Curve::getCurve( const string& type ) {
    Curve* curve;

    if( type == PointSetCurve::getXMLNameStatic() ){
        curve = new PointSetCurve();
    }
    else {
        cout << "Invalid type of " << getXMLNameStatic() << " requested: " << type << "." << endl;
        curve = 0;
    }
    return curve;
}

//! Print out the curve to an XML File
void Curve::outputAsXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( Curve::getXMLNameStatic(), aOut, aTabs, name, 0, getXMLName() );
    XMLWriteElementCheckDefault( title, "title", aOut, aTabs );
    XMLWriteElementCheckDefault( numericalLabel, "numericalLabel", aOut, aTabs );
    XMLWriteElementCheckDefault( xAxisLabel, "xAxisLabel", aOut, aTabs );
    XMLWriteElementCheckDefault( yAxisLabel, "yAxisLabel", aOut, aTabs );
    XMLWriteElementCheckDefault( xAxisUnits, "xAxisUnit", aOut, aTabs );
    XMLWriteElementCheckDefault( yAxisUnits, "yAxisUnit", aOut, aTabs );
    outputAsXMLDerived( aOut, aTabs );
    XMLWriteClosingTag( Curve::getXMLNameStatic(), aOut, aTabs );
}

//! Get the curve name.
const std::string Curve::getName() const {
    return name;
}

//! Get the curve title.
const std::string Curve::getTitle() const {
    return title;
}

//! Set the curve title.
void Curve::setTitle( const std::string& titleIn ){
    title = titleIn;
}

//! Get the numerical value or label associated with this curve.
double Curve::getNumericalLabel() const {
    return numericalLabel;
}

//! Set the numerical value or label associated with this curve.
void Curve::setNumericalLabel( const double numericalLabelIn ) {
    numericalLabel = numericalLabelIn;
}

//! Get the X axis label.
const std::string Curve::getXAxisLabel() const {
    return xAxisLabel;
}

//! Get the y axis label.
const std::string Curve::getYAxisLabel() const {
    return yAxisLabel;
}

//! Set the X axis label.
void Curve::setXAxisLabel( const std::string& xLabelIn ) {
    xAxisLabel = xLabelIn;
}

//! Set the Y axis label. 
void Curve::setYAxisLabel( const std::string& yLabelIn ) {
    yAxisLabel = yLabelIn;
}

//! Get the X axis units.
const std::string Curve::getXAxisUnits() const {
    return xAxisUnits;
}

//! Get the Y axis units.
const std::string Curve::getYAxisUnits() const {
    return yAxisUnits;
}

//! Set the X axis units.
void Curve::setXAxisUnits( const std::string& xUnitsIn ){
    xAxisUnits = xUnitsIn;
}

//! Set the Y axis units.
void Curve::setYAxisUnits( const std::string& yUnitsIn ){
    yAxisUnits = yUnitsIn;
}

//! Calculate the hamming distance between this Curve and another Curve over a given time interval and step.
double Curve::getHammingDistance( const Curve* otherCurve, const double xStart, const double xEnd, const double xInterval ) const {
    double sum = 0;
    for( double xVal = xStart; xVal <= xEnd; xVal += xInterval ){
        double thisY = getY( xVal );
        double otherY = otherCurve->getY( xVal );
        double difference = fabs( thisY - otherY );
        sum += difference;
    }
    return sum;
}
