/*! 
* \file point_set_curve.cpp
* \ingroup Util
* \brief PointSetCurve class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/point_set.h"
#include "util/base/include/xml_helper.h"

#include <vector>
#include <cassert>
#include <cfloat>

using namespace std;

/*! \brief Constructor
* \warning This curve is takes responsibility for this PointSet one it is constructed. 
* \param pointSetIn The PointSet which defines this curve's data.
*/
PointSetCurve::PointSetCurve( PointSet* pointSetIn ) {
    /*! \pre pointSet is not null */
    assert( pointSetIn );
    pointSet = pointSetIn;
    numericalLabel = 0;
}

//! Destructor
PointSetCurve::~PointSetCurve(){
    delete pointSet;
}

//! Return the name of the XML tag this objects uses to write itself out.
string PointSetCurve::getXMLTagName() {
    const string XML_ELEMENT_NAME = "PointSetCurve";
    return XML_ELEMENT_NAME;
}

//! Get the X value corresponding to a given Y value.
double PointSetCurve::getY( const double xValue ) const {
    double retValue;

    // First check if the point exists.
    if( pointSet->containsX( xValue ) ){
        retValue = pointSet->getY( xValue );
    } else {
        // Need to interpolate.
        double x1 = pointSet->getNearestXBelow( xValue );
        double x2 = pointSet->getNearestXAbove( xValue );

        // First check if both points are invalid. This means are curve has no points.
        if( x1 == -DBL_MAX && x2 == -DBL_MAX ){
            retValue = -DBL_MAX;
        }
        else {
            // Check if the point below is invalid.
            if( x1 == -DBL_MAX ){
                // Get the next value above x2
                x1 = pointSet->getNearestXAbove( x2 );

                // Check if that is valid
                if( x1 == -DBL_MAX ){
                    // There is only one valid point.
                    x1 = x2;
                }
            }

            // Check if the point above is invalid
            if( x2 == -DBL_MAX ){
                // Get the next value below x1
                x2 = pointSet->getNearestXBelow( x1 );

                // Check if that is valid.
                if( x2 == -DBL_MAX ){
                    // There is only one valid point.
                    x2 = x1;
                }
            }
            retValue = linearInterpolateY( xValue, x1, pointSet->getY( x1 ), x2, pointSet->getY( x2 ) );
        }
    }
    return retValue;
}

//! Get the y value corresponding to a given X value.
double PointSetCurve::getX( const double yValue ) const {
      double retValue;

    // First check if the point exists.
    if( pointSet->containsY( yValue ) ){
        retValue = pointSet->getX( yValue );
    } else {
        // Need to interpolate.
        double y1 = pointSet->getNearestYBelow( yValue );
        double y2 = pointSet->getNearestYAbove( yValue );

        // First check if both points are invalid. This means are curve has no points.
        if( y1 == -DBL_MAX && y2 == -DBL_MAX ){
            retValue = -DBL_MAX;
        }
        else {
            // Check if the point below is invalid.
            if( y1 == -DBL_MAX ){
                // Get the next value above y2
                y1 = pointSet->getNearestYAbove( y2 );

                // Check if that is valid
                if( y1 == -DBL_MAX ){
                    // There is only one valid point.
                    y1 = y2;
                }
            }

            // Check if the point above is invalid
            if( y2 == -DBL_MAX ){
                // Get the next value below x1
                y2 = pointSet->getNearestXBelow( y1 );

                // Check if that is valid.
                if( y2 == -DBL_MAX ){
                    // There is only one valid point.
                    y2 = y1;
                }
            }
            retValue = linearInterpolateX( yValue, pointSet->getX( y1 ), y1, pointSet->getX( y2 ), y2 );
        }
    }
    return retValue;
}

//! Set the Y value for a point associated with an X value.
bool PointSetCurve::setY( const double xValue, const double yValue ){
    // Need to do more here I think. Add point?
    return pointSet->setY( xValue, yValue );
}

//! Set an X value for a point associated with a Y value.
bool PointSetCurve::setX( const double yValue, const double xValue ){
    // Need to do more here I think. Add point?
    return pointSet->setX( yValue, xValue );
}

//! Get the X coordinates of all points within the specified domain for the underlying pointset. 
const std::vector<double> PointSetCurve::getXCoords( const double lowDomain, const double highDomain, const int minPoints ) const {
    return pointSet->getXCoords( lowDomain, highDomain, minPoints );
}

//! Get the Y coordinates of all points within the specified range from the underlying pointset.
const std::vector<double> PointSetCurve::getYCoords( const double lowRange, const double highRange, const int minPoints ) const {
    return pointSet->getYCoords( lowRange, highRange, minPoints );
}

//! Return a vector of pairs of x y coordinates sorted in increasing x order.
std::vector<std::pair<double,double> > PointSetCurve::getSortedPairs( const double lowDomain, const double highDomain, const int minPoints ) const {
    return pointSet->getSortedPairs( lowDomain, highDomain, minPoints );
}

//! Get the curve title.
const std::string PointSetCurve::getTitle() const {
    return title;
}

//! Set the curve title.
void PointSetCurve::setTitle( const std::string& titleIn ){
    title = titleIn;
}

//! Get the numerical value or label associated with this curve.
double PointSetCurve::getNumericalLabel() const {
    return numericalLabel;
}

//! Set the numerical value or label associated with this curve.
void PointSetCurve::setNumericalLabel( const double numericalLabelIn ) {
    numericalLabel = numericalLabelIn;
}

//! Get the X axis label.
const std::string PointSetCurve::getXAxisLabel() const {
    return xAxisLabel;
}

//! Get the y axis label.
const std::string PointSetCurve::getYAxisLabel() const {
    return yAxisLabel;
}

//! Set the X axis label.
void PointSetCurve::setXAxisLabel( const std::string& xLabelIn ) {
    xAxisLabel = xLabelIn;
}

//! Set the Y axis label. 
void PointSetCurve::setYAxisLabel( const std::string& yLabelIn ) {
    yAxisLabel = yLabelIn;
}

//! Get the X axis units.
const std::string PointSetCurve::getXAxisUnits() const {
    return xAxisUnits;
}

//! Get the Y axis units.
const std::string PointSetCurve::getYAxisUnits() const {
    return yAxisUnits;
}

//! Set the X axis units.
void PointSetCurve::setXAxisUnits( const std::string& xUnitsIn ){
    xAxisUnits = xUnitsIn;
}

//! Set the Y axis units.
void PointSetCurve::setYAxisUnits( const std::string& yUnitsIn ){
    yAxisUnits = yUnitsIn;
}

//! Integrate the curve. Currently uses a trapezoidal integration.
double PointSetCurve::getIntegral( const double lowDomain, const double highDomain ) const {
    typedef pair<double,double> PointPair;

    // Get the underlying points. 
    vector<PointPair> sortedPoints = pointSet->getSortedPairs( lowDomain, highDomain );
    double sum = 0;

    if( !sortedPoints.empty() ){
        // If the lowDomain is defined and is not the first point, add a point to represent represent that.
        if( lowDomain != -DBL_MAX && !util::isEqual( sortedPoints[ 0 ].first, lowDomain ) ){
            PointPair pointA( lowDomain, getY( lowDomain ) );
            sortedPoints.insert( sortedPoints.begin(), pointA );
        }

        // If the highDomain is defined and is not the last point, add a point to represent that.
        if( highDomain != DBL_MAX && !util::isEqual( sortedPoints[ sortedPoints.size() - 1 ].first, highDomain ) ){
            PointPair pointB( highDomain, getY( highDomain ) );
            sortedPoints.push_back( pointB );
        }

        // Perform the integration.

        for( int i = 1; i < static_cast<int>( sortedPoints.size() ); i++ ){
            double height = sortedPoints[ i ].first - sortedPoints[ i - 1 ].first;
            sum += 0.5 * height * ( sortedPoints[ i ].second + sortedPoints[ i - 1 ].second );
        }
    }
    return sum;

}

//! Get the value of the curve discounted to lowDomain values. Add lots more doc here. Also this doesnt really need the pair, just the x's.
double PointSetCurve::getDiscountedValue( const double lowDomain, const double highDomain, const double discountRate ) const {
    // <Formula> pv(x, yearsInFuture, discountRate) = x / ( 1 + discountRate )^n
    typedef pair<double,double> PointPair;
    typedef vector<PointPair>::const_iterator PPIter;
    double sum = 0;

    // Get the underlying points in this range.
    vector<PointPair> sortedPoints = pointSet->getSortedPairs( lowDomain, highDomain );

    if( !sortedPoints.empty() ){
        // If lowdomain is not the first point, add a point to represent represent that.
        if( !util::isEqual( sortedPoints[ 0 ].first, lowDomain ) ){
            PointPair pointA( lowDomain, getY( lowDomain ) );
            sortedPoints.insert( sortedPoints.begin(), pointA );
        }

        // If the highDomain is not the last point, add a point to represent that.
        if( !util::isEqual( sortedPoints[ sortedPoints.size() - 1 ].first, highDomain ) ){
            PointPair pointB( highDomain, getY( highDomain ) );
            sortedPoints.push_back( pointB );
        }

        // Set the base year.
        const double baseYear = sortedPoints[ 0 ].first;

        // Now iterate through the vector and discount.
        for( PPIter point = sortedPoints.begin(); point != sortedPoints.end(); point++ ){
            for( double year = point->first; year < ( point + 1 )->first; year++ ){
                double currValue = getY( year );
                sum += ( currValue / pow( 1 + discountRate, year - baseYear ) );
            }
        }
    }
    return sum;
}
//! Print out the curve to an XML File
void PointSetCurve::toXML( ostream& out, Tabs* tabs ) const {
    // This is a temporary hack. 
    XMLWriteOpeningTag( getXMLTagName(), out, tabs, static_cast<int>( numericalLabel ), title );
    pointSet->toXML( out, tabs );
    XMLWriteClosingTag( getXMLTagName(), out, tabs );
}

//! Perform a linear interpolation determining a y value.
double PointSetCurve::linearInterpolateY( const double xVal, const double x1, const double y1, const double x2, const double y2 ) {
    const double slope = getSlope( x1, y1, x2, y2 );
    const double yIntercept = -1 * slope * x1;
    return ( slope * xVal + yIntercept );
}

//! Perform a linear interpolation determining an x value.
double PointSetCurve::linearInterpolateX( const double yVal, const double x1, const double y1, const double x2, const double y2 ) {
    const double slope = getSlope( x1, y1, x2, y2 );
    const double yIntercept = -1 * slope * x1;
    return( ( yVal - yIntercept ) / slope );
}

//! Determine the slope of a line.
double PointSetCurve::getSlope( const double x1, const double y1, const double x2, const double y2 ) {
    return ( y2 - y1 ) / ( x2 - x1 );
}

//! Determine the y intercept of a line based on a slope and a point.
double PointSetCurve::getYIntercept( const double slope, const double x1, const double y1 ){
    return ( y1 - slope * x1 );
}

//! Determine the x intercept of a line based on a slope and a point.
double PointSetCurve::getXIntercept( const double slope, const double x1, const double y1 ){
    const double yIntercept = getYIntercept( slope, x1, y1 );
    return ( -1 * yIntercept / slope );
}

void PointSetCurve::print( std::ostream& out, const double lowDomain, const double highDomain, const double lowRange, const double highRange, const int minPoints ) const {

    // Get the sorted points in the specified range.
    vector<pair<double,double> > sortedPairs = pointSet->getSortedPairs( lowDomain, highDomain, minPoints );
    typedef vector<pair<double,double> >::iterator pairIter;
    typedef vector<pair<double,double> >::const_iterator constPairIter;

    // Remove any points not in the specified range.
    for( pairIter iter = sortedPairs.begin(); iter != sortedPairs.end(); iter++ ){
        // If it is out of range remove it.
        if( ( iter->second < lowRange ) || ( iter->second > highRange ) ){
            sortedPairs.erase( iter );
        }
    }

    // Now print the labels.
    out << title << endl;
    out << xAxisLabel << "," << yAxisLabel << endl;

    // Print the points
    for( constPairIter iter = sortedPairs.begin(); iter != sortedPairs.end(); iter++ ){
        out << iter->first << "," << iter->second << endl;
    }
    out << endl;
}