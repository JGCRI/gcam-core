#ifndef _POINT_SET_CURVE_H_
#define _POINT_SET_CURVE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file point_set_curve.h
* \ingroup Util
* \brief The PointSetCurve class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>
#include <xercesc/dom/DOMNode.hpp>
#include "util/curves/include/curve.h"

class PointSet;
class Tabs;

/*!
* \ingroup Util
* \brief This class defines a curve based on a set of points.  
* \author Josh Lurz
*/

class PointSetCurve: public Curve {
    friend std::ostream& operator<<( std::ostream& os, const PointSetCurve& curve ){
        curve.print( os );
        return os;
    }
public:
    PointSetCurve( PointSet* pointSetIn = 0 );
    PointSetCurve( const PointSetCurve& curveIn );
    PointSetCurve( const std::string pointSetType, const std::string dataPointType, const std::vector<double> yValues, const double xStart, const double xInterval );
    ~PointSetCurve();
    PointSetCurve* clone() const;
    static const std::string& getXMLNameStatic();
    const std::string& getXMLName() const;
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    bool XMLParseDerived( const xercesc::DOMNode* node );
    double getY( const double xValue ) const;
    double getX( const double yValue ) const;
    bool setY( const double xValue, const double yValue );
    bool setX( const double yValue, const double xValue );
    double getSlope( const double x1, const double x2 ) const;
    double getMaxX() const;
    double getMaxY() const;
    double getMinX() const;
    double getMinY() const;
    std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    double getIntegral( const double lowDomain, const double highDomain ) const;
    double getDiscountedValue( const double lowDomain, const double highDomain, const double discountRate ) const;
    void invertAxises();
protected:
    static const std::string XML_NAME; //!< The name of the XML tag associated with this object.
    PointSet* pointSet;
    static double linearInterpolateY( const double xVal, const double x1, const double y1, const double x2, const double y2 );
    static double linearInterpolateX( const double yVal, const double x1, const double y1, const double x2, const double y2 );
    static double getSlope( const double x1, const double y1, const double x2, const double y2 );
    static double getYIntercept( const double slope, const double x1, const double y1 );
    static double getXIntercept( const double slope, const double x1, const double y1 );
    void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
};
#endif // _POINT_SET_CURVE_H_