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
#include "util/curves/include/curve.h"

class PointSet;
class Tabs;

/*!
* \ingroup CIAM
* \brief This class defines a curve based on a set of points.  
* \author Josh Lurz
*/

class PointSetCurve: public Curve {
    friend std::ostream& operator<<( std::ostream& os, const PointSetCurve& curve ){
        curve.print( os );
        return os;
    }
public:
    PointSetCurve( PointSet* pointSetIn );
    PointSetCurve( const PointSetCurve& curveIn );
    ~PointSetCurve();
    double getY( const double xValue ) const;
    double getX( const double yValue ) const;
    bool setY( const double xValue, const double yValue );
    bool setX( const double yValue, const double xValue );
    const std::vector<double> getXCoords( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    const std::vector<double> getYCoords( const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
    std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    const std::string getTitle() const;
    void setTitle( const std::string& titleIn );
    double getNumericalLabel() const;
    void setNumericalLabel( const double numericalLabelIn );
    const std::string getXAxisLabel() const;
    const std::string getYAxisLabel() const;
    void setXAxisLabel( const std::string& xLabelIn );
    void setYAxisLabel( const std::string& yLabelIn );
    const std::string getXAxisUnits() const;
    const std::string getYAxisUnits() const;
    void setXAxisUnits( const std::string& xUnitsIn );
    void setYAxisUnits( const std::string& yUnitsIn );
    double getIntegral( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX ) const;
    double getDiscountedValue( const double lowDomain, const double highDomain, const double discountRate ) const;
    void toXML( std::ostream& out, Tabs* tabs ) const ;
protected:    
    PointSet* pointSet;
    std::string XML_ELEMENT_NAME;
    std::string title;
    double numericalLabel;
    std::string xAxisLabel;
    std::string yAxisLabel;
    std::string xAxisUnits;
    std::string yAxisUnits;
    static double linearInterpolateY( const double xVal, const double x1, const double y1, const double x2, const double y2 );
    static double linearInterpolateX( const double yVal, const double x1, const double y1, const double x2, const double y2 );
    static double getSlope( const double x1, const double y1, const double x2, const double y2 );
    static double getYIntercept( const double slope, const double x1, const double y1 );
    static double getXIntercept( const double slope, const double x1, const double y1 );
    void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
};
#endif // _POINT_SET_CURVE_H_