#ifndef _CURVE_H_
#define _CURVE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file curve.h
* \ingroup Util
* \brief The Curve class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>

class Tabs;

/*!
* \ingroup CIAM
* \brief A class which defines the interface to a curve. 
* \author Josh Lurz
*/

class Curve {
    friend std::ostream& operator<<( std::ostream& os, const Curve& curve ){
        curve.print( os );
        return os;
    }
public:
    Curve(){};
    Curve( const Curve& curveIn ){};
    virtual ~Curve(){};
    bool operator==( const Curve& rhs ) const {
        return( getXCoords() == rhs.getXCoords() && getYCoords() == rhs.getYCoords() );
    }
    bool operator!=( const Curve& rhs ) const {
        return !( *this == rhs );
    }

    virtual double getX( const double yValue ) const = 0;
    virtual double getY( const double xValue ) const = 0;
    virtual bool setX( const double yValue, const double xValue ) = 0;
    virtual bool setY( const double xValue, const double yValue ) = 0;
    virtual const std::vector<double> getXCoords( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual const std::vector<double> getYCoords( const double lowRange= -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual const std::string getTitle() const = 0;
    virtual void setTitle( const std::string& titleIn ) = 0;
    virtual double getNumericalLabel() const = 0;
    virtual void setNumericalLabel( const double numericalLabelIn ) = 0;
    virtual const std::string getXAxisLabel() const = 0;
    virtual const std::string getYAxisLabel() const = 0;
    virtual void setXAxisLabel( const std::string& xLabelIn ) = 0;
    virtual void setYAxisLabel( const std::string& yLabelIn ) = 0;
    virtual const std::string getXAxisUnits() const = 0;
    virtual const std::string getYAxisUnits() const = 0;
    virtual void setXAxisUnits( const std::string& xUnitsIn ) = 0;
    virtual void setYAxisUnits( const std::string& yUnitsIn ) = 0;
    virtual double getIntegral( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX ) const = 0;
    virtual double getDiscountedValue( const double lowDomain, const double highDomain, const double discountRate ) const = 0;
    virtual void toXML( std::ostream& out, Tabs* tabs ) const = 0;
protected:    
    virtual void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;
};
#endif // _CURVE_H_