#ifndef _POINT_SET_H_
#define _POINT_SET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file point_set.h
* \ingroup Util
* \brief The PointSet class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>
#include <iosfwd>

class Tabs;

/*!
* \ingroup CIAM
* \brief A class which defines the interface to set of points. 
* \note This is not a multiset, so only unique points are allowed.
* \author Josh Lurz
*/

class PointSet {
    friend std::ostream& operator<<( std::ostream& os, const PointSet& pointSet ) {
        pointSet.print( os );
        return os;
    }
public:
    PointSet(){};
    virtual ~PointSet(){};
    virtual double getY( const double xValue ) const = 0;
    virtual double getX( const double yValue ) const = 0;
    virtual bool setY( const double xValue, const double yValue ) = 0;
    virtual bool setX( const double yValue, const double xValue ) = 0;
    virtual bool removePointFindX( const double xValue ) = 0;
    virtual bool removePointFindY( const double yValue ) = 0;
    virtual const std::vector<double> getXCoords( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual const std::vector<double> getYCoords( const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual bool containsX( const double x ) const = 0;
    virtual bool containsY( const double y ) const = 0;    
    virtual double getNearestXBelow( const double x ) const = 0;
    virtual double getNearestXAbove( const double x ) const = 0;
    virtual double getNearestYBelow( const double x ) const = 0;
    virtual double getNearestYAbove( const double x ) const = 0;
    virtual void toXML( std::ostream& out, Tabs* tabs ) const = 0;
protected:    
    virtual void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;
};



#endif // _POINT_SET_H_