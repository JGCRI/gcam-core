#ifndef _XY_DATA_POINT_H_
#define _XY_DATA_POINT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file xy_data_point.h
* \ingroup Util
* \brief The XYDataPoint class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include "util/curves/include/explicit_point_set.h"

/*!
* \ingroup CIAM
* \brief A PointSet subclass which defines a simple x, y point.
* \author Josh Lurz
*/

class XYDataPoint: public ExplicitPointSet::DataPoint {
        friend std::ostream& operator<<( std::ostream& os, const XYDataPoint& dataPoint ){
            dataPoint.print( os );
            return os;
        }
    public:
        XYDataPoint( const double xIn = 0, const double yIn = 0 );
        ~XYDataPoint();
        bool operator==( const XYDataPoint& rhs ) const;
        bool operator!=( const XYDataPoint& rhs ) const;
	    DataPoint* XYDataPoint::clone() const;
        double getX() const;
        double getY() const;
        void setX( const double xValue );
        void setY( const double yValue );
        void toXML( std::ostream& out, Tabs* tabs ) const;
    protected:
        double x;
        double y;
        void print( std::ostream& out ) const;
    };
#endif // _XY_POINT_H_