#ifndef _EXPLICIT_POINT_SET_H_
#define _EXPLICIT_POINT_SET_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file explicit_point_set.h
* \ingroup Util
* \brief The ExplicitPointSet class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \todo Add an iterator??
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>
#include <functional>
#include "util/curves/include/point_set.h"

class Tabs;

/*!
* \ingroup CIAM
* \brief A PointSet subclass which can be described as a set of points. 
* \author Josh Lurz
*/

class ExplicitPointSet: public PointSet {
    friend class ExplicitPointSetTests;
    friend std::ostream& operator<<( std::ostream& os, const ExplicitPointSet& pointSet ) {
        pointSet.print( os );
        return os;
    }
public:
    class DataPoint {
        friend std::ostream& operator<<( std::ostream& os, const DataPoint& dataPoint ){
            dataPoint.print( os );
            return os;
        }
    public:
        DataPoint(){};
        virtual ~DataPoint(){};
        bool operator==( const DataPoint& rhs ) const {
            return( ( getX() == rhs.getX() ) && ( ( getY() == rhs.getY() ) ) );
        }
        bool operator!=( const DataPoint& rhs ) const {
            return !( *this == rhs );
        }
        virtual DataPoint* clone() const = 0;
        virtual double getX() const = 0;
        virtual double getY() const = 0;
        virtual void setX( const double xValue ) = 0;
        virtual void setY( const double yValue ) = 0;
        virtual void toXML( std::ostream& out, Tabs* tabs ) const = 0;
        /*!
        * \brief Binary comparison operator used for DataPoint pointers to order by increasing x values. 
        * \author Josh Lurz
        */  
        struct LesserX : public std::binary_function<DataPoint*,DataPoint*,bool>
        {
            //! Operator which performs comparison. 
            bool operator()( const DataPoint* lhs, const DataPoint* rhs ) const
            {   
                return lhs->getX() < rhs->getX();
            }
        };
    protected:
        virtual void print( std::ostream& out ) const = 0;
    };
    

    ExplicitPointSet();
    ExplicitPointSet( const ExplicitPointSet& rhs );
    ~ExplicitPointSet();
    ExplicitPointSet& operator=( const ExplicitPointSet& rhs );
    bool operator==( const ExplicitPointSet& rhs ) const;
    bool operator!=( const ExplicitPointSet& rhs ) const;
    void copy( const ExplicitPointSet& rhs );
    void clear();
    bool addPoint( DataPoint* dataPoint );
    double getY( const double xValue ) const;
    double getX( const double yValue ) const;
    bool setY( const double xValue, const double yValue );
    bool setX( const double yValue, const double xValue );
    bool removePointFindX( const double xValue );
    bool removePointFindY( const double yValue );
    const std::vector<double> getXCoords( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    const std::vector<double> getYCoords( const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
    std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    bool containsX( const double x ) const;
    bool containsY( const double y ) const;
    double getNearestXBelow( const double x ) const;
    double getNearestXAbove( const double x ) const;
    double getNearestYBelow( const double x ) const;
    double getNearestYAbove( const double x ) const;
    void toXML( std::ostream& out, Tabs* tabs ) const;
protected:   
    std::vector<DataPoint*> points;
    const DataPoint* findX( const double xValue ) const;
    DataPoint* findX( const double xValue );
    const DataPoint* findY( const double yValue ) const;
    DataPoint* findY( const double yValue );
    void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
};
#endif // _EXPLICIT_POINT_SET_H_