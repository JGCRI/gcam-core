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
#include <xercesc/dom/DOMNode.hpp>
#include "util/curves/include/point_set.h"

class Tabs;
class DataPoint;

/*!
* \ingroup Util
* \brief A PointSet subclass which can be described as a set of points. 
* \author Josh Lurz
*/

class ExplicitPointSet: public PointSet {
    friend std::ostream& operator<<( std::ostream& os, const ExplicitPointSet& pointSet ) {
        pointSet.print( os );
        return os;
    }
public:
    ExplicitPointSet();
    ExplicitPointSet( const ExplicitPointSet& rhs );
    ~ExplicitPointSet();
    ExplicitPointSet& operator=( const ExplicitPointSet& rhs );
    bool operator==( const ExplicitPointSet& rhs ) const;
    bool operator!=( const ExplicitPointSet& rhs ) const;
    ExplicitPointSet* clone() const;
    static const std::string& getXMLNameStatic();
    bool addPoint( DataPoint* dataPoint );
    double getY( const double xValue ) const;
    double getX( const double yValue ) const;
    bool setY( const double xValue, const double yValue );
    bool setX( const double yValue, const double xValue );
    bool removePointFindX( const double xValue );
    bool removePointFindY( const double yValue );
    double getMaxX() const;
    double getMaxY() const;
    double getMinX() const;
    double getMinY() const;
    std::vector<std::pair<double,double> > getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const;
    bool containsX( const double x ) const;
    bool containsY( const double y ) const;
    double getNearestXBelow( const double x ) const;
    double getNearestXAbove( const double x ) const;
    double getNearestYBelow( const double x ) const;
    double getNearestYAbove( const double x ) const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void XMLParse( const xercesc::DOMNode* node );
    void invertAxises();
protected:   
    static const std::string XML_NAME; //!< The name of the XML tag associated with this object.
    std::vector<DataPoint*> points;
    typedef std::vector<DataPoint*>::iterator DataPointIterator;
    typedef std::vector<DataPoint*>::const_iterator DataPointConstIterator;
	const std::string& getXMLName() const;
	void copy( const ExplicitPointSet& rhs );
    void clear();
	const DataPoint* findX( const double xValue ) const;
    DataPoint* findX( const double xValue );
    const DataPoint* findY( const double yValue ) const;
    DataPoint* findY( const double yValue );
    void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const;
};
#endif // _EXPLICIT_POINT_SET_H_
