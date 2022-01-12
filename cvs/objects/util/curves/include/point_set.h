#ifndef _POINT_SET_H_
#define _POINT_SET_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
* \file point_set.h
* \ingroup Util
* \brief The PointSet class header file.
* \author Josh Lurz
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>
#include <iosfwd>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/data_definition_util.h"

class Tabs;
class DataPoint;

// Need to forward declare the subclasses as well.
class ExplicitPointSet;

/*!
* \ingroup Util
* \brief A class which defines the interface to set of points. 
* \note This is not a multiset, so only unique points are allowed.
* \author Josh Lurz
*/

class PointSet : private boost::noncopyable {
    friend std::ostream& operator<<( std::ostream& os, const PointSet& pointSet ) {
        pointSet.print( os );
        return os;
    }
public:
    typedef std::vector<std::pair<double,double> > SortedPairVector;
    PointSet();
    virtual ~PointSet();
    virtual PointSet* clone() const = 0;
    static const std::string& getXMLNameStatic();
    virtual const std::string& getXMLName() const;
    static PointSet* getPointSet( const std::string& type );
    virtual bool addPoint( DataPoint* pointIn ) = 0;
    virtual double getY( const double xValue ) const = 0;
    virtual double getX( const double yValue ) const = 0;
    virtual bool setY( const double xValue, const double yValue ) = 0;
    virtual bool setX( const double yValue, const double xValue ) = 0;
    virtual bool removePointFindX( const double xValue ) = 0;
    virtual bool removePointFindY( const double yValue ) = 0;
    virtual double getMaxX() const = 0;
    virtual double getMaxY() const = 0;
    virtual double getMinX() const = 0;
    virtual double getMinY() const = 0;
    virtual SortedPairVector getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual bool containsX( const double x ) const = 0;
    virtual bool containsY( const double y ) const = 0;    
    virtual double getNearestXBelow( const double x ) const = 0;
    virtual double getNearestXAbove( const double x ) const = 0;
    virtual double getNearestYBelow( const double x ) const = 0;
    virtual double getNearestYAbove( const double x ) const = 0;
    virtual void outputAsXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void invertAxises() = 0;
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of PointSet to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( PointSet, ExplicitPointSet )
    )

    virtual void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;
};



#endif // _POINT_SET_H_
