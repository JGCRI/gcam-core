#ifndef _CURVE_H_
#define _CURVE_H_
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
* \file curve.h
* \ingroup Util
* \brief The Curve class header file.
* \author Josh Lurz
*/

#include <vector>
#include <iosfwd>
#include <string>
#include <cfloat>
#include <functional>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/data_definition_util.h"

class Tabs;

// Need to forward declare the subclasses as well.
class PointSetCurve;

/*!
* \ingroup Util
* \brief A class which defines the interface to a curve. 
* \author Josh Lurz
*/

class Curve : private boost::noncopyable {
    friend std::ostream& operator<<( std::ostream& os, const Curve& curve ){
        curve.print( os );
        return os;
    }
public:
    typedef std::vector<std::pair<double,double> > SortedPairVector;
    Curve();
    virtual ~Curve();
    virtual bool operator==( const Curve& rhs ) const;
    virtual bool operator!=( const Curve& rhs ) const;
    virtual Curve* clone() const = 0;
    static Curve* getCurve( const std::string& type );
    static const std::string& getXMLNameStatic();
    virtual const std::string& getXMLName() const;
    void outputAsXML( std::ostream& aOut, Tabs* aTabs ) const;
    virtual void outputAsXMLDerived( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void invertAxises() = 0;
    const std::string getName() const;
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
    
    virtual double getX( const double yValue ) const = 0;
    virtual double getY( const double xValue ) const = 0;
    virtual bool setX( const double yValue, const double xValue ) = 0;
    virtual bool setY( const double xValue, const double yValue ) = 0;
    virtual double getMaxX() const = 0;
    virtual double getMaxY() const = 0;
    virtual double getMinX() const = 0;
    virtual double getMinY() const = 0;
    virtual SortedPairVector getSortedPairs( const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX, const int minPoints = 0 ) const = 0;
    virtual double getIntegral( const double lowDomain, const double highDomain ) const = 0;
    virtual double getDiscountedValue( const double lowDomain, const double highDomain, const double discountRate ) const = 0;
    virtual double getSlope( const double x1, const double x2 ) const = 0;
    virtual double getHammingDistance( const Curve* otherCurve, const double xStart, const double xEnd, const double xInterval ) const;

protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Curve to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Curve, PointSetCurve ),
                
        DEFINE_VARIABLE( SIMPLE, "numericalLabel", numericalLabel, double ),
        DEFINE_VARIABLE( SIMPLE, "name", name, std::string ),
        DEFINE_VARIABLE( SIMPLE, "title", title, std::string ),
        DEFINE_VARIABLE( SIMPLE, "xAxisLabel", xAxisLabel, std::string ),
        DEFINE_VARIABLE( SIMPLE, "yAxisLabel", yAxisLabel, std::string ),
        DEFINE_VARIABLE( SIMPLE, "xAxisUnit", xAxisUnits, std::string ),
        DEFINE_VARIABLE( SIMPLE, "yAxisUnit", yAxisUnits, std::string )
    )
    
    void copy( const Curve& aOther );

    virtual void print( std::ostream& out, const double lowDomain = -DBL_MAX, const double highDomain = DBL_MAX,
        const double lowRange = -DBL_MAX, const double highRange = DBL_MAX, const int minPoints = 0 ) const = 0;

     /*!
    * \brief Binary comparison operator which compares two Curves by least minimum X value.
    * \author Josh Lurz
    */  
    struct LesserMinX: public std::binary_function<Curve*,Curve*,bool>
    {
        //! Operator which performs comparison. 
        bool operator()( const Curve* lhs, const Curve* rhs ) const
        {   
            return ( lhs->getMinX() < rhs->getMinX() );
        }
    };
    /*!
    * \brief Binary comparison operator which compares two Curves by least minimum Y value.
    * \author Josh Lurz
    */  
    struct LesserMinY: public std::binary_function<Curve*,Curve*,bool>
    {
        //! Operator which performs comparison. 
        bool operator()( const Curve* lhs, const Curve* rhs ) const
        {   
            return ( lhs->getMinY() < rhs->getMinY() );
        }
    };
    /*!
    * \brief Binary comparison operator which compares two Curves by least maximum X value.
    * \author Josh Lurz
    */  
    struct LesserMaxX: public std::binary_function<Curve*,Curve*,bool>
    {
        //! Operator which performs comparison. 
        bool operator()( const Curve* lhs, const Curve* rhs ) const
        {   
            return ( lhs->getMaxX() < rhs->getMaxX() );
        }
    };
    /*!
    * \brief Binary comparison operator which compares two Curves by least maximum Y value.
    * \author Josh Lurz
    */  
    struct LesserMaxY: public std::binary_function<Curve*,Curve*,bool>
    {
        //! Operator which performs comparison. 
        bool operator()( const Curve* lhs, const Curve* rhs ) const
        {   
            return ( lhs->getMaxY() < rhs->getMaxY() );
        }
    };
};
#endif // _CURVE_H_
