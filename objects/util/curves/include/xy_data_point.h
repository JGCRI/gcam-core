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
*/

#include <iosfwd>
#include <xercesc/dom/DOMNode.hpp>
#include "util/curves/include/data_point.h"

/*!
* \ingroup Util
* \brief A DataPoint subclass which defines a simple x, y point.
* \author Josh Lurz
*/

class XYDataPoint: public DataPoint {
        friend std::ostream& operator<<( std::ostream& os, const XYDataPoint& dataPoint ){
            dataPoint.print( os );
            return os;
        }
    public:
        XYDataPoint( const double xIn = 0, const double yIn = 0 );
        ~XYDataPoint();
        const std::string& getXMLName() const;
        static const std::string& getXMLNameStatic();
        bool operator==( const XYDataPoint& rhs ) const;
        bool operator!=( const XYDataPoint& rhs ) const;
	    DataPoint* clone() const;
        double getX() const;
        double getY() const;
        void setX( const double xValue );
        void setY( const double yValue );
        void toInputXML( std::ostream& out, Tabs* tabs ) const;
        void XMLParse( const xercesc::DOMNode* node );
        void invertAxises();
    protected:
        static const std::string XML_NAME;  //!< The name of the XML tag associated with this object.
        double x;
        double y;
        void print( std::ostream& out ) const;
    };
#endif // _XY_POINT_H_
