#ifndef _DATA_POINT_H_
#define _DATA_POINT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file data_point.h
* \ingroup Util
* \brief The DataPoint class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <functional>

class Tabs;

/*!
* \ingroup Util
* \brief An abstract base class which defines a single point. 
* \author Josh Lurz
*/

class DataPoint {
        friend std::ostream& operator<<( std::ostream& os, const DataPoint& dataPoint ){
            dataPoint.print( os );
            return os;
        }
    public:
        DataPoint();
        virtual ~DataPoint();
        static const std::string& getXMLNameStatic();
        virtual const std::string& getXMLName() const;
        static DataPoint* getDataPoint( const std::string& type );
        virtual bool operator==( const DataPoint& rhs ) const;
        virtual bool operator!=( const DataPoint& rhs ) const;
        virtual bool operator<( const DataPoint& rhs ) const;
        virtual bool operator>( const DataPoint& rhs ) const;
        virtual bool operator<=( const DataPoint& rhs ) const;
        virtual bool operator>=( const DataPoint& rhs ) const;
        virtual DataPoint* clone() const = 0;
        virtual double getX() const = 0;
        virtual double getY() const = 0;
        virtual void setX( const double xValue ) = 0;
        virtual void setY( const double yValue ) = 0;
        virtual void toXML( std::ostream& out, Tabs* tabs ) const = 0;
        virtual void XMLParse( const xercesc::DOMNode* node ) = 0;
        virtual void invertAxises() = 0;
        
        /*!
        * \brief Binary comparison operator used for DataPoint pointers to order by increasing values. 
        * \author Josh Lurz
        */  
        struct Lesser : public std::binary_function<DataPoint*,DataPoint*,bool>
        {
            //! Operator which performs comparison. 
            bool operator()( const DataPoint* lhs, const DataPoint* rhs ) const
            {   
                return ( ( lhs->getX() < rhs->getX() ) || ( ( lhs->getX() == rhs->getX() ) && ( lhs->getY() < rhs->getY() ) ) );
            }
        };
        /*!
        * \brief Binary comparison operator which compares two DataPoints by least X value.
        * \author Josh Lurz
        */  
        struct LesserX : public std::binary_function<DataPoint*,DataPoint*,bool>
        {
            //! Operator which performs comparison. 
            bool operator()( const DataPoint* lhs, const DataPoint* rhs ) const
            {   
                return ( lhs->getX() < rhs->getX() );
            }
        };
        /*!
        * \brief Binary comparison operator which compares two DataPoints by least Y value.
        * \author Josh Lurz
        */  
        struct LesserY : public std::binary_function<DataPoint*,DataPoint*,bool>
        {
            //! Operator which performs comparison. 
            bool operator()( const DataPoint* lhs, const DataPoint* rhs ) const
            {   
                return ( lhs->getY() < rhs->getY() );
            }
        };

    protected:
        static const std::string XML_NAME; //!< The name of the XML tag associated with this object.
        virtual void print( std::ostream& out ) const = 0;
    };
#endif // _DATA_POINT_H_