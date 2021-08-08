#ifndef _XML_HELPER_H_
#define _XML_HELPER_H_
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
 * \file xml_helper.h
 * \ingroup Objects
 * \brief A set of helper function for writing xml data.
 * \note This file contains a series of global utility functions for writing XML data.
 * \todo This file needs refactoring. The XML writing utility functions should
 *       be moved to a non-static XMLWriter class. The class should store the
 *       tabs object and output stream.
 * \details This library contains a set of routines for reading xml data and
 *          attribute values. It is a templated library so that it should work
 *          with any data type.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <map>
#include <memory>
#include <typeinfo>

#include "util/base/include/model_time.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/time_vector.h"

/*!
 * \ingroup Objects
 * \brief A basic class which is a container for a variable containing the
 *        current level of indention in the xml being written.
 * \todo Replace this class with an integer.
 * \author Josh Lurz
 */
class Tabs {

private:
    //! Current number of tabs to write out in order to properly format xml.
   unsigned short mNumTabs;
   //! Number of spaces making up a tab
   unsigned short mTabWidth;
   //! Use tabs or spaces
   bool mUseTabs;
public:

   enum { DEFAULT_TAB_WIDTH = 3 };

   //! Constructor
   Tabs(
      bool           aUseTabs  = true,
      unsigned short aTabWidth = DEFAULT_TAB_WIDTH )
      : mNumTabs(0), mTabWidth(aTabWidth), mUseTabs(aUseTabs) {}

   //! Increase the current level of indentation.
   void increaseIndent() { ++mNumTabs; }

   //! Decrease the current level of indentation.
   void decreaseIndent() {
      if ( mNumTabs ) {
        --mNumTabs;
      }
   }

   /*! Write out the contained number of tabs to the specified output stream.
    *
    * \param out Stream to which to write the tabs->
    * \return void
    */
   void writeTabs( std::ostream& out ) const {
      if ( mUseTabs ) {
         for ( int i = 0; i != mNumTabs; ++i ) {
            out << "\t";
         }
      }
      else if ( mTabWidth ) {
         std::string::size_type  n = mTabWidth * mNumTabs;
         if ( n ) {
            std::string spaces( n, ' ' );
            out << spaces;
         }
      }
   }
};

//! Function to write the argument element to xml in proper format.
/*!
* This function is used to write a single element containing a single value and an optional year to the output stream
* in XML. If the year is not passed in, the function will not print the year attribute.
* \param value Value to print to XML.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs.
* \param year Optional year value to print as an attribute.
* \param name Optional name value to print as an attribute.
* \param fillout Optional attribute which specifies the value should be applied to all following time periods.
*/
template<class T>
void XMLWriteElement( const T value, const std::string elementName, std::ostream& out, const Tabs* tabs, const int year = 0, const std::string name = "", const bool fillout = false ) {

   tabs->writeTabs( out );

   out << "<" << elementName;

   if ( name != "" ) {
      out << " name=\"" << name << "\"";
   }

   if( year != 0 ){
      out << " year=\"" << year << "\"";
   }
   if( fillout ){
       out << " fillout=\"" << 1 << "\"";
   }
   out << ">";

   out << value;

   out << "</" << elementName << ">" << std::endl;
}
//! Function to write the argument element to xml with a integer attribute in proper format.
/*!
* This function is used to write a single element containing a single value along with an integer attribute to the output stream
* in XML.
* \param value Value to print to XML.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs.
* \param aAttrs Map of attribute name to attribute value.
*/
template<class T, class U>
void XMLWriteElementWithAttributes( const T value, const std::string elementName,
                                   std::ostream& out, const Tabs* tabs,
                                   const std::map<std::string, U> aAttrs )
{
    tabs->writeTabs( out );
    out << "<" << elementName;
    typedef typename std::map<std::string, U>::const_iterator MapIterator;
    for( MapIterator entry = aAttrs.begin(); entry != aAttrs.end(); ++entry ){
        out << " " << entry->first <<"=\"" << entry->second << "\"";
    }
    out << ">" << value << "</" << elementName << ">" << std::endl;
}

/*! \brief Write an element XML tag.
* \details This function is used to write an XML element tag and an optional name and tag type to the output stream.
* The name and tag type are optional attributes. The name and tag type may be left out, but the tag will not be written
* without both tag type and tag name present. If the arguments are left out, the function will not write the attribute.
* The function increases the indent level after writing the tag so that subsequent elements are correctly indented.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param Tabs The number of tabs to print before the element.
* \param tagType Optional tag type value to print as an attribute.
* \param typeName Optional type name value to print as an attribute.
*/

inline void XMLWriteElementTag( const std::string& elementName, std::ostream& out, Tabs* tabs, const std::string& tagType = "", const std::string& typeName = "")
{
   tabs->writeTabs( out );

   out << "<" << elementName;

   if ( ( typeName != "" ) &&  ( tagType != "") ){
	   out << " " << tagType << "=\"" << typeName << "\"";
		   
   }
   out << ">" << std::endl;
   tabs->increaseIndent();
}

/*!  \brief Write a closing XML tag.
* \details This function is used to write a closing XML element tag. It decreases the indent before writing the tag,
* and adds a newline.
* \note Closing tags cannot have attributes.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param tabs The number of tabs to print before the element.
*/
inline void XMLWriteCloseElementTag( std::string& elementName, std::ostream& out, Tabs* tabs)
{
    tabs->decreaseIndent();
    tabs->writeTabs( out );
    out << "</" << elementName;
    out << ">" << std::endl;
}

/*! \brief Write an opening XML tag.
* \details This function is used to write an opening XML tag and an optional name and year to the output stream.
* The name and year are optional attributes. The name and year may be left out, or only the year may be left out, but
* the year cannot be written without a year unless the empty string is included in the function arguments, due
* to the way that C++ default arguments work. If the arguments are left out, the function will not write the attribute.
* The function increases the indent level after writing the tag so that subsequent elements are correctly indented.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param Tabs The number of tabs to print before the element.
* \param name Optional name value to print as an attribute.
* \param year Optional year value to print as an attribute.
* \param type Optional type name to print as an attribute.
*/
inline void XMLWriteOpeningTag( const std::string& elementName, std::ostream& out, Tabs* tabs, const std::string& name = "", const int year = 0, const std::string& type = "" ) {

    tabs->writeTabs( out );
    out << "<" << elementName;

    if( year ){
        out << " year=\"" << year << "\"";
    }
    if ( name != "" ){
        out << " name=\"" << name << "\"";
    }
    if( type != "" ){
        out << " type=\"" << type << "\"";
    }
    out << ">" << std::endl;
    tabs->increaseIndent();
}

/*! \brief Write an opening XML tag.
 * \details This function is used to write an opening XML tag and attributes specified as a map of strings
 * where key is attribute name and value is attribute value. The function increases the indent level after
 * writing the tag so that subsequent elements are correctly indented.
 * \param aElementName Name of the element.
 * \param aOut Stream to print to.
 * \param aTabs The number of tabs to print before the element.
 * \param aAttrs A map of attributes to include where key is attribute name and value is attribute value.
 */
inline void XMLWriteOpeningTag( const std::string& aElementName, std::ostream& aOut, Tabs* aTabs, const std::map<std::string, std::string>& aAttrs ) {
    
    aTabs->writeTabs( aOut );
    aOut << "<" << aElementName;

    for( auto iter = aAttrs.begin(); iter != aAttrs.end(); ++iter ) {
        aOut << " " << (*iter).first << "=\"" << (*iter).second << "\"";
    }
    aOut << ">" << std::endl;
    aTabs->increaseIndent();
}

/*!  \brief Write a closing XML tag.
* \details This function is used to write a closing XML tag. It decreases the indent before writing the tag,
* and adds a newline.
* \note Closing tags cannot have attributes.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param tabs The number of tabs to print before the element.
*/
inline void XMLWriteClosingTag( const std::string& elementName, std::ostream& out, Tabs* tabs ) {

    tabs->decreaseIndent();
    tabs->writeTabs( out );
    out << "</" << elementName;
    out << ">" << std::endl;
}

//! Function to write the argument element to xml in proper format if it is not equal to the default value for the element..
/*!
* This function is used to write a single element containing a single value and an optional year to the output stream
* in XML if the value is not equal to the default value.. If the year is not passed in, the function will not print the year attribute.
* \param value Value to print to XML.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs.
* \param defaultValue Default value to compare the value to.
* \param year Optional year value to print as an attribute.
* \param name Optional name value to print as an attribute.
* \param fillout Optional boolean whether to add the fillout attribute with a true value.
*/
template<class T>
void XMLWriteElementCheckDefault( const T value, const std::string elementName, std::ostream& out, const Tabs* tabs, const T defaultValue = T(), const int year = 0, const std::string name = "", const bool fillout = false ) {
   if( !util::isEqual( value, defaultValue ) ) {
       XMLWriteElement( value, elementName, out, tabs, year, name, fillout );
   }
}

/*!
* \brief Function which writes out the values contained in a vector.
* \details This function is used to write out the values of a vector in XML format, along with their year tag.
* The function will also avoid writing out elements if they all have default values, and will collapse consecutive
* equal values into one element with a fillout attribute.
* \param aOutputVector The vector of values to write out.
* \param aElementName The elementName to write out for each value.
* \param aOut Stream to print to.
* \param aTabs A tabs object responsible for printing the correct number of tabs.
* \param modeltime A pointer to the global modeltime object.
* \param aDefaultValue Default value for items in this vector.
*/
template<class T>
void XMLWriteVector( const std::vector<T>& aOutputVector, const std::string& aElementName, std::ostream& aOut, Tabs* aTabs, const Modeltime* modeltime, const T aDefaultValue = T() ) {

    for( unsigned int i = 0; i < aOutputVector.size(); i++ ){
        // Determine the correct year.
        unsigned int year = modeltime->getper_to_yr( i );

        // Determine if we can use fillout.
        unsigned int canSkip = 0;
        for( unsigned int j = i + 1; j < aOutputVector.size(); j++ ){
            if( util::isEqual( aOutputVector.at( i ), aOutputVector.at( j ) ) ){
                canSkip++;
            }
            else {
                break;
            }
        }
        if( canSkip > 0 ){
            // Need to write out the default value because they could be cleared
            // by an earlier fillout.
            XMLWriteElement( aOutputVector.at( i ), aElementName, aOut, aTabs, year, "", true );
            // If canSkip gets to last element in vector, value of i forces breakout of first FOR loop.
            i += canSkip;
        }
        else {
            // Can't skip at all or can't skip anymore.
            // Write out default if fillout and other values are being used together,
            // or if fillout is not used and all values are unique.
            XMLWriteElement( aOutputVector.at( i ), aElementName, aOut, aTabs, year, "", false );
        }
    }
}

/*!
* \brief Function which writes out the values contained in a YearVector.
* \details This function is used to write out the values of a YearVector in XML
*          format, along with their year tag. The function will also avoid
*          writing out elements if they all have default values, and will collapse
*          consecutive equal values into one element with a fillout attribute.
* \param aOutputVector The YearVector of values to write out.
* \param aElementName The elementName to write out for each value.
* \param aOut Stream to print to.
* \param aTabs A tabs object responsible for printing the correct number of
*        tabs.
* \param aFirstYear The year of the first element in the vector.
* \param aDefaultValue Default value for items in this vector.
*/
template<class T>
void XMLWriteVector( const objects::YearVector<T>& aOutputVector,
                     const std::string& aElementName,
                     std::ostream& aOut,
                     Tabs* aTabs,
                     const unsigned int aFirstYear,
                     const T aDefaultValue = T() )
{
    for( unsigned int i = aFirstYear; i < aFirstYear + aOutputVector.size(); ++i ){
        // Determine if we can use fillout.
        unsigned int canSkip = 0;
        for( unsigned int j = i + 1; j < aFirstYear + aOutputVector.size(); ++j ){
            if( util::isEqual( aOutputVector[ i ], aOutputVector[ j ] ) ){
                ++canSkip;
            }
            else {
                break;
            }
        }
        if( canSkip > 0 ){
            // Need to write out the default value because they could be cleared
            // by an earlier fillout.
            XMLWriteElement( aOutputVector[ i ], aElementName, aOut, aTabs, i, "", true );
            // If canSkip gets to last element in vector, value of i forces breakout of first FOR loop.
            i += canSkip;
        }
        else {
            // Can't skip at all or can't skip anymore.
            // Write out default if fillout and other values are being used together,
            // or if fillout is not used and all values are unique.
            XMLWriteElement( aOutputVector[ i ], aElementName, aOut, aTabs, i, "", false );
        }
    }
}

/*!
* \brief Function which writes out the values contained in a PeriodVector.
* \details This function is used to write out the values of a PeriodVector in XML
*          format, along with their year tag. The function will also avoid
*          writing out elements if they all have default values, and will collapse
*          consecutive equal values into one element with a fillout attribute.
* \param aOutputVector The PeriodVector of values to write out.
* \param aElementName The elementName to write out for each value.
* \param aOut Stream to print to.
* \param aTabs A tabs object responsible for printing the correct number of
*        tabs.
* \param aModeltime The Modeltime object.
* \param aDefaultValue Default value for items in this vector.
*/
template<class T>
void XMLWriteVector( const objects::PeriodVector<T>& aOutputVector,
                     const std::string& aElementName,
                     std::ostream& aOut,
                     Tabs* aTabs,
                     const Modeltime* aModeltime,
                     const T aDefaultValue = T() )
{
    for( unsigned int i = 0; i < aOutputVector.size(); i++ ){
        // Determine the correct year.
        unsigned int year = aModeltime->getper_to_yr( i );

        // Determine if we can use fillout.
        unsigned int canSkip = 0;
        for( unsigned int j = i + 1; j < aOutputVector.size(); j++ ){
            if( util::isEqual( aOutputVector[ i ], aOutputVector[ j ] ) ){
                canSkip++;
            }
            else {
                break;
            }
        }
        if( canSkip > 0 ){
            // Need to write out the default value because they could be cleared
            // by an earlier fillout.
            XMLWriteElement( aOutputVector[ i ], aElementName, aOut, aTabs, year, "", true );
            // If canSkip gets to last element in vector, value of i forces breakout of first FOR loop.
            i += canSkip;
        }
        else {
            // Can't skip at all or can't skip anymore.
            // Write out default if fillout and other values are being used together,
            // or if fillout is not used and all values are unique.
            XMLWriteElement( aOutputVector[ i ], aElementName, aOut, aTabs, year, "", false );
        }
    }
}

#endif // _XML_HELPER_H_
