#ifndef _XML_HELPER_H_
#define _XML_HELPER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file xml_helper.h
* \ingroup Objects
* \brief A set of helper function for reading xml data.
* \todo This class needs an overall cleanup
* \warning This class is hacked b/c of poor MSVC template support. This makes it much uglier. 
* This library contains a set of routines for reading xml data and attribute values.
* It is a templated library so that it should work with any data type.
*
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <sstream>
#include <cassert>
#include <vector>
#include <map>
#include <memory>

#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include "boost/lexical_cast.hpp"

#include "util/base/include/model_time.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/time_vector.h"

/*!
* \ingroup Objects
* \brief A basic class which is a container for a variable containing the current level of indention in the xml being written.
* \author Josh Lurz
*/

class Tabs {
   
private:
  int numTabs; //!< Current number of tabs to write out in order to properly format xml.
public:
    //! Constructor
    Tabs() { numTabs = 0; }

   //! Increase the current level of indentation.
   void increaseIndent(){ numTabs++; }
   
   //! Decrease the current level of indentation. 
   void decreaseIndent(){ numTabs--; }
   
   //! Write out the contained number of tabs to the specified output stream.
   /*!
   \param out Stream to which to write the tabs->
   \return void
   */
   void writeTabs( std::ostream& out ) const {
      for ( int i = 0; i < numTabs; i++ ){
         out << "\t";
      }
   }
};

/*! 
* \ingroup Objects
* \brief A helper class of functions used to parse XML DOM trees.
* \author Josh Lurz
*/

template<class T>
class XMLHelper {
public:
   static T getValue( const xercesc::DOMNode* node );
   static T getAttr( const xercesc::DOMNode* node, const std::string attrName );
   static std::string safeTranscode( const XMLCh* toTranscode );

   static void insertValueIntoVector( const xercesc::DOMNode* node,
                                      std::vector<T>& insertToVector,
                                      const Modeltime* modeltime );
   
   static void insertValueIntoVector( const xercesc::DOMNode* aNode,
                                      objects::YearVector<T>& aYearVector );
   
   static void insertValueIntoVector( const xercesc::DOMNode* aNode,
                                      objects::PeriodVector<T>& aPeriodVector,
                                      const Modeltime* aModeltime );

   static int getNodePeriod ( const xercesc::DOMNode* node, const Modeltime* modeltime );
   static bool parseXML( const std::string& aXMLFile, IParsable* aModelElement );
   static const std::string& text();
   static const std::string& name();
   static void cleanupParser();
private:
    static xercesc::XercesDOMParser** getParserPointerInternal();
    static xercesc::ErrorHandler** getErrorHandlerPointerInternal();
    static void initParser();
    static xercesc::XercesDOMParser* getParser();
};

/*! \brief Returns the data value associated with the element node.
* \details This function first finds the child node of this element, a text node which contains the data value.
* It then converts the data using the stringstream library into the requested data type and returns the value.
* The function will throw an error if the input node is not an element, but will return the default value if the element has no data value. 
* \warning Since this function has a templated type only for the return argument, it must be called as getXMLValue<type>.
* \param node A pointer to a node for which to return the value.
* \return Value of type T from child of node.
* \sa getAttr
*/
template<class T>
T XMLHelper<T>::getValue( const xercesc::DOMNode* node ){
   // make sure we were passed a valid node reference which is an element.
   assert( node ); 
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // get the first child, which should contain the value.
   xercesc::DOMNode* curr = node->getFirstChild();
   
   // make sure that the above returned a TEXT_NODE, otherwise value will not be correct.
   if ( !curr || curr->getNodeType() != xercesc::DOMNode::TEXT_NODE ){
      return T();
   }

   // Attempt to convert the XML string to the appropriate type. This uses the
   // boost library lexical_cast operation, which is similar to the C++
   // static_cast, but allows conversion from a string into its numerical value.
   // This operation will fail if the string cannot be converted into the
   // expected type, in which case this function will return the default value
   // of the type.
   try {
       T returnValue = boost::lexical_cast<T>( safeTranscode( curr->getNodeValue() ) );
       return returnValue;
   }
   catch( boost::bad_lexical_cast& ) {
       try {
           // Cast the value to a string to print a more useful error message.
           // This cast should not fail because the value is read as a string.
           const std::string valueAsString = boost::lexical_cast<std::string>( safeTranscode( curr->getNodeValue() ) );
           std::cout << "Cast of node with value " << valueAsString << " to return type failed." << std::endl;
       }
       catch( boost::bad_lexical_cast& ){
           // The cast to a string should never fail because a string is read
           // in.
           assert( false );
       }
       // Set the return value to the default value.
       return T();
   }
}

/*! Returns the requested attribute of the element node passed to the function.
* \details This function searches for the attribute with name attrName of the argument node.
* It then converts it to type T and returns the value. If the function is not passed an element
* it will throw an error. If the requested attribute is not present, the function will return the default
* constructor for type T. (zero for doubles, or false for boolean)
* \warning It must be called as getXMLValue<type> because it is templated only on the return type.
* \param node A pointer to a node for which to fetch the attribute.
* \param attrName The name of the attribute to fetch.
* \return Value of type T from the attribute with name attrName of the node.
* \sa getValue
*/
template<class T>
T XMLHelper<T>::getAttr( const xercesc::DOMNode* node, const std::string attrName ) {
   /*! \pre Make sure we were passed a valid node reference. */
   assert( node );
   
   /*! \pre Make sure it is an element before we cast, if function is used correctly it will be. */
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // need to cast node to an element.
   const xercesc::DOMElement* element = static_cast<const xercesc::DOMElement*>( node );
   
   // get the attribute with the name which was passed in.
   
   XMLCh* nameChars = xercesc::XMLString::transcode( attrName.c_str() );
   xercesc::DOMAttr* nameAttr = element->getAttributeNode( nameChars );
   xercesc::XMLString::release( &nameChars );
  
   // Check if the name attribute exists.
   if( !nameAttr ){
      return T();
   }

   // Attempt to convert the XML string to the appropriate type. This uses the
   // boost library lexical_cast operation, which is similar to the C++
   // static_cast, but allows conversion from a string into its numerical value.
   // This operation will fail if the string cannot be converted into the
   // expected type, in which case this function will return the default value
   // of the type.
   try {
       T returnValue = boost::lexical_cast<T>( safeTranscode( nameAttr->getValue() ) );
       return returnValue;
   }
   catch( boost::bad_lexical_cast& ) {
       try {
           // Cast the value to a string to print a more useful error message.
           // This cast should not fail because the value is read as a string.
           const std::string valueAsString = boost::lexical_cast<std::string>( safeTranscode( nameAttr->getValue() ) );
           std::cout << "Cast of node with value " << valueAsString << " to return type failed." << std::endl;
       }
       catch( boost::bad_lexical_cast& ){
           // The cast to a string should never fail because a string is read
           // in.
           assert( false );
       }
       // Set the return value to the default value.
       return T();
   }
}

/*! 
* \brief Function which takes a node and inserts its value into the correct position 
* in an argument vector based on the year attribute.
* Updated to fill out for the rest of the time period if the fillout attribute is true.
*
* This function when passed a node, vector and modeltime object will first extract the year attribute and lookup
* the corresponding period from the modeltime object. It will then insert the item in that position in the vector.
*
* \warning Make sure the node passed as an argument as a year attribute.
* \param node A pointer to a node from which to extract the data.
* \param insertToVector A vector passed by reference in which to insert the value.
* \param modeltime A pointer to the modeltime object to use to determine the correct period.
*/

template<class T>
void XMLHelper<T>::insertValueIntoVector( const xercesc::DOMNode* node, std::vector<T>& insertToVector, const Modeltime* modeltime ) {
   
   /*! \pre Make sure we were passed a valid node reference. */
   assert( node );
   
   const int year = XMLHelper<int>::getAttr( node, "year" );
   // boolean to fill out the readin value to all the periods
   const bool fillout = XMLHelper<bool>::getAttr( node, "fillout" );
   
   // Check to make sure the year attribute returned non-zero.
   if (  year == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Year value not set for vector input tag >>" << XMLHelper<std::string>::safeTranscode( node->getNodeName() ) << "<<" << std::endl;
        return;
   }
   
   const int maxperiod = modeltime->getmaxper();
   int period = modeltime->getyr_to_per( year );

   // Check that the period returned correctly.
   // Check to make sure the year attribute returned non-zero.
   if ( !( ( period >= 0 ) && ( period < modeltime->getmaxper() ) ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Period value is out of bounds for vector input tag >>" << XMLHelper<std::string>::safeTranscode( node->getNodeName() ) << "<<" << std::endl;
        return;
   }
   
   // Check that the period is less than the size of the vector.
   assert( period < static_cast<int>( insertToVector.size() ) );
   insertToVector[ period ] =  XMLHelper<T>::getValue( node );
   
   if (fillout) {
      // Check that the max period is equal to the size of the vector.
      assert( maxperiod == static_cast<int>( insertToVector.size()) );
      // will not do if period is already last period or maxperiod
      for ( int i = period + 1; i < maxperiod; ++i ) {
         insertToVector[ i ] =  insertToVector[ period ];
      }
   }
}

/*! 
* \brief Function which takes a node and inserts its value into the correct
*        position in a YearVector based on the year XML attribute. If the XML
*        attribute "fillout" is set the data will be copied to the end of the
*        vector.
*
* This function when passed a node and YearVector will first extract the year
* attribute and will then insert the item in that position in the vector.
*
* \warning Make sure the node passed as an argument has a year attribute.
* \param aNode A pointer to a node from which to extract the data.
* \param aYearVector A YearVector passed by reference in which to insert the
*        value.
*/
template<class T>
void XMLHelper<T>::insertValueIntoVector( const xercesc::DOMNode* aNode,
                                          objects::YearVector<T>& aYearVector )
{
   /*! \pre Make sure we were passed a valid node reference. */
   assert( aNode );
   
   const int year = XMLHelper<int>::getAttr( aNode, "year" );

   // boolean to fill out the readin value to all the periods
   const bool fillout = XMLHelper<bool>::getAttr( aNode, "fillout" );
   
   // Check to make sure the year attribute returned non-zero.
   if( year == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Year value not set for vector input tag: "
                 << XMLHelper<std::string>::safeTranscode( aNode->getNodeName() )
                 << "." << std::endl;
        return;
   }
   // Ensure that the year is legal.
   typename objects::YearVector<T>::iterator pos = aYearVector.find( year );
   if( pos == aYearVector.end() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Year value is out of bounds for input tag:" 
                << XMLHelper<std::string>::safeTranscode( aNode->getNodeName() )
                << "." << std::endl;
        return;
   }

   // Get the value for the node.
   double value = XMLHelper<T>::getValue( aNode );

   // Assign the value into the vector.
   *pos = value;
   
   if( fillout ) {
       // Set the value for all years after the current point.
       ++pos;
       for( ; pos != aYearVector.end(); ++pos ){
           *pos = value;
       }
   }
}

/*! 
* \brief Function which takes a node and inserts its value into the correct
*        position in a PeriodVector based on the year XML attribute. If the XML
*        attribute "fillout" is set the data will be copied to the end of the
*        vector.
* \details This function when passed a node and PeriodVector will first extract
*          the year attribute and will then insert the item in that position in
*          the vector.
* \warning Make sure the node passed as an argument has a year attribute.
* \param aNode A pointer to a node from which to extract the data.
* \param aPeriod A PeriodVector passed by reference in which to insert the
*        value.
* \param aModeltime Modeltime object.
*/
template<class T>
void XMLHelper<T>::insertValueIntoVector( const xercesc::DOMNode* aNode,
                                          objects::PeriodVector<T>& aPeriodVector,
                                          const Modeltime* aModeltime )
{
   
   /*! \pre Make sure we were passed a valid node reference. */
   assert( aNode );
   
   const int year = XMLHelper<int>::getAttr( aNode, "year" );
   // boolean to fill out the readin value to all the periods
   const bool fillout = XMLHelper<bool>::getAttr( aNode, "fillout" );
   
   // Check to make sure the year attribute returned non-zero.
   if( year == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Year value not set for vector input tag >>"
                << XMLHelper<std::string>::safeTranscode( aNode->getNodeName() )
                << "<<" << std::endl;
        return;
   }
   
   int period = aModeltime->getyr_to_per( year );

   // Check that the period returned correctly.
   // Check to make sure the year attribute returned non-zero.
   if ( !( ( period >= 0 ) && ( period < static_cast<int>( aPeriodVector.size() ) ) ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Period value is out of bounds for vector input tag >>"
                << XMLHelper<std::string>::safeTranscode( aNode->getNodeName() )
                << "<<" << std::endl;
        return;
   }
   
   aPeriodVector[ period ] =  XMLHelper<T>::getValue( aNode );
   
   if( fillout ) {
      // will not do if period is already last period or maxperiod
      for ( unsigned int i = period + 1; i < aPeriodVector.size(); ++i ) {
         aPeriodVector[ i ] =  aPeriodVector[ period ];
      }
   }
}

/*! 
* \brief Return the period cooresponding to the year in the node,
* works analogous to insertValueIntoVector, returning the appropriate period
* \warning Make sure the node passed as an argument as a year attribute.
* \param node A pointer to a node from which to extract the data.
* \param modeltime A pointer to the modeltime object to use to determine the correct period.
*/

template<class T>
int XMLHelper<T>::getNodePeriod ( const xercesc::DOMNode* node, const Modeltime* modeltime ) {
    /*! \pre Make sure we were passed a valid node reference. */
    assert( node );

    const int year = XMLHelper<int>::getAttr( node, "year" );

    // Check to make sure the year attribute returned non-zero. 
    assert( year != 0 );

    int period = modeltime->getyr_to_per( year );

    // Check that the period returned correctly.
    assert( ( period >= 0 ) && ( period <= modeltime->getmaxper() ) );
    return period;
}

/*! \brief Function which converts XMLCh* to a string without leaking memory.
* \details This function when passed an XMLCh* string will call the XMLString::transcode method to
* convert the string into a dynamically allocated char* buffer. The function will then
* convert the buffer into a string and free the buffer. This function should always be used instead
* of the XMLString::transcode( XMLCh* ).
* \warning Always use this function instead of XMLString::transcode( XMLCh* ) otherwise memory will leak.
* \warning This function replaces XMLString::transcode( XMLCh* ) but not XMLString::transcode( char* ). 
* The latter version is used to create an XMLCh* string. This must still be done with XMLString::transcode. 
* Be very careful to free memory when doing so.
* \param toTranscode string to be converted to a standard string.
* \return An STL string equivalent to the XMLCh* string passed into the function.
*/

template<class T>
std::string XMLHelper<T>::safeTranscode( const XMLCh* toTranscode ) {
   char* transcoded = xercesc::XMLString::transcode( toTranscode );
   std::string retString = transcoded;
   xercesc::XMLString::release( &transcoded );
   return retString;
}

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
* The function will also avoid writing out elements if they have default values, and will collapse consecutive 
* equal values into one element with a fillout attribute.
* \param outputVector The vector of values to write out.
* \param elementName The elementName to write out for each value.
* \param out Stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs. 
* \param modeltime A pointer to the global modeltime object. 
* \param defaultValue Default value for items in this vector. 
*/
template<class T>
void XMLWriteVector( const std::vector<T>& outputVector, const std::string& elementName, std::ostream& out, Tabs* tabs, const Modeltime* modeltime, const T defaultValue = T() ) {

    for( unsigned int i = 0; i < outputVector.size(); i++ ){
        // Determine the correct year. 
        unsigned int year = modeltime->getper_to_yr( i );

        // Determine if we can use fillout.
        unsigned int canSkip = 0;
        for( unsigned int j = i + 1; j < outputVector.size(); j++ ){
            if( util::isEqual( outputVector.at( i ), outputVector.at( j ) ) ){
                canSkip++;
            }
            else {
                break;
            }
        }
        if( canSkip > 0 ){
            XMLWriteElementCheckDefault( outputVector.at( i ), elementName, out, tabs, defaultValue, year, "", true );
            i += canSkip;
        } else {
            // Can't skip any. write normally.
            XMLWriteElementCheckDefault( outputVector.at( i ), elementName, out, tabs, defaultValue, year );
        }
    }
}

/*! 
* \brief Function which writes out the values contained in a YearVector. 
* \details This function is used to write out the values of a YearVector in XML
*          format, along with their year tag. The function will also avoid
*          writing out elements if they have default values, and will collapse
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
            XMLWriteElementCheckDefault( aOutputVector[ i ], aElementName, aOut, aTabs, aDefaultValue, i, "", true );
            i += canSkip;
        } else {
            // Can't skip any. write normally.
            XMLWriteElementCheckDefault( aOutputVector[ i ], aElementName, aOut, aTabs, aDefaultValue, i );
        }
    }
}

/*! 
* \brief Function which writes out the values contained in a PeriodVector. 
* \details This function is used to write out the values of a PeriodVector in XML
*          format, along with their year tag. The function will also avoid
*          writing out elements if they have default values, and will collapse
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
            XMLWriteElementCheckDefault( aOutputVector[ i ], aElementName, aOut, aTabs, aDefaultValue, year, "", true );
            i += canSkip;
        } else {
            // Can't skip any. write normally.
            XMLWriteElementCheckDefault( aOutputVector[ i ], aElementName, aOut, aTabs, aDefaultValue, year );
        }
    }
}

/*!
* \brief Function to parse an XML file, returning a pointer to the root.
* 
* This is a very simple function which calls the parse function and handles the exceptions which it may throw.
* It also takes care of fetching the document and its root element.
* \param aXMLFile The name of the file to parse.
* \param aParser A pointer to an already created xercesDOMParser.
* \param aModelElement Element to call XMLParse on.
* \return Whether parsing was successful.
*/

template <class T>
bool XMLHelper<T>::parseXML( const std::string& xmlFile, IParsable* aModelElement ) {
    xercesc::XercesDOMParser* parser = XMLHelper<T>::getParser();
    try {
        parser->parse( xmlFile.c_str() );
    } catch ( const xercesc::XMLException& toCatch ) {
        std::string message = XMLHelper<std::string>::safeTranscode( toCatch.getMessage() );
        std::cout << "ERROR: XML Read Exception message is:" << std::endl << message << std::endl;
        return false;
    } catch ( const xercesc::DOMException& toCatch ) {
        std::string message = XMLHelper<std::string>::safeTranscode( toCatch.msg );
        std::cout << "ERROR: XML Read Exception message is:" << std::endl << message << std::endl;
        return false;
    } catch ( const xercesc::SAXException& toCatch ){
        std::string message = XMLHelper<std::string>::safeTranscode( toCatch.getMessage() );
        std::cout << "ERROR: XML Read Exception message is:" << std::endl << message << std::endl;
        return false;
    } catch (...) {
        std::cout << "ERROR:Unexpected XML Read Exception." << std::endl;
        return false;
    }

    aModelElement->XMLParse( parser->getDocument()->getDocumentElement() );
    parser->resetDocumentPool();
    parser->resetCachedGrammarPool();
    return true;
}

/*! \brief Function which initializes the XML Platform and creates an instance
* of an error handler and parser. 
* \note Logs are not initialized yet so they cannot be used.
* \author Josh Lurz
*/
template<class T>
void XMLHelper<T>::initParser() {
    // Initialize the Xerces platform.
    try {
        xercesc::XMLPlatformUtils::Initialize();
    } catch ( const xercesc::XMLException& toCatch ) {
        std::string message = XMLHelper<std::string>::safeTranscode( toCatch.getMessage() );
        std::cout << "Severe error during XML Platform initialization: "<< std::endl << message << std::endl;
        exit( -1 );
    }
    
    // Initialize the instances of the parser and error handler. 
    *getParserPointerInternal() = new xercesc::XercesDOMParser();
    (*getParserPointerInternal())->setValidationScheme( xercesc::XercesDOMParser::Val_Always );
    (*getParserPointerInternal())->setDoNamespaces( false );
    (*getParserPointerInternal())->setDoSchema( true );
    (*getParserPointerInternal())->setCreateCommentNodes( false ); // No comment nodes
    (*getParserPointerInternal())->setIncludeIgnorableWhitespace( false ); // No text nodes

    *getErrorHandlerPointerInternal() = ( (xercesc::ErrorHandler*)new xercesc::HandlerBase() );
    (*getParserPointerInternal())->setErrorHandler( *getErrorHandlerPointerInternal() );
}

/*! \brief Return the text string.
* \author Josh Lurz
* \return The #text string.
*/
template<class T>
const std::string& XMLHelper<T>::text(){
    const static std::string TEXT = "#text";
    return TEXT;
}

/*! \brief Return the name string.
* \author Josh Lurz
* \return The name string.
*/
template<class T>
const std::string& XMLHelper<T>::name(){
    const static std::string NAME = "name";
    return NAME;
}

/*! \brief Function which returns a pointer to a XercesDOMParser*.
* \details This function first checks if the parser has already been initialized.
* If it hasn't, it initializes the parser. It then returns a pointer to the parser.
* \author Josh Lurz
* \warning The user must call cleanupParser after the parser is finished being used 
* to prevent a memory leak.
* \return A pointer to a XercesDOMParser.
*/
template<class T>
xercesc::XercesDOMParser* XMLHelper<T>::getParser() {
    // If the parser has not been initialized already, initialize it.
    if( !(*getParserPointerInternal()) ){
        initParser();
    }
    
    // Return a pointer to the already initialized parser. 
    return *getParserPointerInternal();
}

/*! \brief Function which cleans up the memory used by the XML Parser.
* \details This function deletes the parser, errorhandler, and instructs
* the XMLPlatform to free its memory.
* \author Josh Lurz
* \warning This function must be called if getParser is ever called.
*/
template<class T>
void XMLHelper<T>::cleanupParser(){
    delete *getErrorHandlerPointerInternal();
    delete *getParserPointerInternal();
// The XML database will terminate xerces if it is compiled in. Terminating
// twice will cause a crash.
#if( !__USE_XML_DB__ )
    xercesc::XMLPlatformUtils::Terminate();
#endif
}

/*! \brief Reset the name to number mapping for a vector to the current names and numbers of the map.
* \details This function is used to reset and update a map to contain the correct name to index mapping for a
* vector of items.
* \note T must support the getName function.
* \author Josh Lurz
*/
template<class T>
static void resetMapIndices( const std::vector<T>& aItems, std::map<std::string, int>& aIndiceMap ){
    aIndiceMap.clear();
    for( int i = 0; i < static_cast<int>( aItems.size() ); ++i ){
        aIndiceMap[ aItems[ i ]->getName() ] = i;
    }  
} 

/*! \brief Function which positions a node containing children within a vector
*          of its parent, and if successful parses the new node.
* \details This function will look at the name and delete attributes of the node
*          to determine if the model node which corresponds to the input should
*          be added, modified, or deleted. After it determines this it will make
*          this change to the data structure passed in.
* \param aNode The node pointing to the container node in the XML tree. 
* \param aContainerSet The vector of objects of the type pointed to by node.
* \param aNewObject An object to use if the xml node is unique. This node is
*        deleted if it is not needed.
* \param aIDAttr Name of the attribute containing the unique identifier for the
*        node. Defaults to "name".
*/
template<class T, class U>
void parseContainerNode( const xercesc::DOMNode* aNode,
                         std::vector<T*>& aContainerSet, 
                         U* aNewObject,
                         const std::string& aIDAttr = XMLHelper<std::string>::name() )
{
    assert( aNode );
    // Force U* to implement IParsable. TODO: Activate this check when all
    // elements of the model properly implement the interface. IParsable*
    // parsable = aNewNode;

    // Have an auto_ptr keep the new memory.
    std::auto_ptr<U> newObjWrapper( aNewObject );

    // First determine if the node exists. 
    const std::string objName = XMLHelper<std::string>::getAttr( aNode, aIDAttr );
    
    // Search the insert to vector for an item with the name.
    typename std::vector<T*>::iterator iter = util::searchForValue( aContainerSet, objName );
   
    // Determine if we should be deleting a node. 
    bool shouldDelete = XMLHelper<bool>::getAttr( aNode, "delete" );
    
    // Check if the node already exists in the model tree. 
    if( iter != aContainerSet.end() ){     
        // Modify or delete the node based on the contents of the delete attribute.
        if( shouldDelete ) {
            // Perform deletion.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << objName << std::endl;
            
            // Clean up the memory the vector points at.
            delete *iter;

            // Remove the pointer from the vector. 
            aContainerSet.erase( iter );
        }
        // Parse the XML data into the existing node to modify it.
        else {
           (*iter)->XMLParse( aNode );
        }
    } 
    // The node does not already exist.
    else {
        if( shouldDelete ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not delete node " << objName << " as it does not exist." << std::endl;
        } 
        else if( XMLHelper<bool>::getAttr( aNode, "nocreate" ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << objName << " as the nocreate input flag was set." << std::endl;
        }
        else {
            aNewObject->XMLParse( aNode );
            aContainerSet.push_back( newObjWrapper.release() );
        }
    }
}

/*! \brief Function which parses a node containing model-children, such as a region, and determines what to do with it.
* \details This function will look at the name and delete attributes of the node to determine if the model node which 
* corresponds to the input should be added, modified, or deleted. After it determines this it will make this change 
* to the model tree. 
* \param node The node pointing to the container node in the XML tree. 
* \param insertToVector The vector of objects of the type pointed to by node.
* \param corrMap The map of node name attributes to locations within insertToVector.
* \param newNode An object to use if the xml node is unique. This node is deleted if it is not needed.
* \param attrName Name of the attribute which contains the name identifier.
* \todo Remove this function and use only the above variant once the entire model is converted.
*/
template<class T, class U> 
void parseContainerNode( const xercesc::DOMNode* node, std::vector<U>& insertToVector, 
                         std::map<std::string,int>& corrMap, T* newNode, const std::string& attrName = "name" )
{
    assert( node );
    // Have an auto_ptr keep the new memory.
    std::auto_ptr<T> newNodePtr( newNode );

    // First determine if the node exists. 
    const std::string objName = XMLHelper<std::string>::getAttr( node, attrName );
    std::map<std::string,int>::const_iterator iter = corrMap.find( objName );
   
    // Determine if we should be deleting a node. 
    bool shouldDelete = XMLHelper<bool>::getAttr( node, "delete" );
    
    // Check if the node already exists in the model tree. 
    if( iter != corrMap.end() ){     
        // Modify or delete the node based on the contents of the delete attribute.
        if( shouldDelete ) {
            // Perform deletion.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << objName << std::endl;
            
            // Create an iterator which points at the location which should be deleted.
            typedef typename std::vector<U>::iterator VectorIterator;
            VectorIterator delIter = insertToVector.begin() + iter->second;
            // Clean up the memory the vector points at.
            delete *delIter;
            // Remove the pointer from the vector. 
            insertToVector.erase( delIter );

            // Now reset the map. There is probably a more efficient way to do this.
            resetMapIndices( insertToVector, corrMap );
        }
        // Otherwise modify node. 
        else {
           insertToVector[ iter->second ]->XMLParse( node );
        }
    } 
    // The node does not already exist.
    else {
        if( shouldDelete ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not delete node " << objName << " as it does not exist." << std::endl;
        } 
        else if( XMLHelper<bool>::getAttr( node, "nocreate" ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << objName << " as the nocreate input flag was set." << std::endl;
        }
        else {
            newNode->XMLParse( node );
            insertToVector.push_back( newNodePtr.release() );
            corrMap[ newNode->getName() ] = static_cast<int>( insertToVector.size() ) - 1;
        }
    }
}

/*! \brief Parses a single container node held by an auto_ptr.
* \details Looks at the name and delete attributes of the node to determine if
*          the model node which corresponds to the input should be added,
*          modified, or deleted and makes the corresponding change to the model
*          tree.
* \param aNode The node pointing to the container node in the XML tree. 
* \param aContainer The auto_ptr which contains the model's corrsponding object.
* \param aNewNode An object to insert into the model tree if the node is unique.
*        This node is deleted if it is not needed.
* \param aAttrName Name of the attribute which contains the name identifier.
* \sa parseContainerNode
*/
template<class T, class U> 
void parseSingleNode( const xercesc::DOMNode* aNode, std::auto_ptr<U>& aContainer, 
                      T* aNewNode, const std::string& aAttrName = "name" )
{
    assert( aNode );
    assert( aNewNode );

    // Have an auto_ptr keep the new memory.
    std::auto_ptr<T> newNodePtr( aNewNode );

    // Determine if we should be deleting a node. 
    bool shouldDelete = XMLHelper<bool>::getAttr( aNode, "delete" );
    
    // Check if the container has already been created.
    if( aContainer.get() ){
        // Modify or delete the node based on the contents of the delete attribute.
        if( shouldDelete ) {
            // Perform deletion.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Deleting node: " << XMLHelper<std::string>::getAttr( aNode, aAttrName ) << std::endl;
            aContainer.reset( 0 );
        }
        // Otherwise modify node. 
        else {
           aContainer->XMLParse( aNode );
        }
    } 
    // The node does not already exist.
    else {
        if( shouldDelete ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Could not delete node " << XMLHelper<std::string>::getAttr( aNode, aAttrName )
                    << " as it does not exist." << std::endl;
        } 
        else if( XMLHelper<bool>::getAttr( aNode, "nocreate" ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Did not create node " << XMLHelper<std::string>::getAttr( aNode, aAttrName )
                    << " as the nocreate input flag was set." << std::endl;
        }
        else {
            aContainer = newNodePtr;
            aContainer->XMLParse( aNode );
        }
    }
}

template<class T>
xercesc::XercesDOMParser** XMLHelper<T>::getParserPointerInternal(){
    static xercesc::XercesDOMParser* parser;
    return &parser;
}

template<class T>
xercesc::ErrorHandler** XMLHelper<T>::getErrorHandlerPointerInternal(){
    static xercesc::ErrorHandler* errorHandler;
    return &errorHandler;
}

#endif // _XML_HELPER_H_
