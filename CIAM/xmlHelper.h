#ifndef _XML_HELPER_H_
#define _XML_HELPER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file xmlHelper.h
* \ingroup CIAM
* \brief A set of helper function for reading xml data.
*
* This library contains a set of routines for reading xml data and attribute values.
* It is a templated library so that it should work with any data type.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <sstream>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include "modeltime.h"
#include "Util.h"

/*!
* \ingroup CIAM
* \brief A basic class which is a container for a static variable containing the current level of indention in the xml being written.
* \author Josh Lurz
*/

class Tabs {
   
private:
   static int numTabs; //!< Current number of tabs to write out in order to properly format xml.
public:
   //! Increase the current level of indentation.
   static void increaseIndent(){ numTabs++; }
   
   //! Decrease the current level of indentation. 
   static void decreaseIndent(){ numTabs--; }
   
   //! Write out the contained number of tabs to the specified output stream.
   /*!
   \param out Stream to which to write the tabs.
   \return void
   */
   static void writeTabs( std::ostream& out ){
      for ( int i = 0; i < numTabs; i++ ){
         out << "\t";
      }
   }
};

/*! 
* \ingroup CIAM
* \brief A helper class of functions used to parse XML DOM trees.
* \author Josh Lurz
*/

template<class T>
class XMLHelper {
   
public:
   static T getValue( const xercesc::DOMNode* node );
   static std::string getValueString( const xercesc::DOMNode* node );
   static T getAttr( const xercesc::DOMNode* node, const std::string attrName );
   static std::string getAttrString( const xercesc::DOMNode* node, const std::string attrName );
   static std::string safeTranscode( const XMLCh* toTranscode );
   static void insertValueIntoVector( const xercesc::DOMNode* node, std::vector<T>& insertToVector, const Modeltime* modeltime, const bool isPopulationData = false );
   static int getNodePeriod ( const xercesc::DOMNode* node, const Modeltime* modeltime, const bool isPopulationData = false );
   static xercesc::DOMNode* parseXML( const std::string& xmlFile, xercesc::XercesDOMParser* parser );
};

//! Returns the data value associated with the element node.

/*! 
* This function first finds the child node of this element, a text node which contains the data value.
* It then converts the data using the stringstream library into the requested data type and returns the value.
* The function will throw an error if the input node is not an element, but will return the default value if the element has no data value. 
* \warning Since this function has a templated type only for the return argument, it must be called as getXMLValue<type>.
* \warning If the value is a string and there might be spaces in it getXMLValueString must be used. 
* \param node A pointer to a node for which to return the value.
* \return Value of type T from child of node.
* \sa getValueString
* \sa getAttr
* \sa getAttrString
*/

template<class T>
T XMLHelper<T>::getValue( const xercesc::DOMNode* node ){
   
   T retValue; // Variable of requested type which will hold the return value.
   
   xercesc::DOMNode* curr = 0; // Node pointer which will point to the node containing the value of the element passed.
   
   // make sure we were passed a valid node reference which is an element.
   assert( node ); 
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // get the first child, which should contain the value.
   curr = node->getFirstChild();
   
   // make sure that the above returned a TEXT_NODE, otherwise value will not be correct.
   if ( !curr || curr->getNodeType() != xercesc::DOMNode::TEXT_NODE ){
      return T();
   }
   
   else {
      // convert the returned string to the return type.
      std::istringstream target( safeTranscode( curr->getNodeValue() ) );
      target >> retValue;
      return retValue;
   }
}

//! Returns the string value associated with the element node.

/*! 
* This function first finds the child node of this element, a text node which contains the data value.
* It then returns the string associated with the data value
* The function will throw an error if the input node is not an element, but will return the default value if the element has no data value. 
*
* \warning This function must be used for strings which might contain a space instead of the templated version becuase of conversion problems.
* \param node A pointer to a node for which to return the value.
* \return string from child of node.
* \sa getValue
* \sa getAttr
* \sa getAttrString
*/

template<class T>
std::string XMLHelper<T>::getValueString( const xercesc::DOMNode* node ) {
   xercesc::DOMNode* curr = 0; // Node pointer which will point to the node containing the value of the element passed.
   
   // make sure we were passed a valid node reference which is an element.
   assert( node ); 
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // get the first child, which should contain the value.
   curr = node->getFirstChild();
   
   // make sure that the above returned a TEXT_NODE, otherwise value will not be correct.
   if ( !curr || curr->getNodeType() != xercesc::DOMNode::TEXT_NODE ){
      return "";
   }
   
   else {
      return safeTranscode( curr->getNodeValue() );
   }
}

//! Returns the requested attribute of the element node passed to the function.

/*!
* This function searches for the attribute with name attrName of the argument node.
* It then converts it to type T and returns the value. If the function is not passed an element
* it will throw an error. If the requested attribute is not present, the function will return the default
* constructor for type T.
*
* \warning It must be called as getXMLValue<type> because it is templated only on the return type.
* \warning If the attribute is a string and there might be spaces in it getXMLAttrString must be used.
* \param node A pointer to a node for which to fetch the attribute.
* \param attrName The name of the attribute to fetch.
* \return Value of type T from the attribute with name attrName of the node.
* \sa getAttrString
* \sa getValue
* \sa getValueString
*/

template<class T>
T XMLHelper<T>::getAttr( const xercesc::DOMNode* node, const std::string attrName ) {
   
   T retValue; // Variable of requested type which will hold the return value.
   
   /*! \pre Make sure we were passed a valid node reference. */
   assert( node );
   
   /*! \pre Make sure it is an element before we cast, if function is used correctly it will be. */
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // need to cast node to an element.
   const xercesc::DOMElement* element = static_cast<const xercesc::DOMElement*>( node );
   
   // get the attribute with the name which was passed in.
   
   XMLCh* nameChars =  xercesc::XMLString::transcode( attrName.c_str() );
   xercesc::DOMAttr* nameAttr = element->getAttributeNode( nameChars );
   xercesc::XMLString::release( &nameChars );
   if( !nameAttr ){
      return T();
   } 
   
   else {
      // convert the returned string to the return type
      std::istringstream target( safeTranscode( nameAttr->getValue() ) );
      target >> retValue;
      return retValue;
   }
}

//! Returns the requested attribute of the element node passed to the function.

/*!
* This function searches for the attribute with name attrName of the argument node.
* It will then return the string interpretation of the value. If the function is not passed an element
* it will throw an error. If the requested attribute is not present, the function will return the empty string. 
*
* \warning This function must be used instead of getXMLAttr<string> if there are spaces in the string.
* \param node A pointer to a node for which to fetch the attribute.
* \param attrName The name of the attribute to fetch.
* \return String from the attribute with name attrName of the node.
* \sa getAttr
* \sa getValue
* \sa getValueString
*/
template<class T>
std::string XMLHelper<T>::getAttrString( const xercesc::DOMNode* node, const std::string attrName ) {
   
   //! \pre Make sure we were passed a valid node reference.
   assert( node );
   
   //! \pre Make sure it is an element before we cast, if function is used correctly it will be.
   assert( node->getNodeType() == xercesc::DOMNode::ELEMENT_NODE );
   
   // need to cast node to an element.
   const xercesc::DOMElement* element = static_cast<const xercesc::DOMElement*>( node );
   
   // get the attribute with the name which was passed in.
   XMLCh* tempChars = xercesc::XMLString::transcode( attrName.c_str() ); 
   xercesc::DOMAttr* nameAttr = element->getAttributeNode( tempChars );
   xercesc::XMLString::release( &tempChars );
   
   if( !nameAttr ){
      return "";
   } 
   
   else {
      return safeTranscode( nameAttr->getValue() );
   }
}

/*! 
* \brief Function which takes a node and inserts its value into the correct position 
* in an argument vector based on the year attribute.
* \updated to fill out for the rest of the time period if the fillout attribute is true
*
* This function when passed a node, vector and modeltime object will first extract the year attribute and lookup
* the corresponding period from the modeltime object. It will then insert the item in that position in the vector.
*
* \warning Make sure the node passed as an argument as a year attribute.
* \param node A pointer to a node from which to extract the data.
* \param insertToVector A vector passed by reference in which to insert the value.
* \param modeltime A pointer to the modeltime object to use to determine the correct period.
* \param isPopulationData A flag which tells the function the vector is a demographics vector.
*/

template<class T>
void XMLHelper<T>::insertValueIntoVector( const xercesc::DOMNode* node, std::vector<T>& insertToVector, const Modeltime* modeltime, const bool isPopulationData ) {
   
   //! \pre Make sure we were passed a valid node reference.
   assert( node );
   
   const int year = XMLHelper<int>::getAttr( node, "year" );
   // boolean to fill out the readin value to all the periods
   const bool fillout = XMLHelper<bool>::getAttr( node, "fillout" );
   
   // Check to make sure the year attribute returned non-zero. 
   assert( year != 0 );
   
   int period = 0;
   const int maxperiod = modeltime->getmaxper();
   
   if( isPopulationData == false ) {   
      period = modeltime->getyr_to_per( year );
      
      
      // Check that the period returned correctly.
      assert( ( period >= 0 ) && ( period <= modeltime->getmaxper() ) );
   }
   else {
      period = modeltime->convertYearToPopPeriod( year );
      
      // Check that the period returned correctly.
      assert( ( period >= 0 ) && ( period <= modeltime->getmaxpopdata() ) );
      
   }
   
   // Check that the period is less than the size of the vector.
   assert( period < static_cast<int>( insertToVector.size() ) );
   insertToVector[ period ] =  XMLHelper<double>::getValue( node );
   
   if (fillout) {
      // Check that the max period is equal to the size of the vector.
      assert( maxperiod == static_cast<int>(insertToVector.size()) );
      // will not do if period is already last period or maxperiod
      for (int i = period+1; i < maxperiod; i++) {
         insertToVector[ i ] =  insertToVector[ period ];
      }
   }
   
}

/*! 
* \brief Return the period cooresponding to the year in the node 
*
* works analogous to insertValueIntoVector, returning the appropriate period
*
* \todo Make this work for the demographics object. 
* \warning Make sure the node passed as an argument as a year attribute.
* \param node A pointer to a node from which to extract the data.
* \param modeltime A pointer to the modeltime object to use to determine the correct period.
*/

template<class T>
int XMLHelper<T>::getNodePeriod ( const xercesc::DOMNode* node, const Modeltime* modeltime, const bool isPopulationData ) {
   
   //! \pre Make sure we were passed a valid node reference.
   assert( node );
   
   const int year = XMLHelper<int>::getAttr( node, "year" );
   
   // Check to make sure the year attribute returned non-zero. 
   assert( year != 0 );
   
   int period = 0;
   
   if( isPopulationData == false ) {   
      period = modeltime->getyr_to_per( year );
      
      // Check that the period returned correctly.
      assert( ( period >= 0 ) && ( period <= modeltime->getmaxper() ) );
   }
   else {
      period = modeltime->convertYearToPopPeriod( year );
      
      // Check that the period returned correctly.
      assert( ( period >= 0 ) && ( period <= modeltime->getmaxpopdata() ) );
      
   }
   
   return period;
   
}

//! Function which converts XMLCh* to a string without leaking memory.

/*! 
* This function when passed an XMLCh* string will call the XMLString::transcode method to
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
* \param year Optional year value to print as an attribute.
* \param name Optional name value to print as an attribute.
* \return void
*/

template<class T>
void XMLWriteElement( const T value, const std::string elementName, std::ostream& out, const int year = 0, const std::string name = "" ) {
   
   Tabs::writeTabs( out );
   
   out << "<" << elementName;
   
   if ( name != "" ) {
      out << " name=\"" << name << "\"";
   }
   
   
   if( year != 0 ){
      out << " year=\"" << year << "\"";
   }
   
   out << ">";
   
   out << value;
   
   out << "</" << elementName << ">" << std::endl;
}

//! Function to write the argument element to xml in proper format if it is not equal to the default value for the element..
/*! 
* This function is used to write a single element containing a single value and an optional year to the output stream
* in XML if the value is not equal to the default value.. If the year is not passed in, the function will not print the year attribute.
* \param value Value to print to XML.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param defaultValue Default value to compare the value to. 
* \param year Optional year value to print as an attribute.
* \param name Optional name value to print as an attribute.
* \return void
*/
template<class T>
void XMLWriteElementCheckDefault( const T value, const std::string elementName, std::ostream& out, const double defaultValue, const int year = 0, const std::string name = "" ) {
   
   if( !util::isEqual( value, defaultValue ) ) {
      Tabs::writeTabs( out );
      
      out << "<" << elementName;
      
      if ( name != "" ) {
         out << " name=\"" << name << "\"";
      }
      
      if( year != 0 ){
         out << " year=\"" << year << "\"";
      }
      
      out << ">";
      out << value;
      out << "</" << elementName << ">" << std::endl;
   }
}
/*!
* \brief Function to parse an XML file, returning a pointer to the root.
* 
* This is a very simple function which calls the parse function and handles the exceptions which it may throw.
* It also takes care of fetching the document and its root element.
* \param xmlFile The name of the file to parse.
* \param parser A pointer to an already created xercesDOMParser.
* \return The root element of the newly parsed DOM document.
*/

template <class T>
xercesc::DOMNode* XMLHelper<T>::parseXML( const std::string& xmlFile, xercesc::XercesDOMParser* parser ) {
   
   xercesc::DOMDocument* doc = 0;
   
   try {
      const unsigned long startMillis = xercesc::XMLPlatformUtils::getCurrentMillis();
      parser->parse( xmlFile.c_str() );
      const unsigned long endMillis = xercesc::XMLPlatformUtils::getCurrentMillis();
      long parseTime = endMillis - startMillis;
      std::cout << "Parsing took " << parseTime / 1000.0 << " seconds." << std::endl;
   } catch ( const xercesc::XMLException& toCatch ) {
      std::string message = XMLHelper<std::string>::safeTranscode( toCatch.getMessage() );
      std::cout << "Exception message is:" << std::endl << message << std::endl;
      return 0;
   } catch ( const xercesc::DOMException& toCatch ) {
      std::string message = XMLHelper<std::string>::safeTranscode( toCatch.msg );
      std::cout << "Exception message is:" << std::endl << message << std::endl;
      return 0;
   } catch ( const xercesc::SAXException& toCatch ){
      std::string message = XMLHelper<std::string>::safeTranscode( toCatch.getMessage() );
      std::cout << "Exception message is:" << std::endl << message << std::endl;
      return 0;
   } catch (...) {
      std::cout << "Unexpected Exception." << std::endl;
      return 0;
   }
   
   doc = parser->getDocument();
   return doc->getDocumentElement();
}
#endif // _XML_HELPER_H_
