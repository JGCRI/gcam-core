/*! 
\file xmlHelper.h
\ingroup CIAM
A set of helper function for reading xml data.
This library contains a set of routines for reading xml data and attribute values.
It is a templated library so that it should work with any data type.
\author Josh Lurz
\date $Date$
\version $Revision$
*/

#ifndef _XML_HELPER_H_
#define _XML_HELPER_H_
#pragma once

#include <string>
#include <iostream>
#include <sstream>
#include <cassert>
#include <vector>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

/*!
* \ingroup CIAM
*  A basic class which is a container for a static variable containing the current level of indention in the xml being written.
* \author Josh Lurz
* \date $Date$
* \version $Revision&
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
	static void writeTabs( ostream& out ){
		for ( int i = 0; i < numTabs; i++ ){
			out << "\t";
		}
	}
};

/*! 
* \ingroup CIAM
* A helper class of functions used to parse XML DOM trees.
* \author Josh Lurz
* \date $Date$
* \version $Version$
*/

template<class T>
class XMLHelper {
	
public:
	static T getValue( const DOMNode* node );
	static string getValueString( const DOMNode* node );
	static T getAttr( const DOMNode* node, const string attrName );
	static string getAttrString( const DOMNode* node, const string attrName );
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
T XMLHelper<T>::getValue( const DOMNode* node ){
	
	T retValue; // Variable of requested type which will hold the return value.
	
	DOMNode* curr = 0; // Node pointer which will point to the node containing the value of the element passed.
	
	// make sure we were passed a valid node reference which is an element.
	assert( node ); 
	assert( node->getNodeType() == DOMNode::ELEMENT_NODE );
	
	// get the first child, which should contain the value.
	curr = node->getFirstChild();
	
	// make sure that the above returned a TEXT_NODE, otherwise value will not be correct.
	if ( !curr || curr->getNodeType() != DOMNode::TEXT_NODE ){
		return T();
	}
	
	else {
		// convert the returned string to the return type.
		istringstream target( XMLString::transcode( curr->getNodeValue() ) );
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
string XMLHelper<T>::getValueString( const DOMNode* node ) {
	DOMNode* curr = 0; // Node pointer which will point to the node containing the value of the element passed.
	
	// make sure we were passed a valid node reference which is an element.
	assert( node ); 
	assert( node->getNodeType() == DOMNode::ELEMENT_NODE );
	
	// get the first child, which should contain the value.
	curr = node->getFirstChild();
	
	// make sure that the above returned a TEXT_NODE, otherwise value will not be correct.
	if ( !curr || curr->getNodeType() != DOMNode::TEXT_NODE ){
		return "";
	}
	
	else {
		return XMLString::transcode( curr->getNodeValue() );
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
T XMLHelper<T>::getAttr( const DOMNode* node, const string attrName ) {
	
	T retValue; // Variable of requested type which will hold the return value.
	
	/*! \pre Make sure we were passed a valid node reference. */
	assert( node );
	
	/*! \pre Make sure it is an element before we cast, if function is used correctly it will be. */
	assert( node->getNodeType() == DOMNode::ELEMENT_NODE );
	
	// need to cast node to an element.
	const DOMElement* element = static_cast<const DOMElement*>( node );
	
	// get the attribute with the name which was passed in.
	DOMAttr* nameAttr = element->getAttributeNode( XMLString::transcode( attrName.c_str() ) );
	if( !nameAttr ){
		return T();
	} 
	
	else {
		// convert the returned string to the return type
		istringstream target( XMLString::transcode( nameAttr->getValue() ) );
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
string XMLHelper<T>::getAttrString( const DOMNode* node, const string attrName ) {
	
	//! \pre Make sure we were passed a valid node reference.
	assert( node );
	
	//! \pre Make sure it is an element before we cast, if function is used correctly it will be.
	assert( node->getNodeType() == DOMNode::ELEMENT_NODE );
	
	// need to cast node to an element.
	const DOMElement* element = static_cast<const DOMElement*>( node );
	
	// get the attribute with the name which was passed in.
	DOMAttr* nameAttr = element->getAttributeNode( XMLString::transcode( attrName.c_str() ) );
	
	if( !nameAttr ){
		return "";
	} 
	
	else {
		return XMLString::transcode( nameAttr->getValue() );
	}
}

//! Function to write the argument element to xml in proper format.
/*! 
* This function is used to write a single element containing a single value and an optional year to the output stream
* in XML. If the year is not passed in, the function will not print the year attribute.
* \param value Value to print to XML.
* \param elementName Name of the element.
* \param out Stream to print to.
* \param year Optional year value to print as an attribute.
* \return void
*/
template<class T>
void XMLWriteElement( const T value, const string elementName, ostream& out, const int year = 0, const string name = "" ) {
	
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
	
	out << "</" << elementName << ">" << endl;
}

#endif // _XML_HELPER_H_
