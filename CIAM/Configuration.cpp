#include "Definitions.h"
#include <string>
#include <map>
#include <iostream>
#include <xercesc/dom/DOM.hpp>
#include "Configuration.h"
#include "xmlHelper.h"

using namespace std;
using namespace xercesc;

// Static initializations.
bool Configuration::confExists = false;
Configuration* Configuration::instance = 0;

//! Destructor
Configuration::~Configuration() {
	confExists = false;
}

//! Create the one instance of the singleton.
Configuration* Configuration::getInstance() {

	if ( !confExists ) {
		confExists = true;
		instance = new Configuration();
	}

	return instance;
}

//! Initialize Configuration object with xml data
void Configuration::XMLParse( const DOMNode* root ) {	
	
	DOMNode* currSectionNode = 0;
	DOMNode* currValueNode = 0;
	DOMNodeList* nodeSectionList = 0;
	DOMNodeList* nodeValueList = 0;
	string sectionName;
	string valueName;
	int tempValue = 0;

	//! \pre Assume we are passed a valid node.
	assert( root );
	
	nodeSectionList = root->getChildNodes();
	
	for( int i = 0; i < nodeSectionList->getLength(); i++ ) {
		
		currSectionNode = nodeSectionList->item( i );
		sectionName = XMLString::transcode( currSectionNode->getNodeName() );		
		nodeValueList = currSectionNode->getChildNodes();
		
		for( int j = 0; j < nodeValueList->getLength(); j++ ) {
			currValueNode = nodeValueList->item( j );

			if ( currValueNode->getNodeType() != DOMNode::ELEMENT_NODE ) {
				continue;
			}

			valueName = XMLHelper<string>::getAttrString( currValueNode, "name" );
			
			if( sectionName == "Files" ){
				fileMap[ valueName ] = XMLHelper<string>::getValueString( currValueNode );
			}
			
			else if(  sectionName == "Strings" ) {
				stringMap[ valueName ] = XMLHelper<string>::getValueString( currValueNode );
			}
			
			else if(  sectionName == "Bools" ) {
				int tempInt = XMLHelper<int>::getValue( currValueNode );
				if ( tempInt ) {
					boolMap[ valueName ] = true;
				}
				else {
					boolMap[ valueName ] = false;
				}
			}
			
			else if(  sectionName == "Ints" ) {
				intMap[ valueName ] = XMLHelper<int>::getValue( currValueNode );
			}
			
			else if(  sectionName == "Doubles" ) {
				doubleMap[ valueName ] = XMLHelper<double>::getValue( currValueNode );
			}
			
			else {
				// some warning
			} // else
		} // for ( int j = 0;...
	} // for ( int i = 0;...
}

//! Print the internal variables to XML output.
void Configuration::toDebugXML( ostream& out ) const {
		
	// write heading for XML input file
	out << "<Configuration>" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// Write each type.
	
	// First write files.
	Tabs::writeTabs( out );
	out << "<Files>" << endl;
	Tabs::increaseIndent();

	// Loop through the map
	for ( map<string,string>::const_iterator fileIter = fileMap.begin(); fileIter != fileMap.end(); fileIter++ ) {
		XMLWriteElement( fileIter->second, "Value", out, 0, fileIter->first );
	}

	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Files>" << endl;
	// Done with files.

	// Write strings.
	Tabs::writeTabs( out );
	out << "<Strings>" << endl;
	Tabs::increaseIndent();

	// Loop through the map
	for ( map<string,string>::const_iterator stringIter = stringMap.begin(); stringIter != stringMap.end(); stringIter++ ) {
		XMLWriteElement( stringIter->second, "Value", out, 0, stringIter->first );
	}

	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Strings>" << endl;
	// Done with strings.

	// Write bools.
	Tabs::writeTabs( out );
	out << "<Bools>" << endl;
	Tabs::increaseIndent();

	// Loop through the map
	for ( map<string,bool>::const_iterator boolIter = boolMap.begin(); boolIter != boolMap.end(); boolIter++ ) {
		XMLWriteElement( boolIter->second, "Value", out, 0, boolIter->first );
	}

	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Bools>" << endl;
	// Done with bools.

	// Write ints.
	Tabs::writeTabs( out );
	out << "<Ints>" << endl;
	Tabs::increaseIndent();

	// Loop through the map
	for ( map<string,int>::const_iterator intIter = intMap.begin(); intIter != intMap.end(); intIter++ ) {
		XMLWriteElement( intIter->second, "Value", out, 0, intIter->first );
	}

	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Ints>" << endl;
	// Done with ints.

	// Write doubles.
	Tabs::writeTabs( out );
	out << "<Doubles>" << endl;
	Tabs::increaseIndent();

	// Loop through the map
	for ( map<string,double>::const_iterator doubleIter = doubleMap.begin(); doubleIter != doubleMap.end(); doubleIter++ ) {
		XMLWriteElement( doubleIter->second, "Value", out, 0, doubleIter->first );
	}

	Tabs::decreaseIndent();
	Tabs::writeTabs( out );
	out << "</Doubles>" << endl;
	// Done with doubles.

	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	out << "</Configuration>" << endl;
}

//! Fetch a filename from the configuration object.
string Configuration::getFile( const string& key ) const {

	map<string,string>::const_iterator found = fileMap.find( key );

	if ( found != fileMap.end() ) {
		return found->second;
	}

	else {
		// issue warning
		return "";
	}
}

//! Fetch a string from the configuration object.
string Configuration::getString( const string& key ) const {

	map<string,string>::const_iterator found = stringMap.find( key );

	if ( found != stringMap.end() ) {
		return found->second;
	}

	else {
		// issue warning
		return "";
	}
}

//! Fetch a bool from the configuration object.
bool Configuration::getBool( const string& key ) const {

	map<string,bool>::const_iterator found = boolMap.find( key );

	if ( found != boolMap.end() ) {
		return found->second;
	}

	else {
		// issue warning
		return false;
	}
}

//! Fetch an int from the configuration object.
int Configuration::getInt( const string& key ) const {

	map<string,int>::const_iterator found = intMap.find( key );

	if ( found != intMap.end() ) {
		return found->second;
	}

	else {
		// issue warning
		return 0;
	}
}

//! Fetch a double from the configuration object.
double Configuration::getDouble( const string& key ) const {

	map<string,double>::const_iterator found = doubleMap.find( key );

	if ( found != doubleMap.end() ) {
		return found->second;
	}

	else {
		// issue warning
		return 0;
	}
}