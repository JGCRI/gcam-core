/*! 
* \file Logger.cpp
* \ingroup CIAM
* \brief Logger class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <ctime>
#include <string>
#include <iostream>
#include <sstream>
#include <cassert>
#include <xercesc/dom/DOM.hpp>

#include "Logger.h"
#include "xmlHelper.h"

using namespace std;
using namespace xercesc;

//! Default Constructor
PassToParentStreamBuf::PassToParentStreamBuf() {
	parent = 0;
}

//! Overriding overflow function which passes its argument to its parent.
int PassToParentStreamBuf::overflow( int ch ){
	
	/*! \pre Make sure the parent is not null. */
	assert( parent );
	return parent->receiveCharFromUnderStream( ch );
}

//! Overriding underflow function which should not be reached because this is a write-only stream.
int PassToParentStreamBuf::underflow( int ch ) {
	
	/*! \pre This function should never be called. */
	assert( false );
	return 0;
}

//! Set the parent of the stream to which we will pass all data.
void PassToParentStreamBuf::setParent( Logger* parentIn ) {
	
	/*! \pre Make sure a non-null parent is passed in. */
	assert( parentIn );
	parent = parentIn;
}

//! Constructor which sets default values.
Logger::Logger( const string& fileNameIn ) : ostream( &underStream ) {
	
	// Initialize all variables which are not set by Configuration values.
	currentNestLevel = 0;
	currentWarningLevel = DEBUG_LEVEL;
	currentLine = 0;
	
	// Set the understream's parent to this Logger.
	underStream.setParent( this );

	// Initialize the attributes to default values.
	fileName = fileNameIn;

	minLogWarningLevel = 0;
	
	minToScreenWarningLevel = 5;

	logTabSize = 3;
	
	printLogNest = false;
	
	printLogWarningLevel = false;

	printLogTimeStamp = false;

	printLogDateStamp = false;

	printLogLineNumber = false;

	printLogFileName = false;

	printLogFullPath = false;
}

//! Virtual destructor
Logger::~Logger() {
}

//! Increase the nesting level
void Logger::increaseNest() {
	currentNestLevel++;
}

//! Decrease the nesting level
void Logger::decreaseNest() {
	currentNestLevel--;
}

//! Set the current warning level.
void Logger::setLevel( const WarningLevel newLevel ) {
	currentWarningLevel = newLevel;
}

//! Set the current line number.
void Logger::setLine( const int lineIn ) {
	currentLine = lineIn;
}

//! Set the current file name.
void Logger::setFile( const string& fileIn ) {
	currentFile = fileIn;
}

//! Receive a single character from the underlying stream and buffer it, printing the buffer it is a newline.
int Logger::receiveCharFromUnderStream( int ch ) {
	
	buf << (char)ch;

	if( ch == '\n' ) {
		string buffer;
		buf >> buffer;
		printToScreenIfConfigured( currentLine, currentFile, currentWarningLevel, buffer );
		logCompleteMessage( currentLine, currentFile, currentWarningLevel, buffer );
	}
	
	return ch;
}

//! Print the message to the screen if the Logger is configured to.
void Logger::printToScreenIfConfigured( const int line, const string& file, const WarningLevel warningLevelIn, const string& message ){
	
	// Decide whether to print the message
	if ( warningLevelIn >= minToScreenWarningLevel ) {
		
		stringstream buffer;
		bool printColon = false;
		
		// Print the date.
		if ( printLogDateStamp ) {
			buffer << getDateString() << " ";
			printColon = true;
		}
		
		// Print the timestamp.
		if ( printLogTimeStamp ) {
			buffer << getTimeString() << " ";
			printColon = true;
		}
		
		// Print the warning level
		if ( printLogWarningLevel ) {
			buffer << "Level " << warningLevelIn << " ";
			printColon = true;
		}
		
		// Print the file name.
		if ( printLogFileName ) {
			printColon = true;
			if ( printLogFullPath ) {
				buffer << file;
			}
			else {
				buffer << getFileNameFromPath( file );
			}
		}
		
		// Print the line number
		if ( printLogLineNumber ) {
			printColon = true;
			buffer << "(" << line << ")";
		}
		
		if( printColon ){
			buffer << ":";
		}
		
		buffer << message;
		
		// Print the message
		cerr << "Log Message: " << buffer.str() << endl;
	}
}

//! Get the current date as a string
const string Logger::getDateString() {
	time_t currTime;
	stringstream buffer;
	string retString;
	struct tm* timeInfo;
	
	time( &currTime );
	timeInfo = localtime( &currTime );

	// Create the string
	buffer << ( timeInfo->tm_year + 1900 ); // Set the year
	buffer << "-";
	
	if( timeInfo->tm_mday < 10 ){
		buffer << "0";
	}
	
	buffer << timeInfo->tm_mday; // Set the day
	buffer << "-";

	if( timeInfo->tm_mon + 1 < 10 ) {
		buffer << "0";
	}
	buffer << ( timeInfo->tm_mon + 1 ); // Month's in ctime range from 0-11
	
	buffer >> retString;

	return retString;
}

//! Get the current time as a string.
const string Logger::getTimeString() {
	time_t currTime;
	stringstream buffer;
	string retString;
	struct tm* timeInfo;
	
	time( &currTime );
	timeInfo = localtime( &currTime );
	
	if( timeInfo->tm_hour < 10 ){
		buffer << "0";
	}
	buffer << timeInfo->tm_hour;
	buffer << ":";
	
	if( timeInfo->tm_min < 10 ){
		buffer << "0";
	}
	buffer << timeInfo->tm_min;
	buffer << ":";
	
	if( timeInfo->tm_sec < 10 ){
		buffer << "0";
	}
	buffer << timeInfo->tm_sec;
	
	buffer >> retString;
	return retString;
}

//! Shorten the full path into the file name.
const string Logger::getFileNameFromPath( const string& fullPath ) {
	int position = fullPath.rfind( "\\" ); // find the last /. This may need modification for unix.
	return fullPath.substr( position ); // return all characters past the last /.
}

void Logger::XMLParse( const DOMNode* node ) {
	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string nodeName;

	// assume we were passed a valid node.
	assert( node );
	
	name = XMLHelper<string>::getAttrString( node, "name" );
	type = XMLHelper<string>::getAttrString( node, "type" );
	// get the children of the node.
	nodeList = node->getChildNodes();

	// loop through the children
	for ( int i = 0; i < static_cast<int> ( nodeList->getLength() ); i++ ){
		curr = nodeList->item( i );

		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		
		if ( nodeName == "FileName" ){
			fileName = XMLHelper<string>::getValueString( curr );
		}
		else if ( nodeName == "printLogNest" ) {
			printLogNest = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogTimeStamp" ) {
			printLogTimeStamp = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogDateStamp" ) {
			printLogDateStamp = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogLineNumber" ) {
			printLogLineNumber = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogWarningLevel" ) {
			printLogWarningLevel = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogFileName" ) {
			printLogFileName = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "printLogFullPath" ) {
			printLogFullPath = XMLHelper<bool>::getValue( curr );
		}
		else if ( nodeName == "minLogWarningLevel" ) {
			minLogWarningLevel = XMLHelper<int>::getValue( curr );
		}
		else if ( nodeName == "minToScreenWarningLevel" ) {
			minToScreenWarningLevel = XMLHelper<int>::getValue( curr );
		}
		else if ( nodeName == "logTabSize" ) {
			logTabSize = XMLHelper<int>::getValue( curr );
		}
		else if ( nodeName == "headerMessage" ) {
			headerMessage = XMLHelper<string>::getValueString( curr );
		}
	}
}

void Logger::toDebugXML( ostream& out ) const {
	
	// write out the root tag.
	out << "<Logger name=\"" << name << "\" type=\"" << type << "\" >" << endl;

	// increase the indent.
	Tabs::increaseIndent();

	XMLWriteElement( fileName, "fileName", out );
	XMLWriteElement( minLogWarningLevel, "minLogWarningLevel", out );
	XMLWriteElement( minToScreenWarningLevel, "minToScreenWarningLevel", out );
	XMLWriteElement( logTabSize, "logTabSize", out );
	XMLWriteElement( printLogNest, "printLogNest", out );
	XMLWriteElement( printLogWarningLevel, "printLogWarningLevel", out );
	XMLWriteElement( printLogTimeStamp, "printLogTimeStamp", out );
	XMLWriteElement( printLogDateStamp, "printLogDateStamp", out );
	XMLWriteElement( printLogLineNumber, "printLogLineNumber", out );
	XMLWriteElement( printLogFileName, "printLogFileName", out );
	XMLWriteElement( printLogFullPath, "printLogFullPath", out );

	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</Logger>" << endl;
}

//! Parses the header of a log file replacing special strings.
void Logger::parseHeader( string& headerIn ) {
	
	static const basic_string <char>::size_type npos = static_cast<char>( -1 );
	int offset = 0;
	int leftBracket = 0;
	int rightBracket = 0;
	string command;
	string toReplaceString;
	string replaceWithString;

	// Loop through the string.
	while( offset < static_cast<int>( headerIn.size() ) && offset != npos ){
		
		// Find the first left bracket.
		leftBracket = headerIn.find_first_of( "{", offset );
		offset = leftBracket;

		// Exit if we do not find it.
		if( leftBracket == npos ){
			break;
		}
		
		rightBracket = headerIn.find_first_of( "}", offset );
		offset = rightBracket;

		// Exit if we do not find it.
		if( rightBracket == npos ){
			break;
		}

		command = headerIn.substr( leftBracket + 1, rightBracket - leftBracket - 1 );
		toReplaceString = command;

		if( command == "date" ) {
			replaceWithString = getDateString();
		}
		else if( command == "time" ) {
			replaceWithString = getTimeString();
		}
		else {
			continue;
		}

		headerIn.replace( leftBracket, rightBracket - leftBracket + 1, replaceWithString );
	}
}