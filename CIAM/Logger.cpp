#include "Definitions.h"
#include <ctime>
#include <string>
#include <iostream>
#include <sstream>
#include <cassert>
#include "Logger.h"
#include "Configuration.h"

using namespace std;

//! Default Constructor
PassToParentStreamBuf::PassToParentStreamBuf() {
	parent = 0;
}

//! Overriding overflow function which passes its argument to its parent.
int PassToParentStreamBuf::overflow( int ch ){
	//! \pre Make sure the parent is not null.
	assert( parent );
	return parent->receiveCharFromUnderStream( ch );
}

//! Overriding underflow function which should not be reached because this is a write-only stream.
int PassToParentStreamBuf::underflow( int ch ) {
	
	//! \pre This function should never be called..
	assert( false );
	return 0;
}

//! Set the parent of the stream to which we will pass all data.
void PassToParentStreamBuf::setParent( Logger* parentIn ) {
	
	//! \pre Make sure a non-null parent is passed in.
	assert( parentIn );
	parent = parentIn;
}

//! Constructor which reads in values from configuration file.
Logger::Logger() : ostream( &underStream ) {
	
	// Initialize all variables which are not set by Configuration values.
	currentNestLevel = 0;
	currentWarningLevel = DEBUG_LEVEL;
	currentLine = 0;

	// Get an instance of the Configuration class.
	const Configuration* conf = Configuration::getInstance();
	
	// Set the understream's parent to this Logger.
	underStream.setParent( this );

	// Read in the configuration information from the Configuration file.
	minLogWarningLevel = conf->getInt( "minLogWarningLevel" );
	
	logTabSize = conf->getInt( "logTabSize" );
	
	printLogNest = conf->getBool( "printLogNest" );
	
	printLogWarningLevel = conf->getBool( "printLogWarningLevel" );

	printLogTimeStamp = conf->getBool( "printLogTimeStamp" );

	printLogDateStamp = conf->getBool( "printLogDateStamp" );

	printLogLineNumber = conf->getBool( "printLogLineNumber" );

	printLogFileName = conf->getBool( "printLogFileName" );

	printLogFullPath = conf->getBool( "printLogFullPath" );
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
		logCompleteMessage( currentLine, currentFile, currentWarningLevel, buffer );
	}
	
	return ch;
}

//! Get the current date as a string
const string Logger::getDateString() const {
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
const string Logger::getTimeString() const {
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
const string Logger::getFileNameFromPath( const string& fullPath ) const {
	int position = fullPath.rfind( "\\" ); // find the last /. This may need modification for unix.
	return fullPath.substr( position ); // return all characters past the last /.
}