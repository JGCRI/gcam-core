#ifndef _LOGGER_H_
#define _LOGGER_H_
#pragma once

/*! 
* \file Logger.h
* \ingroup CIAM
* \brief The Logger class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <iostream>
#include <fstream>
#include <sstream>
#include <streambuf>
#include <xercesc/dom/DOM.hpp>

using namespace std;
using namespace xercesc;

//! Macro used to insert the line and file into a logging command.
#define LOG( logger, level ) logger->setLevel( level ); logger->setLine( __LINE__ ); logger->setFile( __FILE__ ); *logger

// Forward definition of the Logger class.
class Logger; 

/*!
* \ingroup CIAM
* \brief This is an overridden streambuffer class used by the Logger class.
* 
* This is a very simple class which contains a pointer to its parent Logger.
* When the streambuf receives a character it passes it to its parent stream for processing.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \warning Overriding the iostream class is somewhat difficult so this class may be somewhat esoteric.
* \warning This is a write-only streambuf.
*/

class PassToParentStreamBuf: streambuf {
	
	friend Logger;
	
public:
	
	PassToParentStreamBuf();
	int overflow( int ch );
	int underflow( int ch );
	void setParent( Logger* parentIn );
	void toDebugXML( ostream& out ) const;

private:

	//! A pointer to the parent logger which will receive all data. 
	Logger* parent;
};

// Forward definition of LoggerFactory class.
class LoggerFactory;

/*! 
* \ingroup CIAM
* \brief This is an abstract class which defines the interface to a Logger. 
*
* Loggers may come in many different forms, but must use the defined interface.
* Each error message is given a priority, and the user may set the level of log messages they wish to print.
* Loggers are singletons and can only be instantiated by the LoggerFactory class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \warning This is an abstract class and cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class Logger: public ostream {
	
	//! Friend declaration to allow LoggerFactory to create Loggers.
	friend LoggerFactory;
	
public:
	
	//! Enumeration which describes the possible levels for messages.
	enum WarningLevel 
	{
		DEBUG_LEVEL, //!< Debugging warning level.
			NOTICE_LEVEL, //!< Notice warning level.
			WARNING_LEVEL, //!< Warning warning level.
			ERROR_LEVEL, //!< Error warning level.
			SEVERE_LEVEL //!< Severe warning level.
	};
	
	//! Virtual destructor.
	virtual ~Logger();
	
	//! Pure virtual function called to begin logging.
	virtual void open( const char[] = 0 ) = 0;
	
	void increaseNest();
	
	void decreaseNest();
	
	int receiveCharFromUnderStream( int ch );
	
	//! Pure virtual function called to complete the log and clean up.
	virtual void close() = 0;
	
	void setLevel( const WarningLevel newLevel );
	
	void setLine( const int lineIn );
	
	void setFile( const string& fileIn );
	
	void toDebugXML( ostream& out ) const;

protected:
	//! Logger name
	string name;

	//! Logger type
	string type;
	
	//! File name of the file it uses.
	string fileName;
	
	//! Header message to print at the beginning of the log.
	string headerMessage;

	//! Defines the minimum level of warnings which should be omitted.
	int minLogWarningLevel;
	
	//! Defines the mininum level of warnings to print to the console.
	int minToScreenWarningLevel;

	//! Defines the current warning level.
	WarningLevel currentWarningLevel;
	
	//! Defines the current line number
	int currentLine;
	
	//! Defines the current file name.
	string currentFile;
	
	//! Defines the current level of nesting. May be ignored by subclass.
	int currentNestLevel;
	
	//! Defines the tab size
	int logTabSize;
	
	//! Defines whether to print the nesting.
	bool printLogNest;
	
	//! Defines whether to print the warning level.
	bool printLogWarningLevel;

	//! Defines whether or not to print the timestamp.
	bool printLogTimeStamp;
	
	//! Defines whether to print the datestamp.
	bool printLogDateStamp;
	
	//! Defines whether to print the line number.
	bool printLogLineNumber;
	
	//! Defines whether to print the file name.
	bool printLogFileName;
	
	//! Defines whether to print the full path
	bool printLogFullPath;
	
	Logger( const string& fileNameIn = "" );
	
	static const string getDateString();
	
	static const string getTimeString();
	
	// make static
	static const string getFileNameFromPath( const string& fullPath );
	
	//! Log a message with the given warning level.
	virtual void logCompleteMessage( const int line, const string& file, const WarningLevel warningLevel, const string& message ) = 0;
	
	void static parseHeader( string& headerIn );

private:

	//! Buffer which contains characters waiting to be printed.
	stringstream buf;
	
	//! Underlying ofstream
	PassToParentStreamBuf underStream;

	void XMLParse( const DOMNode* node );
};

#endif // _LOGGER_H_