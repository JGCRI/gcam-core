#ifndef _XML_LOGGER_H_
#define _XML_LOGGER_H_
#pragma once

/*! 
* \file XMLLogger.h
* \ingroup CIAM
* \brief The XMLLogger class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iostream>
#include "Logger.h"

/*! 
* \ingroup CIAM
* \brief This is a class which implements the Logger interface.. 
*
* This Logger prints message to a log file in an XML format as defined by the XMLLog schema..
* It does not support nesting or options to activate or deactive sections of the log message.
* It does support the option to print absolute or relative pathnames.
*
* \author Josh Lurz
* \warning Since XMLLoggers can only be created by the LoggerFactory, public functions not in the Logger interface will be unusable.
*/

class XMLLogger: public Logger {
	friend LoggerFactory;
private:
	
	//! The filestream to which data is written.
	ofstream logFile;
	
	XMLLogger( const string& loggerName ="" );

public:
	virtual void open( const char[] = 0 );
	virtual void close();
	virtual void logCompleteMessage( const int line, const string& file, const WarningLevel warningLevel, const string& message );	
};

#endif _XML_LOGGER_H_