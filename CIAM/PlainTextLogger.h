#ifndef _PLAIN_TEXT_LOGGER_H_
#define _PLAIN_TEXT_LOGGER_H_

#include "Definitions.h"
#include <string>
#include <iostream>
#include "Logger.h"

/*! 
* \ingroup CIAM
* \brief This is a class which implements the Logger interface.. 
*
* This Logger is very simple and prints log messages to a file in a plain text format.
* It does support nesting and options to activate or deactive sections of the log line.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \warning Since PlainTextLoggers can only be created by the LoggerFactory, public functions not in the Logger interface will be unusable.
*/

class PlainTextLogger: public Logger {
	
	friend LoggerFactory;

public:
	virtual void open( const char[] = 0 );
	virtual void close();
	virtual void logCompleteMessage( const int line, const string& file, const WarningLevel warningLevel, const string& message );	

private:
	
	//! The filestream to which data is written.
	ofstream logFile;
	
	PlainTextLogger();
};

#endif _PLAIN_TEXT_LOGGER_H_