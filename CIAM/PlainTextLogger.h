#ifndef _PLAIN_TEXT_LOGGER_H_
#define _PLAIN_TEXT_LOGGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file PlainTextLogger.h
* \ingroup CIAM
* \brief The PlainTextLogger class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <fstream>
#include "Logger.h"

/*! 
* \ingroup CIAM
* \brief This is a class which implements the Logger interface.. 
*
* This Logger is very simple and prints log messages to a file in a plain text format.
* It does support nesting and options to activate or deactive sections of the log line.
*
* \author Josh Lurz
* \warning Since PlainTextLoggers can only be created by the LoggerFactory, public functions not in the Logger interface will be unusable.
*/

class PlainTextLogger: public Logger {
    friend class LoggerFactory;
private:

    //! The filestream to which data is written.
    std::ofstream logFile;

    PlainTextLogger( const std::string& loggerName ="" );

public:
    virtual void open( const char[] = 0 );
    virtual void close();
    virtual void logCompleteMessage( const int line, const std::string& file, const WarningLevel warningLevel, const std::string& message );	
};

#endif // _PLAIN_TEXT_LOGGER_H_

