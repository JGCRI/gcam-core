#ifndef _XML_LOGGER_H_
#define _XML_LOGGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file xml_logger.h
* \ingroup Util
* \brief The XMLLogger class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/logger/include/logger.h"

/*! 
* \ingroup Objects
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
    friend class LoggerFactory;
public:
    void open( const char[] = 0 );
    void close();
    void logCompleteMessage( const std::string& aMessage );	

private:
    std::ofstream mLogFile; //!< The filestream to which data is written.
    XMLLogger( const std::string& loggerName ="" );
};

#endif // _XML_LOGGER_H_

