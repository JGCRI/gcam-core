#ifndef _LOGGER_H_
#define _LOGGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file logger.h
* \ingroup CIAM
* \brief The Logger class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include <sstream>
#include <xercesc/dom/DOMNode.hpp>
#include "util/logger/include/ilogger.h"

// Forward definition of the Logger class.
class Logger; 
class Tabs;

/*!
* \ingroup CIAM
* \brief This is an overridden streambuffer class used by the Logger class.
* 
* This is a very simple class which contains a pointer to its parent Logger.
* When the streambuf receives a character it passes it to its parent stream for processing.
*
* \author Josh Lurz
* \warning Overriding the iostream class is somewhat difficult so this class may be somewhat esoteric.
* \warning This is a write-only streambuf.
*/

class PassToParentStreamBuf: std::streambuf {
    friend class Logger;

public:
    PassToParentStreamBuf();
    int overflow( int ch );
    int underflow( int ch );
    void setParent( Logger* parentIn );
    void toDebugXML( std::ostream& out ) const;
private:
    //! A pointer to the parent logger which will receive all data. 
    Logger* mParent;
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

class Logger: public ILogger {

    //! Friend declaration to allow LoggerFactory to create Loggers.
    friend class LoggerFactory;
public:
    virtual ~Logger(); //!< Virtual destructor.
    virtual void open( const char[] = 0 ) = 0; //!< Pure virtual function called to begin logging.
    int receiveCharFromUnderStream( int ch ); //!< Pure virtual function called to complete the log and clean up.
    virtual void close() = 0;
    void setLevel( const ILogger::WarningLevel newLevel );
    void toDebugXML( std::ostream& out, Tabs* tabs ) const;
protected:
    std::string mName; //!< Logger name
    std::string mType; //!< Logger type
    std::string mFileName; //!< File name of the file it uses.
    std::string mHeaderMessage; //!< Header message to print at the beginning of the log.
    ILogger::WarningLevel mMinLogWarningLevel; //!< Defines the minimum level of messages which should be printed.
    ILogger::WarningLevel mMinToScreenWarningLevel; //!< Defines the mininum level of warnings to print to the console.
    ILogger::WarningLevel mCurrentWarningLevel; //!< Defines the current warning level.
    bool mPrintLogWarningLevel; //!< Defines whether to print the warning level.
    Logger( const std::string& aFileName = "" );
    //! Log a message with the given warning level.
    virtual void logCompleteMessage( const std::string& aMessage ) = 0;
    void printToScreenIfConfigured( const std::string& aMessage );
    static void parseHeader( std::string& aHeader );
    const static int MAX_LINE_SIZE = 1000;
private:
    std::stringstream mBuf; //!< Buffer which contains characters waiting to be printed.
    PassToParentStreamBuf mUnderStream; //!< Underlying ofstream
    void XMLParse( const xercesc::DOMNode* node );
    static const std::string getTimeString();
    static const std::string getDateString();

};

#endif // _LOGGER_H_

