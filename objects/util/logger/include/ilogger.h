#ifndef _ILOGGER_H_
#define _ILOGGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ilogger.h
* \ingroup objects
* \brief The ILogger interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iostream>
#include <string>
/*! 
* \ingroup objects
* \brief This is an abstract class which defines the interface to a Logger.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class ILogger: public std::ostream {
public:
    //! Enumeration which describes the possible levels for messages.
    enum WarningLevel 
    {
        DEBUG, //!< Debugging warning level.
        NOTICE, //!< Notice warning level.
        WARNING, //!< Warning warning level.
        ERROR, //!< Error warning level.
        SEVERE //!< Severe warning level.
    };
    ILogger( std::streambuf* aStreamBuf ): std::ostream( aStreamBuf ){}
    virtual ~ILogger(){};
    virtual void open( const char[] = 0 ) = 0;
    virtual int receiveCharFromUnderStream( int ch ) = 0;
    virtual void close() = 0;
    virtual void setLevel( const WarningLevel newLevel ) = 0;
    static ILogger& getLogger( const std::string& aLoggerName );
};

#endif // _ILOGGER_H_

