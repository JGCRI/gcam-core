#ifndef _LOGGER_FACTORY_H_
#define _LOGGER_FACTORY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file logger_factory.h
* \ingroup Objects
* \brief The LoggerFactory class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <xercesc/dom/DOMNode.hpp>

// Forward Declaration
class Logger;
class Tabs;

/*! 
* \ingroup Objects
* \brief This is a factory class which is used to instantiate create loggers.
* \author Josh Lurz
* \warning This class cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class LoggerFactory {
public:
    static Logger& getLogger( const std::string& aLogName );
    static void cleanUp();
    static void toDebugXML( std::ostream& aOut, Tabs* aTabs );
    static void XMLParse( const xercesc::DOMNode* aRoot );
private:
    static std::map<std::string,Logger*> mLoggers; //!< Map of logger names to loggers.
    //! Private undefined constructor to prevent creating a LoggerFactory.
    LoggerFactory();
    //! Private undefined copy constructor to prevent  copying a LoggerFactory.
    LoggerFactory( const LoggerFactory& );
    //! Private undefined assignment operator to prevent  copying a LoggerFactory.
    LoggerFactory& operator= ( const LoggerFactory& );
};

#endif // _LOGGER_FACTORY_H_

