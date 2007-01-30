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
#include "util/base/include/iparsable.h"

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
    friend class LoggerFactoryWrapper;
public:
    static Logger& getLogger( const std::string& aLogName );
    static void toDebugXML( std::ostream& aOut, Tabs* aTabs );
private:
    static std::map<std::string,Logger*> mLoggers; //!< Map of logger names to loggers.
    static void XMLParse( const xercesc::DOMNode* aRoot );
    static void cleanUp();
    //! Private undefined constructor to prevent creating a LoggerFactory.
    LoggerFactory();
    //! Private undefined copy constructor to prevent  copying a LoggerFactory.
    LoggerFactory( const LoggerFactory& );
    //! Private undefined assignment operator to prevent  copying a LoggerFactory.
    LoggerFactory& operator= ( const LoggerFactory& );
};

/*! 
* \ingroup Objects
* \brief This is a proxy or wrapper class which allows the IParsable functions to be translated into
* the static LoggerFactory calls. This is required because a static class cannot have virtual functions,
* nor can it inherit them. This class does not have any data members, or functions not defined by the
* IParsable class. This class also ensures that the LoggerFactory::cleanUp method is called, so this wrapper 
* class should not be destroyed until the end of the model.
* \todo LoggerFactory should be converted to a singleton instead of a static class so that this is not 
* necessary. 
* \author Josh Lurz
* \warning This class cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/
class LoggerFactoryWrapper: public IParsable {
public:
    ~LoggerFactoryWrapper() {
        LoggerFactory::cleanUp();
    }
    bool XMLParse( const xercesc::DOMNode* aRoot ){
        LoggerFactory::XMLParse( aRoot );
        return true;
    }
};

#endif // _LOGGER_FACTORY_H_

