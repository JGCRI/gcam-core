#ifndef _LOGGER_FACTORY_H_
#define _LOGGER_FACTORY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file LoggerFactory.h
* \ingroup CIAM
* \brief The LoggerFactory class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>

// Forward Declaration
class Logger;

/*! 
* \ingroup CIAM
* \brief This is a factory class which is used to instantiate create loggers.
* \author Josh Lurz
* \warning This class cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class LoggerFactory {

public:

   static Logger* getLogger( const std::string& loggerName );
	
   static void cleanUp();
	
   static void toDebugXML( std::ostream& out );

   static void XMLParse( const xercesc::DOMNode* root );

private:
   static std::map<std::string,Logger*> loggers; //!< Map of logger names to loggers.

	static bool loggerCreated; //!< Flag which tells whether the logger has already been created.

	//! Private undefined constructor to prevent a programmer from creating a LoggerFactory.
	LoggerFactory();
	
	//! Private undefined copy constructor to prevent a programmer from copying a LoggerFactory.
	LoggerFactory( const LoggerFactory& );

	//! Private undefined assignment operator to prevent a programmer from copying a LoggerFactory.
	LoggerFactory& operator= ( const LoggerFactory& );
};

#endif // _LOGGER_FACTORY_H_

