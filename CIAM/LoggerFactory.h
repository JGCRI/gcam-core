#ifndef _LOGGER_FACTORY_H_
#define _LOGGER_FACTORY_H_
#pragma once

/*! 
* \file LoggerFactory.h
* \ingroup CIAM
* \brief The LoggerFactory class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <map>
#include "Logger.h"

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief This is a factory class which is used to instantiate create loggers.
*
*
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \warning This class cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class LoggerFactory {


public:
	//! Initialize the LoggerFactory
	static void initialize( XercesDOMParser* parser );

	//! Returns an instance of the Logger class selected in the Configuration file.
	static Logger* getLogger( const string& loggerName );
	
	//! Resets its state and deletes the logger.
	static void cleanUp();
	
	void toDebugXML( ostream& out ) const;

private:
	static map<string,Logger*> loggers; //!< Map of logger names to loggers.

	static bool loggerCreated; //!< Flag which tells whether the logger has already been created.

	//! Private constructor to prevent a programmer from creating a LoggerFactory.
	LoggerFactory();
	
	//! Private copy constructor to prevent a programmer from copying a LoggerFactory.
	LoggerFactory( const LoggerFactory& loggerFactoryIn ){};

	//! Private assignment operator to prevent a programmer from copying a LoggerFactory.
	LoggerFactory& operator= ( const LoggerFactory& loggerFactoryIn ){ return *this; };
	
	static void XMLParse( const DOMNode* root );
};

#endif // _LOGGER_FACTORY_H_