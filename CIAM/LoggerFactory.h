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
#include "Logger.h"

using namespace std;

/*! 
* \ingroup CIAM
* \brief This is a factory class which is used to instantiate create loggers.
*
* The LoggerFactory class itself cannot be instantiated, the only method which may be used is getLogger()
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
* \warning This class cannot be instantiated.
* \warning Loggers can only be created by the LoggerFactory.
*/

class LoggerFactory {
public:
	//! Returns an instance of the Logger class selected in the Configuration file.
	static Logger* getLogger();

	//! Returns whether the Logger is complete and ready to be used.
	static bool loggerIsReady();
	
	//! Resets its state and deletes the logger.
	static void cleanUp();

private:
	static Logger* logger; //!< Pointer to the contained logger class.
	static bool loggerCreated; //!< Flag which tells whether the logger has already been created.

	//! Private constructor to prevent a programmer from creating a LoggerFactory.
	LoggerFactory();
	
	//! Private copy constructor to prevent a programmer from copying a LoggerFactory.
	LoggerFactory( const LoggerFactory& loggerFactoryIn ){};

	//! Private assignment operator to prevent a programmer from copying a LoggerFactory.
	LoggerFactory& operator= ( const LoggerFactory& loggerFactoryIn ){ return *this; };

};

#endif // _LOGGER_FACTORY_H_