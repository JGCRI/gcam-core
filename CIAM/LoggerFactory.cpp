#include "Definitions.h"
#include <string>
#include "PlainTextLogger.h"
#include "Configuration.h"
#include "LoggerFactory.h"
#include "Logger.h"


// Intialize static data members
Logger* LoggerFactory::logger = 0;
bool LoggerFactory::loggerCreated = false;

//! Returns whether the logger is ready to log.
bool LoggerFactory::loggerIsReady() {
	return loggerCreated;
}

//! Returns the instance of the Logger, creating it if neccessary.
Logger* LoggerFactory::getLogger() {
	if( loggerCreated ) {
		return logger;
	}
	else {
		Configuration* conf = Configuration::getInstance();
		string loggerType = conf->getString( "loggertype" );

		// Select the appropriate logger.
		if ( loggerType == "PlainTextLogger" ){
			logger = new PlainTextLogger();
		}
		else { // by default create plain text logger.
			logger = new PlainTextLogger();
		}

		loggerCreated = true;
		logger->open();

		return logger;
	}

}

//! Cleans up the logger.
void LoggerFactory::cleanUp() {
	if( loggerCreated ){
		logger->close();
		delete logger;
		logger = 0;
		loggerCreated = false;
	}
}