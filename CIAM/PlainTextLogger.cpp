#include "Definitions.h"
#include <iostream>
#include <string>
#include "Logger.h"
#include "PlainTextLogger.h"
#include "Configuration.h"

//! Constructor
PlainTextLogger::PlainTextLogger(){
}

//! Tells the logger to begin logging.
void PlainTextLogger::open( const char[] ){
	string loggerFileName;
	const Configuration* conf = Configuration::getInstance();

	loggerFileName = conf->getFile( "textLogFileName" );
	if( loggerFileName == "" ) { // set a default value
		cout << "Using default log file name." << endl;
		loggerFileName = "Log.txt";
	}

	logFile.open( loggerFileName.c_str(), ios::out );
}

//! Tells the logger to finish logging.
void PlainTextLogger::close(){
	logFile.close();
}

//! Logs a single message.
void PlainTextLogger::logCompleteMessage( const int line, const string& file, const WarningLevel warningLevelIn, const string& message ) {
	
	// Decide whether to print the message
	if ( warningLevelIn >= minLogWarningLevel ){
		
		// Print the tabs.
		if ( printLogNest ) {
			for ( int nest = 0; nest < currentNestLevel; nest++ ) {
				for ( int space = 0; space < logTabSize; space++ ) {
					logFile << " ";
				}
			}
		}

		// Print the date.
		if ( printLogDateStamp ) {
			logFile << getDateString() << " ";
		}
		
		// Print the timestamp.
		if ( printLogTimeStamp ) {
			logFile << getTimeString() << " ";
		}

		// Print the warning level
		if ( printLogWarningLevel ) {
			logFile << "Level " << warningLevelIn << ": ";
		}

		// Print the file name.
		if ( printLogFileName ) {
			if ( printLogFullPath ) {
				logFile << file;
			}
			else {
				logFile << getFileNameFromPath( file );
			}
		}
		
		// Print the line number
		if ( printLogLineNumber ) {
			logFile << "(" << line << "): ";
		}
		else {
			logFile << ": ";
		}

		// Print the message
		logFile << message << endl;
	}
}
