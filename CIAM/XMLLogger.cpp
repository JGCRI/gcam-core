#include "Definitions.h"
#include <iostream>
#include <string>
#include <sstream>
#include "scenario.h"
#include "Logger.h"
#include "XMLLogger.h"
#include "Configuration.h"

extern time_t ltime;

//! Constructor
XMLLogger::XMLLogger( const string& loggerName ):Logger( loggerName ){
}

//! Tells the logger to begin logging.
void XMLLogger::open( const char[] ){
	if( fileName == "" ) { // set a default value
		cout << "Using default log file name." << endl;
		fileName = "Log.xml";
	}

	logFile.open( fileName.c_str(), ios::out );

	// Print the header message
	string dateString = Scenario::XMLCreateDate( ltime );
	logFile << "<XMLLogger name=\"" << name << "\" date=\"" << dateString << "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"D:\\cvs\\Code\\EXE\\XMLLog.xsd\">" << endl;
}

//! Tells the logger to finish logging.
void XMLLogger::close(){
	// Print the closing tag
	logFile << "</XMLLogger>" << endl;
	logFile.close();
}

//! Logs a single message.
void XMLLogger::logCompleteMessage( const int line, const string& file, const WarningLevel warningLevelIn, const string& message ) {
	
	// Decide whether to print the message
	if ( warningLevelIn >= minLogWarningLevel ){
		
		// Print the opening log tag.
		logFile << "\t<LogEntry>" << endl;
		
		// Print the warning level
		logFile << "\t\t<WarningLevel>" << warningLevelIn << "</WarningLevel>" << endl;
		
		// Print the file.
		if ( printLogFullPath ) {		
			logFile << "\t\t<Filename>" << file << "</Filename>" << endl;
		}
		else {
			logFile << "\t\t<Filename>" << getFileNameFromPath( file ) << "</Filename>" << endl;
		}

		// Print the line.
		logFile << "\t\t<LineNumber>" << line << "</LineNumber>" << endl;

		// Print the message
		logFile << "\t\t<Message>" << message << "</Message>" << endl;

		// Print the closing tag.
		logFile << "\t</LogEntry>" << endl;
	}
}
