/*! 
* \file xml_logger.cpp
* \ingroup CIAM
* \brief XMLLogger class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <ctime>
#include <fstream>
#include "util/logger/include/xml_logger.h"
#include "util/logger/include/logger.h"
#include "util/base/include/util.h"

using namespace std;

//! Constructor
XMLLogger::XMLLogger( const string& aLoggerName ):Logger( aLoggerName ){
}

//! Tells the logger to begin logging.
void XMLLogger::open( const char[] ){
	if( mFileName == "" ) { // set a default value
		cout << "Using default log file name." << endl;
		mFileName = "log.xml";
	}

    mLogFile.open( mFileName.c_str(), ios::out );

	// Print the header message
	time_t ltime;
	time(&ltime);
	string dateString = util::XMLCreateDate( ltime );
	mLogFile << "<XMLLogger name=\"" << mName << "\" date=\"" << dateString << "\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"D:\\cvs\\Code\\EXE\\XMLLog.xsd\">" << endl;
}

//! Tells the logger to finish logging.
void XMLLogger::close(){
	// Print the closing tag
	mLogFile << "</XMLLogger>" << endl;
	mLogFile.close();
}

//! Logs a single message.
void XMLLogger::logCompleteMessage( const string& aMessage ){
	// Decide whether to print the message
	if ( mCurrentWarningLevel >= mMinLogWarningLevel ){
		// Print the opening log tag.
		mLogFile << "\t<LogEntry>" << endl;
		
		// Print the warning level
		mLogFile << "\t\t<WarningLevel>" << mCurrentWarningLevel << "</WarningLevel>" << endl;

		// Print the message
		mLogFile << "\t\t<Message>" << aMessage << "</Message>" << endl;

		// Print the closing tag.
		mLogFile << "\t</LogEntry>" << endl;
	}
}
