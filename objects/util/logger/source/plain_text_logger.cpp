/*! 
* \file plain_text_logger.cpp
* \ingroup CIAM
* \brief PlainTextLogger class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <sstream>
#include "util/logger/include/logger.h"
#include "util/logger/include/plain_text_logger.h"

using namespace std;

//! Constructor
PlainTextLogger::PlainTextLogger( const string& loggerName ):Logger( loggerName ){
}

//! Tells the logger to begin logging.
void PlainTextLogger::open( const char[] ){
    if( fileName == "" ) { // set a default value
        cout << "Using default log file name." << endl;
        fileName = "Log.txt";
    }

    logFile.open( fileName.c_str(), ios::out );

    // Print the header message
    if( headerMessage != "" ){
        parseHeader( headerMessage );
        logFile << headerMessage << endl << endl;
    }
}

//! Tells the logger to finish logging.
void PlainTextLogger::close(){
    logFile.close();
}

//! Logs a single message.
void PlainTextLogger::logCompleteMessage( const int line, const string& file, const WarningLevel warningLevelIn, const string& message ) {

    // Decide whether to print the message
    if ( warningLevelIn >= minLogWarningLevel ){
        stringstream buffer;
        bool printColon = false;

        // Print the tabs.
        if ( printLogNest ) {
            for ( int nest = 0; nest < currentNestLevel; nest++ ) {
                for ( int space = 0; space < logTabSize; space++ ) {
                    buffer << " ";
                }
            }
        }

        // Print the date.
        if ( printLogDateStamp ) {
            buffer << getDateString() << " ";
            printColon = true;
        }

        // Print the timestamp.
        if ( printLogTimeStamp ) {
            buffer << getTimeString() << " ";
            printColon = true;
        }

        // Print the warning level
        if ( printLogWarningLevel ) {
            buffer << "Level " << warningLevelIn << " ";
            printColon = true;
        }

        // Print the file name.
        if ( printLogFileName ) {
            printColon = true;
            if ( printLogFullPath ) {
                buffer << file;
            }
            else {
                buffer << getFileNameFromPath( file );
            }
        }

        // Print the line number
        if ( printLogLineNumber ) {
            printColon = true;
            buffer << "(" << line << ")";
        }

        if( printColon ){
            buffer << ":";
        }

        buffer << message;

        logFile << buffer.str() << endl;
    }
}
