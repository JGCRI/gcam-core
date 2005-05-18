/*! 
* \file plain_text_logger.cpp
* \ingroup Objects
* \brief PlainTextLogger class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include "util/logger/include/logger.h"
#include "util/logger/include/plain_text_logger.h"

using namespace std;

//! Constructor
PlainTextLogger::PlainTextLogger( const string& aLoggerName ):Logger( aLoggerName ){
}

//! Tells the logger to begin logging.
void PlainTextLogger::open( const char[] ){
    if( mFileName == "" ) { // set a default value
        cout << "Using default log file name." << endl;
        mFileName = "log.txt";
    }

    mLogFile.open( mFileName.c_str(), ios::out );

    // Print the header message
    if( mHeaderMessage != "" ){
        parseHeader( mHeaderMessage );
        mLogFile << mHeaderMessage << endl << endl;
    }
}

//! Tells the logger to finish logging.
void PlainTextLogger::close(){
    mLogFile.close();
}

//! Logs a single message.
void PlainTextLogger::logCompleteMessage( const string& aMessage ){
    // Decide whether to print the message
    if ( mCurrentWarningLevel >= mMinLogWarningLevel ){
        // Print the warning level
        if ( mPrintLogWarningLevel ) {
            mLogFile << "Level " << mCurrentWarningLevel << ":";
        }
        mLogFile << aMessage << endl;
    }
}
