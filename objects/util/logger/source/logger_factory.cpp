/*! 
* \file logger_factory.cpp
* \ingroup Objects
* \brief LoggerFactory class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "util/base/include/xml_helper.h"
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/logger/include/ilogger.h"
// Logger subclass headers.
#include "util/logger/include/plain_text_logger.h"
#include "util/logger/include/xml_logger.h"

using namespace std;
using namespace xercesc;

map<string,Logger*> LoggerFactory::mLoggers;

//! Parse the XML data.
void LoggerFactory::XMLParse( const DOMNode* aRoot ){
	/*! \pre assume we were passed a valid node. */
	assert( aRoot );
	
	// get the children of the node.
	DOMNodeList* nodeList = aRoot->getChildNodes();
	
	// loop through the children
	for ( unsigned int i = 0; i < nodeList->getLength(); ++i ){
		DOMNode* curr = nodeList->item( i );
		string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		
		if( nodeName == "Logger" ) {
			// get the Logger type.
			string loggerType = XMLHelper<string>::getAttrString( curr, "type" );
			
			// Add additional types here.
            Logger* newLogger = 0;
			if( loggerType == "PlainTextLogger" ){
				newLogger = new PlainTextLogger();
			}
			else if( loggerType == "XMLLogger" ){
				newLogger = new XMLLogger();
			}
			else {
                cerr << "Unknown Logger Type: " << loggerType << endl;
                return;
			}
			
			newLogger->XMLParse( curr );
			newLogger->open();
			mLoggers[ newLogger->mName ] = newLogger;
		}
	}
}

//! Single static method of ILogger interface.
ILogger& ILogger::getLogger( const string& aLoggerName ){
    return LoggerFactory::getLogger( aLoggerName );
}

//! Returns the instance of the Logger, creating it if neccessary.
Logger& LoggerFactory::getLogger( const string& aLoggerName ) {
	map<string,Logger*>::const_iterator logIter = mLoggers.find( aLoggerName );
	
	if( logIter != mLoggers.end() ) {
		return *logIter->second;
	}
	else {
		cout << "Creating an uninitialized logger." << endl;
		Logger* newLogger = new PlainTextLogger( aLoggerName );
		newLogger->open();
        mLoggers[ aLoggerName ] = newLogger;
		return *mLoggers[ aLoggerName ];
	}
}

//! Cleans up the logger.
void LoggerFactory::cleanUp() {
	for( map<string,Logger*>::iterator logIter = mLoggers.begin(); logIter != mLoggers.end(); logIter++ ){
		logIter->second->close();
		delete logIter->second;
	}
}

/*! \brief Writes out the LoggerFactory to an XML file. 
*
* \param aOut Output stream to write to.
* \param aTabs A tabs object responsible for printing the correct number of tabs. 
* \warning This method is NOT constant, because static methods are not allowed to be declared const.
*/
void LoggerFactory::toDebugXML( ostream& aOut, Tabs* aTabs ) {
	
    XMLWriteOpeningTag( "LoggerFactory", aOut, aTabs );
	for( map<string,Logger*>::const_iterator logIter = mLoggers.begin(); logIter != mLoggers.end(); ++logIter ){
		logIter->second->toDebugXML( aOut, aTabs );
	}
	XMLWriteClosingTag( "LoggerFactory", aOut, aTabs );
}

