#include "Definitions.h"
#include <string>
#include <map>
#include <cassert>

// xerces xml headers
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOM.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include "xmlHelper.h"
// end of xerces headers

#include "LoggerFactory.h"
#include "Logger.h"

// Logger subclass headers.
#include "PlainTextLogger.h"
#include "XMLLogger.h"

map<string,Logger*> LoggerFactory::loggers;

//! Parse the XML data.
void LoggerFactory::XMLParse( const DOMNode* root ) {
	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string loggerType;
	string nodeName;
	
	Logger* newLogger = 0;
	
	cout << "In XML Parse" << endl;
	
	// assume we were passed a valid node.
	assert( root );
	
	// get the children of the node.
	nodeList = root->getChildNodes();
	
	// loop through the children
	for ( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
		
		if( nodeName == "Logger" ) {
			// get the Logger type.
			loggerType = XMLHelper<string>::getAttrString( curr, "type" );
			
			// Add additional types here.
			if( loggerType == "PlainTextLogger" ){
				newLogger = new PlainTextLogger();
			}
			else if( loggerType == "XMLLogger" ){
				newLogger = new XMLLogger();
			}
			else {
				newLogger = new PlainTextLogger();
			}
			
			newLogger->XMLParse( curr );
			newLogger->open();
			loggers[ newLogger->name ] = newLogger;
		}
	}
}

//! Initialize the LoggerFactory
void LoggerFactory::initialize( XercesDOMParser* parser ){
	
	cout << "Initializing the LoggerFactory." << endl;
	// This cannot be in the configuration.xml file so that the Configuration class can safely use the LoggerFactory.
	const string loggerFileName = "LoggerFactory.xml";
	
	DOMDocument* doc = 0;
	DOMNode* root = 0;

	// Parse the LoggerFactory configuration file.
	try {
		const unsigned long startMillis = XMLPlatformUtils::getCurrentMillis();
		parser->parse( loggerFileName.c_str() );
		const unsigned long endMillis = XMLPlatformUtils::getCurrentMillis();
		long parseTime = endMillis - startMillis;
		cout << "Parsing took " << parseTime / float( 1000 ) << " seconds." << endl;
	} catch ( const XMLException& toCatch ) {
		string message = XMLHelper<string>::safeTranscode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
	} catch ( const DOMException& toCatch ) {
		string message = XMLHelper<string>::safeTranscode( toCatch.msg );
		cout << "Exception message is:" << endl << message << endl;
	} catch ( const SAXException& toCatch ){
		string message = XMLHelper<string>::safeTranscode( toCatch.getMessage() );
		cout << "Exception message is:" << endl << message << endl;
	} catch (...) {
		cout << "Unexpected Exception." << endl;
	}
	
	doc = parser->getDocument();
	root = doc->getDocumentElement();
	XMLParse( root );
}

//! Returns the instance of the Logger, creating it if neccessary.
Logger* LoggerFactory::getLogger( const string& loggerName ) {
	map<string,Logger*>::const_iterator logIter = loggers.find( loggerName );
	
	if( logIter != loggers.end() ) {
		return logIter->second;
	}
	else {
		cout << "Creating uninitialized logger." << endl;
		Logger* newLogger = new PlainTextLogger( loggerName );
		newLogger->open();
		loggers[ loggerName ] = newLogger;
		return newLogger;
	}
}

//! Cleans up the logger.
void LoggerFactory::cleanUp() {
	for( map<string,Logger*>::iterator logIter = loggers.begin(); logIter != loggers.end(); logIter++ ){
		logIter->second->close();
		delete logIter->second;
	}
	loggers.clear();
	
}

void LoggerFactory::toDebugXML( ostream& out ) const {
	
	// write out the root tag.
	out << "<LoggerFactory>" << endl;

	// increase the indent.
	Tabs::increaseIndent();

	for( map<string,Logger*>::const_iterator logIter = loggers.begin(); logIter != loggers.end(); logIter++ ){
		logIter->second->toDebugXML( out );
	}
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</LoggerFactory>" << endl;
}


	