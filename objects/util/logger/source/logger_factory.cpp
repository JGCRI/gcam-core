/*! 
* \file logger_factory.cpp
* \ingroup CIAM
* \brief LoggerFactory class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <cassert>

// xerces xml headers
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/sax/HandlerBase.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include "util/base/include/xml_helper.h"
// end of xerces headers

#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"

// Logger subclass headers.
#include "util/logger/include/plain_text_logger.h"
#include "util/logger/include/xml_logger.h"

using namespace std;
using namespace xercesc;

map<string,Logger*> LoggerFactory::loggers;

//! Parse the XML data.
void LoggerFactory::XMLParse( const DOMNode* root ) {
	DOMNode* curr = 0;
	DOMNodeList* nodeList;
	string loggerType;
	string nodeName;
	
	Logger* newLogger = 0;
	
	/*! \pre assume we were passed a valid node. */
	assert( root );
	
	// get the children of the node.
	nodeList = root->getChildNodes();
	
	// loop through the children
	for ( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
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

/*! \brief Writes out the LoggerFactory to an XML file. 
*
* \param out Output stream to write to.
* \return void
* \warning This method is NOT constant, because static methods are not allowed to be declared const.
*/
void LoggerFactory::toDebugXML( ostream& out, Tabs* tabs ) {
	
	// write out the root tag.
	out << "<LoggerFactory>" << endl;

	// increase the indent.
	tabs->increaseIndent();

	for( map<string,Logger*>::const_iterator logIter = loggers.begin(); logIter != loggers.end(); logIter++ ){
		logIter->second->toDebugXML( out, tabs );
	}
	
	// decrease the indent.
	tabs->decreaseIndent();
	
	// write the closing tag.
	tabs->writeTabs( out );
	out << "</LoggerFactory>" << endl;
}

