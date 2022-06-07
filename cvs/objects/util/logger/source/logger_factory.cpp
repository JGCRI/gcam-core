/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file logger_factory.cpp
* \ingroup Objects
* \brief LoggerFactory class source file.
* \author Josh Lurz
* \date $Date: 2007/01/11 00:13:34 $
* \version $Revision: 1.7.2.4 $
*/

#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <cassert>
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/logger_factory.h"
#include "util/logger/include/logger.h"
#include "util/logger/include/ilogger.h"
// Logger subclass headers.
#include "util/logger/include/plain_text_logger.h"
#include "util/logger/include/xml_logger.h"

using namespace std;

map<string,Logger*> LoggerFactory::mLoggers;

bool LoggerFactory::XMLParse(rapidxml::xml_node<char>* & aNode) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    
    if( nodeName == "Logger" ) {
        // get the Logger type.
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        string loggerType = attrs["type"];
        
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
        }
        
        ParseChildData parseChildHelper(aNode, attrs);
        parseChildHelper.setContainer(newLogger);
        ExpandDataVector<Logger::SubClassFamilyVector> getDataVector;
        newLogger->doDataExpansion( getDataVector );
        getDataVector.getFullDataVector(parseChildHelper);
        newLogger->open();
        mLoggers[ newLogger->mName ] = newLogger;
        return true;
    }
    else {
        return false;
    }
}

//! Single static method of ILogger interface.
ILogger& ILogger::getLogger( const string& aLoggerName ){
    return LoggerFactory::getLogger( aLoggerName );
}

//! Returns the instance of the Logger, creating it if necessary.
Logger& LoggerFactory::getLogger( const string& aLoggerName ) {
	map<string,Logger*>::const_iterator logIter = mLoggers.find( aLoggerName );
	
	if( logIter != mLoggers.end() ) {
		return *logIter->second;
	}
	else {
		cout << "Creating an uninitialized logger " << aLoggerName << endl;
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
    mLoggers.clear();
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

/*!
 * \brief Log to all configured loggers that the scenario identified by the given scenario
 *        name is starting.
 */
void LoggerFactory::logNewScenarioStarting( const string& aScenarioName ) {
	for( map<string,Logger*>::const_iterator logIter = mLoggers.begin(); logIter != mLoggers.end(); ++logIter ){
        ILogger::WarningLevel oldLevel = logIter->second->setLevel( ILogger::WARNING );
        (*logIter->second) << "Starting new scenario: " << aScenarioName << endl;
        logIter->second->setLevel( oldLevel );
    }
}

const string& LoggerFactoryWrapper::getXMLNameStatic() {
    const static string XML_NAME = "LoggerFactoryWrapper";
    return XML_NAME;
}

const string& LoggerFactoryWrapper::getXMLName() const {
    return getXMLNameStatic();
}

bool LoggerFactoryWrapper::XMLParse(rapidxml::xml_node<char>* & aNode) {
    return LoggerFactory::XMLParse(aNode);
}
