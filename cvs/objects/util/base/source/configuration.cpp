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
* \file configuration.cpp
* \ingroup Util
* \brief Configuration class source file.
* \author Josh Lurz
* \date $Date: 2007/01/11 00:13:33 $
* \version $Revision: 1.8.2.6 $
*/
#include "util/base/include/definitions.h"
#include <string>
#include <map>
#include <iostream>
#include "util/base/include/configuration.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

// Static initializations.
std::auto_ptr<Configuration> Configuration::gInstance;

//! Private constructor to prevent a programmer from creating a second object.
Configuration::Configuration(): mLogFile( "main_log" ){
}

/*! \brief Get a pointer to the instance of the Configuration object.
*
* If the static instance of the Configuration class has not been created, get Instance() will create it.
* Otherwise getInstance will return a pointer to the instance of the Configuration class.
*
* \warning The user is responsible for deleting the instance when they are finished with it.
* \return A pointer to the single instance of the Configuration class.
*/
Configuration* Configuration::getInstance() {
    if( !gInstance.get() ){
        gInstance.reset( new Configuration() );
    }
    return gInstance.get();
}

bool Configuration::XMLParse( rapidxml::xml_node<char>* aRoot ) {
    /*! \pre Assume we are passed a valid node. */
    assert( aRoot );
    
    
    for( auto currSectionNode = aRoot->first_node(); currSectionNode; currSectionNode = currSectionNode->next_sibling() ) {
        if ( currSectionNode->type() != rapidxml::node_element ) {
            continue;
        }
        
        string sectionName = XMLParseHelper::getNodeName(currSectionNode);
        
        for( auto currValueNode = currSectionNode->first_node(); currValueNode; currValueNode = currValueNode->next_sibling() ) {

            if ( currValueNode->type() != rapidxml::node_element ) {
                continue;
            }

            map<string, string> attrs = XMLParseHelper::getAllAttrs(currValueNode);
            const string valueName = attrs["name"];
            
            if( sectionName == "Files" ){
                fileMap[ valueName ] = XMLParseHelper::getValue<string>( currValueNode );
                auto attrIter = attrs.find("write-output");
                mShouldWriteFileMap[ valueName ] = attrIter != attrs.end() ? XMLParseHelper::getValue<bool>((*attrIter).second) : false;
                attrIter = attrs.find("append-scenario-name");
                mShouldAppendScnFileMap[ valueName ] = attrIter != attrs.end() ? XMLParseHelper::getValue<bool>((*attrIter).second) : false;
            }
            
            else if(  sectionName == "Strings" ) {
                stringMap[ valueName ] = XMLParseHelper::getValue<string>( currValueNode );
            }
            
            else if(  sectionName == "Bools" ) {
                boolMap[ valueName ] = XMLParseHelper::getValue<bool>( currValueNode );
            }
            
            else if(  sectionName == "Ints" ) {
                intMap[ valueName ] = XMLParseHelper::getValue<int>( currValueNode );
            }
            
            else if(  sectionName == "Doubles" ) {
                doubleMap[ valueName ] = XMLParseHelper::getValue<double>( currValueNode );
            }
            else if( sectionName == "ScenarioComponents" ){
                scenarioComponents.push_back( XMLParseHelper::getValue<string>( currValueNode ) );
            }
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << sectionName << " found while parsing configuration." << endl;
            }
        } // for ( int j = 0;...
    } // for ( int i = 0;...
    return true;
}

//! Print the internal variables to XML output.
void Configuration::toDebugXML( ostream& out, Tabs* tabs ) const {
		
	// write heading for XML input file
	XMLWriteOpeningTag( "Configuration", out, tabs );
	
	// Write each type.
	// Write files.
    map<string, string> attrs;
    XMLWriteOpeningTag( "Files", out, tabs );
	for ( map<string,string>::const_iterator fileIter = fileMap.begin(); fileIter != fileMap.end(); fileIter++ ) {
        // we can rely on the defaults if the key did not exist in these maps
        attrs[ "write-output" ] = util::toString(shouldWriteFile( fileIter->first ));
        attrs[ "append-scenario-name" ] = util::toString(shouldAppendScnToFile( fileIter->first ));
        attrs[ "name" ] = fileIter->first;
		XMLWriteElementWithAttributes( fileIter->second, "Value", out, tabs, attrs );
	}
    XMLWriteClosingTag( "Files", out, tabs );
    
    // Write Strings
    XMLWriteOpeningTag( "Strings", out, tabs );
	for ( map<string,string>::const_iterator stringIter = stringMap.begin(); stringIter != stringMap.end(); stringIter++ ) {
		XMLWriteElement( stringIter->second, "Value", out, tabs, 0, stringIter->first );
	}
    XMLWriteClosingTag( "Strings", out, tabs );
    
    // Write Bools
    XMLWriteOpeningTag( "Bools", out, tabs );
	for ( map<string,bool>::const_iterator boolIter = boolMap.begin(); boolIter != boolMap.end(); boolIter++ ) {
		XMLWriteElement( boolIter->second, "Value", out, tabs, 0, boolIter->first );
	}
    XMLWriteClosingTag( "Bools", out, tabs );
    
    // Write Ints
    XMLWriteOpeningTag( "Ints", out, tabs );
	for ( map<string,int>::const_iterator intIter = intMap.begin(); intIter != intMap.end(); intIter++ ) {
		XMLWriteElement( intIter->second, "Value", out, tabs, 0, intIter->first );
	}
    XMLWriteClosingTag( "Ints", out, tabs );

	// Write doubles.
	XMLWriteOpeningTag( "Doubles", out, tabs );
	for ( map<string,double>::const_iterator doubleIter = doubleMap.begin(); doubleIter != doubleMap.end(); doubleIter++ ) {
		XMLWriteElement( doubleIter->second, "Value", out, tabs, 0, doubleIter->first );
	}
    XMLWriteClosingTag( "Doubles", out, tabs );

    // Write ScenarioComponents.
    XMLWriteOpeningTag( "ScenarioComponents", out, tabs );
	for ( list<string>::const_iterator scenIter = scenarioComponents.begin(); scenIter != scenarioComponents.end(); ++scenIter ) {
		XMLWriteElement( *scenIter, "Value", out, tabs );
	}
    XMLWriteClosingTag( "ScenarioComponents", out, tabs );
    
    XMLWriteClosingTag( "Configuration", out, tabs );
}

/*! 
* \brief Fetch a filename from the Configuration object.
* 
* This method is used to get filename values read from the configuration file,
* in the filenames section. A filename is a type of string, separated only for clarity.
* If the key is not found, the function will log a warning message
* and return the default value argument if one is passed. Otherwise, it will return the default value 
* defined in Configuration.h.
* 
* \warning The xmlParse function must be called before this function, otherwise the Configuration object will be empty.
* \param key Key to lookup, as specified in the Configuration.xml file as a name value.
* \param defaultValue Optional default argument which will be returned if the key is not found.
* \param mustExist If a warning message should be generated when the key is not found.
* \return Returns the value found in the map for the specified key, or if none is found the default value.
*/
const string& Configuration::getFile( const string& key, const string& defaultValue, const bool mustExist ) const {
	map<string,string>::const_iterator found = fileMap.find( key );
	if ( found != fileMap.end() ) {
		return found->second;
	}
	else {
        if( mustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find filename: " << key << endl;
        }
		return defaultValue;
	}
}

/*!
 * \brief Get the flag if the file of given key should or should not be written.
 * \details If the key is not found, the function will log a warning message
 * and return the default value argument if one is passed. Otherwise, it will return the default value 
 * defined in Configuration.h.
 * \param aKey Key to lookup, as specified in the Configuration.xml file as a name value.
 * \param aDefaultValue Optional default argument which will be returned if the key is not found.
 * \param aMustExist If a warning message should be generated when the key is not found.
 * \return Returns the value found in the map for the specified key, or if none is found the default value.
 */
bool Configuration::shouldWriteFile( const string& aKey, const bool aDefaultValue, const bool aMustExist ) const {
    map<string,bool>::const_iterator found = mShouldWriteFileMap.find( aKey );
    if ( found != mShouldWriteFileMap.end() ) {
        return found->second;
    }
    else {
        if( aMustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find if should write the file with key: " << aKey << endl;
        }
        return aDefaultValue;
    }
}

/*!
 * \brief Get the flag if the scenario name should be post-pended to the filename of given key.
 * \details If the key is not found, the function will log a warning message
 * and return the default value argument if one is passed. Otherwise, it will return the default value 
 * defined in Configuration.h.
 * \param aKey Key to lookup, as specified in the Configuration.xml file as a name value.
 * \param aDefaultValue Optional default argument which will be returned if the key is not found.
 * \param aMustExist If a warning message should be generated when the key is not found.
 * \return Returns the value found in the map for the specified key, or if none is found the default value.
 */
bool Configuration::shouldAppendScnToFile( const string& aKey, const bool aDefaultValue, const bool aMustExist ) const {
    map<string,bool>::const_iterator found = mShouldAppendScnFileMap.find( aKey );
    if ( found != mShouldAppendScnFileMap.end() ) {
        return found->second;
    }
    else {
        if( aMustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find if should append the scenario name to file with key: " << aKey << endl;
        }
        return aDefaultValue;
    }
}

/*! 
* \brief Fetch a string from the Configuration object.
* 
* This method is used to get string values read from the configuration file,
* in the strings section. If the key is not found, the function will log a warning message
* and return the default value argument if one is passed. Otherwise, it will return the default value 
* defined in Configuration.h.
* 
* \warning The xmlParse function must be called before this function, otherwise the Configuration object will be empty.
* \param key Key to lookup, as specified in the Configuration.xml file as a name value.
* \param defaultValue Optional default argument which will be returned if the key is not found.
* \param mustExist If a warning message should be generated when the key is not found.
* \return Returns the value found in the map for the specified key, or if none is found the default value.
*/
const string& Configuration::getString( const string& key, const string& defaultValue, const bool mustExist ) const {
	map<string,string>::const_iterator found = stringMap.find( key );
	if ( found != stringMap.end() ) {
		return found->second;
	}
	else {
        if( mustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find String: " << key << endl;
        }
		return defaultValue;
	}
}

/*! 
* \brief Fetch a bool from the Configuration object.
* 
* This method is used to get double values read from the configuration file,
* in the bools section. If the key is not found, the function will log a warning message
* and return the default value argument if one is passed. Otherwise, it will return the default value 
* defined in Configuration.h.
* 
* \warning The xmlParse function must be called before this function, otherwise the Configuration object will be empty.
* \param key Key to lookup, as specified in the Configuration.xml file as a name value.
* \param defaultValue Optional default argument which will be returned if the key is not found.
* \param mustExist If a warning message should be generated when the key is not found.
* \return Returns the value found in the map for the specified key, or if none is found the default value.
*/
bool Configuration::getBool( const string& key, const bool defaultValue, const bool mustExist ) const {

	map<string,bool>::const_iterator found = boolMap.find( key );

	if ( found != boolMap.end() ) {
		return found->second;
	}
	else {
        if( mustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find bool: " << key << endl;
        }
		return defaultValue;
	}
}

/*! 
* \brief Fetch an int from the Configuration object.
* 
* This method is used to get double values read from the configuration file,
* in the ints section. If the key is not found, the function will log a warning message
* and return the default value argument if one is passed. Otherwise, it will return the default value 
* defined in Configuration.h.
* 
* \warning The xmlParse function must be called before this function, otherwise the Configuration object will be empty.
* \param key Key to lookup, as specified in the Configuration.xml file as a name value.
* \param defaultValue Optional default argument which will be returned if the key is not found.
* \param mustExist If a warning message should be generated when the key is not found.
* \return Returns the value found in the map for the specified key, or if none is found the default value.
*/
int Configuration::getInt( const string& key, const int defaultValue, const bool mustExist ) const {

	map<string,int>::const_iterator found = intMap.find( key );

	if ( found != intMap.end() ) {
		return found->second;
	}
	else {
        if( mustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find int: " << key << endl;
        }
		return defaultValue;
	}
}

/*! 
* \brief Fetch a double from the Configuration object.
* 
* This method is used to get double values read from the configuration file,
* in the doubles section. If the key is not found, the function will log a warning message
* and return the default value argument if one is passed. Otherwise it will return the default value 
* defined in Configuration.h.
* 
* \warning The xmlParse function must be called before this function, otherwise the Configuration object will be empty.
* \param key Key to lookup, as specified in the Configuration.xml file as a name value.
* \param defaultValue Optional default argument which will be returned if the key is not found.
* \param mustExist If a warning message should be generated when the key is not found.
* \return Returns the value found in the map for the specified key, or if none is found the default value.
*/
double Configuration::getDouble( const string& key, const double defaultValue, const bool mustExist ) const {

	map<string,double>::const_iterator found = doubleMap.find( key );
	if ( found != doubleMap.end() ) {
		return found->second;
	}
	else {
        if( mustExist ) {
            ILogger& log = ILogger::getLogger( mLogFile );
            log.setLevel( ILogger::WARNING );
            log << "Could not find double: " << key << endl;
        }
		return defaultValue;
	}
}

/*!
* \brief Fetch a list of filenames for all scenario components.
* \details This method returns a vector of filenames for each scenario component that should 
* be read in. 
* \warning The XMLParse function must be called before this function, otherwise the Configuration object will be empty.
* \return A list of scenario component filenames.
*/
const list<string>& Configuration::getScenarioComponents() const {
    return scenarioComponents;
}
