#ifndef _CONFIGURATION_H_
#define _CONFIGURATION_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file configuration.h
* \ingroup CIAM
* \brief The Configuration class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <map>

class Tabs;

/*! 
* \ingroup CIAM
* \brief This class is used as a container of configuration values which can be accessed throughout the program.
*
* The class is a singleton, so that only one can exist at any time during the program. It parses an XML file
* in the executable directory to instantiate its values. The values can be in one of 5 categories: files, strings,
* bools, ints, and doubles. To add a value it must only be added to the XML file in the appropriate section, the parser
* does not need to be changed. Then to access the variable, use the get method appropriate for the value's type.
*
* \author Josh Lurz
* \bug Bools are currently stored in XML as ints due to conversion problems.
* \warning The class is a singleton, so it may not be created with the constructor. Instead call getInstance to return a pointer to the instance.
* \warning The user must call delete on the object when they are finished with it.
*/

class Configuration {

private:
	static bool confExists; //!< Flag which indicates if the instace has been created.
	static Configuration* instance; //!< A pointer to the static singleton instance.
	std::map<std::string, std::string> fileMap; //!< A map of the file names the program uses.
	std::map<std::string, std::string> stringMap; //!< A map of the strings the program uses.
	std::map<std::string, bool> boolMap; //!< A map of the bools the program uses.
	std::map<std::string, int> intMap;  //!< A map of the ints the program uses.
	std::map<std::string, double> doubleMap;  //!< A map of the doubles the program uses.

	Configuration();

	//! Private undefined constructor to prevent a programmer from creating a second object.
	Configuration( const Configuration& );

	//! Private undefined assignment operator to prevent a programmer from creating a second object.
	Configuration& operator= ( const Configuration& );

public:
	static Configuration* getInstance();
	~Configuration();
	void XMLParse( const xercesc::DOMNode* tempnode );
	void toDebugXML( std::ostream& out, Tabs* tabs ) const;
	std::string getFile( const std::string& key, const std::string& defaultValue = "" ) const;
	std::string getString( const std::string& key, const std::string& defaultValue = "" ) const;
	bool getBool( const std::string& key, const bool defaultValue = false ) const;
	int getInt( const std::string& key, const int defaultValue = 0 ) const;
	double getDouble( const std::string& key, const double defaultValue = 0 ) const;

};

#endif // _CONFIGURATION_H_

