#ifndef _CONFIGURATION_H_
#define _CONFIGURATION_H_
#pragma once

#include <xercesc/dom/DOM.hpp>
#include <string>
#include <map>

using namespace xercesc;
using namespace std;

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
* \date $Date$
* \version $Revision$
* \todo Print a warning to the log when a value does not exist in the map. 
* \bug Bools are currently stored in XML as ints due to conversion problems.
* \warning The class is a singleton, so it may not be created with the constructor. Instead call getInstance to return a pointer to the instance.
* \warning The user must call delete on the object when they are finished with it.
*/

class Configuration {

private:
	static bool confExists; //!< Flag which indicates if the instace has been created.
	static Configuration* instance; //!< A pointer to the static singleton instance.
	map<string, string> fileMap; //!< A map of the file names the program uses.
	map<string, string> stringMap; //!< A map of the strings the program uses.
	map<string, bool> boolMap; //!< A map of the bools the program uses.
	map<string, int> intMap;  //!< A map of the ints the program uses.
	map<string, double> doubleMap;  //!< A map of the doubles the program uses.
	
	//! Private constructor to prevent a programmer from creating a second object.
	Configuration(){};
	
	//! Private copy constructor to prevent a programmer from creating a second object.
	Configuration( const Configuration& confIn ){};

	//! Private assignment operator to prevent a programmer from creating a second object.
	Configuration& operator= ( const Configuration& confIn ){ return *this; };

public:
	static Configuration* getInstance();
	~Configuration();
	void XMLParse( const DOMNode* tempnode ); // initialize with xml data
	void toDebugXML( ostream& out ) const;
	string getFile( const string& key ) const;
	string getString( const string& key ) const;
	bool getBool( const string& key ) const;
	int getInt( const string& key ) const;
	double getDouble( const string& key ) const;

};

#endif // _CONFIGURATION_H_