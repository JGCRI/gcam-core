/*! 
* \file market_info.cpp
* \ingroup CIAM
* \brief MarketInfo class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cassert>
#include <map>
#include <string>

#include "marketplace/include/market_info.h"
#include "util/base/include/xml_helper.h"

using namespace std;

/*! \brief Default constructor */
MarketInfo::MarketInfo(){
}

//! Destructor
MarketInfo::~MarketInfo(){
}

/*! \brief Write out XML for debugging purposes.
*
* This method is called by the Market::toDebugXML method to write out information for the MarketInformation object.
* It prints the current state of all internal variables.
*
* \param out Output stream to print to.
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void MarketInfo::toDebugXML( ostream& out, Tabs* tabs ) const {
	XMLWriteOpeningTag( "MarketInfo", out, tabs );
  
   // Write out all the name value pairs.
   for( map<string,double>::const_iterator iter = infoMap.begin(); iter != infoMap.end(); iter++ ){
       XMLWriteOpeningTag( "Pair", out, tabs );
	   
       XMLWriteElement( iter->first, "key", out, tabs );
       XMLWriteElement( iter->second, "value", out, tabs );

	   XMLWriteClosingTag( "Pair", out, tabs );
   }
   // finished writing xml for the class members.
   
   XMLWriteClosingTag( "MarketInfo", out, tabs );
}

/*! \brief Set a name and value for a piece of information related to the market.
* \details This function will check the marketInfo map for the associated key, if it exists
* it will update the associated value to itemValue, otherwise it will create a new name value pair.
* \param itemName The string to use as the key for this information value.
* \param itemValue The value to be associated with this key. 
*/
void MarketInfo::addItem( const string& itemName, const double itemValue ){

    // First check for the value.
    map<string,double>::iterator iter = infoMap.find( itemName );
    if( iter != infoMap.end() ){
        // Update the value to itemValue.
        iter->second = itemValue;
    }
    else {
        // Add the pair.
        infoMap[ itemName ] = itemValue;
    }
}

/*! \brief Get the value of the information stored with itemName as the key.
* \details This function will query the infoMap for the value 
* associated with the key itemName. If the itemName does not exist, it will return 0.
* It will also print a warning if this occurs. 
* \author Josh Lurz
* \param itemName The key for the value to be queried.
* \return The value associated with itemName if it exists, 0 otherwise.
* \todo Is zero the best return value for a non-existant key?
*/
double MarketInfo::getItemValue( const string& itemName ) const {
    // First check for the value.
    double retValue;

    map<string,double>::const_iterator iter = infoMap.find( itemName );
    if( iter != infoMap.end() ){
        // Set the return value to this key's value.
        retValue = iter->second;
    }
    else {
        // Set the return value to the default.
        /* \todo Use a logger here. */
        cout << "Error: " << itemName << " was not found in the market's information store." << endl;
        retValue = 0;
    }
    return retValue;
}
