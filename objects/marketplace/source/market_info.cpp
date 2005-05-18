/*! 
* \file market_info.cpp
* \ingroup Objects
* \brief MarketInfo class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <map>
#include <string>

#include "marketplace/include/market_info.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"

using namespace std;

/*! \brief Default constructor */
MarketInfo::MarketInfo(){
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
   for( CInfoIterator iter = mInfoMap.begin(); iter != mInfoMap.end(); iter++ ){
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
* \param aName The string to use as the key for this information value.
* \param aValue The value to be associated with this key. 
*/
void MarketInfo::addItem( const string& aName, const double aValue ){

    // First check for the value.
    InfoIterator iter = mInfoMap.find( aName );
    if( iter != mInfoMap.end() ){
        // Update the value to itemValue.
        iter->second = aValue;
    }
    else {
        // Add the pair.
        mInfoMap.insert( make_pair( aName, aValue ) );
    }
}

/*! \brief Get the value of the information stored with itemName as the key.
* \details This function will query the infoMap for the value 
* associated with the key itemName. If the itemName does not exist, it will return 0.
* It will also print a warning if this occurs. 
* \author Josh Lurz
* \param aName The key for the value to be queried.
* \param aMustExist Whether it is an error if the item is missing.
* \return The value associated with itemName if it exists, 0 otherwise.
* \todo Is zero the best return value for a non-existant key?
*/
double MarketInfo::getItemValue( const string& aName, bool aMustExist ) const {
    // Search for the value.
    CInfoIterator iter = mInfoMap.find( aName );
    if( iter != mInfoMap.end() ){
        // Return the value associated with this key.
        return iter->second;
    }

    // Report an error if this value was required to equist.
    if( aMustExist ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << aName << " was not found in the market's information store." << endl;
    }
    // Return 0 as the default value
    return 0;
}
