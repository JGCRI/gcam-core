/*! 
* \file info.cpp
* \ingroup Objects
* \brief The Info class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "containers/include/info.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"

using namespace std;

/*! \brief Constructor
* \details Constructs the Info object by allocating a hashmap to store the
*          information objects, and initializes a link to the conceptual parent
*          of this hashmap. Any search that fails in this hashmap will proceed
*          to search the parent hashmap. This link may be null in which case all
*          searches are local.
* \param aParentInfo A pointer to the parent Info object of this Info object
*        which may be null.
*/
Info::Info( const IInfo* aParentInfo ) :
mParentInfo( aParentInfo ),
mInfoMap( new InfoMap( getInitialSize() ) )
{
}

/*! \brief Destructor
* \details The explicit destructor is needed to prevent it from being inlined
*          into the header file where the hashmap definition file would not have
*          been seen yet. The auto_ptr will delete the hashmap automatically
*          here.
*/
Info::~Info(){
}

bool Info::setBoolean( const string& aStringKey, const bool aValue ){
    return setItemValueLocal( aStringKey, aValue );
}

bool Info::setInteger( const string& aStringKey, const int aValue ){
    return setItemValueLocal( aStringKey, aValue );
}

bool Info::setDouble( const string& aStringKey, const double aValue ){
    return setItemValueLocal( aStringKey, aValue );
}

bool Info::setString( const string& aStringKey, const string& aValue ){
    return setItemValueLocal( aStringKey, aValue );
}
    
const bool Info::getBoolean( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    bool value = getItemValueLocal<bool>( aStringKey, found );
    
    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getBoolean( aStringKey, true );
        }
        // The item was not found and there was no parent to search.
        else if( aMustExist ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

const int Info::getInteger( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    int value = getItemValueLocal<int>( aStringKey, found );
    
    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getInteger( aStringKey, true );
        }
        // The item was not found and there was no parent to search.
        else if( aMustExist ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

const double Info::getDouble( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    double value = getItemValueLocal<double>( aStringKey, found );

    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getDouble( aStringKey, true );
        }
        // The item was not found and there was no parent to search.
        else if( aMustExist ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

const string Info::getString( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    string value = getItemValueLocal<string>( aStringKey, found );
    
    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getString( aStringKey, true );
        }
        // The item was not found and there was no parent to search.
        else if( aMustExist ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

bool Info::hasValue( const string& aStringKey ) const {
    // Check the local store.
    bool currHasValue = mInfoMap->find( aStringKey ) != mInfoMap->end();

    // If the value was not found, check the parent.
    if( !currHasValue ){
        currHasValue = mParentInfo->hasValue( aStringKey );
    }
    return currHasValue;
}

void Info::toDebugXML( const int aperiod, Tabs* aTabs, ostream& aOut ) const {
    XMLWriteOpeningTag( "Info", aOut, aTabs );
    for( InfoMap::const_iterator item = mInfoMap->begin(); item != mInfoMap->end(); ++item ){
        XMLWriteOpeningTag( "Pair", aOut, aTabs );
        XMLWriteElement( item->first, "Key", aOut, aTabs );
        
        // Since the type is unknown attempt to convert to all known types until
        // one doesn't fail.
        while( 1 ){
            if( attemptPrint( item->second, string(), aOut, aTabs ) ){
                break;
            }
            if( attemptPrint( item->second, double(), aOut, aTabs ) ){
                break;
            }
            if( attemptPrint( item->second, bool(), aOut, aTabs ) ){
                break;
            }
            if( attemptPrint( item->second, int(), aOut, aTabs ) ){
                break;
            }
            // This is an unknown type. Should not be possible.
            assert( false );
        }
        XMLWriteClosingTag( "Pair", aOut, aTabs );
    }
    XMLWriteClosingTag( "Info", aOut, aTabs );
}

/*! \brief Return the initial size for the underlying hashmap.
* \details Returns how many slots to allocate initially for the hashmap. The
*          hashmap will increase in size if it gets too full, but the resize
*          operation is slow and so should be avoided if possible. This number
*          should also be prime to reduce the number of collisions in the
*          hashmap.
* \return The initial size of the underlying hashmap.
*/
size_t Info::getInitialSize() const {
    // Medium size prime number.
    const size_t INITIAL_SIZE = 29;
    return INITIAL_SIZE;
}

/*! \brief Print a warning message to the user that the item does not exist in
*          the local Info or any of its ancestors.
* \param aStringKey The string key being searched for when the error occurred.
*/
void Info::printItemNotFoundWarning( const string& aStringKey ) const {
    // Print a warning message.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << aStringKey << " was not found in the information store." << endl;
}

/*! \brief Print a warning that the existing value was of a wrong type.
* \param aStringKey The key which pointed to the incorrect type.
* \param aIsUpdate Whether the error occurred during an update.
*/
void Info::printBadCastWarning( const string& aStringKey, bool aIsUpdate ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );

    // Print different warnings for updates and gets.
    if( aIsUpdate ){
        mainLog << aStringKey << " was found but the existing and new types"
                << "do not match." << endl;
    }
    // It is an error during a get.
    else {
        mainLog << aStringKey << " cannot be retrieved because the current"
                << "and requested types do not match." << endl;
    }
}

/*! \brief Print a warning that the variable would shadow a variable in an
*          ancestor.
* \param aStringKey The key which points to the variable.
*/
void Info::printShadowWarning( const string& aStringKey ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << aStringKey << " will shadow a variable in a parent Info." << endl;
}
