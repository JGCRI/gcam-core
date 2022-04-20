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
* \file info.cpp
* \ingroup Objects
* \brief The Info class source file.
* \author Josh Lurz
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
Info::Info( const IInfo* aParentInfo, const string& aOwnerName ) :
mOwnerName( aOwnerName ),
mParentInfo( aParentInfo )
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
    
bool Info::getBoolean( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    bool value = getItemValueLocal<bool>( aStringKey, found );
    
    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getBooleanHelper( aStringKey, found );
        }
        // The item must exist and was not found or there was no parent to search.
        if( aMustExist && !found ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

int Info::getInteger( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    int value = getItemValueLocal<int>( aStringKey, found );
    
    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getIntegerHelper( aStringKey, found );
        }
        // The item must exist and was not found or there was no parent to search.
        if( aMustExist && !found ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

double Info::getDouble( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    double value = getItemValueLocal<double>( aStringKey, found );

    // If the item wasn't found search the parent info.
    if( !found ){
        if( mParentInfo ){
            value = mParentInfo->getDoubleHelper( aStringKey, found );
        }
        // The item must exist and was not found or there was no parent to search.
        if( aMustExist && !found ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    return value;
}

const string Info::getString( const string& aStringKey, const bool aMustExist ) const
{
    // Perform a local search.
    bool found = false;
    const string value = getItemValueLocal<string>( aStringKey, found );
    if( !found ){
        // If the item wasn't found search the parent info.
        if( mParentInfo ){
            return mParentInfo->getStringHelper( aStringKey, found );
        }
        // The item must exist and was not found or there was no parent to search.
        if( aMustExist && !found ){
            printItemNotFoundWarning( aStringKey );
        }
    }
    // Value will be initialized to the default value string by
    // getItemValueLocal
    return value;
}

bool Info::getBooleanHelper( const string& aStringKey, bool& aFound ) const
{
    // Perform a local search.
    bool value = getItemValueLocal<bool>( aStringKey, aFound );
    
    // If the item wasn't found and parent exists, search the parent info.
    if( !aFound && mParentInfo ){
        value = mParentInfo->getBooleanHelper( aStringKey, aFound );
    }
    return value;
}

int Info::getIntegerHelper( const string& aStringKey, bool& aFound ) const
{
    // Perform a local search.
    int value = getItemValueLocal<int>( aStringKey, aFound );
    
    // If the item wasn't found and parent exists, search the parent info.
    if( !aFound && mParentInfo ){
        value = mParentInfo->getIntegerHelper( aStringKey, aFound );
    }
    return value;
}

double Info::getDoubleHelper( const string& aStringKey, bool& aFound ) const
{
    // Perform a local search.
    double value = getItemValueLocal<double>( aStringKey, aFound );
    
    // If the item wasn't found and parent exists, search the parent info.
    if( !aFound && mParentInfo ){
        value = mParentInfo->getDoubleHelper( aStringKey, aFound );
    }
    return value;
}

const string Info::getStringHelper( const string& aStringKey, bool& aFound ) const
{
    // Perform a local search.
    const string value = getItemValueLocal<string>( aStringKey, aFound );
    
    // If the item wasn't found and parent exists, search the parent info.
    if( !aFound && mParentInfo ){
        return mParentInfo->getStringHelper( aStringKey, aFound );
    }
    return value;
}

bool Info::hasValue( const string& aStringKey ) const {
#if GCAM_PARALLEL_ENABLED
    // get a read lock on the info map
    tbb::spin_rw_mutex::scoped_lock readlock(mInfoMapMutex,false);
#endif
    // Check the local store. 
    bool currHasValue = ( mInfoMap.find( aStringKey ) != mInfoMap.end() );

#if GCAM_PARALLEL_ENABLED
    // the lock on our local map is no longer needed.  Release before
    // recursing into parent structures
    readlock.release();
#endif
    // If the value was not found, check the parent.
    if( !currHasValue && mParentInfo ){
        currHasValue = mParentInfo->hasValue( aStringKey );
    }
    return currHasValue;
}

void Info::toDebugXML( const int aperiod, Tabs* aTabs, ostream& aOut ) const {
#if GCAM_PARALLEL_ENABLED
    // get read lock for the info map
    tbb::spin_rw_mutex::scoped_lock readlock(mInfoMapMutex, false);
#endif
    XMLWriteOpeningTag( "Info", aOut, aTabs );
    for( InfoMap::const_iterator item = mInfoMap.begin(); item != mInfoMap.end(); ++item ){
        XMLWriteOpeningTag( "Pair", aOut, aTabs );
        XMLWriteElement( item->first, "Key", aOut, aTabs );
        auto& currType = item->second.type();
        if(currType == typeid(bool)) {
            printItem<bool>( item->second, aOut, aTabs );
        }
        else if(currType == typeid(int)) {
            printItem<int>( item->second, aOut, aTabs );
        }
        else if(currType == typeid(double)) {
            printItem<double>( item->second, aOut, aTabs );
        }
        else if(currType == typeid(string)) {
            printItem<string>( item->second, aOut, aTabs );
        }
        XMLWriteClosingTag( "Pair", aOut, aTabs );
    }
    XMLWriteClosingTag( "Info", aOut, aTabs );
}

/*! \brief Print a warning message to the user that the item does not exist in
*          the local Info or any of its ancestors.
* \param aStringKey The string key being searched for when the error occurred.
*/
void Info::printItemNotFoundWarning( const string& aStringKey ) const {
    // Print a warning message.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
    mainLog << aStringKey << " from " << mOwnerName << " was not found in the information store." << endl;
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
        mainLog << aStringKey << " from " << mOwnerName << " was found but the existing and new types"
                << " do not match." << endl;
    }
    // It is an error during a get.
    else {
        mainLog << aStringKey << " from " << mOwnerName << " cannot be retrieved because the current"
                << "and requested types do not match." << endl;
    }
}

/*! \brief Print a warning that the variable would shadow a variable in an
*          ancestor.
* \param aStringKey The key which points to the variable.
*/
void Info::printShadowWarning( const string& aStringKey ) const {
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::WARNING );
    mainLog << aStringKey << " from " << mOwnerName << " will shadow a variable in a parent Info." << endl;
}

