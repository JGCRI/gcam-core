#ifndef _INFO_H_
#define _INFO_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file info.h
* \ingroup objects
* \brief The Info class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <iosfwd>
#include <boost/any.hpp>
#include <boost/noncopyable.hpp>
#include "containers/include/iinfo.h"

// Can't forward declare because operations are used in template functions.
#include "util/base/include/hash_map.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/configuration.h"

class Tabs;

/*!
* \ingroup Objects
* \brief This class contains a set of properties which can be accessed by their
*        unique identifier.
* \author Josh Lurz
* \todo Add longevity to properties.
*/

class Info: public IInfo, boost::noncopyable
{
    friend class InfoFactory;
public:
    ~Info();

    bool setBoolean( const std::string& aStringKey, const bool aValue );
    
    bool setInteger( const std::string& aStringKey, const int aValue );
    
    bool setDouble( const std::string& aStringKey, const double aValue );
    
    bool setString( const std::string& aStringKey, const std::string& aValue );
    
    const bool getBoolean( const std::string& aStringKey, const bool aMustExist ) const;

    const int getInteger( const std::string& aStringKey, const bool aMustExist ) const;

    const double getDouble( const std::string& aStringKey, const bool aMustExist ) const;
    
    const std::string getString( const std::string& aStringKey, const bool aMustExist ) const;

    bool hasValue( const std::string& aStringKey ) const;

    void toDebugXML( const int aPeriod, Tabs* aTabs, std::ostream& aOut ) const;
protected:
    Info( const IInfo* aParentInfo );
private:
    template<class T> bool setItemValueLocal( const std::string& aStringKey, const T& aValue );
    
    template<class T> const T getItemValueLocal( const std::string& aStringKey, bool& aExists ) const;
    
    size_t getInitialSize() const;
    
    void printItemNotFoundWarning( const std::string& aStringKey ) const;
    
    void printBadCastWarning( const std::string& aStringKey, bool aIsUpdate ) const;
    
    void printShadowWarning( const std::string& aStringKey ) const;
    
    template<class T> bool attemptPrint( const boost::any& aValue, T aDummy, 
                                         std::ostream& aOut, Tabs* aTabs ) const;
    //! Type of the internal storage map.
    typedef HashMap<const std::string, boost::any> InfoMap;
    
    //! Internal storage mapping item names to item values.
    std::auto_ptr<InfoMap> mInfoMap;

    //! A pointer to the parent of this Info object which can be null.
    const IInfo* mParentInfo;
};

/*! \brief Set a name and value for a piece of information related.
* \details This function will check the info map for the associated key, if it
*          exists it will update the associated value to itemValue, otherwise it
*          will create a new name value pair.
* \param aStringKey The string key to use as the key for this information value.
* \param aValue The value to be associated with this key. 
*/
template<class T> bool Info::setItemValueLocal( const std::string& aStringKey,
                                                const T& aValue )
{
    /*! \pre A valid key was passed. */
    assert( !aStringKey.empty() );

    // If debug checking is turned on search for the key in the current object
    // to determine if the type of the existing and new types match. Search in
    // the parent to see if this new item will shadow a variable in the parent.
    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    if( debugChecking ){
        InfoMap::const_iterator curr = mInfoMap->find( aStringKey );
        if( curr != mInfoMap->end() ){
            // Check that the types match.
            try {
                boost::any_cast<T>( curr->second );
            }
            catch( boost::bad_any_cast ){
                printBadCastWarning( aStringKey, true );
            }
        }
        // Check if the value exists in a parent info.
        else if( mParentInfo && mParentInfo->hasValue( aStringKey ) ){
            printShadowWarning( aStringKey );
        }
    }

    // Add the value regardless of whether a warning was printed.
    mInfoMap->insert( make_pair( aStringKey, boost::any( aValue ) ) );
    return true;
}

/*! \brief Get the value of an item based on a string key and a passed in
*          location.
* \author Josh Lurz
* \param aStringKey The string key for which to find the value.
* \param aMustExist Whether it is an error for the item to be missing.
* \param aExists Return parameter to update with whether the item existed.
* \return The value associated with the key if it exists, the default value
*         otherwise.
*/
template<class T>
const T Info::getItemValueLocal( const std::string& aStringKey,
                                 bool& aExists ) const
{
    /*! \pre A valid key was passed. */
    assert( !aStringKey.empty() );

    // Check for the value.
    InfoMap::const_iterator curr = mInfoMap->find( aStringKey );
    if( curr != mInfoMap->end() ){
        aExists = true;
        // Attempt to set the return value to the found value. This requires
        // converting the data from the actual type to the requested type.
        try {
            return boost::any_cast<T>( curr->second );
        }
        // Catch bad data conversions and print an error.
        catch( boost::bad_any_cast ){
            printBadCastWarning( aStringKey, false );
        }
    }

    // Return the default value if a successful return has not already occurred.
    aExists = false;
    return T();
}

/*! \brief Try to print a value to XML based on a guess at its type.
* \details Since the Info object does not directly store the type of each of its
*          values, the only way to print values is to enumerate through all
*          known types and attempt to cast it to each.
* \param aValue Value stored as an any type.
* \param aDummy Dummy parameter which determines into what type to cast.
* \param aOut Output stream.
* \param aTabs Tabs object.
* \return Whether the operation was successful.
*/
template<class T>
bool Info::attemptPrint( const boost::any& aValue, T aDummy,
                         std::ostream& aOut, Tabs* aTabs ) const
{
    // Since the type is unknown attempt to convert to all known types until
    // one doesn't fail. Try and convert the value to a string.
    try {
        T value = boost::any_cast<T>( aValue );
        XMLWriteElement( value, "Value", aOut, aTabs );
        return true;
    }
    // Catch bad data conversions.
    catch( boost::bad_any_cast ){
        return false;
    }
}
#endif // _INFO_H_
