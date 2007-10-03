#ifndef _NAME_EQUALS_H_
#define _NAME_EQUALS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file name_equals.h
 * \ingroup Objects
 * \brief The NameEquals class header file
 * \author Josh Lurz
 */

#include <string>
#include <cassert>

// class INamed;
#include "util/base/include/inamed.h"
#include "util/base/include/util.h"

/*! 
 * \ingroup Objects
 * \brief Helper struct which implements the equals operator for INamed pointers.
 * \todo Is this worth a source file?
 * \author Josh Lurz
 */
template <class T>
struct NameEquals: public std::unary_function<const T,bool> {
	NameEquals( const std::string& aName );
	bool operator()( const T& aObject );

	//! Stored name to compare against.
	const std::string mName;
};

/*!
 * \brief Constructor for the operator which stores the name to compare against
 *        the name of another object.
 * \param aName Name of the object which is being compared against.
 */
template <class T>
inline NameEquals<T>::NameEquals( const std::string& aName ):mName( aName ){
	/*! \pre Object name is not empty. */
	//assert( !mName.empty() );
}

/*! \brief Equals operator which determines if the given object has the same
 *         name as another object.
 * \param aObject Object to compare against.
 * \return Whether the object is equal to the stored object.
 */
template <class T>
inline bool NameEquals<T>::operator()( const T& aObject ){
	/*! \pre Object is non-null. */
	//assert( aObject );
	/*! \pre Object name is not empty. */
	//assert( !aObject->getName().empty() );

    return util::InterfaceGetter<INamed,T>()( aObject )->getName() == mName;
}

#endif // _NAME_EQUALS_H_
