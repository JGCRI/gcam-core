#ifndef _INAMED_H_
#define _INAMED_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file inamed.h
 * \ingroup Objects
 * \brief The INamed interface header file.
 * \author Josh Lurz
 */

#include <string>
/*! 
 * \ingroup Objects
 * \brief An interface that specifies a function to return the name of the
 *        object.
 * \details The getName function specified by this interface must return a
 *          constant string by reference. This means the string must be allocated
 *          permanently by the object, not created on the stack. This is to
 *          ensure that loops on objects calling this function will be fast. The
 *          identifier returned by getName must be unique within the current
 *          context of the object. This means that within any container each
 *          INamed object should return a different identifier. These identifiers
 *          do not have to be globally unique. Names should be non-null.
 */

class INamed {
public:
    //! Destructor.
    virtual inline ~INamed();

    /*!
     * \brief Get the name string from this object.
     * \return The name as a constant reference string.
     */
    virtual const std::string& getName() const = 0;
};

// Inline definitions.
INamed::~INamed(){
}

#endif // _INAMED_H_
