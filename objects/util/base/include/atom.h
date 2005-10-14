#ifndef _ATOM_H_
#define _ATOM_H_
#if defined(_MSC_VER)
#pragma once
// Ignore warnings about finding the hash function through argument dependent
// lookup. This is the intended behavior.
#pragma warning( disable:4675 )
#endif

/*! 
* \file atom.h  
* \ingroup util
* \brief Header file for the Atom class.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <boost/noncopyable.hpp>

namespace objects {
    /*!
    * \ingroup util
    * \brief An immutable class which represents a single unique string.
    * \details Atoms are unique identifiers which may be located and used throughout
    *          the model. They allow for string sharing as all pointers to the same
    *          string point to the same Atom. It is also faster to compare two Atoms
    *          than two strings as all that is required is checking if the pointers
    *          are equal. Lastly, Atoms are optimized for hash-map storage as they
    *          precompute their hash code. This means seaching for Atoms in a
    *          hashmap is nearly as fast as an array reference unless there is a
    *          collision. Atoms cannot be changed once they are created. The memory
    *          management of Atoms is handled by the AtomRegistry and occurs
    *          automatically.
    * \warning Atoms are automatically deallocated, they must always be allocated
    *          with new and should not be either explicitly deallocated or contained
    *          within an auto pointer.
    * \author Josh Lurz
    */
    class Atom : protected boost::noncopyable {
	public:
		Atom( const std::string& aUniqueID );
		~Atom();
		inline const std::string& getID() const;
		inline const size_t getHashCode() const;
	protected:
		//! The unique string identifier of the atom.
		const std::string mUniqueID;

		//! The precomputed hash code of the atom.
		size_t mHashCode;
	};

	/*! \brief Atom hash function specialization.
	* \details This function allows the boost hash function to hook into the Atom's
	*          precomputed hashcode so that calls to boost the hash function
	*          specialized on Atom pointers will call this function and use the
	*          precomputed value.
	*/
	inline size_t hash_value( const Atom* const& aAtom ){
		return aAtom->getHashCode();
	}


	// Inline function definitions.

	/*! \brief Get the unique identifier of the Atom.
	* \return The unique identifier of the Atom.
	*/
	const std::string& Atom::getID() const {
		return mUniqueID;
	}

	/*! \brief Get the precomputed hash code for the Atom.
	* \details Atoms precompute there hashcode to optimize lookups. 
	* \return The hashcode for the atom.
	*/
	const size_t Atom::getHashCode() const {
		return mHashCode;
	}
}
#endif // _ATOM_H_
