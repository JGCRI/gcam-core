#ifndef _ATOM_REGISTRY_H_
#define _ATOM_REGISTRY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file atom_registry.h
* \ingroup Util
* \brief The objects::AtomRegistry class header file.
* \author Josh Lurz
*/

#include <list>
#include <boost/shared_ptr.hpp>
#include <boost/noncopyable.hpp>
#include <memory>

// Forward declare the HashMap.
template <class T, class U> class HashMap;

/*!
* \brief The objects namespace.
* \note The AtomRegistry must be in a namespace so that it can interoperate
*       correctly with boost hashing code.
*/
namespace objects {
    /*! 
    * \ingroup util
    * \brief A registry that is responsible for tracking all Atoms created by the
    *        model.
    * \details The AtomRegistry is a class which tracks all Atoms within the model
    *          and ensures they are unique. It also is responsible for deallocating
    *          Atoms. The AtomRegistry is a singleton class which is only accessible
    *          through the static getInstance function. This returns the global
    *          instance of the registry. Registries cannot be allocated and the
    *          single instance is deallocated automatically when the model
    *          completes. The registry will deallocate all registered Atoms at this
    *          time. When Atoms are created they automatically register themselves
    *          with the registry. The registry ensures at this time that the atoms
    *          are unique. Non-unique atoms are not registered and will leak memory
    *          as they will never be deallocated. Atoms cannot be registered outside
    *          of their constructors. Registered atoms are kept for the entire
    *          lifetime of the model. They may be fetched using the findAtom
    *          function which searches the internal hashmap to find the requested
    *          Atom.
    * \author Josh Lurz
    */
    class AtomRegistry: boost::noncopyable {
        friend class Atom;
    public:
		~AtomRegistry();
		static AtomRegistry* getInstance();
		const Atom* findAtom( const std::string& aID ) const;
	private:
		AtomRegistry();
		bool registerAtom( Atom* aAtom );
		bool isCurrentlyDeallocating() const;
        static unsigned int getInitialSize();

		//! Boolean which tracks whether the atom registry is currently
		//! deallocating its atoms for error checking.
		bool mIsCurrentlyDeallocating;

		//! Typedef to simplify using the hashmap of atoms.
		typedef HashMap<const std::string, boost::shared_ptr<Atom> > AtomMap;

		/*! \brief A list of unique Atoms stored as a hashmap for quick
        *          searching.
		* \details The hashmap itself is an auto pointer so that the the hashmap
		*          can be forward declared to prevent excessive recompilation
        *          when the internal hashmap class changes. The Atoms are stored
		*          with a shared_ptr so that they are deallocated automatically.
		*          A standard auto_ptr is not used because that could cause an
		*          Atom to be unintentionally deallocated during a hashmap
		*          resize operation.
        */
		std::auto_ptr<AtomMap> mAtoms;
	};
}

#endif // _ATOM_REGISTRY_H_
