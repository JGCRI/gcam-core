/*! 
* \file atom.cpp
* \ingroup Util
* \brief Atom class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include "util/base/include/atom.h"
#include "util/base/include/atom_registry.h"
#include <boost/functional/hash/hash.hpp>

using namespace std;

namespace objects {
	/*! \brief Constructor which registers the Atom with the AtomRegistry so that it
	*          can be located from throughout the model and deallocated
	*          automatically. 
	*/
	Atom::Atom( const string& aUniqueID ):mUniqueID( aUniqueID ){
		// Register the atom. The registry checks for uniqueness and handles
		// deallocation.
		AtomRegistry::getInstance()->registerAtom( this );

		// Compute the hash used for storage of atoms within maps.
		boost::hash<std::string> hashFunction;
		mHashCode = hashFunction( aUniqueID );
	}

	/*! \brief Destructor
	* \details The destructor checks if the AtomRegistry is currently deallocating
	*          Atoms, and if prints a warning if it is not. This would mean that the
	*          Atom was allocated incorrectly. 
	*/
	Atom::~Atom(){
		if( !AtomRegistry::getInstance()->isCurrentlyDeallocating() ){
			cout << "Atom is being deallocated before the AtomRegistry deleted it." << endl;
		}
	}
}