/*! 
* \file info_factory.cpp
* \ingroup Objects
* \brief The InfoFactory class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include "containers/include/info_factory.h"
#include "containers/include/info.h"

using namespace std;

/*! \brief Factory method that constructs an Info item with a given parent.
* \param aParentInfo The parent of the IInfo object to create, null is
*        permitted.
* \return A newly constructed Info object with the given parent.
*/
IInfo* InfoFactory::constructInfo( const IInfo* aParentInfo, const string& aOwnerName ){
    return new Info( aParentInfo, aOwnerName );
}
