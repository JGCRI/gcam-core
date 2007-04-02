/* string_hash.h
 * Created: 02/28/2007
 * Version: 02/28/2007
 *
 * This software, which is provided in confidence, was prepared by employees
 * of Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software
 * which should not be copied or otherwise disseminated outside your
 * organization without the express written authorization from Battelle.
 * All rights to the software are reserved by Battelle.   Battelle makes no
 * warranty, express or implied, and assumes no liability or responsibility
 * for the use of this software.
 */

#if !defined( __STRING_HASH_H )
#define __STRING_HASH_H     // prevent multiple includes

// include files ***********************************************************

#include <string>

// namespaces **************************************************************

namespace ObjECTS {

// string_hash *************************************************************

// VariableMap::hash *******************************************************

/*!
 * Return a numeric hash of the specified string
 * \param aString the string to hash
 * \return a numeric hash of the specified string
 * \remarks This is the default STL hash function from
 *          http://www.sgi.com/tech/stl/stl_hash_fun.h
 */
inline size_t string_hash( const std::string& aString )
{
   size_t       h   = 0;
   const char * ptr = aString.c_str();
   for ( ; *ptr; ++ptr )
   {
      h = 5 * h + *ptr;
   }

   return h;
}

} // namespace ObjECTS

#endif   // __STRING_HASH_H

// end of string_hash.h ****************************************************

