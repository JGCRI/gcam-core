/*! 
* \file Definitions.h	
* \ingroup CIAM
* \brief A set of standard definitions which should be included in all files.
* 
* This is a set of definitions, used mainly to work around platform
* and compiler specific bugs. The intention is to hide many of the hacks
* used to avoid compiler bugs.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#ifndef _DEFINITIONS_H_
#define _DEFINITIONS_H_
#pragma once

// In VC 6.0 turn off warning 4786. 
#if defined(_MSC_VER) && _MSC_VER <= 1200
#pragma warning( disable : 4786 )  

// This only disables the warning if it is called directly before defining a class.
class msVC6_4786WorkAround {
public:
	msVC6_4786WorkAround() {}
};
static msVC6_4786WorkAround emptyStatic;
#endif

// VC 6.0 does not define min and max in <algorithm>
#if defined(_MSC_VER) && _MSC_VER < 1300

#ifndef min
namespace std {
template<class T>
const T& min(const T& x, const T& y)
{
	return ( x < y ? x : y);
}
}
#endif // min
 
#ifndef max
namespace std {
template<class T>
const T& max(const T& x, const T& y)
{
return ( x > y ? x : y);
}
}
#endif // max

#endif // _MSC_VER

// This allows for memory leak debugging.
#if defined(_MSC_VER)
#   ifdef _DEBUG
        // usually the followin two lines are defined in Microsoft's generated stdafx.h
#       define VC_EXTRALEAN // do not include rarely used parts
#       include <afxwin.h>  // MFC core und standard components
         // extra definition for check whether all needed headers are included
#       undef SEARCH_MEMORY_LEAKS_ENABLED
#       define SEARCH_MEMORY_LEAKS_ENABLED
#   endif   // _DEBUG
#endif  // _MSC_VER

#ifndef WIN32  // Remove the _stdcall needed for WIN32 from externs
#define _stdcall
#endif


namespace std {
template <class T>
//! Helper function to determine the sign of a number.
   const int sign( const T number ) {
      return ( number < 0 )?(-1):(1);
   }
}

#endif // _DEFINITIONS_H_