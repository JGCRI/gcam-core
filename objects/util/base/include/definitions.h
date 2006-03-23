#ifndef _DEFINITIONS_H_
#define _DEFINITIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file definitions.h	
* \ingroup Objects
* \brief A set of standard definitions which should be included in all files.
* \details This is a set of definitions, used mainly to work around platform and
*          compiler specific bugs. The intention is to hide many of the hacks
*          used to avoid compiler bugs.
* \author Josh Lurz
*/

// Configuration constants.

//! A flag which tells whether to attempt linking of fortran portions.
#define __HAVE_FORTRAN__ 1

//! A flag which turns on or off compilation of database code. 
#define __HAVE_DB__ 1
// End of configuration constants.

//! A flag which turns on or off the compilation of the XML database code.
#define __USE_XML_DB__ 1

// Turn off MTL warnings.
#undef __IGNORE_MTL_WARNINGS__
#define __IGNORE_MTL_WARNINGS__
#if defined(_MSC_VER)
#pragma warning( disable: 4275 )
#pragma warning( disable: 4267 )
#pragma warning( disable: 4244 )
#endif

// This allows for memory leak debugging.
#if defined(_MSC_VER)
#   ifdef _DEBUG
// usually the following two lines are defined in Microsoft's generated stdafx.h
#       define VC_EXTRALEAN // do not include rarely used parts
// extra definition for check whether all needed headers are included
#       undef SEARCH_MEMORY_LEAKS_ENABLED
#       define SEARCH_MEMORY_LEAKS_ENABLED
#   endif   // _DEBUG
#endif  // _MSC_VER

// Remove the _stdcall needed for WIN32 from externs
#if !defined(WIN32) && !defined(_stdcall) 
#define _stdcall
#endif

#endif // _DEFINITIONS_H_
