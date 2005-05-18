#ifndef _FUNCTION_MANAGER_H_
#define _FUNCTION_MANAGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file FunctionManager.h
* \ingroup Objects
* \brief Function Manager class header file.
*
*  Static class which will hold a pointer to each function, and will be mapped by
*  the name of the class.
*
* \author Pralit Patel
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <string>
#include <memory>

class IFunction;

/*! 
* \ingroup Objects
* \brief CHANGE
* \details CHANGE
*
* \note CHANGE
* \author Pralit Patel, Josh Lurz
*/

class FunctionManager {
public:
	static const IFunction* getFunction( const std::string& aFunctionName );
private:
    FunctionManager();
	~FunctionManager();
	std::map<std::string, IFunction*> mFunctions; //!< Maps the function's name to the pointer to the funtion
	typedef std::map<std::string, IFunction*>::iterator FunctionsIterator;
};

#endif // _FUNCTION_MANAGER_H_

