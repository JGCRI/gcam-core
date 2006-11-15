#ifndef _FUNCTION_MANAGER_H_
#define _FUNCTION_MANAGER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file function_manager.h
* \ingroup Objects
* \brief FunctionManager class header file.
* \author Pralit Patel
* \author Josh Lurz
*/

#include <map>
#include <string>
#include <memory>

class IFunction;

/*! 
* \ingroup Objects
* \brief A static class used to access production and demand functions.
* \details The FunctionManager contains a mapping of function name to instances
*          of production and demand functions. Each production and demand
*          function is only instantiated once in the model. All technologies and
*          consumers contain a reference to these same functions. The
*          FunctionManager is responsible for instantiating these function
*          objects, distributing them to technologies and consumers, and
*          deallocating them at the end of the model run. The FunctionManager
*          cannot be directly instantiated as it is a static class, the only
*          access to the it is through the static getFunction method.
* \author Pralit Patel, Josh Lurz
*/
class FunctionManager {
public:
	static const IFunction* getFunction( const std::string& aFunctionName );
private:
    FunctionManager();
	~FunctionManager();

    //! Maps the function's name to the pointer to the funtion
	std::map<std::string, IFunction*> mFunctions;
	typedef std::map<std::string, IFunction*>::iterator FunctionsIterator;
};

#endif // _FUNCTION_MANAGER_H_

