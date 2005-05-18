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
* \file function_manager.cpp
* \ingroup Objects
* \brief The Function Manager class source file.
*
*  Define the functions to get a pointer to a function from a function's name
*  and to initialize the functions and map.
*
* \author Pralit Patel
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <map>

#include "util/base/include/util.h"
#include "functions/include/function_manager.h"
#include "functions/include/production_demand_function.h"

using namespace std;

//! Default Constructor
FunctionManager::FunctionManager() {
    mFunctions["CES"] = new CESProductionFn;
	mFunctions["Leontief"] = new LeontiefProductionFn;
	mFunctions["HouseholdDemandFn"] = new HouseholdDemandFn;
	mFunctions["GovtDemandFn"] = new GovtDemandFn;
	mFunctions["TradeDemandFn"] = new TradeDemandFn;
	mFunctions["InvestDemandFn"] = new InvestDemandFn;
}

//! Destructor
FunctionManager::~FunctionManager() {
	//delete each pointer to function
	for( FunctionsIterator funcIter = mFunctions.begin(); funcIter != mFunctions.end(); ++funcIter ) {
		delete funcIter->second;
	}
}

/*! \brief get the pointer the the function class represented by functionName
 *
 * \author Josh Lurz
 * \param aFunctionName The name of the function you want to get
 * \return The pointer to the requested function
 */
const IFunction* FunctionManager::getFunction( const string& aFunctionName ) {
    // Allocate the static function manager if it does not already exist.
    const static FunctionManager functionManager;

    const IFunction* tempFn = util::searchForValue( functionManager.mFunctions, aFunctionName );
	// checking to see if functionName exists in the map
	if ( !tempFn ) {
		cout << "Could not find Production or Demand Function. Check function type: " << aFunctionName << endl;
	}
	return tempFn;
}

