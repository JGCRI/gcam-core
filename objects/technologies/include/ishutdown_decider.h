#ifndef _ISHUTDOWN_DECIDER_H_
#define _ISHUTDOWN_DECIDER_H_
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
* \file ishutdown_decider.h
* \ingroup Objects
* \brief The IShutdownDecider interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
struct ProductionFunctionInfo;

/*! 
* \ingroup Objects
* \brief This is the interface to an object responsible for making the shutdown
*        decision for a vintage.
* \author Josh Lurz
*/
class IShutdownDecider
{
public:
    inline IShutdownDecider();
	inline virtual ~IShutdownDecider();
    virtual double calcShutdownCoef( const ProductionFunctionInfo& aFuncInfo,
                                     const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aPeriod ) const = 0;
};

// Define empty inline methods.
//! Constructor
inline IShutdownDecider::IShutdownDecider(){
}

//! Destructor
inline IShutdownDecider::~IShutdownDecider(){
}

#endif // _ISHUTDOWN_DECIDER_H_
