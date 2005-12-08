#ifndef _IDISTRIBUTOR_H_
#define _IDISTRIBUTOR_H_
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
* \file idistributor.h
* \ingroup Objects
* \brief The IDistributor interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class IInvestable;
class IExpectedProfitRateCalculator;
class NationalAccount;

#include <vector>

/*! 
* \ingroup Objects
* \brief This is the interface to an object responsible for distributing
*        investment or output
* \author Josh Lurz
*/
class IDistributor
{
public:
    inline IDistributor();
	inline virtual ~IDistributor();
    virtual double distribute( const IExpectedProfitRateCalculator* aRateCalc,
                               std::vector<IInvestable*>& aInvestables,
                               NationalAccount& aNationalAccount,
                               const std::string& aRegionName,
                               const std::string& aSectorName,
                               const double aAmount,
                               const int aPeriod ) const = 0;

    virtual double calcCapitalOutputRatio( const std::vector<IInvestable*>& aInvestables,
                                           const IExpectedProfitRateCalculator* aExpProfitRateCalc,  
                                           const NationalAccount& aNationalAccount,
                                           const std::string& aRegionName,
                                           const std::string& aSectorName,
                                           const int aPeriod ) const = 0;
};

// Define empty inline methods.
//! Constructor
inline IDistributor::IDistributor(){
}

//! Destructor
inline IDistributor::~IDistributor(){
}

#endif // _IDISTRIBUTOR_H_
