#ifndef _RATE_LOGIT_DISTRIBUTOR_H_
#define _RATE_LOGIT_DISTRIBUTOR_H_
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
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file rate_logit_distributor.h
* \ingroup Objects
* \brief The RateLogitDistributor class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class IInvestable;
class IExpectedProfitRateCalc;

#include "investment/include/idistributor.h"
#include <vector>

/*! 
* \ingroup Objects
* \brief This object distributes investment using a logit determined by expected
*        profits.
* \author Josh Lurz
*/
class RateLogitDistributor: public IDistributor
{
public:
    RateLogitDistributor( const double aInvestmentLogitExp );
    double distribute( const IExpectedProfitRateCalculator* IExpectedProfitRateCalculator,
                       std::vector<IInvestable*>& aInvestables,
                       NationalAccount& aNationalAccount,
                       const std::string& aRegionName,
                       const std::string& aSectorName,
                       const double aAmount,
                       const int aPeriod ) const;
private:
    double mInvestmentLogitExp; //!<  The investment logit exponential(RHOINV)
};


#endif // _RATE_LOGIT_DISTRIBUTOR_H_
