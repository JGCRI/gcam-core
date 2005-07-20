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
	express or implied, and assumes no liability or responsibility for the 
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
* \details The base functionality of the RateLogitDistributor is to calculate a
*          set of shares which are used to distribute investment. These shares
*          are based on profits calculated by an IExpectedProfitCalculator
*          object which is passed into the functions on this interface. Once a
*          set of shares is calculated, they can be used to either distribute an
*          amount of investment or calculate an average capital to output
*          coefficient. Calculating a capital to output coefficient must use the
*          same shares as distributing investment since the capital to output
*          ratio is on a per unit basis, which is only valid given the same
*          distribution of investment.
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
    
    double calcCapitalOutputRatio( const std::vector<IInvestable*>& aInvestables,
                                   const IExpectedProfitRateCalculator* aRateCalc,  
                                   const NationalAccount& aNationalAccount,
                                   const std::string& aRegionName,
                                   const std::string& aSectorName,
                                   const int aPeriod ) const;
private:
    //!  The investment logit exponential(RHOINV)
    double mInvestmentLogitExp;

    const std::vector<double> calcInvestmentShares( const std::vector<IInvestable*>& aInvestables,
                                                    const IExpectedProfitRateCalculator* aRateCalc,
                                                    const NationalAccount& aNationalAccount,
                                                    const std::string& aRegionName,
                                                    const std::string& aSectorName,
                                                    const int aPeriod ) const;
};


#endif // _RATE_LOGIT_DISTRIBUTOR_H_
