#ifndef _LEVELIZED_COST_CALCULATOR_H_
#define _LEVELIZED_COST_CALCULATOR_H_
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
* \file levelized_cost_calculator.h
* \ingroup Objects
* \brief The SimpleExpectedProfitCalculator interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "investment/include/iexpected_profit_calculator.h"
class IInvestable;
/*! 
* \ingroup Objects
* \brief This is the interface to an object responsible for calculating the
*        levelized cost of a technology.
* \author Josh Lurz
*/
class LevelizedCostCalculator: public IExpectedProfitRateCalculator
{
public:
    LevelizedCostCalculator();
    static const std::string& getXMLNameStatic();
    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;
    double calcSectorExpectedProfitRate( const std::vector<IInvestable*>& aInvestables,
                                         const NationalAccount& aNationalAccount,
                                         const std::string& aRegionName,
                                         const std::string& aGoodName,
                                         const double aInvestmentLogitExp,
                                         const bool aIsShareCalc,
                                         const int aPeriod ) const;

    double calcTechnologyExpectedProfitRate( const ProductionFunctionInfo& aTechProdFuncInfo,
                                             const NationalAccount& aNationalAccount,
                                             const std::string& aRegionName,
                                             const std::string& aSectorName,
                                             const double aDelayedInvestmentTime,
                                             const int aLifetime,
                                             const int aTimeStep,
                                             const int aPeriod ) const;
};

#endif // _LEVELIZED_COST_CALCULATOR_H_
