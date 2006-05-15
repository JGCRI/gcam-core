#ifndef _SIMPLE_EXPECTED_PROFIT_CALCULATOR_H_
#define _SIMPLE_EXPECTED_PROFIT_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/* 
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
* \file simple_expected_profit_calculator.h
* \ingroup Objects
* \brief The SimpleExpectedProfitCalculator class header file.
* \author Josh Lurz
*/
#include <iosfwd>
#include "investment/include/iexpected_profit_calculator.h"
class IInvestable;
class Tabs;

/*! 
 * \ingroup Objects
 * \brief This object calculates expected profits across a sector or subsector
 *        using a logit formulation.
 * \details TODO
 * \author Josh Lurz
 */
class SimpleExpectedProfitCalculator: public IExpectedProfitRateCalculator
{
public:
    SimpleExpectedProfitCalculator();
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

#endif // _SIMPLE_EXPECTED_PROFIT_CALCULATOR_H_
