#ifndef _IEXPECTED_PROFIT_CALCULATOR_H_
#define _IEXPECTED_PROFIT_CALCULATOR_H_
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
* \file iexpected_profit_calculator.h
* \ingroup Objects
* \brief The IExpectedProfitCalculator interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <iosfwd>
class NationalAccount;
class IInvestable;
class Input;
class IFunction;
class Tabs;
struct ProductionFunctionInfo;
/*! 
* \ingroup Objects
* \brief This is the interface to an object responsible for calculating the expected profit rate of a technology.
* \author Josh Lurz
*/
class IExpectedProfitRateCalculator
{
public:
    inline IExpectedProfitRateCalculator();
	inline virtual ~IExpectedProfitRateCalculator();
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const = 0;   
    virtual double calcSectorExpectedProfitRate( const std::vector<IInvestable*>& aInvestables,
                                                 const NationalAccount& aNationalAccount,
                                                 const std::string& aRegionName,
                                                 const std::string& aGoodName,
                                                 const double aInvestmentLogitExp,
                                                 const bool aIsShareCalc,
                                                 const int aPeriod ) const = 0;

    virtual double calcTechnologyExpectedProfitRate( const ProductionFunctionInfo& aTechProdFuncInfo,
                                                     const NationalAccount& aNationalAccount,
                                                     const std::string& aRegionName,
                                                     const std::string& aSectorName,
                                                     const double aDelayedInvestmentTime,
                                                     const int aLifetime,
                                                     const int aTimeStep,
                                                     const int aPeriod ) const = 0;
};

// Define empty inline methods.
//! Constructor
inline IExpectedProfitRateCalculator::IExpectedProfitRateCalculator(){
}

//! Destructor
inline IExpectedProfitRateCalculator::~IExpectedProfitRateCalculator(){
}

#endif // _IEXPECTED_PROFIT_CALCULATOR_H_
