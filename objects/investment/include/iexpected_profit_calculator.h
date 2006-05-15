#ifndef _IEXPECTED_PROFIT_CALCULATOR_H_
#define _IEXPECTED_PROFIT_CALCULATOR_H_
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
 * \file iexpected_profit_calculator.h
 * \ingroup Objects
 * \brief The IExpectedProfitCalculator interface header file.
 * \author Josh Lurz
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
 * \brief This is the interface to an object responsible for calculating the
 *        expected profit rate of a ProductionSector, Subsector or
 *        ProductionTechnology.
 * \details TODO
 * \author Josh Lurz
 */
class IExpectedProfitRateCalculator
{
public:
    IExpectedProfitRateCalculator();

	virtual ~IExpectedProfitRateCalculator();

    // TODO: Inherit so that documentation is inherited.
    // TODO: Add an XMLParse since there is a toInputXML.
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    /*!
     * \brief Calculate the expected profit rate for a ProductionSector or
     *        Subsector.
     * \details Determines the expected profit rate for a Sector or Subsector
     *          using the given parameters and the passed in investable
     *          children.
     * \param aInvestables Children of the current sector or subsector for which
     *        to calcuate the average expected profit rate.
     * \param aNationalAccount Regional national accounts container.
     * \param aRegionName Name of the region in which expected profit rates are
     *        being calculated.
     * \param aSectorName Name of the sector for which expected profit rates are
     *        being calculated.
     * \param aInvestmentLogitExp The investment logit exponential.
     * \param aIsShareCalc Whether this calculation is for the investment share
     *        calculation.
     * \param aPeriod Period in which to calculate expected profits.
     * \return The expected profit rate for the ProductionSector or Subsector.
     */
    virtual double calcSectorExpectedProfitRate( const std::vector<IInvestable*>& aInvestables,
                                                 const NationalAccount& aNationalAccount,
                                                 const std::string& aRegionName,
                                                 const std::string& aSectorName,
                                                 const double aInvestmentLogitExp,
                                                 const bool aIsShareCalc,
                                                 const int aPeriod ) const = 0;

    /*!
     * \brief Calculate the expected profit rate for a ProductionTechnology
     * \details Determines the expected profit rate for a ProductionTechnology
     *          using the passed in parameters.
     * \param aTechProdFuncInfo A structure containing the necessary information
     *        to call the production function for the given
     *        ProductionTechnology. This includes the production function
     *        itself.
     * \param aNationalAccount Regional national accounts container.
     * \param aRegionName Name of the region in which expected profit rates are
     *        being calculated.
     * \param aSectorName Name of the sector for which expected profit rates are
     *        being calculated.
     * \param aDelayedInvestmentTime Amount of time between the investment
     *        occurring and the ProductionTechnology being brought online.
     * \param aLifetime Lifetime of the ProductionTechnology.
     * \param aTimeStep Timestep for the period in which investment is
     *        occurring.
     * \param aPeriod Period in which to calculate expected profits.
     * \return The expected profit rate for the ProductionTechnology.
     */
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
