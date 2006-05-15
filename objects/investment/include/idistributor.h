#ifndef _IDISTRIBUTOR_H_
#define _IDISTRIBUTOR_H_
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
 * \file idistributor.h
 * \ingroup Objects
 * \brief The IDistributor interface header file.
 * \author Josh Lurz
 */

class IInvestable;
class IExpectedProfitRateCalculator;
class NationalAccount;

#include <vector>

/*! 
 * \ingroup Objects
 * \brief Interface to an object responsible for distributing investment.
 * \details This interface makes the decision as to how to distribute a given
 *          level of investment across a single level of IInvestable objects.
 *          This decision must use the specified expected profit rate
 *          calculator.
 * \author Josh Lurz
 */
class IDistributor
{
public:
    IDistributor();
    virtual ~IDistributor();

    /*! 
     * \brief Distribute a quantity of investment across one level of investable
     *        objects.
     * \details Distributes investment across a set of investable objects. A
     *          expected profit rate calculator is passed which should be used
     *          if distribution is based on relative expected profit rates. This
     *          function will attempt to distribute all of the given investment.
     * \param aRateCalc Expected profit rate calculate.
     * \param aInvestable Vector of objects across which to distribute
     *        investment.
     * \param aNationalAccount Regional national accounts container.
     * \param aRegionName Name of the region in which investment is occurring.
     * \param aSectorName Name of the sector in which investment is occurring.
     * \param aAmount Amount of investment to distribute.
     * \param aPeriod Period in which to distribute investment.
     * \return Amount of investment actually distributed. This may be less than
     *         the requested amount of investment if there are not enough
     *         profitable investable objects or the investable objects specify a
     *         fixed investment level.
     */
    virtual double distribute( const IExpectedProfitRateCalculator* aRateCalc,
                               std::vector<IInvestable*>& aInvestables,
                               NationalAccount& aNationalAccount,
                               const std::string& aRegionName,
                               const std::string& aSectorName,
                               const double aAmount,
                               const int aPeriod ) const = 0;
    
    /*!
     * \brief Return the amount of capital required to produce one unit of
     *        output.
     * \details Determines the amount of capital required to produce one unit of
     *          output. If there was only one subsector and one technology, this
     *          would be equal to the capital coefficient multiplied by the
     *          overall scalar(alpha zero). This function must use the same
     *          methods for calculating profits and distributing investment as
     *          will later be used once the investment level is known for this
     *          function to be correct.
     * \param aInvestable Vector of objects across which to calculate the
     *        capital to output ratio.
     * \param aExpProfitRateCalculator The object responsible for calculating
     *        expected profits.
     * \param aNationalAccount The regional national accounts container.
     * \param aRegionName Region name.
     * \param aSectorName Sector name.
     * \param aPeriod Model period.
     * \return The amount of capital required to produce one unit of output.
     */
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
