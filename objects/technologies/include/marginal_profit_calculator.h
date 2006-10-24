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

#ifndef _MARGINAL_PROFIT_CALCULATOR_H_
#define _MARGINAL_PROFIT_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file marginal_profit_calculator.h
 * \ingroup Objects
 * \brief The MarginalProfitCalculator header file.
 * \author Josh Lurz
 */

class Technology;

/*!
 * \brief Calculates the short term marginal profit for Technologies, normalized
 *        to the non-energy cost.
 * \details Determines the short term marginal profit for Technologies. Marginal
 *          revenue is first calculated, this includes the value of both primary
 *          and secondary good, and emissions subsidies and taxes. Marginal cost
 *          is normalized to the non-energy cost. If non-energy costs are zero,
 *          the marginal cost is returned as an absolute. Marginal costs are
 *          calculated as the variable or fuel costs, and do not include
 *          non-energy costs. This profit rate would be greater than the long
 *          term profit rate given that non-energy costs were greater than zero.
 */
class MarginalProfitCalculator
{
public:
    MarginalProfitCalculator( const Technology* aTechnology );
    
    double calcShortTermMarginalProfit( const std::string& aRegionName,
                                        const std::string& aSectorName,
                                        const int aPeriod ) const;
private:
    //! Technology for which to calculate marginal profits.
    const Technology* mTechnology;
};

#endif // _MARGINAL_PROFIT_CALCULATOR_H_
