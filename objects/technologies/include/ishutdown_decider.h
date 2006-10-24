#ifndef _ISHUTDOWN_DECIDER_H_
#define _ISHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file ishutdown_decider.h
 * \ingroup Objects
 * \brief The IShutdownDecider interface header file.
 * \author Josh Lurz
 */

#include "util/base/include/istandard_component.h"
#include <cfloat>

struct ProductionFunctionInfo;
class Tabs;

/*! 
 * \ingroup Objects
 * \brief This is the interface to an object responsible for making the shutdown
 *        decision for a vintage.
 * \author Josh Lurz
 */
class IShutdownDecider: public IParsedComponent
{
public:
    // Clone operator must be declared explicitly even though it is inherited
    // from IStandardComponent so that the return type can be changed. Since
    // this class is a subtype of IStandardComponent, this is legal and referred
    // to as a covariant return type.
    virtual IShutdownDecider* clone() const = 0;

    /*!
     * \brief Calculate the coefficient which represents what fraction of the
     *        total potential output to produce.
     * \details The Technology uses shutdown deciders to determine how much of
     *          its total potential output for a vintage to produce. The
     *          Technology may use a set of IShutdownDeciders, in which case the
     *          fraction of output to produce will be the product of all the
     *          shutdown coefficients.
     * \note MiniCAM and SGM differ in how the pass in the marginal profit
     *       information. MiniCAM calculates it before calling this function and
     *       passes it in as aCalculatedProfitRate. SGM initializes the
     *       variables in the ProductionFunctionInfo so that it can be
     *       calculated when required.
     * \param aFuncInfo The Technology's production information.
     * \param aCalculatedProfitRate The profit rate of the Technology. If this
     *        is set to the uncalculated profit rate constant, the
     *        IShutdownDecider will calculate the value from the
     *        ProductionFunctionInfo.
     * \param RegionName Region name.
     * \param aSectorName Sector name.
     * \param aInitialTechYear The initial operational year of the Technology.
     * \param aPeriod Period in which to calculate the shutdown coefficient.
     * \return The fraction of capital or output to operate.
     */
    virtual double calcShutdownCoef( const ProductionFunctionInfo* aFuncInfo,
                                     const double aCalculatedProfitRate,
                                     const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aInitialTechYear,
                                     const int aPeriod ) const = 0;

    /*!
     * \brief Return a constant to represent a state where the profit rate has
     *        not yet been calculated.
     * \return A constant to represent a state where the profit rate has not yet
     *         been calculated.
     */
    static double getUncalculatedProfitRateConstant();
};


inline double IShutdownDecider::getUncalculatedProfitRateConstant(){
    return DBL_MAX;
}

#endif // _ISHUTDOWN_DECIDER_H_
