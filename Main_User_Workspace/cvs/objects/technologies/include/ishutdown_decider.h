#ifndef _ISHUTDOWN_DECIDER_H_
#define _ISHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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
