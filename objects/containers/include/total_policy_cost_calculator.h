#ifndef _TOTAL_POLICY_COST_CALCULATOR_H_
#define _TOTAL_POLICY_COST_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file total_policy_cost_calculator.h
* \ingroup Objects
* \brief The TotalPolicyCostCalculator class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <memory>
#include <vector>

class IScenarioRunner;
class Curve;

/*! 
* \ingroup Objects
* \brief A delegate class which calculates the total cost of a policy given an
*        already run scenario.
* \details This class runs a scenario multiple times while varying a fixed
*          carbon price, to determine the MAC curve and total cost for the
*          scenario.
* \author Josh Lurz
*/
class TotalPolicyCostCalculator {
public:
    explicit TotalPolicyCostCalculator( IScenarioRunner* aSingleScenario );
    ~TotalPolicyCostCalculator();
    bool calculateAbatementCostCurve();
    void printOutput() const;
private:
    //! The total global cost of the policy.
    double mGlobalCost;

    //! The total global cost of the policy discounted at the read-in rate.
    double mGlobalDiscountedCost;

    //! Whether costs have been successfully run.
    bool mRanCosts;

    //! The number of points to use to calculate the marginal abatement curve.
    unsigned int mNumPoints;

    //! The name of the GHG for which to calculate the marginal abatement curve.
    std::string mGHGName;

    //! The scenario runner which controls running the initial scenario, and all
    //! fixed taxed scenarios after. This is a weak reference.
    IScenarioRunner* mSingleScenario;

    typedef std::map<const std::string, double> RegionalCosts;
    typedef RegionalCosts::const_iterator CRegionalCostsIterator;

    //! Total costs indexed by region name.
    RegionalCosts mRegionalCosts;

    //! Total discounted costs indexed by region name.
    RegionalCosts mRegionalDiscountedCosts;

    typedef std::vector<std::map<const std::string, const Curve*> > VectorRegionCurves;
    typedef VectorRegionCurves::iterator VectorRegionCurvesIterator;
    typedef VectorRegionCurves::const_iterator CVectorRegionCurvesIterator;
    typedef std::map<const std::string, const Curve* > RegionCurves;
    typedef RegionCurves::const_iterator CRegionCurvesIterator;
    typedef RegionCurves::iterator RegionCurvesIterator;
    VectorRegionCurves mEmissionsQCurves;
    VectorRegionCurves mEmissionsTCurves;
    VectorRegionCurves mPeriodCostCurves;
    RegionCurves mRegionalCostCurves;

    bool runTrials();
    void createCostCurvesByPeriod();
    void createRegionalCostCurves();
};
#endif // _TOTAL_POLICY_COST_CALCULATOR_H_
