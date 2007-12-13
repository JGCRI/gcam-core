/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

#ifndef _POLICY_TARGET_RUNNER_H_
#define _POLICY_TARGET_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file policy_target_runner.h
 * \ingroup Objects
 * \brief The PolicyTargetRunner class header file.
 * \author Josh Lurz
 */

#include <memory>
#include <vector>
#include "containers/include/iscenario_runner.h"
#include "util/base/include/value.h"

class Timer;
class TotalPolicyCostCalculator;
class ITarget;
class Modeltime;

/*! 
 * \ingroup Objects
 * \brief A scenario runner that determines an optimal Peck-Wan carbon price
 *        pathway.
 * \details This scenario runner runs the scenario multiple times to determine a
 *          near optimal carbon price pathway. The input parameters for the
 *          pathway are an initial tax year, an interest rate, and a final
 *          climate parameter level, such as temperature or concentration. The
 *          model will construct a Hotelling price path such that the climate
 *          parameter level increases until the target level is reached, and
 *          then the climate parameter is held constant.
 *
 *          The scenario runner performs a two level search to determine this
 *          pathway. First, it iterates to determine the year in which the
 *          climate parameters reaches the target level. This year may be past
 *          the final model year, in which case the price path would be a
 *          Hotelling price path. The optimal year to reach the constraint is
 *          defined as the point where net system emissions are zero, i.e. net
 *          industrial emissions equal net ocean uptake plus net terrestrial
 *          emissions. The second level search determines the initial carbon
 *          price to reach the target climate parameter in the trial year. The
 *          scenario runner then finds a carbon price for each period after the
 *          target is reached independently so that the climate parameter
 *          remains at the target level. If the optimal year is between model
 *          years, the scenario runner will readjust the carbon tax in the final
 *          period so that the climate parameter does not exceed the target.
 *          This does not adjust the Hotelling prices in years up to that
 *          period.
 *
 *          This scenario is controlled by the "find-path" boolean or directly
 *          by the BatchRunner. If it is run independently and not from the
 *          BatchRunner, it reads its configuration from the filename specified
 *          by "policy-target-file". If it is run from the BatchRunner the
 *          configuration values are parsed from that file directly.
 *
 * \note Currently this system only tested with concentration stabilization
 *       targets. Forcing targets should be possible, and temperature targets,
 *       which do not stabilize until after the model horizon, could be
 *       converted to forcing targets.
 * \note The LUE feedback must be disconnected for this to work, otherwise at
 *       high carbon prices the land use change emissions cause the sign of the
 *       derivative on total emissions with respect to the carbon price to
 *       changes signs, which defeats the solution mechanism.
 *
 *          <b>XML specification for PolicyTargetRunner</b>
 *          - XML name: \c policy-target-runner
 *          - Contained by: None or BatchRunner.
 *          - Parsing inherited from class: None.
 *          - Attributes:
 *              - \c name PolicyTargetRunner::mName
 *          - Elements:
 *              - \c target-value PolicyTargetRunner::mTargetValue
 *              - \c target-type PolicyTargetRunner::mTargetType
 *                   (optional) The default is concentration.
 *              - \c tax-name PolicyTargetRunner::mTaxName
 *                   (optional) The default is CO2.
 *              - \c target-tolerance PolicyTargetRunner::mTolerance
 *                   (optional) The default is 0.01.
 *              - \c stable-tolerance PolicyTargetRunner::mStableTolerance
 *                   (optional) The default is 0.1.
 *              - \c path-discount-rate PolicyTargetRunner::mPathDiscountRate
 *                   (optional) The default is 0.05.
 *              - \c first-tax-year PolicyTargetRunner::mFirstTaxYear
 *                   (optional) The default is 2020.
 *              - \c earliest-stabilization-year PolicyTargetRunner::mEarliestStabilizationYear
 *                   (optional) The default is 2050.
 *              - \c max-iterations PolicyTargetRunner::mMaxIterations
 *                   (optional) The default is 100.
 *              - \c max-stable-iterations
 *                PolicyTargetRunner::mMaxStableIterations
 *                   (optional) The default is 10.
 *
 * \author Josh Lurz
 */
class PolicyTargetRunner: public IScenarioRunner {
    friend class ScenarioRunnerFactory;
public:
    // IParsable interface
    virtual bool XMLParse( const xercesc::DOMNode* aRoot );

    virtual ~PolicyTargetRunner();

    virtual const std::string& getName() const;

    virtual bool setupScenarios( Timer& timer,
        const std::string aName = "",
        const std::list<std::string> aScenComponents =
            std::list<std::string>() );
    
    virtual bool runScenarios( const int aSingleScenario,
                              const bool aPrintDebugging,
                              Timer& timer );

    virtual void printOutput( Timer& timer,
        const bool aCloseDB = true ) const;

    virtual Scenario* getInternalScenario();
    virtual const Scenario* getInternalScenario() const;
private:
    //! The scenario runner which controls running the initial scenario, and all
    //! fixed taxed scenarios after.
    std::auto_ptr<IScenarioRunner> mSingleScenario;

    //! The delegate object which calculates total costs.
    std::auto_ptr<TotalPolicyCostCalculator> mPolicyCostCalculator;

    //! The name of the policy target runner.
    std::string mName;

    //! The type of policy target.
    std::string mTargetType;

    //! The name of the tax.
    std::string mTaxName;

    //! Path discount rate. This is the interest rate used to determine the
    //! Hotelling tax path.
    Value mPathDiscountRate;

    //! The tolerance as a percent.
    Value mTolerance;

    //! The stabilization tolerance as a percent.
    Value mStableTolerance;

    //! The target for the climate parameter.
    Value mTargetValue;

    //! The first year to tax.
    unsigned int mFirstTaxYear;

    //! The earliest year to try for stabilization.
    unsigned int mEarliestStabilizationYear;
    
    //! The maximum number of bisection iterations to perform when determining the
    //! initial tax or the trial tax in a single future period past the
    //! stabilization year.
    unsigned int mMaxIterations;

    //! The maximum number of iterations to perform when finding the
    //! stabilization year.
    unsigned int mMaxStableIterations;

    //! Whether the target runner has already parsed its data. The XML parse
    //! can be called directly from the BatchRunner and in that case the object
    //! should not parse data from its separate configuration file.
    bool mHasParsedConfig;

    static std::vector<double>
        calculateHotellingPath( const double aIntialTax,
                                const double aHotellingRate,
                                const Modeltime* aModeltime,
                                const int aInitialYear,
                                const int aFinalYear );

    void setTrialTaxes( const std::vector<double> aTaxes );
    
    bool solveInitialTarget( std::vector<double>& aTaxes,
                             const ITarget* aPolicyTarget,
                             const unsigned int aLimitIterations,
                             const double aTolerance,
                             const int aTargetPeriod,
                             Timer& aTimer );
    
    bool solveFutureTarget( std::vector<double>& aTaxes,
                            const ITarget* aPolicyTarget,
                            const unsigned int aLimitIterations,
                            const double aTolerance,
                            const int aPeriod,
                            Timer& aTimer );
    PolicyTargetRunner();
    static const std::string& getXMLNameStatic();
};
#endif // _POLICY_TARGET_RUNNER_H_
