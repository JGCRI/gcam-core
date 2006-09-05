#ifndef _SIMPLE_POLICY_TARGET_RUNNER_H_
#define _SIMPLE_POLICY_TARGET_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*!
 * \file simple_policy_target_runner.h
 * \ingroup Objects
 * \brief The SimplePolicyTargetRunner class header file.
 * \author Jim Naslund
 */

#include <xercesc/dom/DOMNode.hpp>
#include <boost/noncopyable.hpp>
#include "containers/include/iscenario_runner.h"

class Timer;
class ITarget;
class Curve;
class GHGPolicy;
class TotalPolicyCostCalculator;

typedef std::vector<std::pair<double,double> > VectorOfPairs;

/*!
 * \ingroup Objects
 * \brief A scenario runner which allows the user to specify two emissions
 *        pathways to interpolate between to find a climate target.
 * \details This class runs a scenario multiple times. It reads in two emissions
 *          curves and creates a third curve by performing a interpolating
 *          between the two pathways using a constant. The constant is varied
 *          until the target is reached.
 *
 *          This scenario is controlled by the "simple-find-path" boolean or
 *          directly by the BatchRunner. If it is run independently and not from
 *          the BatchRunner, it reads its configuration from the filename
 *          specified by "sPolicyInputFileName". If it is run from the
 *          BatchRunner the configuration values are parsed from that file
 *          directly. It outputs to the filename specified by
 *          "sPolicyOutputFileName". The default output filename is
 *          sPolicyFinalEmissionsCurve.xml.
 *
 *          <b>XML specification for SimplePolicyTargetRunner</b>
 *          - XML name: \c simple-policy-target-runner
 *          - Contained by: SimplePolicyTargetRunner
 *          - Parsing inherited from class: None.
 *          - Elements:
 *              - \c target-year SimplePolicyTargetRunner::mTargetYear
 *              - \c target-value SimplePolicyTargetRunner::mTargetValue
 *              - \c target-type SimplePolicyTargetRunner::mTargetType
 *              - \c target-tolerance SimplePolicyTargetRunner::mTargetTolerance
 *                (optional)
 *                                    The default is 0.005.
 *              - \c Curve SimplePolicyTargetRunner::mLowerBound
 *                  -Attributes:
 *                      - \c type PointSetCurve
 *                      - \c name lower-bound
 *                      - \c wre wre level of curve
 *                  -Elements:
 *                      - \c PointSet
 *                          -Attributes
 *                              - \c type ExplicitPointSet
 *                          -Elements
 *                              - \c DataPoint (there can be up to 8 of these)
 *                                  -Attributes
 *                                      - \c type XYDataPoint
 *                                  -Elements
 *                                      - \c x x point
 *                                      - \c y y point
 *              - \c Curve SimplePolicyTargetRunner::mUpperBound
 *                  -Attributes:
 *                      - \c type PointSetCurve
 *                      - \c name upper-bound
 *                      - \c wre wre level of curve (this has no function, for
 *                        reference only)
 *                  -Elements:
 *                      - \c PointSet
 *                          -Attributes
 *                              - \c type ExplicitPointSet
 *                          -Elements
 *                              - \c DataPoint (there can be a datapoint for
 *                                each period)
 *                                  -Attributes
 *                                      - \c type XYDataPoint
 *                                  -Elements
 *                                      - \c x x point
 *                                      - \c y y point
 *                           
 *
 * \author Jim Naslund
 */
class SimplePolicyTargetRunner: public IScenarioRunner, protected boost::noncopyable {
    friend class ScenarioRunnerFactory;
public:
    virtual ~SimplePolicyTargetRunner();

    virtual bool setupScenarios( Timer& timer,
        const std::string aName = "",
        const std::list<std::string> aScenComponents = std::list<std::string>() );
    
    virtual bool runScenarios( const int aSingleScenario,
                              Timer& timer );
    virtual void printOutput( Timer& timer,
                              const bool aCloseDB ) const;

    virtual Scenario* getInternalScenario();
    virtual const Scenario* getInternalScenario() const;

    // IParsable Interface.
    bool XMLParse( const xercesc::DOMNode* aRoot );

protected:
    //! The scenario runner which controls running the initial scenario, and all
    //! fixed taxed scenarios after.
    std::auto_ptr<IScenarioRunner> mSingleScenario;

    //! The policy target.
    std::auto_ptr<ITarget> mPolicyTarget;

    //! The delegate object which calculates total costs.
    std::auto_ptr<TotalPolicyCostCalculator> mPolicyCostCalculator;

    //! The type of policy target.
    std::string mTargetType;
    
    //! The year in which to reach the target.
    unsigned int mTargetYear;

    //! The target value
    double mTargetValue;

    //! Lower Bound Curve
    std::auto_ptr<Curve> mLowerBound;

    //! Upper Bound Curve
    std::auto_ptr<Curve> mUpperBound;

    //! Interpolated Curve
    std::auto_ptr<Curve> mInterpolatedCurve;

    //! Tolerance as a percent
    double mTolerance;

    //! Whether the target runner has already parsed it's data. The XML parse
    //! can be called directly from the BatchRunner and in that case the object
    //! should not parse data from its separate configuration file.
    bool mHasParsedConfig;

    SimplePolicyTargetRunner();
    static const std::string& getXMLNameStatic();

private:

    std::vector<double> curveToConstraintVector( const Curve* aCurve ) const;
    void combineCurves( const std::vector<double>& aEmissionValues ) const;
    void setTrialTaxes( const std::string& aTaxName, const std::vector<double>& aEmissions );
    static std::vector<double> preComputeDifferences( const VectorOfPairs&,
                                                      const VectorOfPairs& );
    static std::vector<double> preComputeEmissions( const VectorOfPairs& aLower,
                                                    const std::vector<double>& aDifferences,
                                                    const double aConstant );

};

#endif // _SIMPLE_POLICY_TARGET_RUNNER_H_
