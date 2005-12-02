#ifndef _POLICY_TARGET_RUNNER_H_
#define _POLICY_TARGET_RUNNER_H_
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
* \file policy_target_runner.h
* \ingroup Objects
* \brief The PolicyTargetRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <memory>
#include <vector>
#include "containers/include/iscenario_runner.h"

class Timer;
class TotalPolicyCostCalculator;
class ITarget;

/*! 
* \ingroup Objects
* \brief A derived ScenarioRunner class that runs a scenario multiple times in
*        order to reach a given target.
* \details This class runs a scenario multiple times while varying a carbon path
*          to reach a specified target.
* \author Josh Lurz
*/
class PolicyTargetRunner: public IScenarioRunner {
    friend class ScenarioRunnerFactory;
public:
    virtual ~PolicyTargetRunner();

    virtual bool setupScenario( Timer& timer,
        const std::string aName = "",
        const std::list<std::string> aScenComponents = std::list<std::string>() );
    
    virtual bool runScenario( const int aSingleScenario,
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

    //! The policy target.
    std::auto_ptr<ITarget> mPolicyTarget;

    //! The type of policy target.
    std::string mTargetType;
    
    //! The current tax rates being used.
    std::vector<double> mCurrentTaxes;

    //! Path discount rate.
    double mPathDiscountRate;
    
    //! The backstop tax level.
    double mBackstopTax;

    //! The first year to tax.
    unsigned int mFirstTaxYear;
    
    //! The year in which to reach the target.
    unsigned int mTargetYear;

    std::vector<double> calculateHotellingPath( const double aScaler ) const;
    void setTrialTaxes( const std::string& aTaxName, const std::vector<double> aTaxes );
    
    bool solveInitialTarget( const unsigned int aLimitIterations,
                             const double aTolerance,
                             Timer& aTimer );
    
    bool solveFutureTarget( const unsigned int aLimitIterations,
                            const double aTolerance,
                            const int aPeriod,
                            Timer& aTimer );
    PolicyTargetRunner();
    static const std::string& getXMLNameStatic();
};
#endif // _POLICY_TARGET_RUNNER_H_
