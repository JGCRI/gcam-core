#ifndef _MAC_GENERATOR_SCENARIO_RUNNER_H_
#define _MAC_GENERATOR_SCENARIO_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file mac_generator_scenario_runner.h
* \ingroup Objects
* \brief The MACGeneratorScenarioRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <memory>
#include <list>
#include "containers/include/scenario_runner.h"

class Timer;
class Curve;

/*! 
* \ingroup Objects
* \brief A ScenarioRunner that runs a scenario multiple times in order to generate a MAC curve for each time period.
* \details This class runs a scenario multiple times while varying a fixed carbon price,
* to determine the MAC curve and total cost for the scenario.
* \author Josh Lurz
*/
class MACGeneratorScenarioRunner: public ScenarioRunner {
public:
    MACGeneratorScenarioRunner( const std::string aGhgName, const unsigned int aNumPoints );
    virtual ~MACGeneratorScenarioRunner();
    virtual bool setupScenario( Timer& timer, const std::string aName = "", const std::list<std::string> aScenComponents = std::list<std::string>() );
    virtual bool runScenario( Timer& timer );
    virtual void printOutput( Timer& timer, const bool aCloseDB = true ) const;
private:
    double mGlobalCost;
    double mGlobalDiscountedCost;
    unsigned int mNumPoints;
    std::string mGhgName;
    std::auto_ptr<ScenarioRunner> mSingleScenario;
    typedef std::map<const std::string, double> RegionalCosts;
    typedef RegionalCosts::const_iterator CRegionalCostsIterator;
    RegionalCosts mRegionalCosts;
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

    bool calculateAbatementCostCurve();
    bool runTrials();
    void createCostCurvesByPeriod();
    void createRegionalCostCurves();
    };
#endif // _MAC_GENERATOR_SCENARIO_RUNNER_H_
