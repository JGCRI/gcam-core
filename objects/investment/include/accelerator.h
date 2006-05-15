#ifndef _ACCELERATOR_H_
#define _ACCELERATOR_H_
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
 * \file accelerator.h
 * \ingroup Objects
 * \brief The Accelerator class header file.
 * \author Josh Lurz
 */
#include <string>
#include <vector>
#include <memory>

#include "investment/include/iinvestor.h"

class Tabs;
class Demographic;
class Subsector;
class NationalAccount;
class IGrowthCalculator;
class IExpectedProfitRateCalculator;

/*! 
 * \ingroup Objects
 * \brief This class determines the investment level for a ProductionSector by
 *        accelerating the investment level from the previous period.
 * \details The Accelerator determines the investment level for a
 *          ProductionSector by taking the investment or output from the
 *          previous period and increasing it depending on the expected profit
 *          rate. If there was no investment in the previous period the
 *          investment level is determined by the total investment level in the
 *          region.
 * \author Josh Lurz
 */
class Accelerator: public IInvestor
{
public:
    Accelerator();
    ~Accelerator();
    void XMLParse( const xercesc::DOMNode* node ); 
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void completeInit( const std::string& aRegionName, const std::string& aSectorName );
    static const std::string& getXMLNameStatic();
    
    double calcAndDistributeInvestment( std::vector<IInvestable*>& aInvestables,
                                        NationalAccount& aNationalAccount, 
                                        const Demographic* aDemographic,
                                        const int aPeriod );
private:

    //! Investment by period.
    std::vector<double> mInvestments;

    //! Fixed(exogenously specified) investment by period.
    std::vector<double> mFixedInvestments;

    //! Region name of the sector for which investment is being calculated.
    std::string mRegionName;

    //! Name of the sector for which investment is being calculated.
    std::string mSectorName;

    //! The type of the current growth calculation object.
    std::string mGrowthCalculatorType;

    //! The type of the expected profit rate calculator.
    std::string mProfitRateCalculatorType;

    //! Object responsible for calculating economic growth scalar.
    std::auto_ptr<IGrowthCalculator> mGrowthCalculator;

    //! Object responsible for calculating the expected profit rates to
    //! distribute investment.
    std::auto_ptr<IExpectedProfitRateCalculator> mProfitRateCalculator;

    //!  The investment logit exponential(RHOINV).
    double mInvestmentLogitExp;

    //! The expected profit rate function exponential(RINV).
    double mProfitElasExp;

    double calcNewInvestment( std::vector<IInvestable*>& aInvestables,
                              NationalAccount& aNationalAccount, 
                              const double aCapDependencyScalar,
                              const int aPeriod ) const;
};

#endif // _ACCELERATOR_H_
