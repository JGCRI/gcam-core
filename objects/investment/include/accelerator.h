#ifndef _ACCELERATOR_H_
#define _ACCELERATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file accelerator.h
* \ingroup Objects
* \brief The Accelerator class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
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
* \brief This class accelerates investment or output for a sector.
* \details
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
protected:
    std::vector<double> mInvestments; //!< investment for each period
    std::vector<double> mFixedInvestments; //!< Fixed(Exogenously specified) investment by period.
    std::string mRegionName; //!< Region name of the parent sector.
    std::string mSectorName; //!< Sector name of the parent sector.
    std::string mGrowthCalculatorType; //!< The type of the current growth calculation object.
    //! The type of the expected profit rate calculator.
    std::string mProfitRateCalculatorType;
    std::auto_ptr<IGrowthCalculator> mGrowthCalculator; //!< Object responsible for calculating economic growth scalar.
    
    //! Object responsible for calculating the expected profit rates to distribute investment.
    std::auto_ptr<IExpectedProfitRateCalculator> mProfitRateCalculator;
    double mInvestmentLogitExp; //!<  The investment logit exponential(RHOINV)
    double mProfitElasExp; //!< The expected profit rate function exponential. RINV
private:
    double calcNewInvestment( std::vector<IInvestable*>& aInvestables,
                              NationalAccount& aNationalAccount, 
                              const double aCapDependencyScalar,
                              const int aPeriod ) const;
};

#endif // _ACCELERATOR_H_
