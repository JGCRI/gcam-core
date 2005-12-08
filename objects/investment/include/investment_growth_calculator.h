#ifndef _INVESTMENT_GROWTH_CALCULATOR_H_
#define _INVESTMENT_GROWTH_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file investment_growth_calculator.h
* \ingroup Objects
* \brief The InvestmentGrowthCalculator class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>
#include "investment/include/igrowth_calculator.h"

class Tabs;
class Demographic;
class IInvestable;
class NationalAccount;
/*! 
* \ingroup Objects
* \brief This object contains the methodology and variables for growing
*        investment based on a read-in growth rate and regional economic growth.
* \author Josh Lurz
*/
class InvestmentGrowthCalculator: public IGrowthCalculator
{
public:
    InvestmentGrowthCalculator();
    static const std::string& getXMLNameStatic();
    void XMLParse( const xercesc::DOMNode* aCurr );
    void toDebugXML( const int period, std::ostream& aOut, Tabs* aTabs ) const;
    void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    double calcInvestmentDependencyScalar( const std::vector<IInvestable*>& aInvestables,
                                           const Demographic* aDemographic,
                                           const NationalAccount& aNationalAccount,
                                           const std::string& aGoodName,
                                           const std::string& aRegionName,
                                           const double aPrevInvestment,
                                           const double aInvestmentLogitExp,
                                           const int aPeriod );
protected:
    //! Fraction of total to use for new technologies.
    double mAggregateInvestmentFraction;

    //! Investment accelerator scalar.
    double mInvestmentAcceleratorScalar;

    //!< The working age population ratio exponential.
    double mEconomicGrowthExp;

    //!< Earning potential of the marginal dollar under perfect competition.
    double mMarginalValueDollar;

    double calcEconomicGrowthScalar( const Demographic* aDemographic, const int aPeriod ) const;
};

#endif // _INVESTMENT_GROWTH_CALCULATOR_H_
