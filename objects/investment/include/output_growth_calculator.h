#ifndef _OUTPUT_GROWTH_CALCULATOR_H_
#define _OUTPUT_GROWTH_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file output_growth_calculator.h
* \ingroup Objects
* \brief The OutputGrowthCalculator class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>
#include <vector>
#include "investment/include/igrowth_calculator.h"

class Tabs;
class Demographic;
class IInvestable;
class NationalAccount;

/*! 
* \ingroup Objects
* \brief This object contains the standard methodology and variables to calculate the growth of investment.
* \author Josh Lurz
*/
class OutputGrowthCalculator: public IGrowthCalculator
{
public:
    OutputGrowthCalculator();
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
    std::vector<double> mOutputGrowthRate;
    mutable std::vector<double> mTrialCapital;
    double mAggregateInvestmentFraction; //!< Fraction of total to use for new technologies.

    double calcTrialCapital( const std::vector<IInvestable*>& aInvestables,
                             const NationalAccount& aNationalAccount,
                             const std::string& aGoodName,
                             const std::string& aRegionName,
                             const double aInvestmentLogitExp,
                             const int aPeriod );

    double calcOutputGap( const std::vector<IInvestable*>& aInvestables,
                          const std::string& aGoodName,
                          const std::string& aRegionName,
                          const double aInvestmentLogitExp,
                          const int aPeriod ) const;

    double calcSectorCapitalOutputRatio( const std::vector<IInvestable*>& aInvestables,
                                         const NationalAccount& aNationalAccount,
                                         const std::string& aGoodName,
                                         const std::string& aRegionName,
                                         const double aInvestmentLogitExp,
                                         const int aPeriod ) const;
};

#endif // _OUTPUT_GROWTH_CALCULATOR_H_
