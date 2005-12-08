#ifndef _IGROWTH_CALCULATOR_H_
#define _IGROWTH_CALCULATOR_H_
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
* \file igrowth_calculator.h
* \ingroup Objects
* \brief The IGrowthCalculator interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>

class Tabs;
class IInvestable;
class Demographic;
class NationalAccount;

/*! 
* \ingroup Objects
* \brief This is the interface to an internal investment object which calculates the scalar which grows
* the overall sector investment.
* \author Josh Lurz
*/
class IGrowthCalculator
{
public:
    inline IGrowthCalculator();
	inline virtual ~IGrowthCalculator();
    virtual void XMLParse( const xercesc::DOMNode* aCurr ) = 0;
    virtual void toDebugXML( const int period, std::ostream& aOut, Tabs* aTabs ) const = 0;
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const = 0;
    
    virtual double calcInvestmentDependencyScalar( const std::vector<IInvestable*>& aInvestables,
                                                   const Demographic* aDemographic,
                                                   const NationalAccount& aNationalAccount,
                                                   const std::string& aGoodName,
                                                   const std::string& aRegionName,
                                                   const double aInvestmentLogitExp,
                                                   const double aProfitElasExp,
                                                   const int aPeriod ) = 0;
};

// Define empty inline methods.
//! Constructor
inline IGrowthCalculator::IGrowthCalculator(){
}

//! Destructor
inline IGrowthCalculator::~IGrowthCalculator(){
}

#endif // _IGROWTH_CALCULATOR_H_
