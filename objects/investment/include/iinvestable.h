#ifndef _IINVESTABLE_H_
#define _IINVESTABLE_H_
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
* \file iinvestable.h
* \ingroup Objects
* \brief The IInvestable interface header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
class NationalAccount;
class IExpectedProfitRateCalculator;
class IDistributor;

/*! 
* \ingroup Objects
* \brief This the interface to a class which can be invested in.
* \todo Much more here
* \todo Fix argument ordering of these functions to match.
* \author Josh Lurz
*/
class IInvestable
{
public:
    inline IInvestable();
    inline virtual ~IInvestable();
    
    virtual double getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                          const std::string& aRegionName,
                                          const std::string& aSectorName,
                                          const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                          const double aInvestmentLogitExp,
                                          const bool aIsShareCalc,
                                          const int aPeriod ) const = 0;
                                         
    virtual double getFixedInvestment( const int aPeriod ) const = 0;
    virtual double getAnnualInvestment( const int aPeriod ) const = 0;

    virtual double getCapitalOutputRatio( const IDistributor* aDistributor,
                                          const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                          const NationalAccount& aNationalAccount,
                                          const std::string& aRegionName,
                                          const std::string& aSectorName, 
                                          const int aPeriod ) const = 0;

    virtual double getOutput( const int aPeriod ) const = 0;
    
    virtual double distributeInvestment( const IDistributor* aDistributor,
                                         NationalAccount& aNationalAccount,
                                         const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                         const std::string& aRegionName,
                                         const std::string& aSectorName,
                                         const double aNewInvestment,
                                         const int aPeriod ) = 0;
};

// Define empty inline methods.
//! Constructor
inline IInvestable::IInvestable(){
}

//! Destructor
inline IInvestable::~IInvestable(){
}

#endif // _IINVESTABLE_H_
