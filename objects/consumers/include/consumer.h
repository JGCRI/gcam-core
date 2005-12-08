#ifndef _CONSUMER_H_
#define _CONSUMER_H_
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
* \file consumer.h
* \ingroup Objects
* \brief Consumer class header file.
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/base_technology.h"

class NationalAccount;
class Demographic;
class Tabs;
class MoreSectorInfo;
class IVisitor;
class IExpectedProfitRateCalculator;
/*! 
* \ingroup Objects
* \brief CHANGE
* \details CHANGE
*
* \note CHANGE
* \author Pralit Patel, Sonny Kim
*/

class Consumer : public BaseTechnology
{
    friend class SGMGenTable;
public:
	Consumer();
	virtual Consumer* clone() const = 0;
    virtual ~Consumer() {};
    virtual void copyParam( const BaseTechnology* baseTech ) = 0;
	virtual void completeInit( const std::string& aRegionName ) = 0;
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo,
                           const std::string& aRegionName, 
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount,
                           const Demographic* aDemographics,
                           const double aCapitalStock,
                           const int aPeriod ) = 0;

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, const bool aIsNewVintageMode, const int aPeriod ) = 0;

	virtual void updateMarketplace( const std::string& aSectorName, const std::string& aRegionName,
                                    const int aPeriod );
    virtual void finalizePeriod( const std::string& aRegionName, const std::string& aSectorName, 
                                 const int aPeriod ){} // do nothing for now.
    virtual void csvSGMOutputFile( std::ostream& aFile, const int period ) const = 0;
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    // Consumer should be contained directly in Subsector and then all these functions could be removed.
    double getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                  const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                  const double aInvestmentLogitExp,
                                  const bool aIsShareCalc,
                                  const int aPeriod ) const
    { 
        return 0; 
    }

    double getCapitalOutputRatio( const IDistributor* aDistributor,
                                  const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                  const NationalAccount& aNationalAccount,
                                  const std::string& aRegionName,
                                  const std::string& aSectorName, 
                                  const int aPeriod ) const
    {
        return 0;
    }

    double getAnnualInvestment( const int aPeriod ) const { 
        return 0;
    }
    double setInvestment( const std::string& aRegionName, const double aAnnualInvestment,
                          const double aTotalInvestment, const int aPeriod )
    { 
        return 0; 
    }
    double getFixedInvestment( const int aPeriod ) const {
        return 0;
    }
    double getOutput( const int aPeriod ) const {
        return 0;
    }
    double getCapital() const {
        return 0;
    }
    double distributeInvestment( const IDistributor* aDistributor,
        NationalAccount& aNationalAccount,
        const IExpectedProfitRateCalculator* aExpProfitRateCalc,
        const std::string& aRegionName,
        const std::string& aSectorName,
        const double aNewInvestment,
        const int aPeriod )
    {
        return 0;
    };
    void setTypeHelper( TechnologyType* aTechType ){}
protected:
    void calcInputDemand( double aConsumption, const std::string& aRegionName, 
        const std::string& aSectorName, int aPeriod );
    
    void calcEmissions( const std::string& aGoodName, const std::string& aRegionName, const int aPeriod );
	virtual const std::string& getXMLName() const = 0;
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr ) = 0;
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
};

#endif // _CONSUMER_H_

