#ifndef _HOUSEHOLD_CONSUMER_H_
#define _HOUSEHOLD_CONSUMER_H_
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
* \file household_consumer.h
* \ingroup Objects
* \brief HouseholdConsumer class header file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/consumer.h"

class NationalAccount;
class Demographic;
class Tabs;
class MoreSectorInfo;
class IVisitor;

/*! 
* \ingroup Objects
* \brief An object representing a set of households.
* \details TODO
* \author Pralit Patel, Sonny Kim
*/

class HouseholdConsumer : public Consumer
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class GovtResults;
public:
	HouseholdConsumer();
	virtual HouseholdConsumer* clone() const;
    void copyParam( const BaseTechnology* baseTech );
	void copyParamsInto( HouseholdConsumer& householdConsumerIn ) const;

	virtual void completeInit( const std::string& regionName );    
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo,
                           const std::string& aRegionName, 
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount, 
                           const Demographic* aDemographics,
                           const double aCapitalStock,
                           const int aPeriod );

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, const bool aIsNewVintageMode, const int aPeriod );

	static const std::string& getXMLNameStatic();

	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	void accept( IVisitor* aVisitor, const int aPeriod ) const;
	double getLaborSupply() const;
protected:
    bool isCoefBased() const { return true; }
	virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
	void calcBaseScalerLand( const std::string& regionName, const int period );
	void calcBaseScalerLaborMale( const std::string& regionName, const int&period );
	void calcBaseScalerLaborFemale( const std::string& regionName, const int period );
	void calcBaseScalerSavings( const std::string& regionName, const int period );
	void calcBaseLaborDemandPerHH( const std::string& regionName, const int period );
	void calcBaseLandDemandPerHH( const std::string& regionName, const int period );
	void calcFactorDemand( const std::string& regionName, int period );
	void calcFactorSupply( const std::string& regionName, int period );
	double calcSocialSecurityTax( NationalAccount& nationalAccount, const std::string& regionName, int period );
	void calcSavings( double disposableIncome, const std::string& regionName, int period );
	void calcLandSupply( const std::string& regionName, int period );
	void calcLaborSupply( const std::string& regionName, int period );
	void calcIncome( NationalAccount& nationalAccount, const std::string& regionName, int period );
	void calcNoHouseholds( const Demographic* aDemographics, int aPeriod );
    void calcBudget( const std::string& aRegionName, const int aPeriod );
    const std::string getBudgetMarketName() const;

    // corresponds to S2, R1, L1
	double baseScalerLand; //!< base land scaler
	double baseScalerLaborMale; //!< base labor scaler
	double baseScalerLaborFemale; //!< base labor scaler
	double baseScalerSavings; //!< base savings scaler

	// corresponds to S0, R0, L0
	double maxLandSupplyFrac; //!< max land supply fraction
	double maxLaborSupplyFracMale; //!< max labor supply fraction
	double maxLaborSupplyFracFemale; //!< max labor supply fraction
	double fixedLaborSupplyFrac; //!< fixed labor supply fraction for both gender
	double maxSavingsSupplyFrac; //!< maximum savings supply fraction

	double baseLandDemandPerHH; //!< base land demand per household
	double baseLaborDemandPerHH; //!< base labor demand per household
	double landDemand; //!< land demand
	double laborDemand; //!< labor demand
	double householdLandDemand; //!< household land demand
	double householdLaborDemand; //!< household labor demand

	double socialSecurityTaxRate; //!< social security tax rate
	double incomeTaxRate; //!< income tax rate
	double personsPerHousehold; //!< people per household
	double numberOfHouseholds; //!< number of households
	double totalLandArea; //!< total land area
	
	double baseTransfer; //!< base year government transfer
	double transfer; //!< government transfer

	double baseLandSupply; //!< base year land supply, in physical units
	double baseLaborSupply; //!< base year labor supply, in physical untis

	double landSupply; //!< land supply
	double laborSupplyMale; //!< male labor supply
	double laborSupplyFemale; //!< female labor supply
    
    double mInitialSavings;

	double workingAgePopMale; //!< population of working age males(from Demographics)
	double workingAgePopFemale; //!< population of working age females(from Demographics)

	void copy( const HouseholdConsumer& householdConsumerIn );
};

#endif // _HOUSEHOLD_CONSUMER_H_

