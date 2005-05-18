#ifndef _GOVT_CONSUMER_H_
#define _GOVT_CONSUMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file govt_consumer.h
* \ingroup Objects
* \brief GovtConsumer class header file.
*
*  Detailed description.
*
* \author Katherine Chung
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/consumer.h"
#include "util/base/include/value.h"

class NationalAccount;
class Demographic;
class Tabs;
class MoreSectorInfo;

class GovtConsumer : public Consumer
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
    friend class GovtResults;
public:
	GovtConsumer();
	GovtConsumer* clone() const;
	void copyParam( const BaseTechnology* baseTech );
	void copyParamsInto( GovtConsumer& govtConsumerIn ) const;

	void completeInit( const std::string& regionName );
    
    void initCalc( const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, NationalAccount& nationalAccount, 
        Demographic* aDemographics, const double aCapitalStock, const int aPeriod );

    void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const MoreSectorInfo* aMoreSectorInfo, const std::string& aRegionName,
        const std::string& aSectorName, const bool isNewVintageMode, const int aPeriod );

	void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	void updateOutputContainer( OutputContainer* outputContainer,
		const std::string& aRegionName, const std::string& aSectorName, const int aPeriod ) const;
	static const std::string& getXMLNameStatic();
protected:
    virtual bool isCoefBased() const { return true; }
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
private:
	void calcTotalTax( NationalAccount& nationalAccount );
	void calcTransfer( NationalAccount& nationalAccount, const Demographic* demographics, 
        const std::string& regionName, int period );
	void constrainDemand( double budgetScale, const std::string& aRegionName, const int aPeriod  );
	void calcBaseCoef( NationalAccount& nationalAccount, const Demographic* aDemographics );
	void calcGovtTaxOrSubsidy( const std::string& regionName, int period );
	void calcGovtCapitalDemand( const std::string& regionName, int period ); // not currently used
	void calcSubsidy( NationalAccount& nationalAccount, const std::string& regionName, int period );
    void calcDeficit( const std::string& regionName, int period );
	void calcIncome( NationalAccount& nationalAccount, const Demographic* demographics, 
        const std::string& regionName, int period );
	
    void calcBudget();
    
    // Read in parameters.
	Value mBaseDeficit; //!< Base Real deficit
	Value mBaseTransfer; //!< Read in base year real transfer	
	Value mTaxProportional; //!< Proportional Tax
	Value mTaxAdditive; //!< Additive Tax
	Value mTaxCorporate; //!< Corporate income tax
	Value mTaxIBT; //!< Indirect business tax
	Value mRho;

    // Calculated parameters.
    Value mBaseTransferPopCoef;
	Value mSigma;
};

#endif // _GOVT_CONSUMER_H_

