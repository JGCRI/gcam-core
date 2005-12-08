#ifndef _INVEST_CONSUMER_H_
#define _INVEST_CONSUMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file invest_consumer.h
* \ingroup Objects
* \brief Investment Consumer class header file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>

#include "consumers/include/consumer.h"

class NationalAccount;
class Demographic;
class Tabs;
class IVisitor;

class InvestConsumer : public Consumer
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
public:
	InvestConsumer();
	virtual InvestConsumer* clone() const;
	void copyParam( const BaseTechnology* baseTech );
	void copyParamsInto( InvestConsumer& investConsumerIn ) const;
	virtual void completeInit( const std::string& regionName );
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo,
                           const std::string& aRegionName, 
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount, 
                           const Demographic* aDemographics,
                           const double aCapitalStock,
                           const int aPeriod );
    
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const MoreSectorInfo* moreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, const bool aIsNewVintageMode, const int aPeriod );
    
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    static const std::string& getXMLNameStatic();
protected:
    virtual bool isCoefBased() const { return false; }
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

private:
    void allocateTransportationDemand( NationalAccount& aNationalAccount, 
        const std::string& aRegionName, int aPeriod );

	void allocateDistributionCost( NationalAccount& aNationalAccount, const std::string& aRegionName, 
        int aPeriod );

	void calcIncome( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const std::string& aRegionName, int aPeriod );

	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _TRADE_CONSUMER_H_

