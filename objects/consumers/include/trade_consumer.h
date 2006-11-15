#ifndef _TRADE_CONSUMER_H_
#define _TRADE_CONSUMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file trade_consumer.h
* \ingroup Objects
* \brief TradeConsumer class header file.
*
*  Detailed description.
*
* \author Pralit Patel
* \author Sonny Kim
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <iosfwd>

#include "consumers/include/consumer.h"
#include "technologies/include/expenditure.h"

class NationalAccount;
class Demographic;
class Tabs;
class MoreSectorInfo;
class IVisitor;

/*!
 * \brief A consumer representing trade flows out of the region.
 */
class TradeConsumer : public Consumer
{
    friend class SocialAccountingMatrix;
    friend class DemandComponentsTable;
    friend class SectorReport;
    friend class SGMGenTable;
public:
	TradeConsumer();

	TradeConsumer* clone() const;
	void copyParam( const BaseTechnology* baseTech );
	void copyParamsInto( TradeConsumer& tradeConsumerIn ) const;

    virtual void completeInit( const std::string& aRegionName );
    
    virtual void initCalc( const MoreSectorInfo* aMoreSectorInfo,
                           const std::string& aRegionName, 
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount, 
                           const Demographic* aDemographics,
                           const double aCapitalStock,
                           const int aPeriod );

    void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics,
        const MoreSectorInfo* moreSectorInfo, const std::string& aRegionName, 
        const std::string& aSectorName, const bool aIsNewVintageMode, const int aPeriod );
    	
    void csvSGMOutputFile( std::ostream& aFile, const int period ) const;
	virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;

	static const std::string& getXMLNameStatic();
protected:
    bool isCoefBased() const { return false; }
    const std::string& getXMLName() const;
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
    void calcIncome( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
        const std::string& aRegionName, const std::string& aSectorName, int aPeriod );
};

#endif // _TRADE_CONSUMER_H_

