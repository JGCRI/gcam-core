#ifndef _MARKET_BASED_INVESTMENT_H_
#define _MARKET_BASED_INVESTMENT_H_
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
* \file market_based_investment.h
* \todo File name and class name don't match.
* \ingroup Objects
* \brief The MarketBasedInvestor class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "investment/include/iinvestor.h"

class Tabs;
class Demographic;
class IInvestable;
class NationalAccount;

/*! 
* \ingroup Objects
* \brief The MarketBasedInvestor class calculates and distributes new investment to technologies based
* on a zero profit condition.
* \detailed The MarketBasedInvestor class creates a trial market for each sector within a region. The markets
* left hand side is the unit cost, and the right hand side is the price received of the good. The output of new investment is the trial value
* which is adjusted until price received  equals unit cost. The object distributes this output to the subsectors based on the 
* unit cost. Increasing or decreasing output of new technologies will then increase or decrease the price and unit cost. 
* This market will clear when the cost of increasing production by a single unit is equal to the price of the single unit.
* \todo Ron check over these comments and make sure economics are right.
* \note Since these markets are solved as a system, there might be problems if some markets in a region used this method
* and some did not.
* \author Josh Lurz
*/
class MarketBasedInvestor: public IInvestor
{
public:
	MarketBasedInvestor(); // should be protected.
    void XMLParse( const xercesc::DOMNode* node ); 
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void completeInit( const std::string& aRegionName, const std::string& aGoodName );
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
    double mInvestmentLogitExp; //!<  The investment logit exponential(RHOINV)
    std::string mMarketName;
};

#endif // _MARKET_BASED_INVESTMENT_
