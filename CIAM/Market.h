#ifndef _MARKET_H_
#define _MARKET_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file Market.h
* \ingroup CIAM
* \brief The Market class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>

class SavePoint;
class Logger;

/*!
* \ingroup CIAM
* \brief A class which defines a single market object.
* \author Sonny Kim
*/

class Market
{
public:

   Market( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
   virtual ~Market();

   void toDebugXML( const int period, std::ostream& out ) const;
   virtual void derivedToDebugXML( std::ostream& out ) const;
   void addRegion( const std::string& regionNameIn );
   const std::vector<std::string>& getContainedRegions();
   virtual void setCompanionMarketPointer( Market* pointerIn );

   virtual void initPrice();
   void nullPrice();
   virtual void setPrice( const double priceIn );
   void setRawPrice( const double priceIn );
   virtual void setPriceToLast( const double lastPrice );
   virtual double getPrice() const;
   double getRawPrice() const;
   void restorePrice();
   double getChangeInPrice() const;
   double getLogChangeInPrice( const double SMALL_NUM ) const;

   void nullDemand();
   void setRawDemand( const double value );
   virtual void setDemand( const double demandIn );
   void removeFromRawDemand( const double demandIn );
   double getRawDemand() const;
   virtual double getDemand() const;
   void calcLogDemand( const double SMALL_NUM );
   double getLogDemand() const;
   double getChangeInDemand() const;
   double getLogChangeInDemand( const double SMALL_NUM ) const;

   virtual void nullSupply();
   double getRawSupply() const;
   void setRawSupply( const double supplyIn );
   void removeFromRawSupply( const double supplyIn );
   virtual double getSupply() const;
   virtual double getSupplyForChecking() const;
   virtual void setSupply( const double supplyIn );
   void calcLogSupply( const double SMALL_NUM );
   double getLogSupply() const;
   double getChangeInSupply() const;
   double getLogChangeInSupply( const double SMALL_NUM ) const;
   
   void calcExcessDemand();
   double getExcessDemand() const;
   void calcLogExcessDemand( const double SMALL_NUM );
   double getLogExcessDemand() const;
   double getRelativeExcessDemand() const;

   std::string getName() const;
   std::string getRegionName() const;
   std::string getGoodName() const;
   void storeInfoFromLast( const double lastDemand, const double lastSupply, const double lastPrice );
   void storeInfo();
   void restoreInfo();
   
   void setSolveMarket( const bool doSolve );
   virtual bool shouldSolve() const;
   virtual bool shouldSolveNR( const double SMALL_NUM ) const;
   virtual std::string getType() const;
   void createSDPoint();
   void clearSDPoints();
   void print( std::ostream& out ) const;
   void printSupplyDemandDebuggingCurves( Logger* sdLog );
protected:

   std::string good;  //!< market good or fuel
	std::string region;  //!< market region
	bool solveMarket; //!< Toggle for markets that should be solved
	int period; //!< Model period
	double price;  //!< market price
	double storedPrice;  //!< store market price
	double demand; //!<demand for market solution
	double storedDemand; //!< store previous demand
	double supply; //!< supply for market solution
	double storedSupply; //!< store previous supply
	double excessDemand; //!< excess demand for each market
	double logOfExcessDemand; //!< log of excess demand for each market
	double storedExcessDemand; //!< store excess demand
	double derivativeOfExcessDemand; //!< derivative of excess demand
	double logOfDemand; //!< log of demand for each market
	double logOfSupply; //!< log of supply for each market
   std::vector <std::string> containedRegionNames; //! Vector of names of all regions within this vector.
   std::vector< SavePoint* > sdPoints; //! Save SD points used for print Supply-Demand curves.
};

class DemandMarket;

/*!
* \ingroup CIAM
* \brief A derived class which defines a single PriceMarket object for use in solving simultinaity markets.
* \author Steve Smith
*/

class PriceMarket: public Market {
public:
   PriceMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
   PriceMarket( const Market& marketIn );
   virtual void setCompanionMarketPointer( Market* pointerIn );
   virtual void derivedToDebugXML( std::ostream& out ) const;
   virtual std::string getType() const;
   virtual void setPrice( const double priceIn );
   virtual double getPrice() const;
   virtual void setSupply( const double supplyIn );
   virtual double getSupply() const;
   virtual void setDemand( const double demandIn );
   virtual double getDemand() const;

private:
   Market* demandMarketPointer; //!< A pointer to the companion DemandMarket
   int priceMultiplier; //!< Price Multiplier.
};

/*!
* \ingroup CIAM
* \brief A class which defines a DemandMarket object for use in solving simultinaity markets.
* \author Steve Smith
*/

class DemandMarket: public Market {
public:
   DemandMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
   virtual void setCompanionMarketPointer( Market* pointerIn );
   virtual void derivedToDebugXML( std::ostream& out ) const;
   virtual std::string getType() const;
   virtual double getDemand() const;
   virtual double getSupplyForChecking() const;
   virtual void setSupply( const double supplyIn );
   virtual void nullSupply();
private:
   Market* priceMarketPointer; //!< A pointer to the companion PriceMarket.
   double demMktSupply; //!< Raw supply
};

/*!
* \ingroup CIAM
* \brief A class which defines a GHGMarket object which is used for GHG constraint markets. 
* \author Sonny Kim
*/

class GHGMarket: public Market {
public:
   GHGMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
   virtual std::string getType() const;
   virtual void initPrice();
   virtual bool shouldSolve() const;
   virtual bool shouldSolveNR( const double SMALL_NUM ) const;
};

/*!
* \ingroup CIAM
* \brief A class which defines a CalibrationMarket object for use in solving calibration markets.
* \author Josh Lurz
*/

class CalibrationMarket: public Market {
public:
   CalibrationMarket( const std::string& goodNameIn, const std::string& regionNameIn, const int periodIn );
   virtual std::string getType() const;
   virtual void setPriceToLast( const double lastPriceIn );
};

namespace std {
    template <>
    struct std::greater<Market*>
    {
      bool operator()( const Market* lhs, const Market* rhs ) const
        {   
               return lhs->getRelativeExcessDemand() > rhs->getRelativeExcessDemand();
                 }
        };
}
#endif // _MARKET_H_
