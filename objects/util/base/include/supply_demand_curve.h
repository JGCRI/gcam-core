#ifndef _SUPPLY_DEMAND_CURVE_H_
#define _SUPPLY_DEMAND_CURVE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file supply_demand_curve.h
* \ingroup CIAM
* \brief The SupplyDemandCurve class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <functional>
#include <vector>

class ILogger;
class Market;
class World;
class Marketplace; 

/*!
* \ingroup CIAM
* \brief A class which defines a single supply and demand curve. 
* \author Josh Lurz
*/

class SupplyDemandCurve {
public:
   SupplyDemandCurve( Market* marketIn );
   ~SupplyDemandCurve();
   void calculatePoints( const int numPoints, World* world, Marketplace* marketplace, const int period );
   void print( ILogger& aSDLog ) const;

private:
   Market* market; //!< Pointer to the market which the curve is calculating for.

/*!
* \ingroup CIAM
* \brief A class which defines a single supply and demand point. 
* \author Josh Lurz
*/

class SupplyDemandPoint
{
   
public:
   SupplyDemandPoint( const double priceIn = 0, const double demandIn = 0, const double supplyIn = 0 );
   double getPrice() const;
   void print( ILogger& aSDLog ) const;
   
/*!
* \brief Binary comparison operator used for SavePoint pointers to order by increasing price. 
* \author Josh Lurz
*/  
   struct LesserPrice : public std::binary_function<SupplyDemandPoint*,SupplyDemandPoint*,bool>
   {
      //! Operator which performs comparison. 
      bool operator()( const SupplyDemandPoint* lhs, const SupplyDemandPoint* rhs ) const
      {   
         return lhs->getPrice() < rhs->getPrice();
      }
   };

   private:
      double price; //!< Fixed Price
      double demand; //!< Demand at the price.
      double supply; //!< Supply at the price. 
   };
   
   std::vector<SupplyDemandPoint*> points; //!< Vector of points. 

};

#endif // _SUPPLY_DEMAND_CURVE_H_
