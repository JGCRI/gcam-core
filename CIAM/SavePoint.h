#ifndef _SAVE_POINT_H_
#define _SAVE_POINT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file SavePoint.h
* \ingroup CIAM
* \brief The SavePoint class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <iosfwd>
#include <functional>

class Logger;

/*!
* \ingroup CIAM
* \brief A class which defines a single save point for a SavePoint. 
* \author Josh Lurz
*/

class SavePoint
{
   
public:
   SavePoint( const double priceIn = 0, const double demandIn = 0, const double supplyIn = 0 ) : price( priceIn ), demand( demandIn ), supply( supplyIn ){}
   double getPrice() const;
   void toDebugXML( std::ostream& out ) const;
   void print( Logger* sdLog ) const;
   
/*!
* \brief Binary comparison operator used for SavePoint pointers to order by increasing price. 
* \author Josh Lurz
*/  
   struct LesserPrice : public std::binary_function<SavePoint*,SavePoint*,bool>
   {
      //! Operator which performs comparison. 
      bool operator()( const SavePoint* lhs, const SavePoint* rhs ) const
      {   
         return lhs->getPrice() < rhs->getPrice();
      }
   };

   private:
      double price;
      double demand;
      double supply;
};


#endif // _SAVE_POINT_H_
