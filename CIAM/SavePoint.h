#ifndef _SAVE_POINT_H_
#define _SAVE_POINT_H_
#pragma once

/*! 
* \file SavePoint.h
* \ingroup CIAM
* \brief The SavePoint class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <iostream>
#include <functional>

using namespace std;
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
   void toDebugXML( ostream& out ) const;
   void print( ostream& out ) const;

private:
   double price;
   double demand;
   double supply;
};

namespace std {
template <>
struct std::less<SavePoint*>
{
  bool operator()( const SavePoint* lhs, const SavePoint* rhs) const
  {   
       return lhs->getPrice() < rhs->getPrice();
  }
};
}
#endif // _SAVE_POINT_H_
