#ifndef _SOLVER_H_
#define _SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file solver.h
* \ingroup objects
* \brief This is the header file for the solver class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include "solution/util/include/calc_counter.h"

class Marketplace;
class World;
/*! 
* \ingroup objects
* \brief Solver is an abstract class which defines a very basic interface to an object which solves the 
* marketplace. A Solver object must define an init() method for setup, and a solve market which attempts
* to solve the model and returns whether or not it did. 
* \author Josh Lurz
*/

class Solver {
public:
   Solver( Marketplace* marketplaceIn, World* worldIn ):marketplace( marketplaceIn ), world( worldIn ){};
   virtual ~Solver(){};
   virtual void init() = 0;
   virtual bool solve( const int period ) = 0;
protected:
   Marketplace* marketplace; //<! The marketplace to solve. 
   World* world; //!< The world to call calc on.
   CalcCounter calcCounter; //<! Tracks the number of calls to world.calc
};

#endif // _SOLVER_H_

