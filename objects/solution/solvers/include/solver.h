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
#include <memory>
#include <string>

// class CalcCounter; // TODO: Fix this without causing incomplete destructor warnings.
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
    Solver( Marketplace* aMarketplace, World* aWorld ):marketplace( aMarketplace ), world( aWorld ){};
    virtual ~Solver(){};
    virtual void init() = 0;
    virtual bool solve( const int period ) = 0;
    static std::auto_ptr<Solver> getSolver( const std::string& aSolverName, Marketplace* aMarketplace, World* aWorld );
protected:
    Marketplace* marketplace; //<! The marketplace to solve. 
    World* world; //!< The world to call calc on.
    std::auto_ptr<CalcCounter> mCalcCounter; //<! Tracks the number of calls to world.calc
};

#endif // _SOLVER_H_

