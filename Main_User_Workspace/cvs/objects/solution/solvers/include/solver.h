#ifndef _SOLVER_H_
#define _SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file solver.h
* \ingroup Objects
* \brief This is the header file for the Solver class.
* \author Josh Lurz
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
    static std::auto_ptr<Solver> getSolver( const std::string& aSolverName, Marketplace* aMarketplace,
                                            World* aWorld );
protected:
    Marketplace* marketplace; //<! The marketplace to solve. 
    World* world; //!< The world to call calc on.
    std::auto_ptr<CalcCounter> mCalcCounter; //<! Tracks the number of calls to world.calc
};

#endif // _SOLVER_H_

