#ifndef _BISECTION_NR_SOLVER_H_
#define _BISECTION_NR_SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

#include <vector>
#include <memory>
#include "solution/solvers/include/solver.h"
#include "solution/util/include/solver_info_set.h"

/*! 
* \file bisection_nr_solver.h
* \ingroup Objects
* \brief The BisectionNRSolver class header file. 
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class SolverComponent;
class Marketplace;
class World;
/*! 
* \ingroup Objects
* \brief A class which defines an An instance of the Solver class which uses bisection first and then Newton-Rhaphson.
* \author Josh Lurz
*/

class BisectionNRSolver: public Solver {
public:
    BisectionNRSolver( Marketplace* marketplaceIn, World* worldIn );
    virtual ~BisectionNRSolver();
    virtual void init();
    virtual bool solve( const int period );
private:

    std::auto_ptr<SolverComponent> logNewtonRaphson; //!< LogNewtonRaphson solver component.
    std::auto_ptr<SolverComponent> bisectAll; //!< BisectAll solver component.
    std::auto_ptr<SolverComponent> bisectOne; //!< BisectOne solver component.
    bool bugTracking; //!< Turn on to enable bugout tracking in various solution routines
    bool bugMinimal; //!< Turn on minimal tracking of solution results
    bool trackED; //!< Turn on solution mechanism tracking (to cout)
};

#endif // _BISECTION_NR_SOLVER_

