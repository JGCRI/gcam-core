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
    BisectionNRSolver( Marketplace* aMarketplace, World* aWorld );
    virtual ~BisectionNRSolver();
    static const std::string& getName();
    virtual void init();
    virtual bool solve( const int period );
private:
    std::auto_ptr<SolverComponent> mLogNewtonRaphson; //!< LogNewtonRaphson solver component.
    std::auto_ptr<SolverComponent> mBisectAll; //!< BisectAll solver component.
    std::auto_ptr<SolverComponent> mBisectOne; //!< BisectOne solver component.
    std::auto_ptr<SolverComponent> mLogNewtonRaphsonSaveDeriv; //!< LogNewtonRaphsonSaveDerivatives solver component.
    void NRandSingleBisect( const double solTol, const double edSolFloor, const double maxNRRelED,  
                            const int maxCalcsNR, const int maxCalcsBisectOne, SolverInfoSet& sol, 
                            const unsigned int aNumBisectOneCalls, const int period );
};

#endif // _BISECTION_NR_SOLVER_

