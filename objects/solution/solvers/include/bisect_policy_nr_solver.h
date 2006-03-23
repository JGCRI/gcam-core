#ifndef _BISECT_POLICY_NR_SOLVER_H_
#define _BISECT_POLICY_NR_SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

#include <memory>
#include "solution/solvers/include/solver.h"

/*! 
* \file bisect_policy_nr_solver.h
* \ingroup Objects
* \brief The BisectPolicyNRSolver class header file.
*
* \author Josh Lurz
*/

class SolverComponent;
class Marketplace;
class World;
/*!
* \ingroup Objects
* \brief A class which defines an An instance of the Solver class which uses
*        bisection first and then Newton-Rhaphson.
* \author Josh Lurz
*/

class BisectPolicyNRSolver: public Solver {
public:
    BisectPolicyNRSolver( Marketplace* marketplaceIn, World* worldIn );
    virtual ~BisectPolicyNRSolver();
    static const std::string& getNameStatic();
    static const std::string& getName();
    virtual void init();
    virtual bool solve( const int period );
private:
    std::auto_ptr<SolverComponent> mLogNewtonRaphson; //!< LogNewtonRaphson solver component.
    std::auto_ptr<SolverComponent> mBisectAll; //!< BisectAll solver component.
    std::auto_ptr<SolverComponent> mBisectOne; //!< BisectOne solver component.
    std::auto_ptr<SolverComponent> mBisectPolicy; //!< BisectPolicy solver component.
protected:
    static const std::string SOLVER_NAME;
};

#endif // _bisect_policy_nr_solver_

