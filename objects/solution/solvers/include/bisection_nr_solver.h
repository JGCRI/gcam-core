#ifndef _BISECTION_NR_SOLVER_H_
#define _BISECTION_NR_SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

#include <vector>
#include "solution/solvers/include/solver.h"
#include "solution/util/include/solver_library.h"

/*! 
* \file bisection_nr_solver.h
* \ingroup CIAM
* \brief The BisectionNRSolver class header file. 
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

/*! 
* \ingroup CIAM
* \brief A class which defines an An instance of the Solver class which uses bisection first and then Newton-Rhaphson.
* \author Josh Lurz
*/

class BisectionNRSolver: public Solver {
public:
   BisectionNRSolver( Marketplace* marketplaceIn );
   virtual ~BisectionNRSolver();
   virtual bool solve( const int period );
private:

   bool bugTracking; //!< Turn on to enable bugout tracking in various solution routines
   bool bugMinimal; //!< Turn on minimal tracking of solution results
   bool trackED; //!< Turn on solution mechanism tracking (to cout)
   double totIter; //!< Cumulative number of interations

   int Bracket( const double solutionTolerance, const double excessDemandSolutionFloor, 
                         const double bracketInterval, std::vector<SolverLibrary::SolutionInfo>& sol, bool& allbracketed, 
                         bool& firsttime, double& worldCalcCount, const int per );
   int Bisection_all( const double solutionTolerance, const double excessDemandSolutionFloor, const int IterLimit, std::vector<SolverLibrary::SolutionInfo>& sol, double& worldCalcCount, const int per );
   int NR_Ron( const double solutionTolerance, const double excessDemandSolutionFloor, std::vector<SolverLibrary::SolutionInfo>& sol, double& worldCalcCount, const int per );
};

#endif // _BISECTION_NR_SOLVER_

