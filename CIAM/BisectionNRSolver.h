#ifndef _BISECTION_NR_SOLVER_H_
#define _BISECTION_NR_SOLVER_H_

#include <vector>
#include "Solver.h"

using namespace std;

class SolutionInfo;
/*! 
* \file BisectionNRSolver.h
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
   double SMALL_NUM; //!< constant small number to replace for null
   double VERY_SMALL_NUM; //!< constant small number to replace for null
   bool bugTracking; //!< Turn on to enable bugout tracking in various solution routines
   bool bugMinimal; //!< Turn on minimal tracking of solution results
   bool trackED; //!< Turn on solution mechanism tracking (to cout)
   double totIter; //!< Cumulative number of interations

   int Bracket( const double solutionTolerance, const double excessDemandSolutionFloor, 
                         const double bracketInterval, vector<SolutionInfo>& sol, bool& allbracketed, 
                         bool& firsttime, double& worldCalcCount, const int per );
   int Bisection_all( const double solutionTolerance, const double excessDemandSolutionFloor, const int IterLimit, vector<SolutionInfo>& sol, double& worldCalcCount, const int per );
   int NR_Ron( const double solutionTolerance, const double excessDemandSolutionFloor, vector<SolutionInfo>& sol, double& worldCalcCount, const int per );
};

#endif // _BISECTION_NR_SOLVER_