#ifndef _SOLVER_COMPONENT_H_
#define _SOLVER_COMPONENT_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file solver_component.h
* \ingroup objects
* \brief This is the header file for the solver component class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include <string>
#include <memory>
#include <vector>

class CalcCounter; 
class Marketplace;
class SolverInfoSet;
class World;

/*! \brief An abstract class defining an interface to an independent component of a Solver.
* \detailed A SolverComponent is an independent object which takes a Marketplace and attempts
* to clear all markets to a given relative excess demand tolerance within a given number of iterations.
* SolverComponents may use static functions within the SolverLibrary class, they may not however use
* other SolverComponents. This is the role of the Solver object. The main method within a SolverComponent
* is the solve method, which attempts to clear the markets.
* \author Josh Lurz
*/
class SolverComponent {
public:
    //! Return code of the solve method. 
    enum ReturnCode {
        ORIGINAL_STATE,
        SUCCESS,
        FAILURE_ITER_MAX_REACHED,
        FAILURE_WRONG_DIRECTION,
        FAILURE_SOLUTION_SIZE_CHANGED
    };
   SolverComponent( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
   virtual ~SolverComponent();
   static std::auto_ptr<SolverComponent> getSolverComponent( const std::string& solverName, Marketplace* marketplace, World* world, CalcCounter* calcCounter );
   virtual void init() = 0;
   virtual ReturnCode solve( const double solutionTolerance, const double edSolutionFloor, const int maxIterations, SolverInfoSet& solverSet, const int period ) = 0;

protected:
   Marketplace* marketplace; //<! The marketplace to solve. 
   World* world; //<! World to call calc on.
   CalcCounter* calcCounter; //<! Tracks the number of calls to world.calc
   bool bugTracking; //!< Turn on to enable bugout tracking in various solution routines
   bool bugMinimal; //!< Turn on minimal tracking of solution results
   bool trackED; //!< Turn on solution mechanism tracking (to cout)
   
   struct IterationInfo {
   public:
       std::string mName;
       double mRED;
       IterationInfo( const std::string& aName = std::string(), const double aRED = 0 );
   };
   std::vector<IterationInfo> mPastIters;
   virtual const std::string& getName() const = 0;
   void addIteration( const std::string& aSolName, const double aRED );
   bool isImproving( const unsigned int aNumIter ) const;
   void startMethod();
};

#endif // _SOLVER_COMPONENT_H_
