#ifndef _BISECT_POLICY_H_
#define _BISECT_POLICY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file bisect_policy.h
* \ingroup Objects
* \brief This is the header file for the BisectPolicy solver component class.
*
* \author Josh Lurz
*/
#include <string>

class CalcCounter; 
class Marketplace;
class World;
class SolverInfoSet;

/*! 
* \ingroup Objects
* \brief A class interface which defines the BisectPolicy solver component.
* \author Josh Lurz
*/

class BisectPolicy: public SolverComponent {
public:        
    BisectPolicy( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    void init();
    static const std::string& getNameStatic();
    ReturnCode solve( const double solutionTolerance, const double edSolutionFloor,
                      const unsigned int maxIterations, SolverInfoSet& solverSet, const int period );
protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;
};

#endif // _BISECT_POLICY_H_
