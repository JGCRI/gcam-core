#ifndef _BISECT_ALL_H_
#define _BISECT_ALL_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file bisect_all.h
* \ingroup Objects
* \brief This is the header file for the BisectAll solver component class.
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
* \brief A class interface which defines the BisectAll solver component.
* \author Josh Lurz
*/

class BisectAll: public SolverComponent {
public:        
    BisectAll( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    void init();
    static const std::string& getNameStatic();
    ReturnCode solve( const double solutionTolerance, const double edSolutionFloor,
                      const unsigned int maxIterations, SolverInfoSet& solverSet, const int period );

protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;
};

#endif // _BISECT_ALL_H_
