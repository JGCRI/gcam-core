#ifndef _BISECT_ONE_H_
#define _BISECT_ONE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file bisect_one.h
* \ingroup Objects
* \brief This is the header file for the BisectOne solver component class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include<string>

class CalcCounter; 
class Marketplace;
class World;
class SolverInfoSet;

/*! 
* \ingroup Objects
* \brief A class interface which defines the BisectOne solver component.
* \author Josh Lurz
*/

class BisectOne: public SolverComponent {
public:        
    BisectOne( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    ~BisectOne();
    void init();
    static const std::string& getNameStatic();
    ReturnCode solve( const double solutionTolerance, const double edSolutionFloor, const int maxIterations, SolverInfoSet& solverSet, const int period );
protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;
};

#endif // _BISECT_ONE_H_
