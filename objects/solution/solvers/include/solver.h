#ifndef _SOLVER_H_
#define _SOLVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file solver.h
* \ingroup CIAM
* \brief This is the header file for the solver class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class Marketplace;

/*! 
* \ingroup CIAM
* \brief A class interface which defines an abstract solver.
* \author Josh Lurz
*/

class Solver {
public:
   Solver( Marketplace* marketplaceIn ):marketplace( marketplaceIn ){};
   virtual ~Solver(){};
   virtual bool solve( const int period ) = 0;
protected:
   Marketplace* marketplace; //<! The marketplace to solve. 
};

#endif // _SOLVER_H_

