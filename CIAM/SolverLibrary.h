#ifndef _SOLVER_LIBRARY_H_
#define _SOLVER_LIBRARY_H_
#pragma once

/*! 
* \file SolverLibrary.h
* \ingroup CIAM
* \brief A set of helper functions used by solvers.
*
* This library contains a set of generic routines used by the various solution mechanisms.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#if defined(_MSC_VER)
#pragma once
#pragma warning( disable: 4275 )
#endif

#include <mtl/matrix.h>
#include <vector>
#include <string>

using namespace std;
using namespace mtl;

typedef matrix<double, rectangle<>, dense<>, row_major>::type Matrix;

class Marketplace;
class World;

//! Class which contains the solution information for a single market.
class SolutionInfo {
public:
      string marketName; //!< Market area name.
      string marketGood; //< Market fuel or good name.
      double X;		//!< unknown, prices
      double ED;		//!< excess demand for X
      double demand;  //!< demand for X.
      double supply; //!< supply for X
      double dX;		//!< change in excess demand
      double XL;		//!< left bracket
      double XR;		//!< right bracket
      double EDL;		//!< excess demand for left bracket
      double EDR;		//!< excess demand for right bracket
      bool bracketed;	//!< Bracketed or unbrackted.
      SolutionInfo( const string& marketNameIn, const string& marketGoodIn );
      string getName() const;
   };

/*!
* \ingroup CIAM
* \brief A set of static methods in a container class which are used to help solve Marketplaces.
* \author Josh Lurz
*/

class SolverLibrary {
public:
   static vector<SolutionInfo> getMarketsToSolve( const Marketplace* marketplace, const int period, const bool isNR = false );
   static void setPricesToMarkets( Marketplace* marketplace, const vector<SolutionInfo>& solutionVector, const int period );
   static void update( Marketplace* marketplace, vector<SolutionInfo>& solutionVector, const int period );
   static void adjustPriceAndDemandMarkets( const Marketplace* marketplace, vector<SolutionInfo>& solutionVector, const int period );
   static double findMaxExcessDemand( const vector<SolutionInfo>& solutionVector, const double excessDemandSolutionFloor, int& worstMarketIndex, const int period );
   static double getRelativeED( const double excessDemand, const double demand, const double excessDemandFloor );
   static bool isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance, const double excessDemandSolutionFloor );
   static const vector<double> jacobian( const Marketplace* marketplace, const vector<SolutionInfo>& solutionVector, const int marketNumber, const int period );
   static const vector<double> calcDemandElas( const Marketplace* marketplace, const vector<SolutionInfo>& solutionVector, const int marketSolutionNumber, const int period );
   static const vector<double> calcSupplyElas( const Marketplace* marketplace, const vector<SolutionInfo>& solutionVector, const int marketSolutionNumber, const int period );
   static void derivatives( Marketplace* marketplace, World* world, vector<SolutionInfo>& solutionVector, Matrix& JFDM, Matrix& JFSM,double& worldCalcCount, const int per );
   static void invertMatrix( Matrix& A );
   static void checkBracket( const double solutionTolerance, const double excessDemandSolutionFloor, vector<SolutionInfo>& sol, bool& allbracketed );
private:
   static const double SMALL_NUM;
};

#endif // _SOLVER_LIBRARY_H_

