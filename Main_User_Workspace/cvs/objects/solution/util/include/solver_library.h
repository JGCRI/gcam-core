#ifndef _SOLVER_LIBRARY_H_
#define _SOLVER_LIBRARY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*! 
* \file solver_library.h
* \ingroup Objects
* \brief A file containing the header for the static SolverLibrary class which
*        contains helper methods used by SolverComponents.
* \author Josh Lurz, Sonny Kim
*/
#include <vector>
#include <map>
#include <string>
#include <functional>
#include <boost/numeric/ublas/matrix.hpp> 

typedef boost::numeric::ublas::matrix<double> Matrix;

class Marketplace;
class World;
class SolverInfo;
class SolverInfoSet;
class CalcCounter;
namespace objects {
    class Atom;
}
/*!
* \ingroup Objects
* \brief A class with all static functions which are used by the
*        SolverComponents classes and contains common functionality. 
* \author Josh Lurz, Sonny Kim
*/

class SolverLibrary {
public:
    // Some of these still might go.
   static double getRelativeED( const double excessDemand, const double demand, const double excessDemandFloor );

   static bool isWithinTolerance( const double excessDemand, const double demand, const double solutionTolerance,
       const double excessDemandSolutionFloor );

   static void derivatives( Marketplace* marketplace, World* world, SolverInfoSet& solutionVector,
       const double aDeltaPrice, const int per );

   static Matrix invertMatrix( const Matrix& aInputMatrix, bool& aIsSingular );

   static bool bracketOne( Marketplace* marketplace, World* world, const double aBracketInterval,
                           const double aSolutionTolerance, const double aSolutionFloor,
                           SolverInfoSet& aSolSet, SolverInfo* aSol, CalcCounter* aCalcCounter,
                           const int period );

   static void updateMatrices( SolverInfoSet& sol, Matrix& JFSM, Matrix& JFDM, Matrix& JF );

   static bool calculateNewPricesLogNR( SolverInfoSet& solverSet, Matrix& JFSM, Matrix& JFDM, Matrix& JF );

   static bool bracket( Marketplace* marketplace, World* world, const double bracketInterval,
                        const double aSolutionTolerance, const double aSolutionFloor,
                        SolverInfoSet& sol, CalcCounter* aCalcCounter, const int period );

private:
    typedef std::map<const objects::Atom*, std::vector<double> > RegionalMarketValues;

    //! A simple struct to link Supplies and Demands.
    struct RegionalSDDifferences {
        RegionalMarketValues supplies;
        RegionalMarketValues demands;
    };

    //! A function object to compare to values and see if they are approximately equal. 
    struct ApproxEqual : public std::unary_function<double, bool> {
        const double compareValue; //!< A value to compare the argument value against.
        const double tolerance; //!< The tolerance within which to return that the values are equal.
        ApproxEqual( double compareValueIn, double toleranceIn ):
        compareValue( compareValueIn ), tolerance( toleranceIn ){}
        bool operator()( const double value ){
            return fabs( value - compareValue ) < tolerance;
        }
    };

    static bool doRegionalValuesSum( const RegionalMarketValues& regionalValues,
        const std::vector<double>& worldTotals );

    static const RegionalSDDifferences calcRegionalSDDifferences( Marketplace* marketplace, World* world,
        SolverInfoSet& sol, const int per );

    static std::vector<double> storePrices( const SolverInfoSet& aSolverSet );
    static void restorePrices( SolverInfoSet& aSolverSet, const std::vector<double>& aPrices );
};

#endif // _SOLVER_LIBRARY_H_
