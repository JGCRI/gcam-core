#ifndef LOGBROYDEN_HPP_
#define LOGBROYDEN_HPP_

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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/

/*!
 * \file logbroyden.hpp
 * \ingroup objects
 * \brief Header file for the log Broyden solver component
 *
 * \author Robert Link
 */

#include <string>
#include <boost/numeric/ublas/matrix.hpp>
#include "solution/util/include/solvable_nr_solution_info_filter.h"
#include "solution/util/include/edfun.hpp"

#define UBLAS boost::numeric::ublas
#if USE_LAPACK
#define UBMATRIX UBLAS::matrix<double,boost::numeric::ublas::column_major>
#else
#define UBMATRIX UBLAS::matrix<double>
#endif

class CalcCounter; 
class Marketplace;
class World;
class SolutionInfoSet;

/*!
 * \ingroup Objects 
 * \brief SolverComponent based on the Broyden algorithm using
 * logarithmic prices and EDs.
 *
 * \details Broyden's method is exactly analogous to the
 * Newton-Raphson algorithm, including backtracking.  Broyden differs
 * from N-R in that it uses an approximate Jacobian matrix instead of
 * an exact one.  With each iteration step the approximate Jacobian is
 * updated using the secant formula.  For functions where we don't
 * have an analytic expression for the Jacobian (such as the GCAM
 * excess demand function), Broyden's method provides a huge advantage
 * over using finite-difference Jacobians.
 *
 * \author Robert Link
 */
class LogBroyden: public SolverComponent {
public:
  LogBroyden(Marketplace *mktplc, World *world, CalcCounter *ccounter, int itmax=250,
             double ftol=1.0e-4, bool shortcut=true) :
      SolverComponent(mktplc,world,ccounter), mMaxIter(itmax), mFTOL(ftol),
      mShortcut(shortcut) {}
  virtual ~LogBroyden() {}

  // SolverComponent methods
  virtual void init() {
    if(!mSolutionInfoFilter.get())
      mSolutionInfoFilter.reset(new SolvableNRSolutionInfoFilter());
  }
  virtual ReturnCode solve( SolutionInfoSet& aSolutionSet, const int aPeriod );
  virtual const std::string& getXMLName() const {return SOLVER_NAME;}
  
  // IParsable methods
  virtual bool XMLParse( const xercesc::DOMNode* aNode );
  
  static const std::string & getXMLNameStatic(void) {return SOLVER_NAME;}
  
protected:
  //! Perform the Broyden's method iterations.
  int bsolve(VecFVec<double,double> &F, UBLAS::vector<double> &x, UBLAS::vector<double> &fx,
             UBMATRIX &B, int &neval);
  //! Maximum number of main-loop iterations for the root-finding algorithm
  unsigned int mMaxIter;

  //! Tolerance for convergence test in root-finding algorithm 
  //! \warning The SolutionInfo class has its own convergence
  //! tolerance, which it uses to flag certain markets as "unsolved".
  //! If that tolerance is different from this one, the SolutionInfo
  //! might regard a market as unsolved when the solver says it's
  //! solved, or vice versa.
  double mFTOL;

  //! Flag for using "shortcut" Jacobian reset. 
  //! \details Since the Broyden solver requires only an approximate
  //! Jacobian, we can save a lot of time when we encounter a singular
  //! matrix by simply augmenting the diagonal elements anywhere they
  //! appear to be nearly zero (since this is almost always the cause
  //! of singularities in our calculations) without changing the
  //! underlying x values.  This avoids additional evaluations that
  //! would otherwise be required to update fx and B for the changed x
  //! values.  However, this seems not to work too well in the first
  //! two model years (1990 and 2005), so we include a flag to turn it
  //! off in the solver config.
  bool mShortcut;
  
  //! Filter which will be used to determine which markets the solver
  //! will attempt to solve
  std::auto_ptr<ISolutionInfoFilter> mSolutionInfoFilter;

private:
  static std::string SOLVER_NAME;
};

#undef UBLAS
#undef UBMATRIX

#endif  // LOGBROYDEN_HPP_

