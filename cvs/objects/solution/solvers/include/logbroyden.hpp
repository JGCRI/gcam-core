#ifndef LOGBROYDEN_HPP_
#define LOGBROYDEN_HPP_

#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy ( DOE ). NEITHER THE GOVERNMENT NOR THE
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
#include "solution/util/include/ublas-helpers.hpp"

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
             double ftol=1.0e-4) :
      SolverComponent(mktplc,world,ccounter), mMaxIter( itmax ), mFTOL( ftol ),
      mLogPricep( true ), mMaxJacobainReuse( 100 ), mSolutionInfoFilter(0) {}
    LogBroyden() :
        SolverComponent(), mMaxIter( 250 ), mFTOL( 1.0e-4 ),
        mLogPricep( true ), mMaxJacobainReuse( 100 ), mSolutionInfoFilter(0) {}
  virtual ~LogBroyden() {
      delete mSolutionInfoFilter;
  }

  // SolverComponent methods
  virtual void init() {
    if(!mSolutionInfoFilter)
      mSolutionInfoFilter = new SolvableNRSolutionInfoFilter();
  }
  virtual ReturnCode solve( SolutionInfoSet& aSolutionSet, const int aPeriod );
  virtual const std::string& getXMLName() const {return SOLVER_NAME;}
    
  // AParsable methods
  virtual bool XMLParse( rapidxml::xml_node<char>* & aNode );
  
  static const std::string & getXMLNameStatic( void ) {return SOLVER_NAME;}

protected:
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        SolverComponent,
        
        //! Maximum number of main-loop iterations for the root-finding algorithm
        DEFINE_VARIABLE( SIMPLE, "max-iterations", mMaxIter, unsigned int ),
        
        //! Tolerance for convergence test in root-finding algorithm
        //! \warning The SolutionInfo class has its own convergence
        //! tolerance, which it uses to flag certain markets as "unsolved".
        //! If that tolerance is different from this one, the SolutionInfo
        //! might regard a market as unsolved when the solver says it's
        //! solved, or vice versa.
        DEFINE_VARIABLE( SIMPLE, "ftol", mFTOL, double ),
        
        //! flag indicating whether we should work in price or log-price
        DEFINE_VARIABLE( SIMPLE, "is-log-price", mLogPricep, bool ),
                            
        //! Control the number of times we can re-use the Jacobian using Broyden's method
        //! which if set to zero implies this algorithm just collapse to a regular NR algorithm
        DEFINE_VARIABLE( SIMPLE, "max-jacobian-reuse", mMaxJacobainReuse, int ),
        
        //! A filter which will be used to determine which SolutionInfos with solver component
        //! will work on.
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "solution-info-filter", mSolutionInfoFilter, ISolutionInfoFilter* )
    )
    
  //! Perform the Broyden's method iterations.
  int bsolve(VecFVec &F, UBVECTOR &x, UBVECTOR &fx,
             UBMATRIX &B, int &neval, const std::list<int>& allCols);
  //! Additional logging for visualizing solver progress.
  void reportVec(const std::string &aname, const UBVECTOR &av, const std::vector<int> &amktids,
                 const std::vector<bool> &aissolvable);
  void reportPSD(UBVECTOR &arptvec, const std::vector<int> &amktids, const std::vector<bool> &aissolvable);

  // These next two have to be class variables because we sometimes
  // have multiple logbroyden solvers operating.
  static int mLastPer;                 //<! used to detect when the period has changed, so we can reset mPerIter.
  static int mPerIter;                 //<! total iteration count within the period

private:
  static std::string SOLVER_NAME;
};

#endif  // LOGBROYDEN_HPP_

