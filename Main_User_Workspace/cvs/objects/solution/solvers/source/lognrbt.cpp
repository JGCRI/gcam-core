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
* \file lognrbt.cpp
* \ingroup objects
* \brief LogNRbt (Log Newton-Raphson with backtracking) class source file.
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <string>
#include <algorithm>
#include <math.h>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/lognrbt.hpp"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/solution_info.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "solution/util/include/solution_info_filter_factory.h"
#include "solution/util/include/solvable_nr_solution_info_filter.h"

#include "solution/util/include/functor-subs.hpp"
#include "solution/util/include/linesearch.hpp"
#include "solution/util/include/fdjac.hpp" 
#include "solution/util/include/edfun.hpp"
#include "solution/util/include/ublas-helpers.hpp"
#include "solution/util/include/jacobian-precondition.hpp" 
#include "util/base/include/fltcmp.hpp"

#if USE_LAPACK
#include <boost/numeric/bindings/traits/ublas_vector.hpp>
#include <boost/numeric/bindings/traits/ublas_matrix.hpp>
#include <boost/numeric/ublas/operation.hpp>
#include <boost/numeric/bindings/lapack/gesvd.hpp>
#include "solution/util/include/svd_invert_solve.hpp"
#else
#include <boost/numeric/ublas/operation.hpp>
#include <boost/numeric/ublas/lu.hpp>
#endif

#include "util/base/include/timer.h"

using namespace std;
using namespace xercesc;

std::string LogNRbt::SOLVER_NAME = "log-newton-raphson-backtracking-solver-component";

#if USE_LAPACK
#define UBMATRIX boost::numeric::ublas::matrix<double,boost::numeric::ublas::column_major>
#else
#define UBMATRIX boost::numeric::ublas::matrix<double>
#endif
#define UBVECTOR boost::numeric::ublas::vector<double>

namespace {
  // helper functions for the std::transform algorithm
  double SI2lgprice (const SolutionInfo &si) {return log(si.getPrice());}
  double SI2price (const SolutionInfo &si) {return si.getPrice();}
}

bool LogNRbt::XMLParse( const DOMNode* aNode ) {


    // assume we were passed a valid node.
    assert( aNode );
    
    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "max-iterations" ) {
            mMaxIter = XMLHelper<unsigned int>::getValue( curr );
        }
        else if( nodeName == "ftol" ) {
            mFTOL = XMLHelper<double>::getValue(curr);
        }
        else if( nodeName == "solution-info-filter" ) {
            mSolutionInfoFilter.reset(
                SolutionInfoFilterFactory::createSolutionInfoFilterFromString( XMLHelper<string>::getValue( curr ) ) );
        }
        else if(nodeName == "linear-price") {
          mLogPricep = false;
        }
        else if(nodeName == "log-price") {
          mLogPricep = true;    // not strictly necessary, as this is the default.
        } 
        else if( SolutionInfoFilterFactory::hasSolutionInfoFilter( nodeName ) ) {
            mSolutionInfoFilter.reset( SolutionInfoFilterFactory::createAndParseSolutionInfoFilter( nodeName, curr ) );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                << getXMLName() << "." << endl;
        }
    }
    return true;
}

/*! \brief Newton-Raphson solver in log-log space with backtracking
  
 * \details Attempts to solve the selected markets using a
 *          Newton-Raphson solution with backtracking (see Numerical
 *          Recipes sectn. 9.7).  The solution is performed in
 *          log-log space (log(D/S) as a function of log(price)), as
 *          this extends the domain and range to all reals.  Most of
 *          the heavy lifting is done by the function nrsolve(), wile
 *          this function mostly sets up the structures necessary to
 *          call nrsolve. 
 * \author Robert Link
 * \param solnset An initial set of SolutionInfo objects representing all markets which can be filtered.
 * \param period Model period.
 * \return A status code to indicate if the algorithm was successful or not.
 */

SolverComponent::ReturnCode LogNRbt::solve( SolutionInfoSet& solnset, int period ) {
    ReturnCode code = SolverComponent::ORIGINAL_STATE;

    // If all markets are solved, then return with success code.
    if( solnset.isAllSolved() ){
        return code = SolverComponent::SUCCESS;
    }

    startMethod();
    
    // Update the solution vector for the correct markets to solve.
    // Need to update solvable status before starting solution (Ignore return code)
    solnset.updateSolvable( mSolutionInfoFilter.get() );

    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    solverLog.setLevel( ILogger::NOTICE );
    solverLog << "Beginning Newton-Raphson solution for period " << period
              << "Solving " << solnset.getNumSolvable() << "markets.\n";
    ILogger& worstMarketLog = ILogger::getLogger( "worst_market_log" );
    worstMarketLog.setLevel( ILogger::DEBUG );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    singleLog.setLevel( ILogger::DEBUG );
    
    size_t nsolv = solnset.getNumSolvable(); 
    if( nsolv == 0 ){
        solverLog << "No markets were assigned to this solver.  Exiting." << endl;
        return SUCCESS;
    }
    
    solverLog << "Initial market state:\nmkt\tprice\tsupply\tdemand\n";
    std::vector<SolutionInfo> solvables = solnset.getSolvableSet();
    for(size_t i=0; i<solvables.size(); ++i) {
      solverLog << i << "\t" << solvables[i].getPrice()
                << "\t" << solvables[i].getSupply()
                << "\t" << solvables[i].getDemand()
                << "\t\t" << solvables[i].getName() << "\n";
    } 

    Timer& solverTimer = TimerRegistry::getInstance().getTimer( TimerRegistry::SOLVER );
    solverTimer.start();
    
    UBVECTOR x(nsolv), fx(nsolv);
    int neval = 0;

    // set our initial x from the solutionInfoSet
    std::vector<SolutionInfo> smkts(solnset.getSolvableSet());
    if(mLogPricep)
      std::transform(smkts.begin(), smkts.end(), x.begin(), SI2lgprice);
    else
      std::transform(smkts.begin(), smkts.end(), x.begin(), SI2price);

    // This is the closure that will evaluate the ED function
    LogEDFun F(solnset, world, marketplace, period, mLogPricep); 

    // scale the initial guess for use in F
    F.scaleInitInputs(x);
    
    // Call F(x), store the result in fx
    F(x,fx);

    solverLog << "Initial guess:\n" << x << "\nInitial F(x):\n" << fx << "\n";

    // Precondition the x values to avoid singular columns in the Jacobian
    solverLog.setLevel(ILogger::DEBUG);
    UBMATRIX J(F.narg(),F.nrtn());
    fdjac(F, x, fx, J, true);
    int pcfail = jacobian_precondition(x,fx,J,F,&solverLog, mLogPricep);

    if(pcfail) {
      solverLog.setLevel(ILogger::ERROR);
      solverLog << "Unable to find nonsingular initial guess for one or more markets.  nrsolve() will probably fail.\n";
      solverLog.setLevel(ILogger::DEBUG);
    }
    else {
      solverLog << "Revised guess:\n" << x << "\nInitial F(x):\n" << fx << "\n";
    }
    
    // call the solver
    int nrstatus = nrsolve(F, x, fx, J, neval);


    solverTimer.stop();

    solverLog.setLevel(ILogger::NOTICE);
    solverLog << "Newton-Raphson solver:  neval= " << neval << "\nResult:  ";
    if(nrstatus == 0) {
        solverLog << "NR solution success.\n";
        code = SUCCESS;
    }
    else if(nrstatus == -1) {
        code = FAILURE_ITER_MAX_REACHED;
        solverLog << "NR solution failed: Iteration max reached.\n";
    }
    else if(nrstatus == -3) {
        code = FAILURE_ZERO_GRADIENT;
        solverLog << "NR solution failed:  Encountered zero gradient in F*F.\n";
    }
    else if(nrstatus > 0) {
        code = FAILURE_SINGULAR_MATRIX;
        solverLog << "NR solution failed:  Encountered singular matrix (# singular components = " << nrstatus << ").\n";
    }
    else {
        code = FAILURE_UNKNOWN;
        solverLog << "NR solution failed for unknown reason.\n";
    }
    if(!solnset.isAllSolved()) {
        solverLog << "The following markets were not solved:\n";
        solnset.printUnsolved(solverLog);
    }

    solverLog << endl;
    return code;
}


int LogNRbt::nrsolve(VecFVec<double,double> &F, UBVECTOR &x, UBVECTOR &fx, UBMATRIX &J,
                     int &neval)
{
#if !USE_LAPACK
  using boost::numeric::ublas::permutation_matrix;
  using boost::numeric::ublas::lu_factorize;
  using boost::numeric::ublas::lu_substitute;
#endif
  using boost::numeric::ublas::axpy_prod;
  using boost::numeric::ublas::inner_prod;
  int nrow = J.size1(), ncol = J.size2();
  // svd decomposition elements (note nrow == ncol)
#if USE_LAPACK
  UBMATRIX Usv(nrow,ncol),VTsv(ncol,ncol);
  UBVECTOR Ssv(ncol);
  int singcount = 0;
  const int scmax = nrow/2;
#else
  permutation_matrix<int> p(F.narg()); // permutation vector for pivoting in L-U decomposition
#endif
  UBMATRIX Jtmp(nrow, ncol);
  

  ILogger &solverLog = ILogger::getLogger("solver_log");

  // Note that we no longer need to do a jacobian calculation here
  // because we do one in the preconditioner
  F(x,fx);

  solverLog.setLevel(ILogger::DEBUG);
  
  neval += 1 + x.size();        // initial function evaluation + jacobian calculations

  const double FTINY = mFTOL*mFTOL;
  UBVECTOR dx(F.narg());
  UBVECTOR xnew(F.narg());
  UBVECTOR gx(F.narg());
  assert(F.nrtn() == F.narg());
  assert(x.size() == F.narg());
  assert(fx.size() == F.nrtn());

  // We create a functor that computes f(x) = F(x)*F(x).  It also
  // stores the value of F that it produces as an intermediate.
  FdotF<double,double> fnorm(F);
  double f0 = inner_prod(fx,fx); // already have a value of F on input, so no need to call fnorm yet
  if(f0 < FTINY)
    // Guard against F=0 since it can cause a NaN in our solver.  This
    // is a more stringent test than our regular convergence test
    return 0;
  
  for(int iter=0; iter<mMaxIter; ++iter) {
    solverLog << "NR iter= " << iter << "\tneval= " << neval << "\n";
    axpy_prod(fx,J,gx);         // compute the gradient of F*F (= fx^T * J == J^T * fx)
    // axpy_prod clears gx on entry, so we don't have to do it.
    // NB: the order of fx and J in that last call is significant!
    
    // Check for zero gradient.  This indicates a local minimum in f,
    // from which we are unlikely to escape.  We will need to try
    // again with a different initial guess.  (This should be very
    // uncommon)
    if(inner_prod(gx,gx) / (f0+FTINY) < mFTOL) {
      return -3;
    }

    Jtmp = J;                   // save the Jacobian, since gesvd destroys it.

#if USE_LAPACK
    int ierr =
      boost::numeric::bindings::lapack::gesvd('O','A','A', // control parameters
                                              J,           // input matrix
                                              Ssv,Usv,VTsv); // output matrices
    if(ierr != 0) {
      // svd failed.  It's not even clear under what circumstances
      // this can happen
      solverLog.setLevel(ILogger::SEVERE);
      solverLog << "****************SVD failed.  This shouldn't happen.  It can't mean anything good.\n";
      return ierr;
    } 
    
    // At this point, U, S, and VT contain the SVD of the original Jacobian
    solverLog.setLevel(ILogger::DEBUG);
    dx = -1.0*fx; 
    int nsing = svdInvertSolve(Usv,Ssv,VTsv,dx, solverLog);
    
    solverLog.setLevel(ILogger::DEBUG);
    solverLog << "\n****************Iteration " << iter << "\nf0= " << f0
              << "\tnsing= " << nsing
              << "\nx: " << x << "\nF(x): " << fx << "\ndx: " << dx << "\n";


    if(nsing > 0) {
      singcount += nsing;
      if(singcount < scmax) {
        // Try to reset the x value using the preconditioner
        solverLog << "Resetting singular matrix, singcount = " << singcount << "\n";
        J = Jtmp;
        int fail = jacobian_precondition(x, fx, J, F, &solverLog, mLogPricep);
        if(fail)
          return nsing;

        // re-evaluate f0 and gx at the new guess
        double f0 = inner_prod(fx,fx);
        axpy_prod(fx,J,gx);         // compute the gradient of F*F (= fx^T * J == J^T * fx)
        
        // re-solve for dx using the new Jacobian
        ierr = boost::numeric::bindings::lapack::gesvd('O','A','A', // control parameters
                                                       J,           // input matrix
                                                       Ssv,Usv,VTsv); // output matrices
        if(ierr)
          return nsing;
        dx = -1.0*fx;
        svdInvertSolve(Usv, Ssv, VTsv, dx, solverLog);
      }
      else {
        return nsing;
      }
    }
    else
      singcount = 0;
#else  /* No USE_LAPACK.  Use L-U decomposition to do the solution. */
    int itrial = 0;
    /* If the L-U decomposition fails the first time around, we will
       invoke the jacobian preconditioner and try again.  If it fails
       a second time, we bail out */
    do {
      for(size_t i=0; i<p.size(); ++i) p[i] = i;
      int sing = lu_factorize(J,p);
      if(sing>0) {
        int fail=1;
        if(itrial == 0)
          fail = jacobian_precondition(x, fx, J, F, &solverLog, mLogPricep);
        
        if(fail) {
          solverLog.setLevel(ILogger::WARNING);
          solverLog << "Singular Jacobian:\n" << Jtmp << "\n";
          return sing;
        }
      }
      else {
        // L-U decomp was successful.  Continue with the next phase of the algorithm.
        break;
      }
    } while(++itrial < 2);
    
    // J now holds the L-U decomposition of the Jacobian.  Attempt backsubstitution
    dx = -1.0*fx;
    try {
      lu_substitute(J,p,dx);    // solve dx = J^-1 F
    }
    catch (const boost::numeric::ublas::internal_logic &err) {
      // This error seems to be thrown when the Jacobian is
      // ill-conditioned.  We let it go because often the solver will
      // muddle through to a solution.  If not, then it will
      // eventually stop with a genuinely singular matrix.
    }
#endif /* USE_LAPACK */
    
    // dx now holds the newton step.  Execute the line search along
    // that direction.
    double fnew;
    int lserr = linesearch(fnorm,x,f0,gx,dx, xnew,fnew, neval);

    if(lserr != 0) {
      // line search failed.  This means that the descent direction
      // for F only extends a very short way (roughly TOL * x0).
      // The most likely way for this to happen is if we're close to a
      // discontinuity in (probably several components of) the
      // Jacobian.  Using smoother supply and/or demand functions
      // might help here.
      
      // It's also possible we failed because we're really close to
      // the solution (sometimes dx gets really tiny near the
      // solution).  Make a relaxed convergence test and return if we
      // have a "close enough" solution.
      double msf = f0/fx.size();
      if(msf < mFTOL)
        // basically, we're letting ourselves converge to the sqrt of
        // our intended tolerance.
        return 0;
      
      // We're not close enough to the solution, and we don't have a
      // good descent direction.  There are no good options at this point
      // so kick out and hope that the preconditioner can set us straight.
      solverLog << "linesearch failure\n";
        return -4;
    }

    UBVECTOR xstep = xnew-x;    // step in x eventually taken
    solverLog << "################Return from linesearch\nfold= " << f0 << "\tfnew= " << fnew
              << "\n";
    f0 = fnew;
    x  = xnew;
    fnorm.lastF(fx);            // get the last value of big-F
    solverLog << "\nxnew: " << xnew << "\nfxnew: " << fx << "\n";
    
  
    // test for convergence
    double maxval = 0.0;
    for(size_t i=0; i<fx.size(); ++i) {
      double val = fabs(fx[i]);
      maxval = val>maxval ? val : maxval;
    }

    solverLog << "Convergence test maxval: " << maxval << "\n";
    if(maxval <= mFTOL) {
      solverLog << "Solution successful.\n";
      return 0;                 // SUCCESS 
    }
    
    fdjac(F,x,fx,J);            // calculate finite difference Jacobian for the next iteration
    neval += x.size();          // N evaluations from calculating the Jacobian
  }

  // if we get here, then we didn't converge in the number of
  // iterations allowed us.  Return an error code
  solverLog << "\n****************Maximum solver iterations exceeded.\nlastx: " << x
            << "\nlastF: " << fx << "\n";
  return -1;
}
