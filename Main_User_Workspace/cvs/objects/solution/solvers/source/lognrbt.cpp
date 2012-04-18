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
#include "solution/solvers/include/lognrbt.hh"
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

#include <boost/numeric/ublas/lu.hpp>
#include "solution/util/include/functor-subs.hh"
#include "solution/util/include/linesearch.hh"
#include "solution/util/include/fdjac.hh" 
#include "solution/util/include/edfun.hh"
#include "solution/util/include/ublas-helpers.hh"
#include "util/base/include/fltcmp.hh"

#include "util/base/include/fltcmp.hh"


using namespace std;
using namespace xercesc;

std::string LogNRbt::SOLVER_NAME = "log-newton-raphson-backtracking-solver-component";

#define UBMATRIX boost::numeric::ublas::matrix<double>
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

/*! \brief Newton-Raphson solver in log-log space with backgracking
  
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
                << "\t" << solvables[i].getDemand() << "\n";
    } 

    UBVECTOR x(nsolv), fx(nsolv);
    int neval = 0;

    // set our initial x from the solutionInfoSet
    std::vector<SolutionInfo> smkts(solnset.getSolvableSet());
    std::transform(smkts.begin(), smkts.end(), x.begin(), SI2lgprice);

    // This is the closure that will evaluate the ED function
    LogEDFun F(solnset, world, marketplace, period); 
    
    // Call F(x), store the result in fx
    F(x,fx);

    solverLog.setLevel(ILogger::DEBUG);
    solverLog << "Initial guess:\n" << x << "\nInitial F(x):\n" << fx << "\n";
    
    // call the solver
    int nrstatus = nrsolve(F, x, fx, neval);

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
        solverLog << "NR solution failed:  Encountered singular matrix (row " << nrstatus << ").\n";
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


int LogNRbt::nrsolve(VecFVec<double,double> &F, UBVECTOR &x,
                     UBVECTOR &fx, int &neval)
{
  using boost::numeric::ublas::permutation_matrix;
  using boost::numeric::ublas::lu_factorize;
  using boost::numeric::ublas::lu_substitute;
  using boost::numeric::ublas::axpy_prod;
  using boost::numeric::ublas::inner_prod;
  UBMATRIX J(F.narg(),F.nrtn());

  ILogger &solverLog = ILogger::getLogger("solver_log");
  
  F(x,fx);
  fdjac(F,x,fx,J);            // calculate finite difference Jacobian

  solverLog.setLevel(ILogger::DEBUG);
  
  neval += 1 + x.size();        // initial function evaluation + jacobian calculations

  const double FTINY = mFTOL*mFTOL;
  UBMATRIX Jsav(J); // stash a copy of J before we run L-U
  permutation_matrix<int> p(F.narg()); // permutation vector for pivoting in L-U
                             // decomposition (note that a
                             // "permutation matrix" is actually a
                             // vector).
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

    for(size_t i=0; i<p.size(); ++i) p[i] = i;
    Jsav = J;
    int sing = lu_factorize(J,p);
    if(sing>0) {
      solverLog.setLevel(ILogger::WARNING);
      solverLog << "Singular Jacobian:\n" << Jsav << "\n";
      return sing;
    }

    // At this point, J contains the L-U decomposition of the original J
    dx = -1.0*fx; 
    try {
      lu_substitute(J,p,dx);      // solve dx = J^-1 F
    }
    catch(const boost::numeric::ublas::internal_logic &err) {
      // This seems to happen when the Jacobian is ill-conditioned.
      // We'll let it go (possibly with some logging), since there
      // isn't much we can do about it.  Either the solver will muddle
      // through, or it will eventually bomb out with a genuinely
      // singular matrix or with a failure in linesearch.
      //std::cout << "\tFailure in L-U Substitute.\n\tx: " << x << "\tdx: " << dx << "\n";
    }

    solverLog << "\n****************Iteration " << iter << "\nf0= " << f0
              << "\nx: " << x << "\nF(x): " << fx << "\ndx: " << dx << "\n";
    
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
      // good descent direction.  In this bleak situation, take a unit
      // step in the dx direction and hope for the best
      double dxnorm = 1.0/inner_prod(dx,dx);
      xnew = x + dxnorm*dx;
      fnew = fnorm(xnew);
      neval++;              // fnorm performs a function evaluation
      solverLog << "linesearch failure\n";
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
