#ifndef LOGNRBT_HPP_
#define LOGNRBT_HPP_
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
* \file lognrbt.hpp
* \ingroup objects
* \brief Header file for the log Newton-Raphson with backtracking solver component
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
* \brief A SolverComponent based on the Newton-Raphson algorithm with
*        backtracking, using logarithmic values. 
* \details This solver uses a Newton-Raphson algorithm with
*          backtracking to avoid the nonconvergence problems we
*          typically find in a normal N-R solver. 
* \remark This solver component does most of its work by calling an
*         external solver subroutine that takes a closure that knows
*         how to calculate excess demand.  As such, it could be
*         readily modified to call any other solver subroutine we
*         might write, should we wish to try other root-finding
*         algorithms.
* \author Robert Link
*/
class LogNRbt: public SolverComponent {
public:
    LogNRbt( Marketplace* mktplc, World* world, CalcCounter* ccounter, int itmax=250,
             double ftol=1.0e-7 ) : SolverComponent(mktplc,world,ccounter),
                                    mMaxIter(itmax), mFTOL(ftol), mLogPricep(true) {}
    virtual ~LogNRbt() {}
    
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
    int nrsolve(VecFVec<double,double> &F, UBLAS::vector<double> &x,
                UBLAS::vector<double> &fx, UBMATRIX &J, int &neval);
    //! Max iterations for the Newton-Raphson algorithm 
    unsigned int mMaxIter;
  
    //! Tolerance for convergence test in root-finding algorithm. 
    //! \remark 1.0e-7 is a good choice for the convergence tolerance.
    //!          We're using double precision, so we could go lower,
    //!          but this value interacts in subtle ways with the
    //!          various small number definitions scattered throughout
    //!          the code (I am guilty of proliferating these).  At
    //!          any rate, I'm not sure that setting a tighter
    //!          tolerance actually buys us much in terms of the
    //!          quality of the solution. 
    //! \todo Roll all of the hard-wired "small numbers" into a single
    //!       consistent value for the whole code. 
    //! \warning The SolutionInfo class has its own convergence
    //!          tolerance, which it uses to flag certain markets as
    //!          unsolved.  If that tolerance is not the same as this
    //!          one, you may get inconsistent results (e.g., the N-R
    //!          algorithm says the solution converged for all
    //!          markets, while the SolutionInfo is reporting unsolved
    //!          markets. 
    double mFTOL;
  
    //! A filter which will be used to determine which SolutionInfos with solver component
    //! will work on.
    std::auto_ptr<ISolutionInfoFilter> mSolutionInfoFilter;

  bool mLogPricep;              //<! flag indicating whether we should work in price or log-price 

private:
    static std::string SOLVER_NAME;
};

#undef UBLAS
#undef UBMATRIX

#endif // LOGNRBT_HPP_
