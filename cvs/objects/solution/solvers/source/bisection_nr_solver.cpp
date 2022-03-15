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
* \file bisection_nr_solver.cpp
* \ingroup objects
* \brief BisectionNRSolver class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>

#include "solution/solvers/include/bisection_nr_solver.h"
#include "containers/include/world.h"
#include "solution/solvers/include/solver_component.h"
#include "solution/util/include/solution_info_set.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "solution/util/include/calc_counter.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"
#include "util/base/include/auto_file.h"

// need to include these so that we can get the xml names
#include "solution/solvers/include/log_newton_raphson.h"
#include "solution/solvers/include/bisect_all.h"
#include "solution/solvers/include/log_newton_raphson_sd.h"

using namespace std;

//! Constructor
BisectionNRSolver::BisectionNRSolver( Marketplace* aMarketplace, World* aWorld ):Solver( aMarketplace, aWorld ),
mDefaultSolutionTolerance( 0.001),
mDefaultSolutionFloor( 0.0001 ),
mCalibrationTolerance( 0.01 ),
mMaxModelCalcs( 2000 )
{
    // Construct components.
    mCalcCounter = world->getCalcCounter();
}

//! Destructor
BisectionNRSolver::~BisectionNRSolver() {
}

//! Get the solver name.
const string& BisectionNRSolver::getXMLNameStatic() {
    const static string SOLVER_NAME = "BisectionNRSolver";
    return SOLVER_NAME;
}

//! Initialize the solver at the beginning of the model.
void BisectionNRSolver::init() {
}

/*! \brief Solution method to solve all markets for one period.
* \details This is the main solution function called from within the scenario. It is called once for
* each period to clear all markets which should be solved. This solve method first attempts Newton-Raphson
* on all markets, then uses bracketing and bisection on markets that are not solved by Newton-Raphson.
* Bracketing and bisection solves the unsolved markets or brings prices closer to solution for the
* Newton-Raphson to solve in the next attempt.
* \param aPeriod The period to solve.
* \param aSolutionInfoParamParser An object that will be used to set solution info specific value
*                                 when they are initialized.
* \return Whether the markets are all solved.
* \author Josh Lurz, Sonny Kim
*/
bool BisectionNRSolver::solve( const int aPeriod, const SolutionInfoParamParser* aSolutionInfoParamParser ) {
    const Configuration* conf = Configuration::getInstance();

    // Open all log files.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    ILogger& solverLog = ILogger::getLogger( "solver_log" );
    ILogger& singleLog = ILogger::getLogger( "single_market_log" );
    mainLog.setLevel( ILogger::DEBUG );
    solverLog.setLevel( ILogger::NOTICE );
    singleLog.setLevel( ILogger::DEBUG );
    solverLog << endl << "Solution() Begin. Per " << aPeriod << endl;

    // Create and initialize the solution set.
    // This will fetch the markets to solve and update the prices, supplies and demands.
    SolutionInfoSet solution_set( marketplace );
    solution_set.init( aPeriod, mDefaultSolutionTolerance, mDefaultSolutionFloor, aSolutionInfoParamParser ); // determines solvable and unsolvable markets

    mainLog << "Starting Solution. Solving for " << solution_set.getNumSolvable()
            << " markets." << endl;
    solution_set.printMarketInfo( "Begin Solve", mCalcCounter->getPeriodCount(), singleLog );

    // If no markets to solve, break out of solution.
    if ( solution_set.getNumSolvable() == 0 ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved with last period's prices." << endl;
        return true;
    }
    else if( aPeriod == 0 ){
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Period zero has solvable markets." << endl;
    }

    solverLog << "Number of Markets: " << solution_set.getNumSolvable() << endl;
    solverLog << "Solution Information Initialized: Left and Right values are the same." << endl;
    solverLog.setLevel( ILogger::DEBUG );
    solverLog << solution_set << endl;

    // Initialize solver components
    mLogNewtonRaphson->init();
    mBisectAll->init();
    if( mLogNewtonRaphsonSaveDeriv.get() ){
        mLogNewtonRaphsonSaveDeriv->init();
    }

    // Loop is done at least once.
    do {
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Solution() loop. N: " << mCalcCounter->getPeriodCount() << endl;
        solverLog.setLevel( ILogger::DEBUG );

        // Call Newton Rahpson approach first. Will return right away if all solved.
        bool useSaveDerivs = mLogNewtonRaphsonSaveDeriv.get();
        for( int i = 0; i < solution_set.getNumSolvable() && useSaveDerivs; ++i ) {
            // TODO: this should be a read in threshold
            if( solution_set.getSolvable( i ).getRelativeED() >= 1 ) {
                useSaveDerivs = false;
            }
        }
        if( !useSaveDerivs ) {
            mLogNewtonRaphson->solve( solution_set, aPeriod );
        }
        else {
            mLogNewtonRaphsonSaveDeriv->solve( solution_set, aPeriod );
        }

        // If not all solved, then call bracketing and bisection routines.
        if( !solution_set.isAllSolved() ) {
            // Get only unsolved solution set from the complete set.
            SolutionInfoSet unsolved_solution_set( solution_set.getUnsolvedSet() );

            // Attempts bisection on unsolved solution set.
            mBisectAll->solve( unsolved_solution_set, aPeriod );
            solverLog << "Solution after bisect all of unsolved set: " << endl << unsolved_solution_set << endl;

            solverLog << "Solution after bisect all and update: " << endl << solution_set << endl;
        }

        // Determine if the model has solved. 
    } while ( !solution_set.isAllSolved()
              && mCalcCounter->getPeriodCount() < mMaxModelCalcs );
    
    if ( conf->getBool( "CalibrationActive" )
            && !world->isAllCalibrated( aPeriod, mCalibrationTolerance, true ) ) {
        mainLog.setLevel( ILogger::WARNING );
        solverLog << "Model did not calibrate successfully in period " << aPeriod << endl;
        mainLog << "Model did not calibrate successfully in period " << aPeriod << endl;
    }

    // Determine whether the model was successful.
    if( solution_set.isAllSolved() ){
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Model solved normally. Iterations period "<< aPeriod << ": "
                << mCalcCounter->getPeriodCount() << ". Total iterations: "
                << mCalcCounter->getTotalCount() << endl;
        return true;
    }
    
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Model did not solve within set iteration " << mCalcCounter->getPeriodCount() << endl;
    solverLog << "Printing solution information after failed attempt to solve." << endl;
    solverLog << solution_set << endl;

    // Print unsolved markets.
    solution_set.printUnsolved( mainLog );

    if( conf->shouldWriteFile( "supplyDemandOutputFileName" ) ) {
        string logName = conf->getFile( "supplyDemandOutputFileName", "supply_demand_curves" );
        if( conf->shouldAppendScnToFile( "supplyDemandOutputFileName" ) ) {
            logName = util::appendScenarioToFileName( logName );
        }
        ILogger& sdLog = ILogger::getLogger( logName );
        sdLog.setLevel( ILogger::WARNING );
        sdLog << "Supply and demand curves for markets that did not solve in period: " << aPeriod << endl;
        solution_set.findAndPrintSD( world, marketplace, aPeriod, sdLog );
    }
    return false;
}
