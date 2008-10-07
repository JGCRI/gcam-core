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
* \file log_newton_raphson_sd.cpp
* \ingroup objects
* \brief LogNewtonRaphson class source file.
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/log_newton_raphson_sd.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/world.h"
#include "solution/util/include/solver_info_set.h"
#include "solution/util/include/solver_info.h"
#include "solution/util/include/solver_library.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"

using namespace std;

const string LogNewtonRaphsonSaveDeriv::SOLVER_NAME = "LogNewtonRaphsonSaveDeriv";

//! Default Constructor. Need to call constructor of class next up in hierarchy. Constructs the base class. 
LogNewtonRaphsonSaveDeriv::LogNewtonRaphsonSaveDeriv( Marketplace* aMarketplaceIn, World* aWorld,
                                                      CalcCounter* aCalcCounter, double aDeltaPrice ):
LogNewtonRaphson( aMarketplaceIn, aWorld, aCalcCounter, aDeltaPrice ),
savedMatrixSize( 0 )
{
}

//! Default Destructor. Currently does nothing.
LogNewtonRaphsonSaveDeriv::~LogNewtonRaphsonSaveDeriv(){
}

//! Init method.  
void LogNewtonRaphsonSaveDeriv::init() {
    derivativesCalculated = false;
}

//! Get the name of the SolverComponent
const string& LogNewtonRaphsonSaveDeriv::getNameStatic() {
    return SOLVER_NAME;
}

//! Get the name of the SolverComponent
const string& LogNewtonRaphsonSaveDeriv::getName() const {
    return SOLVER_NAME;
}

//! Calculate derivatives
SolverComponent::ReturnCode LogNewtonRaphsonSaveDeriv::calculateDerivatives( SolverInfoSet& solverSet,
                                                                             Matrix& JFSM, Matrix& JFDM,
                                                                             Matrix& JF, int period )
{
    // Only calculated once.
    if ( !derivativesCalculated ) {
        derivativesCalculated = true;

        // Calculate derivatives.
        SolverLibrary::derivatives( marketplace, world, solverSet, mDeltaPrice, period ); 

        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Derivatives calculated" << endl;

        // Update the JF, JFDM, and JFSM matrices
        SolverLibrary::updateMatrices( solverSet, JFSM, JFDM, JF );
        bool isSingular = false;
        JF = SolverLibrary::invertMatrix( JF, isSingular );
        if( isSingular ) {
            solverLog.setLevel( ILogger::ERROR );
            solverLog << "Matrix came back as singluar, could not invert." << endl;
            solverLog.setLevel( ILogger::NOTICE );
            return FAILURE_SINGULAR_MATRIX;
        }

        // Save matricies
        JFSave.assign( JF );
        JFDMSave.assign( JFDM );
        JFSMSave.assign( JFSM );
        savedMatrixSize = solverSet.getNumSolvable();

        // Otherwise restore from saved values
    } else {
        ILogger& solverLog = ILogger::getLogger( "solver_log" );
        solverLog.setLevel( ILogger::NOTICE );
        solverLog << "Using cached derivatives" << endl;
        if ( solverSet.getNumSolvable() != savedMatrixSize ) {
            ILogger& solverLog = ILogger::getLogger( "solver_log" );
            solverLog.setLevel( ILogger::ERROR );
            solverLog << "Matrix sizes changed " << solverSet.getNumSolvable() << ", "<< savedMatrixSize << endl;
            return FAILURE_SOLUTION_SIZE_CHANGED;
        }
        JF.assign( JFSave );
        JFDM.assign( JFDMSave );
        JFSM.assign( JFSMSave );
    }

    return SUCCESS;
}
