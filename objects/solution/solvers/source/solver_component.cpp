/*! 
* \file solver_component.cpp
* \ingroup objects
* \brief SolverComponent class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <memory>
#include <string>
#include <iostream>

#include "solution/solvers/include/solver_component.h"
#include "solution/solvers/include/log_newton_raphson.h"
#include "solution/solvers/include/log_newton_raphson_sd.h"
#include "solution/solvers/include/bisect_all.h"
#include "solution/solvers/include/bisect_one.h"
#include "solution/solvers/include/bisect_policy.h"
#include "util/base/include/configuration.h"
#include "solution/util/include/calc_counter.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/util.h"
// Temporarily put Solver factory method here.
#include "solution/solvers/include/bisection_nr_solver.h"
#include "solution/solvers/include/bisect_policy_nr_solver.h"

using namespace std;

/*! \brief Constructor.
* \detailed This constructor takes as arguments the marketplace, and world which it will be solving, and a pointer to the CalcCounter
* which tracks calls to world.calc(). It also initializes several variables from values in the Configuration object.
* \param marketplaceIn The marketplace which will be used for solving.
* \param worldIn The world which will be used for solving.
* \param calcCounterIn A pointer to the object which tracks calls to world.calc()
*/
SolverComponent::SolverComponent( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn ): marketplace( marketplaceIn ), world( worldIn ), calcCounter( calcCounterIn ){
}

//! Default Destructor.
SolverComponent::~SolverComponent(){
}

/*! \brief Static factory method to generate SolverComponents. 
* \detailed This is a static factory method which when passed in the name of a SolverComponent
* returns an auto_ptr to a new dynamically allocated instance of the appropriate SolverComponent.
* If there is not a SolverComponent of the given name, the function will report an error to the console
* and return a null auto_ptr. This function is designed to encapsulate SolverComponents so that there 
* implementations can be hidden from the Solver, and the Solver does not have to include their header files.
* \note All new SolverComponent sub-classes should be added here. 
* \param solverName The name of the SolverComponent, as defined by the static getName function.
* \param marketplace A pointer to the global marketplace.
* \param world A pointer to the world. 
* \param calcCounter A pointer to the calcCounter object used to track calls to World.calc()
* \return An auto_ptr to the SolverComponent named solverName, null if it does not exist.
*/
auto_ptr<SolverComponent> SolverComponent::getSolverComponent( const string& solverName, Marketplace* marketplace, World* world, CalcCounter* calcCounter ){
    // Check the name against possible components. 
    if( solverName == LogNewtonRaphson::getNameStatic() ){
        return auto_ptr<SolverComponent>( new LogNewtonRaphson( marketplace, world, calcCounter ) );
    }
    else if( solverName == BisectAll::getNameStatic() ){
        return auto_ptr<SolverComponent>( new BisectAll( marketplace, world, calcCounter ) );
    }
    else if( solverName == LogNewtonRaphsonSaveDeriv::getNameStatic() ){
        return auto_ptr<SolverComponent>( new LogNewtonRaphsonSaveDeriv( marketplace, world, calcCounter ) );
    }
    else if( solverName == BisectOne::getNameStatic() ){
        return auto_ptr<SolverComponent>( new BisectOne( marketplace, world, calcCounter ) );
    }
    else if( solverName == BisectPolicy::getNameStatic() ){
        return auto_ptr<SolverComponent>( new BisectPolicy( marketplace, world, calcCounter ) );
    }
    else {
        cout << "Error: Invalid solver component name: " << solverName << endl;
        return auto_ptr<SolverComponent>();
    }
}

//! Struct constructor
SolverComponent::IterationInfo::IterationInfo( const std::string& aName, const double aRED )
:mName( aName ), mRED( aRED ){}

//! Add a solution iteration to the stack.
void SolverComponent::addIteration( const std::string& aSolName, const double aRED ){
    mPastIters.push_back( IterationInfo( aSolName, aRED ) );
}

//! Check for improvement over the last n iterations
bool SolverComponent::isImproving( const unsigned int aNumIter ) const {
    // Check if there are enough iterations to check.
    if( aNumIter >= mPastIters.size()  ){
        return true;
    }

    // Check if there has been improvement
    double currValue = mPastIters.back().mRED;
    // double prevValue = mPastIters.at( mPastIters.size() - aNumIter - 1 ).mRED;

    // return( ( prevValue - currValue ) / currValue > 0.1 ); // This value isnt right.
    unsigned int numBetter = 0;
    // Check how many of the previous are greater than the number.
    for( unsigned int i = 1; i < aNumIter; ++i ){
        double prevValue = mPastIters.at( mPastIters.size() - i - 1 ).mRED;
        if( ( prevValue - currValue ) / currValue > 0.1 ){
            ++numBetter;
        }
    }
    return( static_cast<double>( numBetter ) / ( aNumIter - 1 ) > 0.25 );
}

void SolverComponent::startMethod(){
    // Set the current calculation method.  
    calcCounter->setCurrentMethod( getName() );
    // Clear the stack.
    mPastIters.clear();
}

// Temporarily put solver class definitions here.
// TODO: Determine if a source file is neccessary.
//! Factory method.
std::auto_ptr<Solver> Solver::getSolver( const string& aSolverName, Marketplace* aMarketplace, World* aWorld ){
    if( aSolverName == BisectPolicyNRSolver::getName() ){
        return std::auto_ptr<Solver>( new BisectPolicyNRSolver( aMarketplace, aWorld ) );
    }
    else {
        return std::auto_ptr<Solver>( new BisectionNRSolver( aMarketplace, aWorld ) );
    }
}


