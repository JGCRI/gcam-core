/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Labratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file bisecter.cpp
* \ingroup Objects
* \brief Bisecter class source file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include <string>
#include <cmath>
#include "util/logger/include/ilogger.h"
#include "target_finder/include/bisecter.h"
#include "target_finder/include/itarget.h"
#include "util/base/include/util.h"

using namespace std;

/*! \brief Construct the Bisecter.
* \param aPolicyTarget The policy target.
* \param aTolerance Solution tolerance.
*/
Bisecter::Bisecter( const ITarget* aTarget,
                    const double aTolerance,
                    const double aInitialValue,
                    const unsigned int aPeriod,
                    const double aLowerBound,
                    const double aUpperBound ):
mTarget( aTarget ),
mTolerance( aTolerance ),
mLowerBound( aLowerBound ),
mUpperBound( aUpperBound ),
mCurrentTrial( aInitialValue ),
mPeriod( aPeriod ),
mIterations( 0 ){
}

/*! \brief Get the next trial value and check for solution.
* \details Checks if the PolicyTarget is solved and returns a pair representing
*          the next trial value and whether the PolicyTarget is solved.
* \return A pair representing the next trial value and whether the PolicyTarget
*         is solved.
*/
pair<double, bool> Bisecter::getNextValue() {
    // Get the status of the current trial.
    ITarget::TrialStatus currTrial = mTarget->getStatus( mTolerance, mPeriod );

    bool solved = false;
    switch( currTrial ){
        case ITarget::SOLVED:
            solved = true;
            break;
        case ITarget::LOW:
            // Check if the constraint is not binding.
            if( mCurrentTrial < util::getSmallNumber() ){
                solved = true;
            }

            // Set the lower bound to the current value.
            mLowerBound = mCurrentTrial;

            // If the upper bound is currently unknown set the trial to twice
            // it's current value or 1 if it is currently zero.
            if( mUpperBound == UNKNOWN ){
                if( mCurrentTrial == 0 ){
                    mCurrentTrial = 1;
                }
                else {
                    mCurrentTrial *= 2;
                }
            }
            else {
                // Calculate the new current trial.
                mCurrentTrial = mLowerBound + ( mUpperBound - mLowerBound ) / 2;
            }

            break;
        case ITarget::HIGH:
            // Set the upper bound to the current value.
            mUpperBound = mCurrentTrial;

            // If the lower bound is unknown set it to zero.
            if( mLowerBound == UNKNOWN ){
                mLowerBound = 0;
            }

            // Calculate the new current trial. This will be ignored if the target is
            // solved.
            mCurrentTrial = mLowerBound + ( mUpperBound - mLowerBound ) / 2;

            break;
        default:
            assert( false );
    }

    // Increment the number of trials.
    ++mIterations;

    if( !solved ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Attempting to solve target with scaler " << mCurrentTrial
            << ". Lower bound is " << mLowerBound << " and upper bound is "
            << mUpperBound << "." << endl;
    }
    assert( util::isValidNumber( mCurrentTrial ) );
    assert( mCurrentTrial >= 0 );
    return make_pair( mCurrentTrial, solved );
}

/*! \brief Get the current number of iterations performed.
* \return The current number of iterations performed.
*/
unsigned int Bisecter::getIterations() const {
    return mIterations;
}
