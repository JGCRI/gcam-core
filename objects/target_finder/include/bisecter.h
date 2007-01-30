#ifndef _BISECTER_H_
#define _BISECTER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*!
* \file bisecter.h
* \ingroup Objects
* \brief The Bisecter class header file.
* \author Josh Lurz
*/

class ITarget;

/*! \brief Object which performs bisection on a given target until it
*          reaches a tolerance.
*/
class Bisecter {
public:
    Bisecter( const ITarget* aTarget,
              const double aTolerance,
              const double aMinimum,
              const double aMaximum,
              const double aInitialValue,
              const double aMultiple,
              const unsigned int aYear );

    std::pair<double, bool> getNextValue();

    unsigned int getIterations() const;

    static double undefined();
private:
    /*
    * \brief An enumeration of all states the bisection algorithm may be in at
    *        the end of an iteration.
    */
    enum SolvedState {
        //! Solution has been found.
        eSolved,

        //! Solution has not been found.
        eUnsolved,

        //! The difference between the upper and lower bounds is less than the
        //! tolerance.
        eEmptyBracket,

        //! The difference between the upper bound and the current trial is less
        //! than the tolerance.
        eUpperBoundReached,

        //! The difference between the lower bound and the current trial is less
        //! than the tolerance.
        eLowerBoundReached
    };

    //! The target.
    const ITarget* mTarget;

    //! The tolerance of the target.
    const double mTolerance;

    //! The minimum of the search.
    double mMinimum;

    //! The maximum of the search.
    double mMaximum;

    //! The initial trial value.
    double mInitialGuess;
    
    //! The adjustment to make during each iteration.
    double mMultiple;

    //! The current lower bound.
    double mLowerBound;

    //! The current upper bound.
    double mUpperBound;

    //! The current trial value.
    double mCurrentTrial;
    
    //! The current number of trial values returned.
    unsigned int mIterations;

    //! Year in which the bisecter is operating.
    unsigned int mYear;

    void printState( const SolvedState aState ) const;
};

#endif // _BISECTER_H_
