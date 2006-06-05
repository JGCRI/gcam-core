#ifndef _BISECTER_H_
#define _BISECTER_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
* \file bisecter.h
* \ingroup Objects
* \brief The Bisecter class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

class ITarget;

/*! \brief Object which performs bisection on a given target until it
*          reaches a tolerance.
*/
class Bisecter {
public:
    Bisecter( const ITarget* aTarget,
              const double aTolerance,
              const double aInitialValue,
              const unsigned int aPeriod,
              const double aLowerBound,
              const double aUpperBound );

    std::pair<double, bool> getNextValue();

    unsigned int getIterations() const;
    
    //! A representation of an unknown bound.
    const static int UNKNOWN = -1;
private:

    //! The target.
    const ITarget* mTarget;

    //! The tolerance of the target.
    const double mTolerance;

    //! The current trial value.
    double mCurrentTrial;

    //! The current lower bound.
    double mLowerBound;

    //! The current upper bound.
    double mUpperBound;

    //! The current number of trial values returned.
    unsigned int mIterations;

    //! Period in which the bisecter is operating.
    unsigned int mPeriod;
};
#endif // _BISECTER_H_
