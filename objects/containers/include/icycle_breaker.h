#ifndef _ICYCLE_BREAKER_H_
#define _ICYCLE_BREAKER_H_
#if defined(_MSC_VER_)
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
* \file icycle_breaker.h
* \ingroup objects
* \brief The ICycleBreaker interface header file.
* \author Jim Naslund
*/

class DependencyFinder;

/*!
* \ingroup Objects
* \brief This file defines the interface for a cycle breaker.
* \author Jim Naslund
*/
class ICycleBreaker
{
public:
    inline virtual ~ICycleBreaker();
    /*!
     * \brief Break a single cycle between two sectors by adding two markets for
     *         it, and removes the dependencies or edges from the matrix.
     * \param aFirstSector First node in the cycle.
     * \param aSecondSector Second node in the cycle.
     * \note size_t is a unsigned int used by the standard library to represent
     *       positions with a container.
     */
    virtual void breakCycle( DependencyFinder &aDependencyFinder,
                             const size_t aFirstSector,
                             const size_t aSecondSector ) = 0;
};

//! Empty inline destructor needed so that ICycleBreaker objects can be deleted through
//! base class pointers.
ICycleBreaker::~ICycleBreaker(){
}

#endif // _ICYCLE_BREAKER_H_
