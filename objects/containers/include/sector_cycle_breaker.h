#ifndef _SECTOR_CYCLE_BREAKER_H_
#define _SECTOR_CYCLE_BREAKER_H_
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
 * \file sector_cycle_breaker.h
 * \ingroup objects
 * \brief The SectorCycleBreaker header file.
 * \author Jim Naslund
 */

#include <boost/noncopyable.hpp>
#include "containers/include/icycle_breaker.h"

class Marketplace;

/*!
 * \ingroup Objects
 * \brief This class implements a sector cycle breaker.
 * \author Jim Naslund
 */
class SectorCycleBreaker: public ICycleBreaker,
                          protected boost::noncopyable
{
public:
    SectorCycleBreaker( Marketplace* aMarketPlace, const std::string& aRegionName );
    virtual ~SectorCycleBreaker();
    void breakCycle( DependencyFinder &aDependencyFinder,
                     const size_t aFirstSector, const size_t aSecondSector );
private:
    //! The marketplace for this region.
    Marketplace* mMarketplace;

    //! Name of the region containing the dependency finder.
    const std::string mRegionName;
};

#endif // _SECTOR_CYCLE_BREAKER_H_
