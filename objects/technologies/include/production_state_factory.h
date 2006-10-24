#ifndef _PRODUCTION_STATE_FACTORY_H_
#define _PRODUCTION_STATE_FACTORY_H_
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
 * \file production_state_factory.h
 * \ingroup Objects
 * \brief ProductionStateFactory header file.
 * \author Josh Lurz
 */

#include <string>
#include <memory>

class IProductionState;

/*! 
 * \ingroup Objects
 * \brief A factory which is used to create the various types of production
 *        states.
 * \details The factory encapsulates the creation of various types of production
 *          states so that the Technology does not need to be aware of all
 *          types. This simplifies adding new types and also minimizes
 *          recompilation.
 * \author Josh Lurz
 */
class ProductionStateFactory { 
public:
    static std::auto_ptr<IProductionState> create( const int aInvestYear,
                                                   const int aLifetimeYears,
                                                   const double aFixedOutput,
                                                   const double aInitialOutput,
                                                   const int aPeriod );
};

#endif // _PRODUCTION_STATE_FACTORY_H_
