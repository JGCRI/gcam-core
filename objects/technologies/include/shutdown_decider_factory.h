/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

#ifndef _SHUTDOWN_DECIDER_FACTORY_H_
#define _SHUTDOWN_DECIDER_FACTORY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file shutdown_decider_factory.h
 * \ingroup Objects
 * \brief ShutdownDeciderFactory header file.
 * \author Josh Lurz
 */

#include <string>
#include <memory>

class IShutdownDecider;

/*! 
 * \ingroup Objects
 * \brief A factory which is used to create the various types of shutdown
 *        deciders.
 * \details The factory encapsulates the creation of various types of shutdown
 *          deciders so that the Technology does not need to be aware of all
 *          types. This simplifies adding new types and also minimizes
 *          recompilation.
 * \author Josh Lurz
 */
class ShutdownDeciderFactory { 
public:
    static bool isOfType( const std::string& aType );
    static std::auto_ptr<IShutdownDecider> create( const std::string& aType );
};

#endif // _SHUTDOWN_DECIDER_FACTORY_H_
