#ifndef _CARBON_FLOW_FACTORY_H_
#define _CARBON_FLOW_FACTORY_H_
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
 * \file carbon_flow_factory.h
 * \ingroup Objects
 * \brief CarbonFlowFactory class header file.
 * \author Jim Naslund and Ming Chang
 */

#include <string>

class ACarbonFlow;

/*
 * \brief A class which creates carbon flows.
 * \details This class is used to create carbon flows from xml.
 */
class CarbonFlowFactory{
public:
    static std::auto_ptr<ACarbonFlow> create( const std::string& aType );
    static bool isOfType( const std::string& aType );
};

#endif // _CARBON_FLOW_FACTORY_H_
