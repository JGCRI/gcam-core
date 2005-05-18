#ifndef _VARIABLE_COST_SHUTDOWN_DECIDER_H_
#define _VARIABLE_COST_SHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Labratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responisbility for the 
	use of this software.
*/

/*! 
* \file variable_cost_shutdown_decider.h
* \ingroup Objects
* \brief The VariableCostShutdownDecider header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include "technologies/include/ishutdown_decider.h"

#include <string>
struct ProductionFunctionInfo;
/*! 
* \ingroup Objects
* \brief This object makes the shutdown decision for a vintage based on its
*        variable cost exceeding its price received.
* \details This object makes the shutdown decision for a vintage by determining
*          the point at which its variable costs, defined as the costs per unit
*          of all inputs other than capital, are equal to the price received for
*          the product. This is the classic short-term shutdown decision. This
*          calculation is done as a ratio with the denominator as the capital
*          for the vintage, and when the vintages reaches a minimum level the
*          vintage is smoothly shutdown.
* \author Josh Lurz
*/
class VariableCostShutdownDecider: public IShutdownDecider
{
public:
    VariableCostShutdownDecider();
    VariableCostShutdownDecider* clone() const;
    double calcShutdownCoef( const ProductionFunctionInfo& aFuncInfo,
                             const std::string& aRegionName,
                             const std::string& aSectorName,
                             const int aPeriod ) const;
};


#endif // _VARIABLE_COST_SHUTDOWN_DECIDER_H_
