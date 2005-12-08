#ifndef _PROFIT_SHUTDOWN_DECIDER_H_
#define _PROFIT_SHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file profit_shutdown_decider.h
* \ingroup Objects
* \brief The ProfitShutdownDecider header file.
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
*        profit rate.
* \details This object mimics the Legacy-SGM shutdown decision by calculating a
*          current profit rate as determined by total profits divided by
*          capital. It begins to shutdown the vintage when this profit rate is
*          less than a minimum level, set as 1 percent. Complete shutdown occurs
*          at the zero profit point. This short term shutdown decision includes
*          the payments to capital.
* \author Josh Lurz
*/
class ProfitShutdownDecider: public IShutdownDecider
{
public:
    ProfitShutdownDecider();
    double calcShutdownCoef( const ProductionFunctionInfo& aFuncInfo,
                             const std::string& aRegionName,
                             const std::string& aSectorName,
                             const int aPeriod ) const;
};


#endif // _PROFIT_SHUTDOWN_DECIDER_H_
