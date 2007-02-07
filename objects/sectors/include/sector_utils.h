/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

#ifndef _SECTOR_UTILS_H_
#define _SECTOR_UTILS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file sector_utils.h
 * \ingroup Objects
 * \brief The SectorUtils class header file.
 * \author Josh Lurz
 */

#include <string>
#include <vector>
#include "util/base/include/hash_map.h"

/*! 
 * \ingroup Objects
 * \brief This class contains a set of static helper methods which the various
 *        sectors and subsectors use.
 * \author Josh Lurz
 */
class SectorUtils {
public:
    static bool createTrialSupplyMarket( const std::string& aRegion,
                                         const std::string& aSector );

    static void setTrialSupply( const std::string& aRegion,
                                const std::string& aSector,
                                const double aSupply,
                                const int aPeriod );

    static double getTrialSupply( const std::string& aRegion,
                                  const std::string& aSector,
                                  const int aPeriod );

    static void askToCreateTrialSupply( const std::string& aRegion,
                                        const std::string& aSector );

    static double calcFixedOutputScaleFactor( const double aMarketDemand,
                                              const double aFixedOutput );

    static double normalizeShares( std::vector<double>& aShares );

    static double getVariance( const std::string& aResource,
                               const std::string& aRegion,
                               const int aPeriod );

    static double getCapacityFactor( const std::string& aResource,
                                     const std::string& aRegion,
                                     const int aPeriod );
    
    static double convertEnergyToCapacity( const double aCapacityFactor,
                                           const double aEnergy );

    static double convertCapacityToEnergy( const double aCapacityFactor,
                                           const double aCapacity );

protected:
    static const std::string getTrialMarketName( const std::string& aSector );

    static HashMap<std::string, std::string> sTrialMarketNames;
};

#endif // _SECTOR_UTILS_H_
