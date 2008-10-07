/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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

    static double calcPriceRatio( const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const int aBasePeriod,
                                  const int aCurrentPeriod );

    static const std::string createTFEMarketName( const std::string& aSectorName );

    static void setFinalEnergyFlag( const std::string& aRegionName,
                                    const std::string& aSectorName );

    static bool isFinalEnergySector( const std::string& aRegionName,
                                     const std::string& aSectorName );

    static double getVariance( const std::string& aResource,
                               const std::string& aRegion,
                               const int aPeriod );

    static int getDemandNormPeriod( const int aPeriod );

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
