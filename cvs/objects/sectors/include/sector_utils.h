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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
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
 * \author Josh Lurz, Sonny Kim
 */

#include <string>
#include <vector>
#include <utility>
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class IInfo;

/*! 
 * \ingroup Objects
 * \brief This class contains a set of static helper methods which the various
 *        sectors and subsectors use.
 * \author Josh Lurz
 */
class SectorUtils {
public:
    
    static void addGDPDependency( const gcamstr& aRegionName, const gcamstr& aSectorName );
    static double getGDP( const gcamstr& aRegionName, const int aPeriod );
    static double getGDPPerCap( const gcamstr& aRegionName, const int aPeriod );
    static double getGDPPerCapScaled( const gcamstr& aRegionName, const int aPeriod );
    static double getGDPPPP( const gcamstr& aRegionName, const int aPeriod );
    
    static bool createTrialSupplyMarket( const gcamstr& aRegionName,
                                         const gcamstr& aSectorName,
                                         const IInfo* aTechnologyInfo,
                                         const gcamstr& aMarketName = gcamstr() );

    static bool createTrialSupplyMarket( const gcamstr& aRegionName,
                                         const gcamstr& aSectorName,
                                         const gcamstr& aUnitStr,
                                         const gcamstr& aMarketName = gcamstr() );    

    static void addToTrialDemand( const gcamstr& aRegionName,
                                  const gcamstr& aSectorName,
                                  const Value& aSupply,
                                  const int aPeriod );

    static double getTrialSupply( const gcamstr& aRegionName,
                                  const gcamstr& aSectorName,
                                  const int aPeriod );

    static double calcFixedOutputScaleFactor( const double aMarketDemand,
                                              const double aFixedOutput );

    static double normalizeShares( std::vector<double>& aShares );
    static std::pair<double, double> normalizeLogShares( std::vector<double> & alogShares );

    static double calcPriceRatio( const gcamstr& aRegionName,
                                  const gcamstr& aSectorName,
                                  const int aBasePeriod,
                                  const int aCurrentPeriod );

    static double getDemandPriceThreshold();

    static double adjustDemandForNegativePrice( const double aDemandScalar,
                                                const double aPrice );

    static const std::string createTFEMarketName( const gcamstr& aSectorName );

    static void setFinalEnergyFlag( const gcamstr& aRegionName,
                                    const gcamstr& aSectorName );

    static bool isFinalEnergySector( const gcamstr& aRegionName,
                                     const gcamstr& aSectorName );

    static int getDemandNormPeriod( const int aPeriod );

    static double convertEnergyToCapacity( const double aCapacityFactor,
                                           const double aEnergy );

    static double convertCapacityToEnergy( const double aCapacityFactor,
                                           const double aCapacity );

    static const gcamstr getTrialMarketName( const gcamstr& aSectorName );

    static void setSupplyBehaviorBounds( const gcamstr& aGoodName, const gcamstr& aRegionName,
                                         const double aLowerPriceBound, const double aUpperPriceBound,
                                         const int aPeriod );

     /*! \brief Fills missing period elements in a Value vector with values linearly 
     *        interpolated from initialized or read-in values.
     * \detail This method is intended for enabling variable time-step capability
     *         and filling in values that have not been read-in or initialized
     *         in a PeriodVector.
     * \param aValueVector a period vector of Values.
     */
    static void fillMissingPeriodVectorInterpolated( objects::PeriodVector<Value>& aPeriodVector );

     /*! \brief Fills missing period elements in a Value vector with values 
     *         available from the next initialized or read-in values.
     * \detail This method is intended for enabling variable time-step capability
     *         and filling in values that have not been read-in or initialized
     *         in a PeriodVector.
     * \param aValueVector a period vector of Values.
     */
    static void fillMissingPeriodVectorNextAvailable( objects::PeriodVector<Value>& aPeriodVector );

protected:

    static std::map<gcamstr, gcamstr> sTrialMarketNames;
};

#endif // _SECTOR_UTILS_H_
