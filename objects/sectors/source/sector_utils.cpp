/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file sector_utils.cpp
 * \ingroup Objects
 * \brief The SectorUtils class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <algorithm>
#include <numeric>
#include <cfloat>

#include "sectors/include/sector_utils.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"
#include "util/base/include/util.h"

using namespace std;

extern Scenario* scenario; // for marketplace and modeltime.

HashMap<std::string, std::string> SectorUtils::sTrialMarketNames;

typedef HashMap<string, string>::const_iterator NameIterator;

/*!
 * \brief Create a trial market for the supply of a given good.
 * \details Sets up a trial value market for the given good in the given region.
 *          The trial market will be solved for all periods after the base
 *          period.
 * \author Josh Lurz
 * \param aRegion Region in which to create the market.
 * \param aSector Name of the sector for which to create the market.
 * \return Whether the market was created and did not already exist.
 */
bool SectorUtils::createTrialSupplyMarket( const string& aRegion,
                                           const string& aSector )
{
    // Add the trial market name to the cached list of trial names.
    const string trialName = getTrialMarketName( aSector );
    sTrialMarketNames.insert( make_pair( aSector, trialName ) );

    // Create the additional market.
    Marketplace* marketplace = scenario->getMarketplace();
    bool isNewMarket = marketplace->createMarket( aRegion,
                                                  aRegion,
                                                  trialName,
                                                  IMarketType::TRIAL_VALUE );
    // Set price and output units for period 0 market info
    // Get marketInfo for sector to set marketInfo for trial sector.
    // Sector and it's trial market must have same output units
    const IInfo* marketInfoSupplySector = marketplace->getMarketInfo( aSector, aRegion, 0, true );
    const string outputUnitStr = marketInfoSupplySector->getString( "output-unit", true );
    // no operating trial market for base period, although vector always contains base period
    IInfo* marketInfoTrialSupplySector = marketplace->getMarketInfo( trialName, aRegion, 0, true );
    marketInfoTrialSupplySector->setString( "price-unit", outputUnitStr );
    marketInfoTrialSupplySector->setString( "output-unit", outputUnitStr );

    // Set the market to solve.
    for( int per = 1; per < scenario->getModeltime()->getmaxper(); ++per ){
        marketplace->setMarketToSolve( trialName,
                                       aRegion, per );
    }
    return isNewMarket;
}

/*!
 * \brief Set the trial value of supply for a given sector.
 * \details Sets the known value of the trial market such that at equilibrium,
 *          the set value is equal to the trial value.
 * \author Josh Lurz
 * \param aRegion Region of the market.
 * \param aSector Name of the sector.
 * \param aSupply Known value of supply for the iteration.
 * \param aPeriod Model period.
 */
void SectorUtils::setTrialSupply( const string& aRegion,
                                  const string& aSector,
                                  const double aSupply,
                                  const int aPeriod )
{
    // Market is not created until period 1.
    if( aPeriod == 0 ){
        return;
    }

    // Locate the trial market name.
    NameIterator trialName = sTrialMarketNames.find( aSector );
    
    // Check if the market existed.
    assert( trialName != sTrialMarketNames.end() );

    // Demand is the known value of the trial market. Trial markets do not solve
    // well when the value of one side is zero. When the known value is set into
    // the trial market 1 is added, it is removed by consumers of this value.
    scenario->getMarketplace()->addToDemand( trialName->second,
                                             aRegion, aSupply + 1, aPeriod,
                                             true );
}

/*!
 * \brief Get the trial value of supply for a given sector.
 * \details Gets the trial value of the trial market such that at equilibrium,
 *          the known value is equal to the trial value.
 * \author Josh Lurz
 * \param aRegion Region of the market.
 * \param aSector Name of the sector.
 * \param aPeriod Model period.
 * \return Trial value of supply, -1 if the market does not exist.
 */
double SectorUtils::getTrialSupply( const string& aRegion,
                                    const string& aSector,
                                    const int aPeriod )
{
    // Market is not created yet in period 0.
    if( aPeriod == 0 ){
        return -1;
    }

    // Locate the trial market name.
    NameIterator trialName = sTrialMarketNames.find( aSector );
    
    // Check if the market existed.
    if( trialName == sTrialMarketNames.end() ){
        return -1;
    }

    // Get the trial value of supply from the marketplace, which is stored as
    // the price.
    double trialPrice = scenario->getMarketplace()->getPrice( trialName->second,
                                                              aRegion,
                                                              aPeriod );
    
    // The market should have existed if the trial market name search succeeded.
    assert( trialPrice != Marketplace::NO_MARKET_PRICE );
    
    // Trial markets do not solve well when the value of one side is zero. When
    // the known value is set into the trial market 1 is added, so it must be
    // removed here. Trial price may be less than 1 in disequilibrium.
    return max( trialPrice - 1, 0.0 );
}

/*!
 * \brief Set a request for a sector to create a trial supply market for
 *          itself.
 * \details Sets a flag in the sector's market info asking it to create a trial
 *          supply market for itself. This must be done before the initCalc
 *          method is called for the sector in the period where the trial supply
 *          is required.
 * \param aRegion Region of the sector.
 * \param aSector Sector which should create a trial supply market.
 * \todo Find a more elegant way to do this.
 */
void SectorUtils::askToCreateTrialSupply( const string& aRegion,
                                          const string& aSector )
{
    // Get the market info for the sector.
    Marketplace* marketplace = scenario->getMarketplace();

    // Always set the flag in period 0.
    IInfo* sectorInfo = marketplace->getMarketInfo( aSector, aRegion, 0, true );

    // If the market does not exist the sector info will not exist. The
    // marketplace will print a warning.
    if( sectorInfo ){
        sectorInfo->setBoolean( "create-trial-supply", true );
    }
}

/*!
 * \brief Calculate the scale factor used to reduce fixed output.
 * \details Calculates the scaling factor applied to fixed output in the sector
 *          when fixed output exceeds the market demand. The scale factor is one
 *          when this condition does not occur.
 * \param aMarketDemand Market demand for the good produced by the sector.
 * \param aFixedOutput Fixed output of the sector.
 * \return Fixed output scaling factor.
 */
double SectorUtils::calcFixedOutputScaleFactor( const double aMarketDemand,
                                                const double aFixedOutput )
{
    /*! \pre Market demand and fixed output are positive. */
    assert( aMarketDemand >= 0 && aFixedOutput >= 0 );

	double scaleFactor = 1;
	if( aFixedOutput > aMarketDemand ){
		scaleFactor = aMarketDemand / aFixedOutput;
	}
    /*! \post Scale factor must be less than or equal to 1. */
    assert( scaleFactor <= 1 && scaleFactor >= 0 );
    return scaleFactor;
}

/*!
 * \brief Normalize a set of shares.
 * \details Attempts to normalize a vector of shares such that each share is
 *          equal to the initial share divided by the sum of the shares. If the
 *          sum of the shares is less than a very small number, the function
 *          will set the shares to 1/n, where n is the number of non-zero
 *          shares. If all shares are zero, there are no children which can
 *          produce output and the function returns 0 without adjusting the
 *          shares.
 * \param aShares A set of shares to normalize.
 * \return The normalized sum of the shares.
 */
double SectorUtils::normalizeShares( vector<double>& aShares ){
    // Calculate the total of the shares so they can be normalized.
    const double sum = accumulate( aShares.begin(), aShares.end(), 0.0 );

    typedef vector<double>::iterator VecIterator;
    
    // Check for an unnormalizable vector. Unnormalized shares may be very
    // small.
    if( sum > DBL_MIN ){
        // Adjust all the shares.
        for( VecIterator i = aShares.begin(); i != aShares.end(); ++i ){
            *i /= sum;
        }
        // Check that the normalization was performed correctly.
        assert( util::isEqual( accumulate( aShares.begin(),
                                           aShares.end(), 0.0 ), 1.0 ) );
        
        // If the shares could be normalized assume they were correct.
        return 1;
    }

    // Shares cannot be normalized.
    return 0;
}

/*!
 * \brief Get the name of the trial supply market for a given sector.
 * \param aSector Sector name.
 * \return The name of the trial supply market for the sector.
 */
const string SectorUtils::getTrialMarketName( const string& aSector ){
    return aSector + "-trial-supply";
}

/*!
 * \brief Get the variance of the resource.
 * \details Queries the market-info of the good for the resource variance.
 *          Returns zero if the market does not exist or does not have a
 *          resource variance set.
 * \param aResource Resource for which to get the variance.
 * \param aRegion Region for which to get the variance.
 * \param aPeriod Model period.
 * \return The resource variance.
 */
double SectorUtils::getVariance( const string& aResource,
                                 const string& aRegion,
                                 const int aPeriod )
{
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* resourceInfo =
        marketplace->getMarketInfo( aResource, aRegion, aPeriod, true );

    double variance =
        resourceInfo ? resourceInfo->getDouble( "resourceVariance", true ) : 0;

    assert( variance >= 0 );
    return variance;
}

/*!
 * \brief Get the capacity factor of the resource.
 * \details Queries the market-info of the good for the capacity factor.
 *          Returns zero if the market does not exist or does not have a
 *          capacity factor set.
 * \param aResource Resource for which to get the capacity factor.
 * \param aRegion Region for which to get the capacity factor.
 * \param aPeriod Model period.
 * \return The resource capacity factor.
 */
double SectorUtils::getCapacityFactor( const string& aResource,
                                       const string& aRegion,
                                       const int aPeriod )
{
    // Get resource capacity factor from market info for the sector.
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* resourceInfo =
        marketplace->getMarketInfo( aResource, aRegion, aPeriod, true );

    double resourceCapacityFactor =
        resourceInfo ?
        resourceInfo->getDouble( "resourceCapacityFactor", true ) : 0;
    
    // Resource capacity factor must be between 0 and 1 inclusive.
    assert( resourceCapacityFactor >= 0 && resourceCapacityFactor <= 1 );
    return resourceCapacityFactor;
}

/*!
 * \brief Converts energy to capacity using a given capacity factor.
 * \param aCapacityFactor Capacity factor to use in the conversion.
 * \param aEnergy The energy quantity to convert.
 * \return Capacity equivalent of the energy.
 */
double SectorUtils::convertEnergyToCapacity( const double aCapacityFactor,
                                             const double aEnergy )
{
    /*! \pre Capacity factor must be positive and non-zero. */
    assert( aCapacityFactor > util::getSmallNumber() );

    // Conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const double EJ_PER_GWH = 3.6E-6;
    
    // Number of hours in a year.
    const unsigned int HOURS_PER_YEAR = 8760;

    return aEnergy / ( EJ_PER_GWH * HOURS_PER_YEAR * aCapacityFactor );
}

/*!
 * \brief Convert the capacity of a sector into the energy produced using the
 *        capacity factor.
 * \param aCapacityFactor Capacity factor to use in the conversion.
 * \param aCapacity The capacity quantity to convert.
 * \return The energy equivalent of the capacity.
 */
double SectorUtils::convertCapacityToEnergy( const double aCapacityFactor,
                                             const double aCapacity )
{
    /*! \pre Capacity factor must be positive and non-zero. */
    assert( aCapacityFactor > util::getSmallNumber() );

    // Conversion: 1 gigaWattHour of electricity = 3.6E-6 ExaJoules
    const double EJ_PER_GWH = 3.6E-6;
    // Number of hours in a year.
    const unsigned int HOURS_PER_YEAR = 8760;
    
    return aCapacity / ( aCapacityFactor * EJ_PER_GWH * HOURS_PER_YEAR );
}
