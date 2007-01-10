/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*!
 * \file wind_backup_calculator.cpp
 * \ingroup Objects
 * \brief WindBackupCalculator class source file.
 * \author Marshall Wise, Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include "sectors/include/wind_backup_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Constructor.
 */
WindBackupCalculator::WindBackupCalculator()
{}

WindBackupCalculator* WindBackupCalculator::clone() const {
    return new WindBackupCalculator( *this );
}

bool WindBackupCalculator::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

const string& WindBackupCalculator::getName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& WindBackupCalculator::getXMLNameStatic() {
    const static string XML_NAME = "wind-backup-calculator";
    return XML_NAME;
}

bool WindBackupCalculator::XMLParse( const xercesc::DOMNode* node ){
    // This backup calculator does not need to parse any data.
    return true;
}

void WindBackupCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void WindBackupCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

double WindBackupCalculator::getAverageBackupCapacity( const string& aSector,
                                                       const string& aElectricSector,
                                                       const string& aResource,
                                                       const string& aRegion,
                                                       const double aReserveMargin,
                                                       const double aAverageGridCapacityFactor,
                                                       const int aPeriod ) const
{
    // Preconditions
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );
    assert( aReserveMargin >= 0 );
    assert( aAverageGridCapacityFactor > 0 );
    
    // This can be called from initCalc and the value may not be initialized
    // yet. Should be correct in equalibrium.
    double resourceCapacityFactor = SectorUtils::getCapacityFactor( aResource, aRegion, aPeriod );

    if( resourceCapacityFactor < util::getSmallNumber() ){
        return 0;
    }

    double backupFraction = getBackupCapacityFraction( aSector,
                                                       aElectricSector,
                                                       aResource,
                                                       aRegion,
                                                       aReserveMargin,
                                                       aAverageGridCapacityFactor,
                                                       aPeriod );

    return SectorUtils::convertEnergyToCapacity( resourceCapacityFactor,
                                                 backupFraction );
}

double WindBackupCalculator::getMarginalBackupCapacity( const string& aSector,
                                                        const string& aElectricSector,
                                                        const string& aResource,
                                                        const string& aRegion,
                                                        const double aReserveMargin,
                                                        const double aAverageGridCapacityFactor,
                                                        const int aPeriod ) const
{
    // Preconditions
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );
    assert( aReserveMargin >= 0 );
    assert( aAverageGridCapacityFactor > 0 );

    // Get trial amount of overall regional electricity supplied.
    double elecSupply = SectorUtils::getTrialSupply( aRegion, aElectricSector, aPeriod );
    if( elecSupply < util::getSmallNumber() ){
        return 0;
    }

    double reserveTotal = getReserveTotal( aElectricSector,
        aRegion,
        aReserveMargin,
        aAverageGridCapacityFactor,
        aPeriod );

    double reserveTotalSquared  = pow( reserveTotal, 2 );

    double sectorCapacity = getSectorCapacity( aRegion, aSector, aResource, aPeriod );
    if( sectorCapacity < util::getSmallNumber() ){
        return 0;
    }

    double sectorCapacitySquared = pow( sectorCapacity, 2 );

    double variance = SectorUtils::getVariance( aResource, aRegion, aPeriod );
    double resourceCapacityFactor = SectorUtils::getCapacityFactor( aResource, aRegion, aPeriod );

    // Compute terms for Winds operating reserve due to intermittency formula
    // This is the derivative of the total backup capacity equation.
    double backupCapacity = ( variance * sectorCapacity )
                            / ( ( aReserveMargin * elecSupply )
                            * sqrt( 1.0 + variance * sectorCapacitySquared / reserveTotalSquared )
                            * resourceCapacityFactor / aAverageGridCapacityFactor );

    assert( backupCapacity >= 0 && util::isValidNumber( backupCapacity ) );
    return backupCapacity;
}

/*!
 * \brief Compute the backup capacity fraction.
 * \details Compute operating reserve capacity based on formula from NREL WINDS
 *          model. First compute in terms of backup capacity per total
 *          intermittent capacity, then convert to backup capacity as fraction
 *          of wind resource output in energy terms, since that is what the
 *          model and market are based on.
 * \param aSector The name of the sector which requires backup capacity.
 * \param aElectricSector The name of the electricity sector into which the
 *        sector having a backup amount calculated for will feed.
 * \param aResource The name of the resource the sector consumes.
 * \param aRegion Name of the containing region.
 * \param aReserveMargin Reserve margin for the electricity sector.
 * \param aAverageGridCapacityFactor The average electricity grid capacity
 *        factor.
 * \param aPeriod Model period.
 * \return Percent of reserve capacity per unit of intermittent capacity (e.g.,
 *         GW/GW).
 */
double WindBackupCalculator::getBackupCapacityFraction( const string& aSector,
                                                       const string& aElectricSector,
                                                       const string& aResource,
                                                       const string& aRegion,
                                                       const double aReserveMargin,
                                                       const double aAverageGridCapacityFactor,
                                                       const int aPeriod ) const
{
    // Preconditions
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );
    assert( aReserveMargin >= 0 );
    assert( aAverageGridCapacityFactor > 0 );

    double reserveTotal = getReserveTotal( aElectricSector,
        aRegion,
        aReserveMargin,
        aAverageGridCapacityFactor,
        aPeriod );
    
    if( reserveTotal < util::getSmallNumber() ){
        return 0;
    }

    double reserveTotalSquared  = pow( reserveTotal, 2 );

    double sectorSupplyCapacity = getSectorCapacity( aRegion, aSector, aResource, aPeriod );
    
    double sectorSupplySquared = pow( sectorSupplyCapacity, 2 );

    double variance = SectorUtils::getVariance( aResource, aRegion, aPeriod );

    // Compute terms for Winds operating reserve due to intermittency formula
    double backupCapacityFraction = reserveTotal * 
        ( pow( ( 1.0 + variance * sectorSupplySquared / reserveTotalSquared ), 0.5 ) - 1.0 );

    assert( backupCapacityFraction >= 0 && util::isValidNumber( backupCapacityFraction ) );
    
    // Derivative per unit of intermittent resource supply.
    if ( sectorSupplyCapacity > util::getSmallNumber() ) {
        backupCapacityFraction /= sectorSupplyCapacity;
    }

    // Backup capacity fraction is positive.
    assert( util::isValidNumber( backupCapacityFraction ) && backupCapacityFraction >= 0 );
    return backupCapacityFraction;
}

double WindBackupCalculator::getReserveTotal( const string& aElectricSector,
                                              const string& aRegion,
                                              const double aReserveMargin,
                                              const double aAverageGridCapacityFactor,
                                              const int aPeriod ) const
{
    // Get trial amount of overall regional electricity supplied.
    double elecSupply = SectorUtils::getTrialSupply( aRegion, aElectricSector, aPeriod );

    // Electricity supply must be positive unless it is period 0 where the trial
    // supply market has not been setup. 
    assert( elecSupply >= 0 || aPeriod == 0 );

    // If there is no electricity supply there is no backup requirement.
    if( elecSupply < util::getSmallNumber() ) {
        return 0;
    }

    // Compute reserve capacity as the product of electricity reserve margin
    // and the total regional electric capacity (which is converted from
    // electric supply in Energy units to capacity units using its average
    // capacity factor).
    return aReserveMargin
           * SectorUtils::convertEnergyToCapacity( aAverageGridCapacityFactor, elecSupply );
}

double WindBackupCalculator::getSectorCapacity( const string& aRegion,
                                                const string& aSector,
                                                const string& aResource,
                                                const int aPeriod ) const
{
    // Get amount of energy produced by the sector.
    double sectorSupply = SectorUtils::getTrialSupply( aRegion, aSector, aPeriod );
    if( sectorSupply < util::getSmallNumber() ){
        return 0;
    }

    double resourceCapacityFactor = SectorUtils::getCapacityFactor( aResource, aRegion, aPeriod );
    if( resourceCapacityFactor < util::getSmallNumber() ){
        return 0;
    }

    // Using average capacity factor for resource, translate sector supply
    // in electricity or energy terms in EJ to equivalent capacity terms in
    // GW or gigawatts. Note that model's energy units need to be converted
    // to capacity units for use in the NREL WINDS operating reserve
    // computation.
    return SectorUtils::convertEnergyToCapacity( resourceCapacityFactor, sectorSupply );
}
