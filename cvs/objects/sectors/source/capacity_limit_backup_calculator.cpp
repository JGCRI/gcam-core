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


/*!
 * \file capacity_limit_backup_calculator.cpp
 * \ingroup Objects
 * \brief CapacityLimitBackupCalculator class source file.
 * \author Josh Lurz, Sonny Kim
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <math.h>

#include "sectors/include/capacity_limit_backup_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"
#include "marketplace/include/marketplace.h"

using namespace std;

/*!
 * \brief Constructor.
 */
CapacityLimitBackupCalculator::CapacityLimitBackupCalculator()
{
    mCapacityLimit = 1.0;
    mFmax = 1.0;
    mC = 1.0;
    mTau = 0.1;
}

// Documentation is inherited.
CapacityLimitBackupCalculator* CapacityLimitBackupCalculator::clone() const {
    CapacityLimitBackupCalculator* clone = new CapacityLimitBackupCalculator();
    clone->mCapacityLimit = mCapacityLimit;
    clone->mFmax = mFmax;
    clone->mC = mC;
    clone->mTau = mTau;
    
    return clone;
}

// Documentation is inherited.
bool CapacityLimitBackupCalculator::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

// Documentation is inherited.
const string& CapacityLimitBackupCalculator::getName() const {
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
const string& CapacityLimitBackupCalculator::getXMLNameStatic() {
    const static string XML_NAME = "capacity-limit-backup-calculator";
    return XML_NAME;
}

const string& CapacityLimitBackupCalculator::getXMLName() const {
    return getXMLNameStatic();
}

// Documentation is inherited.
void CapacityLimitBackupCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mCapacityLimit, "capacity-limit", aOut, aTabs );
    XMLWriteElement( mFmax, "fmax", aOut, aTabs );
    XMLWriteElement( mC, "c", aOut, aTabs );
    XMLWriteElement( mTau, "tau", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// Documentation is inherited.
void CapacityLimitBackupCalculator::initCalc( const IInfo* aTechInfo ) {
    // No information needs to be passed in
}

double CapacityLimitBackupCalculator::getMarginalBackupCapacity( const string& aSector,
                                                                 const string& aElectricSector,
                                                                 const string& aResource,
                                                                 const string& aRegion,
                                                                 const double aTechCapacityFactor,
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

    double marginalBackup = getMarginalBackupCapacityFraction( aSector,
                                                               aElectricSector,
                                                               aResource,
                                                               aRegion,
                                                               aTechCapacityFactor,
                                                               aReserveMargin,
                                                               aAverageGridCapacityFactor,
                                                               aPeriod );
 

    // This is confusing but mathematically correct.  The marginal backupCapacityFraction is in units of 
    // GW per GW.  The denominator (intermittent sector capacity GW) needs to be converted to energy,
    // therefore the quotient is divided by the conversion from capacity to energy, 
    // using the technology capacity factor, which is the same as multiplying by
    // the conversion from energy to capacity as done here. Units returned here are GW/EJ.
   
    return SectorUtils::convertEnergyToCapacity( aTechCapacityFactor, marginalBackup );
}

double CapacityLimitBackupCalculator::getAverageBackupCapacity( const string& aSector,
                                                                const string& aElectricSector,
                                                                const string& aResource,
                                                                const string& aRegion,
                                                                const double aTechCapacityFactor,
                                                                const double aReserveMargin,
                                                                const double aAverageGridCapacityFactor,
                                                                const int aPeriod ) const
{
    // Preconditions
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );
    assert( aReserveMargin >= 0 );
    assert( aAverageGridCapacityFactor > 0 );

    // This function has an odd feature that at small shares the curve reverses sharply
    // ultimately looking as if massive amounts of backup should be required.  Clearly
    // this isn't the intention so we will cap the trial share at the share value near
    // the minimum "average backup" value.
    double renewElecShare = std::max(
                                     std::min(
                                              SectorUtils::getTrialSupply( aRegion, aSector, aPeriod ),
                                              // share should never exceed 1
                                              1.0 ),
                                     // avoid the steeply rising inflection point near the
                                     // bottom of the curve by capping the share
                                     0.01 / mCapacityLimit );


    // Compute total backup using the integral of the marginal backup function
    double xmid = mCapacityLimit;
    double capacityRatio = aAverageGridCapacityFactor / aTechCapacityFactor;
    double capacityShare = renewElecShare * capacityRatio;
    
    double totalBackup = mFmax * mTau / mC * ( log ( 1.0 + exp( mC * ( xmid - capacityShare ) / mTau ) )
                         - mC * ( xmid - capacityShare ) / mTau );
    
    // MAW believes this calculation of average backup is correct, but future generations should
    // feel free to re-visit it. The S-curve backup function is 
    // already the marginal fraction of backup required as a function of the intermittent sector 
    // market share.(E.G., at 10% market share, the next unit of share needs 50% backup.)
    // So integrating from 0 to the share does give the cumulative fraction of backup
    // at that share, and dividing that integral by the share gives the average backup fraction
    // up to that market share.
    // This average is what is applied to the total output of the sector to get the total
    // backup energy used.
    double averageBackup = totalBackup / capacityShare;

    // This is confusing but mathematically correct.  The averagebackupFraction is in units of
    // GW per GW.  The denominator (intermittent sector capacity GW) needs to be converted to energy,
    // therefore the quotient is divided by the conversion from capacity to energy, 
    // using the technology capacity factor, which is the same as multiplying by
    // the conversion from energy to capacity as done here. Units returned here are GW/EJ.
   
    return SectorUtils::convertEnergyToCapacity( aTechCapacityFactor, averageBackup );
}

/*!
 * \brief Compute backup required per resource energy output.
 * \details Compute backup required per resource energy output (since energy
 *          output is what the modeled market is based on). Convert intermittent
 *          resource output back to energy using the resource capacity factor.
 *          This is the cost of operating reserve or backup capacity.
 * \param aSector The name of the sector which requires backup capacity.
 * \param aElectricSector The name of the electricity sector into which the
 *        sector having a backup amount calculated for will feed.
 * \param aResource The name of the resource the sector consumes.
 * \param aRegion Name of the containing region.
 * \param aReserveMargin Reserve margin for the electricity sector.
 * \param aAverageGridCapacityFactor The average electricity grid capacity
 *        factor.
 * \param aPeriod Model period.
 * \return Reserve capacity per intermittent electricity resource output
 *         (GW/EJ).
 */
double CapacityLimitBackupCalculator::getMarginalBackupCapacityFraction( const string& aSector,
                                                                         const string& aElectricSector,
                                                                         const string& aResource,
                                                                         const string& aRegion,
                                                                         const double aTechCapacityFactor,
                                                                         const double aReserveMargin,
                                                                         const double aAverageGridCapacityFactor,
                                                                         const int aPeriod ) const
{
    // Preconditions
    assert( aAverageGridCapacityFactor >= 0 && aAverageGridCapacityFactor <= 1 );
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );

    double renewElecShare = std::min( SectorUtils::getTrialSupply( aRegion, aSector, aPeriod ), 1.0 );
    
    // No backup required for zero share.
    if( renewElecShare < util::getVerySmallNumber() ){
        return 0;
    }

    // Capacity limit must be between 0 and 1 inclusive.
    assert( mCapacityLimit >= 0 && mCapacityLimit <= 1 );
    
    // Calculate the marginal backup requirement at this share of the total.
    double backupCapacity;
    double xmid = mCapacityLimit;

    double capacityRatio = aAverageGridCapacityFactor / aTechCapacityFactor;
    double capacityShare = renewElecShare * capacityRatio;
    
    backupCapacity = mFmax / ( 1.0 + exp( mC * ( xmid - capacityShare ) / mTau ) );

    // This returned value is in terms of fraction of backup capacity per capacity
    // of intermittent sector capacity (e.g., GW/GW)

    return backupCapacity;
}

/*!
 * \brief Calculate the capacity share of the intermittent resource within the
 *        electricity sector.
 * \details Calculates the share of capacity of the intermittent resource within
 *          the electricity sector. This is determined using trial values for
 *          the intermittent sector and electricity sector production. The
 *          production is converted to capacity using constant capacity factors.
 * \param aSector The name of the sector which requires backup capacity.
 * \param aElectricSector The name of the electricity sector into which the
 *        sector having a backup amount calculated for will feed.
 * \param aResource The name of the resource the sector consumes.
 * \param aRegion Name of the containing region.
 * \param aReserveMargin Reserve margin for the electricity sector.
 * \param aAverageGridCapacityFactor The average electricity grid capacity
 *        factor.
 * \param aPeriod Model period.
 * \return Share of the intermittent resource within within the electricity
 *         sector.
 */
double CapacityLimitBackupCalculator::calcIntermittentShare( const string& aSector,
                                                             const string& aElectricSector,
                                                             const string& aResource,
                                                             const string& aRegion,
                                                             const double aTechCapacityFactor,
                                                             const double aReserveMargin,
                                                             const double aAverageGridCapacityFactor,
                                                             const int aPeriod ) const
{

    double capacityShare = std::min( std::max( SectorUtils::getTrialSupply( aRegion, aSector, aPeriod ), 0.0 ), 1.0 ) *
                           aAverageGridCapacityFactor / aTechCapacityFactor;
    return capacityShare;
}
