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
 * \file capacity_limit_backup_calculator.cpp
 * \ingroup Objects
 * \brief CapacityLimitBackupCalculator class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/capacity_limit_backup_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Constructor.
 */
CapacityLimitBackupCalculator::CapacityLimitBackupCalculator(): mCapacityLimit( 1 )
{}

// Documentation is inherited.
CapacityLimitBackupCalculator* CapacityLimitBackupCalculator::clone() const {
    return new CapacityLimitBackupCalculator( *this );
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

// Documentation is inherited.
bool CapacityLimitBackupCalculator::XMLParse( const xercesc::DOMNode* node ){
    /*! \pre Assume we are passed a valid node. */
    assert( node );

    const xercesc::DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const xercesc::DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != xercesc::DOMNode::ELEMENT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "capacity-limit" ){
            mCapacityLimit = XMLHelper<double>::getValue( curr );
            // TODO: Correct values above 1 or below 0. Need completeInit.
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown tag " << nodeName << " encountered while processing "
                    << getXMLNameStatic() << endl;
        }
    }

    // TODO: Handle success and failure better.
    return true;
}

// Documentation is inherited.
void CapacityLimitBackupCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mCapacityLimit, "capacity-limit", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// Documentation is inherited.
void CapacityLimitBackupCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mCapacityLimit, "capacity-limit", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

double CapacityLimitBackupCalculator::getMarginalBackupCapacity( const string& aSector,
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

    double marginalBackup = getMarginalBackupCapacityFraction( aSector,
                                                               aElectricSector,
                                                               aResource,
                                                               aRegion,
                                                               aReserveMargin,
                                                               aAverageGridCapacityFactor,
                                                               aPeriod );
    // REVIEW: Is this the right capacity factor?
    return SectorUtils::convertEnergyToCapacity( aAverageGridCapacityFactor,
                                                 marginalBackup );
}

double CapacityLimitBackupCalculator::getAverageBackupCapacity( const string& aSector,
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

    // Determine the intermittent share of output.
    double elecShare = calcIntermittentShare( aSector, aElectricSector, aResource,
                                              aRegion, aReserveMargin, aAverageGridCapacityFactor,
                                              aPeriod );

    // No backup required for zero share.
    if( elecShare < util::getSmallNumber() ){
        return 0;
    }

    // Calculate the integral of the backup function up to the lower of the
    // electricity share and the capacity limit
    double x = min( elecShare, mCapacityLimit );

    // Compute total backup using the integral of the marginal backup function.
    double totalBackup = ( ( A * x * pow( x / mCapacityLimit, B ) ) / ( B + 1 )
                         - ( B * x * pow( x / mCapacityLimit, A ) ) / ( A + 1 ) )
                         / ( A - B );

    // Add to the total backup the quantity above the 1-1 ratio.
    // TODO: Update this to match the smoothing function used for the marginal
    //       calculation.
    if( elecShare > mCapacityLimit ){
        totalBackup += ( elecShare - mCapacityLimit );
    }
    
    // TODO: Is this right?
    double averageBackup = totalBackup / elecShare;

    // Convert to capacity.
    // REVIEW: Is this the right capacity factor?
    return SectorUtils::convertEnergyToCapacity( aAverageGridCapacityFactor,
                                                 averageBackup );
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

    double elecShare = calcIntermittentShare( aSector, aElectricSector,
                                              aResource,
                                              aRegion, aReserveMargin,
                                              aAverageGridCapacityFactor,
                                              aPeriod );

    // No backup required for zero share.
    if( elecShare < util::getSmallNumber() ){
        return 0;
    }

    // Capacity limit must be between 0 and 1 inclusive.
    assert( mCapacityLimit >= 0 && mCapacityLimit <= 1 );
    
    // If the share is below the maximum there will be a backup ratio of less
    // than one. Use a function which gives us a smooth but rapid transition
    // between 0 and 1 as the capacity limit is approached.
    
    // Calculate the amount of backup. If the share is greater than the capacity
    // limit this will be one.
    double backupCapacity;
    if( elecShare < mCapacityLimit  ){
        double ratio = elecShare / mCapacityLimit;
        backupCapacity = ( A * pow( ratio, B ) - B *
                         pow( ratio, A ) ) / ( A - B );
        
        // Backup capacity per unit output is between 0 and 1.
        assert( backupCapacity >= 0 && backupCapacity <= 1 );
    }
    else {
        // Apply a smoothing function to prevent invalid partial derivatives
        // of price with respect to the trial values.
        // TODO: A better smoothing function may be available that would
        //       have a smooth 1st derivative with the above function.
        backupCapacity = 1 + pow( elecShare - mCapacityLimit, 2 );
    }

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
                                                             const double aReserveMargin,
                                                             const double aAverageGridCapacityFactor,
                                                             const int aPeriod ) const
{
    // Get trial amount of overall regional electricity supplied.
    double elecSupply = SectorUtils::getTrialSupply( aRegion, aElectricSector,
                                                     aPeriod );
    
    // Electricity supply must be positive unless it is period 0 where the trial
    // supply market has not been setup. 
    assert( elecSupply >= 0 || aPeriod == 0 );
    
    // Calculate the electric sector capacity.
    double elecSupplyCapacity = util::getLargeNumber();
    if( aAverageGridCapacityFactor > 0 ){
        elecSupplyCapacity = elecSupply / aAverageGridCapacityFactor;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No grid capacity factor found for " << aElectricSector
                << "." << endl;
    }

    double elecShare = 0;
    if( elecSupplyCapacity > 0 ) {
        // Get amount of energy produced by the sector.
        double sectorSupply = SectorUtils::getTrialSupply( aRegion, aSector,
                                                           aPeriod );
        
        // Get resource capacity factor.
        double resourceCapacityFactor =
            SectorUtils::getCapacityFactor( aResource, aRegion, aPeriod );
       
        // Using average capacity factor for resource, translate the energy the
        // sector supplied in electricity or energy terms into EJ. Note that the
        // actual capacity would have conversions for hours per year and EJ to
        // KWH, but these would cancel out in the share calculation below.
        double sectorSupplyCapacity = util::getLargeNumber();
        if( resourceCapacityFactor > 0 ){
            sectorSupplyCapacity = sectorSupply / resourceCapacityFactor;
        }

        // Calculate the shared based on this parameter. Since these are trial
        // values the share may exceed 1.
        elecShare = min( sectorSupplyCapacity / elecSupplyCapacity, 1.0 );

        // Share of electricity must be between 0 and 1 inclusive.
        assert( elecShare >= 0 && elecShare <= 1 );
    }
    return elecShare;
}

