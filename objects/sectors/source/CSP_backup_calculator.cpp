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
 * \file CSP_backup_calculator.cpp
 * \ingroup Objects
 * \brief CSPBackupCalculator class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/CSP_backup_calculator.h"
#include "util/base/include/util.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Constructor.
 */
CSPBackupCalculator::CSPBackupCalculator()
{
    // Maximum Backup Fraction is the maximum backup fraction required.
    mMaxBackupFraction = 0.403;
    // Fraction of electric sector that is intermediate and peak
    mIPFraction = 0.25;
    // exponent parameter a
    A = 4.0;
}

// Documentation is inherited.
CSPBackupCalculator* CSPBackupCalculator::clone() const {
    return new CSPBackupCalculator( *this );
}

// Documentation is inherited.
bool CSPBackupCalculator::isSameType( const std::string& aType ) const {
    return aType == getXMLNameStatic();
}

// Documentation is inherited.
const string& CSPBackupCalculator::getName() const {
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
const string& CSPBackupCalculator::getXMLNameStatic() {
    const static string XML_NAME = "CSP-backup-calculator";
    return XML_NAME;
}

// Documentation is inherited.
bool CSPBackupCalculator::XMLParse( const xercesc::DOMNode* node ){
    // This backup calculator does not need to parse any data.
    return true;
}

// Documentation is inherited.
void CSPBackupCalculator::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

// Documentation is inherited.
void CSPBackupCalculator::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

double CSPBackupCalculator::getMarginalBackupCapacity( const string& aSector,
                                                                 const string& aElectricSector,
                                                                 const string& aResource,
                                                                 const string& aRegion,
                                                                 const double aReserveMargin,
                                                                 const double aAverageGridCapacityFactor,
                                                                 const int aPeriod ) const
{
    
    //! Marginal backup calculation is used for marginal cost of backup capacity
    //! and used in the cost of backup for share equations. 
    //! This cost is not needed for CSP since the backup capacity comes as part
    //! of the CSP technology and is contained in the CSP tech cost already. A zero is returned.

    return 0.0;
}

double CSPBackupCalculator::getAverageBackupCapacity( const string& aSector,
                                                                const string& aElectricSector,
                                                                const string& aResource,
                                                                const string& aRegion,
                                                                const double aReserveMargin,
                                                                const double aAverageGridCapacityFactor,
                                                                const int aPeriod ) const
{
    //! Average backup is needed for CSP since it is used to compute the amount
    //! of energy used by the backup technology.
    
    // Preconditions
    assert( !aSector.empty() );
    assert( !aElectricSector.empty() );
    assert( !aResource.empty() );
    assert( !aRegion.empty() );
    assert( aReserveMargin >= 0 );
    assert( aAverageGridCapacityFactor > 0 );

    // Determine the intermittent share of output.
    // Note that this method in CSP differs as it is based on energy share not capacity
    double elecShare = calcIntermittentShare( aSector, aElectricSector, aResource,
                                              aRegion, aReserveMargin, aAverageGridCapacityFactor,
                                              aPeriod );

    // No backup required for zero share.
    if( elecShare < util::getSmallNumber() ){
        return 0;
    }

    // Calculate average backup energy not inclusing the no-sun days
    // based on the approximation by Yabei Zhang

    double averageBackup = mMaxBackupFraction * pow( elecShare, A);
    
    // Convert to capacity. This needs to be done since Intermittent Subsector
    // assumes it is getting a capacity number and converts back to energy.
    // This will only work in energy units if we assign a capacity factor of 1
    // for the backup capacity factor and read in a one in the intermittent supply
    // sector object for backup capacity factor
    double backupCapacityFactor = 1.0;
    return SectorUtils::convertEnergyToCapacity( backupCapacityFactor,
                                                 averageBackup );
}


/*!
 * \brief Calculate the share of the intermittent resource within the
 *        electricity sector.
 * \details Calculates the share of energy of the intermittent resource within
 *          the electricity sector. This is determined using trial values for
 *          the intermittent sector and electricity sector production.
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
double CSPBackupCalculator::calcIntermittentShare( const string& aSector,
                                                             const string& aElectricSector,
                                                             const string& aResource,
                                                             const string& aRegion,
                                                             const double aReserveMargin,
                                                             const double aAverageGridCapacityFactor,
                                                             const int aPeriod ) const
{
    //! Note that the CSP backup is based on share of energy, not capacity, 
    //! so capacity factor and conversions to capacity not used.
    
    // Get trial amount of overall regional electricity supplied.
    double elecSupply = SectorUtils::getTrialSupply( aRegion, aElectricSector,
                                                     aPeriod );
    
    // Electricity supply must be positive unless it is period 0 where the trial
    // supply market has not been setup. 
    assert( elecSupply >= 0 || aPeriod == 0 );
    
    double elecShare = 0;
    if( elecSupply > 0 ) {
        // Get amount of energy produced by the sector.
        double sectorSupply = SectorUtils::getTrialSupply( aRegion, aSector,
                                                           aPeriod );

        // Calculate the share based on ratio of CSP supply over total elec supply times
        // fraction of total that is peak and intermediate. Since these are trial
        // values the share may exceed 1.
        elecShare = min( sectorSupply / (elecSupply * mIPFraction), 1.0 );

        // Share of electricity must be between 0 and 1 inclusive.
        assert( elecShare >= 0 && elecShare <= 1 );
    }
    return elecShare;
}

