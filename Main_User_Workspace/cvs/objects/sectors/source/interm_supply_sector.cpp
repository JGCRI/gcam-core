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

/*!
* \file interm_supply_sector.cpp
* \ingroup Objects
* \brief IntermittentSupplySector class source file.
* \author Marshall Wise
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "sectors/include/interm_supply_sector.h"
#include "sectors/include/sector_utils.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

IntermittentSupplySector::IntermittentSupplySector( const string& aRegionName ):
SupplySector( aRegionName ),
mElectricSectorName( "electricity" ),
mElectricityReserveMargin( 0 ),
mAverageGridCapacityFactor( 1 ),
mBackupCapacityFactor( 0 ),
mBackupCost( 0 )
{
    // Set that this sector requires a trial supply market.
    mHasTrialSupply = true;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. This function may be virtual to be overridden by derived class
* pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& IntermittentSupplySector::getXMLName() const {
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
const std::string& IntermittentSupplySector::getXMLNameStatic() {
    const static string XML_NAME = "intermittent-supplysector";
    return XML_NAME;
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes
* derived from this class. Since Sector is the generic base class, there are no
* values here.
*
* \author Marshall Wise
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool IntermittentSupplySector::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
    // call the supply sector XML parse (one parent level up) to fill supply sector attributes
    if( SupplySector::XMLDerivedClassParse( aNodeName, aCurr ) ){
    }
    else if( aNodeName == "electric-sector-name" ){
        mElectricSectorName = XMLHelper<string>::getValue( aCurr );
    }
    else if( aNodeName == "electricity-reserve-margin" ){
        mElectricityReserveMargin = XMLHelper<double>::getValue( aCurr );
    }
    else if( aNodeName == "average-grid-capacity-factor" ){
        mAverageGridCapacityFactor = XMLHelper<double>::getValue( aCurr );
    }
    else if( aNodeName == "backup-capacity-factor" ){
        mBackupCapacityFactor = XMLHelper<double>::getValue( aCurr );
    }
    else if( aNodeName == "backup-cost" ){
        mBackupCost = XMLHelper<double>::getValue( aCurr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
* \author Marshall Wise
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void IntermittentSupplySector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toInputXMLDerived( out, tabs );

    XMLWriteElement( mElectricSectorName, "electric-sector-name", out, tabs );
    XMLWriteElementCheckDefault( mElectricityReserveMargin, "electricity-reserve-margin", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mAverageGridCapacityFactor, "average-grid-capacity-factor", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mBackupCapacityFactor, "backup-capacity-factor", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mBackupCost, "backup-cost", out, tabs, 0.0 );
}

//! Write object to debugging xml output stream.
void IntermittentSupplySector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    // Write out parent class information.
    SupplySector::toDebugXMLDerived( period, out, tabs );

    XMLWriteElement( mElectricSectorName, "electric-sector-name", out, tabs );
    XMLWriteElement( mElectricityReserveMargin, "electricity-reserve-margin", out, tabs );
    XMLWriteElement( mAverageGridCapacityFactor, "average-grid-capacity-factor", out, tabs );
    XMLWriteElement( mBackupCapacityFactor, "backup-capacity-factor", out, tabs );
    XMLWriteElement( mBackupCost, "backup-cost", out, tabs );
}

/*! \brief Complete the initialization of the intermittent supply sector.
* \details The intermittent supply sector overrides the SupplySector
*          completeInit so that it can create a trial market for the
*          intermittent supply. This is required to determine the capacity share
*          of the sector.
* \author Josh Lurz
* \param aRegionInfo Region info container.
* \param aDependencyFinder Object which tracks sector dependencies.
* \param aLandAllocator Regional land allocator.
*/
void IntermittentSupplySector::completeInit( const IInfo* aRegionInfo,
                                             DependencyFinder* aDependencyFinder,
                                             ILandAllocator* aLandAllocator,
                                             const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Call the parent supply sector complete init.
    SupplySector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
}

/*! \brief Perform any initializations needed for each period.
* \details Any initializations or calculations that only need to be done once
*          per period(instead of every iteration) should be placed in this
*          function.
* \author Marshall Wise
* \param period Model period
*/
void IntermittentSupplySector::initCalc( NationalAccount* aNationalAccount,
                                         const Demographic* aDemographics,
                                         const int aPeriod )
{
    // add items to sector info must be done before Sector:initCalc() so that
    // information is available to subsector and technology initCalc() routines
    mSectorInfo->setDouble( "electricity-reserve-margin", mElectricityReserveMargin );
    mSectorInfo->setDouble( "average-grid-capacity-factor", mAverageGridCapacityFactor );
    mSectorInfo->setDouble( "backup-capacity-factor", mBackupCapacityFactor );
    mSectorInfo->setDouble( "backup-cost", mBackupCost );

    SupplySector::initCalc( aNationalAccount, aDemographics, aPeriod );
}
