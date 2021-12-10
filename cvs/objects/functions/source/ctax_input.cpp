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
 * \file ctax_input.cpp
 * \ingroup Objects
 * \brief The CTaxInput class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cmath>

#include "functions/include/ctax_input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Sonny Kim
* \return The constant XML_NAME as a static.
*/
const string& CTaxInput::getXMLNameStatic() {
    const static string XML_NAME = "ctax-input";
    return XML_NAME;
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& CTaxInput::getXMLName() const{
    return getXMLNameStatic();
}

const string& CTaxInput::getXMLReportingName() const{
    return getXMLNameStatic();
}

//! Constructor
CTaxInput::CTaxInput()
: mCachedCCoef( 0.0 )
{
}

/*!
 * \brief Destructor.
 * \note An explicit constructor must be defined to avoid the compiler inlining
 *       it in the header file before the header file for the type contained in
 *       the auto_ptr is included.
 */
CTaxInput::~CTaxInput() {
}

/*!
 * \brief Copy constructor.
 * \note This class requires a copy constructor because it has dynamically
 *          allocated memory.
 * \param aOther tax input from which to copy.
 */
CTaxInput::CTaxInput( const CTaxInput& aOther ){
    mName = aOther.mName;
    mCachedCCoef = aOther.mCachedCCoef;
}

CTaxInput* CTaxInput::clone() const {
    return new CTaxInput( *this );
}

bool CTaxInput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void CTaxInput::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mCachedCCoef, "fuel-C-coef", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void CTaxInput::completeInit( const string& aRegionName,
                                 const string& aSectorName,
                                 const string& aSubsectorName,
                                 const string& aTechName,
                                 const IInfo* aTechInfo )
{

    // Add the input dependency to the dependency finder.
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                      aRegionName,
                                                                      getName(),
                                                                      aRegionName );
}

void CTaxInput::initCalc( const string& aRegionName,
                             const string& aSectorName,
                             const bool aIsNewInvestmentPeriod,
                             const bool aIsTrade,
                             const IInfo* aTechInfo,
                             const int aPeriod )
{
    // There must be a valid region name.
    assert( !aRegionName.empty() );
}

void CTaxInput::copyParam( const IInput* aInput,
                             const int aPeriod )
{
}

double CTaxInput::getCO2EmissionsCoefficient( const string& aGHGName,
                                             const int aPeriod ) const
{
    // do not double account taxes on mFuelName
    return 0;
}

double CTaxInput::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

double CTaxInput::getCarbonContent( const int aPeriod ) const {
    return 0;
}

void CTaxInput::setPhysicalDemand( double aPhysicalDemand,
                                     const string& aRegionName,
                                     const int aPeriod )
{
    Marketplace* marketplace = scenario->getMarketplace();
    double taxFraction = marketplace->getPrice( mName, aRegionName, aPeriod, true );
    double ctax = marketplace->getPrice( "CO2", aRegionName, aPeriod, false );
    double priceAdjust = 0.0;
    if( taxFraction != Marketplace::NO_MARKET_PRICE && ctax != Marketplace::NO_MARKET_PRICE ) {
        priceAdjust = (1.0 - std::min( taxFraction, 1.0 )) * ctax * mCachedCCoef;
    }
    mNetTransferAdjust = aPhysicalDemand * priceAdjust;
    marketplace->addToDemand( mName, aRegionName, mNetTransferAdjust, aPeriod );
}

double CTaxInput::getCoefficient( const int aPeriod ) const {
    return 1.0;
}

void CTaxInput::setCoefficient( const double aCoefficient,
                                  const int aPeriod )
{
    // Do nothing.
}

double CTaxInput::getPrice( const string& aRegionName,
                              const int aPeriod ) const
{
    // Constants
    const double CVRT90 = 2.212; // 1975 $ to 1990 $
    // Conversion from teragrams of carbon per EJ to metric tons of carbon per GJ
    const double CVRT_TG_MT = 1e-3;
    // A high tax decreases demand.
    const Marketplace* marketplace = scenario->getMarketplace();
    double taxFraction = marketplace->getPrice( mName, aRegionName, aPeriod, true );
    double ctax = marketplace->getPrice( "CO2", aRegionName, aPeriod, false );
    
    // note we need to perform some unit conversions since C prices and technology
    // costs in different units
    return taxFraction == Marketplace::NO_MARKET_PRICE || ctax == Marketplace::NO_MARKET_PRICE ?
        0.0 : taxFraction * ctax * mCachedCCoef / CVRT90 * CVRT_TG_MT;
}

void CTaxInput::setPrice( const string& aRegionName,
                            const double aPrice,
                            const int aPeriod )
{
}

double CTaxInput::getCalibrationQuantity( const int aPeriod ) const
{
    return -1;
}

bool CTaxInput::hasTypeFlag( const int aTypeFlag ) const {
    return ( ( aTypeFlag & ~IInput::TAX ) == 0 );
}

double CTaxInput::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double CTaxInput::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double CTaxInput::getTechChange( const int aPeriod ) const
{
    return 0;
}

