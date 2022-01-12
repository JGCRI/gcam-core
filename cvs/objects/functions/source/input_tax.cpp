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
 * \file input_tax.cpp
 * \ingroup Objects
 * \brief The InputTax class source file.
 * \author Kate Calvin
 */

#include "util/base/include/definitions.h"
#include <cmath>

#include "functions/include/input_tax.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/tech_vector_parse_helper.h"
#include "technologies/include/icapture_component.h"
#include "functions/include/icoefficient.h"
#include "functions/include/efficiency.h"
#include "functions/include/intensity.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string InputTax::XML_REPORTING_NAME = "input-tax";

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
const string& InputTax::getXMLNameStatic() {
    const static string XML_NAME = "input-tax";
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
const string& InputTax::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& InputTax::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
InputTax::InputTax()
{
    TechVectorParseHelper<Value>::setDefaultValue( Value( 1.0 ), mAdjustedCoefficients );
}

/*!
 * \brief Destructor.
 * \note An explicit constructor must be defined to avoid the compiler inlining
 *       it in the header file before the header file for the type contained in
 *       the auto_ptr is included.
 */
InputTax::~InputTax() {
}

/*!
 * \brief Copy constructor.
 * \note This class requires a copy constructor because it has dynamically
 *          allocated memory.
 * \param aOther tax input from which to copy.
 */
InputTax::InputTax( const InputTax& aOther )
{
    MiniCAMInput::copy( aOther );
    // Do not clone the input coefficient as the calculated
    // coeffient will be filled out later.

    // Do not copy calibration values into the future
    // as they are only valid for one period.
    mName = aOther.mName;
    
    // copy keywords
    mKeywordMap = aOther.mKeywordMap;
}

InputTax* InputTax::clone() const {
    return new InputTax( *this );
}

bool InputTax::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void InputTax::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "current-coef", aOut, aTabs );
    XMLWriteElement( mPhysicalDemand[ aPeriod ], "physical-demand", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputTax::completeInit( const string& aRegionName,
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
    mSectorName = aSectorName;
    
}

void InputTax::initCalc( const string& aRegionName,
                             const string& aSectorName,
                             const bool aIsNewInvestmentPeriod,
                             const bool aIsTrade,
                             const IInfo* aTechInfo,
                             const int aPeriod )
{
    // There must be a valid region name.
    assert( !aRegionName.empty() );
    mAdjustedCoefficients[ aPeriod ] = 1.0;
}

void InputTax::copyParam( const IInput* aInput,
                             const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void InputTax::copyParamsInto( InputTax& aInput,
                                  const int aPeriod ) const
{
    // do nothing 
}


double InputTax::getCO2EmissionsCoefficient( const string& aGHGName,
                                             const int aPeriod ) const
{
    return 0;
}

double InputTax::getPhysicalDemand( const int aPeriod ) const {
    assert( mPhysicalDemand[ aPeriod ].isInited() );
    return mPhysicalDemand[ aPeriod ];
}

double InputTax::getCarbonContent( const int aPeriod ) const {
    return 0;
}

void InputTax::setPhysicalDemand( double aPhysicalDemand,
                                     const string& aRegionName,
                                     const int aPeriod )
{

    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* marketInfo = marketplace->getMarketInfo( mName, aRegionName, 0, true );

    // If tax is shared based, then divide by sector output.
    // Check if marketInfo exists and has the "isShareBased" boolean.
    if( marketInfo && marketInfo->hasValue( "isShareBased" ) ){
        if( marketInfo->getBoolean( "isShareBased", true ) ){
            // Each share is additive
            aPhysicalDemand/= marketplace->getDemand( mSectorName, aRegionName, aPeriod );
        }
    }
    // mPhysicalDemand can be a share if tax is share based.
    mPhysicalDemand[ aPeriod ].set( aPhysicalDemand );
    // Each technology share is additive.
    marketplace->addToDemand( mName, aRegionName, mPhysicalDemand[ aPeriod ],
                              aPeriod, true );
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::NOTICE );
}

double InputTax::getCoefficient( const int aPeriod ) const {
    // Check that the coefficient has been initialized.
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );

    return mAdjustedCoefficients[ aPeriod ];
}

void InputTax::setCoefficient( const double aCoefficient,
                                  const int aPeriod )
{
    // Do nothing.
}

double InputTax::getPrice( const string& aRegionName,
                              const int aPeriod ) const
{
    // A high tax decreases demand.
    return scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod, true );
}

void InputTax::setPrice( const string& aRegionName,
                            const double aPrice,
                            const int aPeriod )
{
    // Not hooking this up yet, it could work.
}

double InputTax::getCalibrationQuantity( const int aPeriod ) const
{
    return -1;
}

bool InputTax::hasTypeFlag( const int aTypeFlag ) const {
    return ( ( aTypeFlag & ~IInput::TAX ) == 0 );
}

double InputTax::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double InputTax::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double InputTax::getTechChange( const int aPeriod ) const
{
    return 0;
}

