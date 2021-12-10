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
 * \file renewable_input.cpp
 * \ingroup Objects
 * \brief The RenewableInput class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include "functions/include/renewable_input.h"
#include "util/base/include/xml_helper.h"

using namespace std;

// static initialize.
const string RenewableInput::XML_REPORTING_NAME = "input-renewable";

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
const string& RenewableInput::getXMLNameStatic() {
    const static string XML_NAME = "renewable-input";
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
const string& RenewableInput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& RenewableInput::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
RenewableInput::RenewableInput() 
{
}

/*! \brief Constructor that sets name attribute.
*
* This allows renewable input object to be created by technology
* objects.
* \author Steve Smith
*/
RenewableInput::RenewableInput( const std::string& aName )
{
    mName = aName;
}

//! Clone the input.
RenewableInput* RenewableInput::clone() const {
    RenewableInput* clone = new RenewableInput();
    clone->copy( *this );
    return clone;
}

bool RenewableInput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void RenewableInput::copy( const RenewableInput& aOther ) {
    MiniCAMInput::copy( aOther );
    
    // calculated parameters are not copied.
}

void RenewableInput::copyParam( const IInput* aInput,
                                const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void RenewableInput::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mPhysicalDemand[ aPeriod ], "physical-demand", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void RenewableInput::completeInit( const string& aRegionName,
                                   const string& aSectorName,
                                   const string& aSubsectorName,
                                   const string& aTechName,
                                   const IInfo* aTechInfo )
{
}

void RenewableInput::initCalc( const string& aRegionName,
                               const string& aSectorName,
                               const bool aIsNewInvestmentPeriod,
                               const bool aIsTrade,
                               const IInfo* aTechInfo,
                               const int aPeriod )
{
}

double RenewableInput::getPrice( const string& aRegionName,
                                 const int aPeriod ) const
{
    // Renewable inputs are free.
    return 0;
}

void RenewableInput::setPrice( const string& aRegionName,
                               const double aPrice,
                               const int aPeriod )
{
    // No price to set, renewable inputs are free.
}

double RenewableInput::getPhysicalDemand( const int aPeriod ) const {
    return mPhysicalDemand[ aPeriod ];
}

void RenewableInput::setPhysicalDemand( double aPhysicalDemand,
                                       const string& aRegionName,
                                       const int aPeriod )
{
    mPhysicalDemand[ aPeriod ].set( aPhysicalDemand );
    // There is no market for the renewable good, so this does not add to the
    // marketplace.
}

double RenewableInput::getCO2EmissionsCoefficient( const string& aGHGName,
                                                const int aPeriod ) const
{
    return 0;
}

double RenewableInput::getCoefficient( const int aPeriod ) const {
    // Average fossil efficiency is 0.33, so return 3 as the intensity to
    // represent that it would take 3 units of fossil equivalent inputs to
    // create one output unit of electricity.
    return 3;
}

void RenewableInput::setCoefficient( const double aCoefficient,
                                     const int aPeriod )
{
    // Cannot set coefficients for renewable inputs.
    assert( false );
}

double RenewableInput::getCalibrationQuantity( const int aPeriod ) const
{
    // This is a hack so that renewable outputs appear to be fixed.
    return -2;
}

bool RenewableInput::hasTypeFlag( const int aTypeFlag ) const {
    return (  ( aTypeFlag & ~IInput::ENERGY ) == 0 );
}

double RenewableInput::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double RenewableInput::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double RenewableInput::getTechChange( const int aPeriod ) const
{
    // Renewable inputs do not have technical change.
    return 0;
}
