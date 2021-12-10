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
 * \file input_OM_fixed.cpp
 * \ingroup Objects
 * \brief The InputOMVar class source file.
 * \author Sonny Kim
 */

#include "util/base/include/definitions.h"
#include "functions/include/input_OM_var.h"
#include "functions/include/function_utils.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string InputOMVar::XML_REPORTING_NAME = "input-OM-var";

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, 
* \return The constant XML_NAME as a static.
*/
const string& InputOMVar::getXMLNameStatic() {
    const static string XML_NAME = "input-OM-var";
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
const string& InputOMVar::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& InputOMVar::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
InputOMVar::InputOMVar()
{
}

//! Clone the input.
InputOMVar* InputOMVar::clone() const {
    InputOMVar* clone = new InputOMVar();
    clone->copy( *this );
    return clone;
}

void InputOMVar::copy( const InputOMVar& aOther ) {
    MiniCAMInput::copy( aOther );
    
    mTechChange = aOther.mTechChange;
    mOMVar = aOther.mOMVar;
    
    // calculated parameters are not copied.
}

bool InputOMVar::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void InputOMVar::copyParam( const IInput* aInput,
                            const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void InputOMVar::copyParamsInto( InputOMVar& aInput,
                                 const int aPeriod ) const
{
    // Copy the coefficients forward. This is done to adjust for technical
    // change which already occurred.
    assert( aPeriod > 0 );
    aInput.mAdjustedCoefficients[ aPeriod ] = mAdjustedCoefficients[ aPeriod - 1 ];
}

void InputOMVar::toDebugXML( const int aPeriod,
                             ostream& aOut,
                             Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( calcOMVarCost(), "levelized-OM-var", aOut, aTabs );
    XMLWriteElement( mOMVar, "OM-var", aOut, aTabs );
    XMLWriteElement( mTechChange, "tech-change", aOut, aTabs );
    XMLWriteElement( mAdjustedCosts[ aPeriod ], "adjusted-cost", aOut, aTabs );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "adjusted-coef", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputOMVar::completeInit( const string& aRegionName,
                               const string& aSectorName,
                               const string& aSubsectorName,
                               const string& aTechName,
                               const IInfo* aTechInfo )
{   
    // Initialize the adjusted costs in all periods to the base calculate
    // levelized OM-var cost.
    // These costs may be adjusted by the Technology, for instance for capture
    // penalties.
    fill( mAdjustedCosts.begin(), mAdjustedCosts.end(), Value( calcOMVarCost() ) );
}

/** Calculate the levelizd OM_fixed cost.
 *
 * \param void 
 * \return Levelized OM_fixed costs.
 * \author Sonny Kim
 */
double InputOMVar::calcOMVarCost( void ) const
{
	//read in as $/MWh
    double OMVarCost = mOMVar /(1000 * FunctionUtils::GJ_PER_KWH());
	
    return OMVarCost; // 1975$/GJ
}

void InputOMVar::initCalc( const string& aRegionName,
                           const string& aSectorName,
                           const bool aIsNewInvestmentPeriod,
                           const bool aIsTrade,
                           const IInfo* aTechInfo,
                           const int aPeriod )
{
    // Initialize the current coefficient to 1 if it has not 
    // been initialized through copyParam. It may be adjusted
    // later when coefficients are copied forward.
    mAdjustedCoefficients[ aPeriod ] = 1;
}

double InputOMVar::getPrice( const string& aRegionName,
                             const int aPeriod ) const
{
    assert( mAdjustedCosts[ aPeriod ].isInited() );
    return mAdjustedCosts[ aPeriod ];
}

void InputOMVar::setPrice( const string& aRegionName,
                           const double aPrice,
                           const int aPeriod ) 
{
    mAdjustedCosts[ aPeriod ] = aPrice;
}

double InputOMVar::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

void InputOMVar::setPhysicalDemand( double aPhysicalDemand,
                                    const string& aRegionName,
                                    const int aPeriod )
{
    // Does not add to the marketplace.
}

double InputOMVar::getCO2EmissionsCoefficient( const string& aGHGName,
                                            const int aPeriod ) const
{
    // Capital cost inputs cannot have emissions coefficients.
    return 0;
}

double InputOMVar::getCoefficient( const int aPeriod ) const {
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );
    return mAdjustedCoefficients[ aPeriod ];
}

void InputOMVar::setCoefficient( const double aCoefficient,
                                 const int aPeriod )
{
    mAdjustedCoefficients[ aPeriod ] = aCoefficient;
}

void InputOMVar::tabulateFixedQuantity( const string& aRegionName,
                                        const double aFixedOutput,
                                        const bool aIsInvestmentPeriod,
                                        const int aPeriod )
{
}

void InputOMVar::scaleCalibrationQuantity( const double aScaleFactor ){
    // Capital cost inputs are not calibrated.
}

double InputOMVar::getCalibrationQuantity( const int aPeriod ) const
{
    // Capital cost inputs are not calibrated.
    return -1;
}

bool InputOMVar::hasTypeFlag( const int aTypeFlag ) const {
    return ( ( aTypeFlag & ~( IInput::OM_VAR ) ) == 0 );
}

double InputOMVar::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double InputOMVar::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double InputOMVar::getTechChange( const int aPeriod ) const
{
    return mTechChange;
}

void InputOMVar::doInterpolations( const int aYear, const int aPreviousYear,
                                    const int aNextYear, const IInput* aPreviousInput,
                                    const IInput* aNextInput )
{
    const InputOMVar* prevOMInput = static_cast<const InputOMVar*>( aPreviousInput );
    const InputOMVar* nextOMInput = static_cast<const InputOMVar*>( aNextInput );
    
    /*!
     * \pre We are given a valid InputOMVar for the previous input.
     */
    assert( prevOMInput );
    
    /*!
     * \pre We are given a valid InputOMVar for the next input.
     */
    assert( nextOMInput );
    
    // tech change is just copied from the next input
    mTechChange = nextOMInput->mTechChange;
    
    // interpolate the costs
    mOMVar = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                       prevOMInput->mOMVar, nextOMInput->mOMVar );
}
