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
 * \brief The InputOMFixed class source file.
 * \author Sonny Kim
 */

#include "util/base/include/definitions.h"
#include "functions/include/input_OM_fixed.h"
#include "functions/include/function_utils.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string InputOMFixed::XML_REPORTING_NAME = "input-OM-fixed";

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
const string& InputOMFixed::getXMLNameStatic() {
    const static string XML_NAME = "input-OM-fixed";
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
const string& InputOMFixed::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& InputOMFixed::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
InputOMFixed::InputOMFixed()
{
}

//! Clone the input.
InputOMFixed* InputOMFixed::clone() const {
    InputOMFixed* clone = new InputOMFixed();
    clone->copy( *this );
    return clone;
}

void InputOMFixed::copy( const InputOMFixed& aOther ) {
    MiniCAMInput::copy( aOther );
    
    mTechChange = aOther.mTechChange;
    mOMFixed = aOther.mOMFixed;
    mCapacityFactor = aOther.mCapacityFactor;
    
    // calculated parameters are not copied.
}

bool InputOMFixed::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void InputOMFixed::copyParam( const IInput* aInput,
                              const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void InputOMFixed::copyParamsInto( InputOMFixed& aInput,
                                   const int aPeriod ) const
{
    // Copy the coefficients forward. This is done to adjust for technical
    // change which already occurred.
    assert( aPeriod > 0 );
    aInput.mAdjustedCoefficients[ aPeriod ] = mAdjustedCoefficients[ aPeriod - 1 ];
}

void InputOMFixed::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mLevelizedOMFixedCost, "levelized-OM-fixed", aOut, aTabs );
    XMLWriteElement( mOMFixed, "OM-fixed", aOut, aTabs );
    XMLWriteElement( mCapacityFactor, "capacity-factor", aOut, aTabs );
    XMLWriteElement( mTechChange, "tech-change", aOut, aTabs );
    XMLWriteElement( mAdjustedCosts[ aPeriod ], "adjusted-cost", aOut, aTabs );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "adjusted-coef", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputOMFixed::completeInit( const string& aRegionName,
                                 const string& aSectorName,
                                 const string& aSubsectorName,
                                 const string& aTechName,
                                 const IInfo* aTechInfo )
{   
    // technology capacity factor
    // capacity factor needed before levelized fixed om cost calculation
    mCapacityFactor = aTechInfo->getDouble("tech-capacity-factor", true);

    // completeInit() is called for each technology for each period
    // so levelized O&M fixed cost calculation is done here.

    mLevelizedOMFixedCost = calcLevelizedOMFixedCost();

    // Initialize the adjusted costs in all periods to the base calculate
    // levelized OM_fixed cost.
    // These costs may be adjusted by the Technology, for instance for capture
    // penalties.
    fill( mAdjustedCosts.begin(), mAdjustedCosts.end(), mLevelizedOMFixedCost );
}

/** Calculate the levelizd fixed O&M cost.
 *
 * \param void 
 * \return Levelized fixed O&M cost.
 * \author Sonny Kim
 */
double InputOMFixed::calcLevelizedOMFixedCost( void ) const
{
    // TODO: Use technology's capacity factor.
    // TODO: Use Value class for units conversion.
    double levelizedOMFixedCost = mOMFixed 
	/ ( FunctionUtils::HOURS_PER_YEAR() * mCapacityFactor * FunctionUtils::GJ_PER_KWH() );

    return levelizedOMFixedCost; // 1975$/GJ
}

void InputOMFixed::initCalc( const string& aRegionName,
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

double InputOMFixed::getPrice( const string& aRegionName,
                               const int aPeriod ) const
{
    assert( mAdjustedCosts[ aPeriod ].isInited() );
    return mAdjustedCosts[ aPeriod ];
}

void InputOMFixed::setPrice( const string& aRegionName,
                             const double aPrice,
                             const int aPeriod ) 
{
    mAdjustedCosts[ aPeriod ] = aPrice;
}

double InputOMFixed::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

void InputOMFixed::setPhysicalDemand( double aPhysicalDemand,
                                      const string& aRegionName,
                                      const int aPeriod )
{
    // Does not add to the marketplace.
}

double InputOMFixed::getCO2EmissionsCoefficient( const string& aGHGName,
                                                 const int aPeriod ) const
{
    // Capital cost inputs cannot have emissions coefficients.
    return 0;
}

double InputOMFixed::getCoefficient( const int aPeriod ) const {
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );
    return mAdjustedCoefficients[ aPeriod ];
}

void InputOMFixed::setCoefficient( const double aCoefficient,
                                   const int aPeriod )
{
    mAdjustedCoefficients[ aPeriod ] = aCoefficient;
}

void InputOMFixed::tabulateFixedQuantity( const string& aRegionName,
                                          const double aFixedOutput,
                                          const bool aIsInvestmentPeriod,
                                          const int aPeriod )
{
}

void InputOMFixed::scaleCalibrationQuantity( const double aScaleFactor ){
    // Capital cost inputs are not calibrated.
}

double InputOMFixed::getCalibrationQuantity( const int aPeriod ) const
{
    // Capital cost inputs are not calibrated.
    return -1;
}

bool InputOMFixed::hasTypeFlag( const int aTypeFlag ) const {
    return ( ( aTypeFlag & ~( IInput::OM_FIXED ) ) == 0 );
}

double InputOMFixed::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double InputOMFixed::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double InputOMFixed::getTechChange( const int aPeriod ) const
{
    return mTechChange;
}

void InputOMFixed::doInterpolations( const int aYear, const int aPreviousYear,
                                     const int aNextYear, const IInput* aPreviousInput,
                                     const IInput* aNextInput )
{
    const InputOMFixed* prevOMInput = static_cast<const InputOMFixed*>( aPreviousInput );
    const InputOMFixed* nextOMInput = static_cast<const InputOMFixed*>( aNextInput );
    
    /*!
     * \pre We are given a valid InputOMFixed for the previous input.
     */
    assert( prevOMInput );
    
    /*!
     * \pre We are given a valid InputOMFixed for the next input.
     */
    assert( nextOMInput );
    
    // tech change is just copied from the next input
    mTechChange = nextOMInput->mTechChange;
    
    // interpolate the costs
    mOMFixed = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                         prevOMInput->mOMFixed, nextOMInput->mOMFixed );
    mLevelizedOMFixedCost = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                      prevOMInput->mLevelizedOMFixedCost,
                                                      nextOMInput->mLevelizedOMFixedCost );
    
    // interpolate capacity factor
    mCapacityFactor = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevOMInput->mCapacityFactor, nextOMInput->mCapacityFactor );
}
