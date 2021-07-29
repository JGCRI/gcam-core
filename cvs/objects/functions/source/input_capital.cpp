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
 * \file input_capital.cpp
 * \ingroup Objects
 * \brief The InputCapital class source file.
 * \author Sonny Kim
 */

#include "util/base/include/definitions.h"
#include "functions/include/input_capital.h"
#include "functions/include/function_utils.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string InputCapital::XML_REPORTING_NAME = "input-capital";

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
const string& InputCapital::getXMLNameStatic() {
    const static string XML_NAME = "input-capital";
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
const string& InputCapital::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& InputCapital::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
InputCapital::InputCapital()
{
}

//! Clone the input.
InputCapital* InputCapital::clone() const {
    InputCapital* clone = new InputCapital();
    clone->copy( *this );
    return clone;
}

void InputCapital::copy( const InputCapital& aOther ) {
    MiniCAMInput::copy( aOther );
    
    mTechChange = aOther.mTechChange;
    mCapitalOvernight = aOther.mCapitalOvernight;
    mFixedChargeRate = aOther.mFixedChargeRate;
    mLifetimeCapital = aOther.mLifetimeCapital;
    mCapacityFactor = aOther.mCapacityFactor;
    
    // calculated parameters are not copied.
}

bool InputCapital::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void InputCapital::copyParam( const IInput* aInput,
                              const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void InputCapital::copyParamsInto( InputCapital& aInput,
                                   const int aPeriod ) const
{
    // Copy the coefficients forward. This is done to adjust for technical
    // change which already occurred.
    assert( aPeriod > 0 );
    aInput.mAdjustedCoefficients[ aPeriod ] = mAdjustedCoefficients[ aPeriod - 1 ];
}

void InputCapital::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mLevelizedCapitalCost, "levelized-capital-cost", aOut, aTabs );
    XMLWriteElement( mCapitalOvernight, "capital-overnight", aOut, aTabs );
    XMLWriteElement( mLifetimeCapital, "lifetime-capital", aOut, aTabs );
    XMLWriteElement( mFixedChargeRate, "mFixedChargeRate", aOut, aTabs );
    XMLWriteElement( mCapacityFactor, "capacity-factor", aOut, aTabs );
    XMLWriteElement( mTechChange, "tech-change", aOut, aTabs );
    XMLWriteElement( mAdjustedCosts[ aPeriod ], "adjusted-cost", aOut, aTabs );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "adjusted-coef", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputCapital::completeInit( const string& aRegionName,
                                 const string& aSectorName,
                                 const string& aSubsectorName,
                                 const string& aTechName,
                                 const IInfo* aTechInfo )
{   
    
    // technology capacity factor
    // capacity factor needed before levelized cost calculation
    mCapacityFactor = aTechInfo->getDouble("tech-capacity-factor", true);
                                           
    // completeInit() is called for each technology for each period
    // so levelized capital cost calculation is done here.

    mLevelizedCapitalCost = calcLevelizedCapitalCost();
    
    // Initialize the adjusted costs in all periods to the base calculate
    // levelized capital cost.
    // These costs may be adjusted by the Technology, for instance for capture
    // penalties.
    fill( mAdjustedCosts.begin(), mAdjustedCosts.end(), mLevelizedCapitalCost );
}

/** Calculate the levelizd capital cost.
 *
 * \param void 
 * \return Levelized capital costs.
 * \author Sonny Kim
 */
double InputCapital::calcLevelizedCapitalCost( void ) const
{
    // TODO: Use more detailed approach for calculating levelized
    // capital cost that includes number of years for construction.
    // TODO: Get interest/discount rate from capital market.
    // TODO: Use Value class for units conversion.
    double levelizedCapitalCost = 
	mFixedChargeRate * mCapitalOvernight / ( FunctionUtils::HOURS_PER_YEAR() * mCapacityFactor * FunctionUtils::GJ_PER_KWH() );

    return levelizedCapitalCost; // 1975$/GJ
}

void InputCapital::initCalc( const string& aRegionName,
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

double InputCapital::getPrice( const string& aRegionName,
                               const int aPeriod ) const
{
    assert( mAdjustedCosts[ aPeriod ].isInited() );
    return mAdjustedCosts[ aPeriod ];
}

void InputCapital::setPrice( const string& aRegionName,
                             const double aPrice,
                             const int aPeriod ) 
{
    mAdjustedCosts[ aPeriod ] = aPrice;
}

double InputCapital::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

void InputCapital::setPhysicalDemand( double aPhysicalDemand,
                                      const string& aRegionName,
                                      const int aPeriod )
{
    // Does not add to the marketplace.
}

double InputCapital::getCO2EmissionsCoefficient( const string& aGHGName,
                                                 const int aPeriod ) const
{
    // Capital cost inputs cannot have emissions coefficients.
    return 0;
}

double InputCapital::getCoefficient( const int aPeriod ) const {
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );
    return mAdjustedCoefficients[ aPeriod ];
}

void InputCapital::setCoefficient( const double aCoefficient,
                                   const int aPeriod )
{
    mAdjustedCoefficients[ aPeriod ] = aCoefficient;
}

void InputCapital::tabulateFixedQuantity( const string& aRegionName,
                                          const double aFixedOutput,
                                          const bool aIsInvestmentPeriod,
                                          const int aPeriod )
{
}

void InputCapital::scaleCalibrationQuantity( const double aScaleFactor ){
    // Capital cost inputs are not calibrated.
}

double InputCapital::getCalibrationQuantity( const int aPeriod ) const
{
    // Capital cost inputs are not calibrated.
    return -1;
}

bool InputCapital::hasTypeFlag( const int aTypeFlag ) const {
    return ( ( aTypeFlag & ~( IInput::CAPITAL ) ) == 0 );
}

double InputCapital::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double InputCapital::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double InputCapital::getTechChange( const int aPeriod ) const
{
    return mTechChange;
}

void InputCapital::doInterpolations( const int aYear, const int aPreviousYear,
                                     const int aNextYear, const IInput* aPreviousInput,
                                     const IInput* aNextInput )
{
    const InputCapital* prevCapInput = static_cast<const InputCapital*>( aPreviousInput );
    const InputCapital* nextCapInput = static_cast<const InputCapital*>( aNextInput );
    
    /*!
     * \pre We are given a valid InputCapital for the previous input.
     */
    assert( prevCapInput );
    
    /*!
     * \pre We are given a valid InputCapital for the next input.
     */
    assert( nextCapInput );
    
    // tech change is just copied from the next input
    mTechChange = nextCapInput->mTechChange;
    
    // interpolate the costs
    mCapitalOvernight = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                  prevCapInput->mCapitalOvernight,
                                                  nextCapInput->mCapitalOvernight );
    mLifetimeCapital = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                 prevCapInput->mLifetimeCapital,
                                                 nextCapInput->mLifetimeCapital );
    mLevelizedCapitalCost = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                      prevCapInput->mLevelizedCapitalCost,
                                                      nextCapInput->mLevelizedCapitalCost );
    
    // interplate capacity factor
    mCapacityFactor = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                prevCapInput->mCapacityFactor,
                                                nextCapInput->mCapacityFactor );
}
