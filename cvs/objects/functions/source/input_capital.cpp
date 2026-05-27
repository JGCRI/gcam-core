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
#include "sectors/include/sector_utils.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"

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
InputCapital::InputCapital():mTrackingMarketName("capital"), mIsActive(false)
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
    
    mCapitalOvernight = aOther.mCapitalOvernight;
    mInterestRate = aOther.mInterestRate;
    mPaybackYears = aOther.mPaybackYears;
    mTrackingMarketName = aOther.mTrackingMarketName;
    
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
}

void InputCapital::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mOvernightCapitalPerOutput, "overnight-capital-per-output", aOut, aTabs );
    XMLWriteElement( mCapitalOvernight, "capital-overnight", aOut, aTabs );
    XMLWriteElement( mPaybackYears, "payback-years", aOut, aTabs );
    XMLWriteElement( mInterestRate, "interest-rate", aOut, aTabs );
    XMLWriteElement( mAdjustedCosts, "adjusted-cost", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void InputCapital::completeInit( const gcamstr& aRegionName,
                                 const gcamstr& aSectorName,
                                 const gcamstr& aSubsectorName,
                                 const gcamstr& aTechName,
                                 const IInfo* aTechInfo )
{
    // technology capacity factor
    // capacity factor needed before levelized cost calculation
    double capFactor = aTechInfo->getDouble(gcamstr("tech-capacity-factor"), true);
                                           
    // completeInit() is called for each technology for each period
    // so levelized capital cost calculation is done here.

    mOvernightCapitalPerOutput = calcLevelizedCapitalCost(capFactor);
    
    // Initialize the adjusted costs in all periods to the base calculate
    // levelized capital cost.
    // These costs may be adjusted by the Technology, for instance for capture
    // penalties.
    
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( aSectorName, aRegionName, mTrackingMarketName, aRegionName);
}

/** Convert the overnight costs per capacity to per output include unit conversions.
 *
 * \param aCapFactor The technology capacity factor.
 * \return Levelized capital costs.
 * \author Sonny Kim
 */
double InputCapital::calcLevelizedCapitalCost( const double aCapFactor ) const
{
    // convert overnight costs per capacity to per output
    // this is going to include unit conversions (quantity and price)
    // as well applying a capacity factor
    double levelizedCapitalCost =
        mCapitalOvernight / ( FunctionUtils::HOURS_PER_YEAR() * aCapFactor * FunctionUtils::GJ_PER_KWH() );

    return levelizedCapitalCost; // 1975$/GJ
}

void InputCapital::initCalc( const gcamstr& aRegionName,
                             const gcamstr& aSectorName,
                             const bool aIsNewInvestmentPeriod,
                             const bool aIsTrade,
                             const IInfo* aTechInfo,
                             const int aPeriod )
{
    mIsActive = aTechInfo->getBoolean("new-vintage-tech", true);
    
    // Ideally we only need to locate the market once, however during completeInit
    // all markets may have not yet been set up.  So, instead we avoid re-lookups
    // if the market has been found.  Unfortunately, this means if the market will
    // never be found we will continue to try to look it up each model period.
    if(!mCachedMarket.hasLocatedMarket()) {
        mCachedMarket = scenario->getMarketplace()->locateMarket( mTrackingMarketName, aRegionName );
    }
}

double InputCapital::getPrice( const gcamstr& aRegionName,
                               const int aPeriod ) const
{
    if(mIsActive) {
        const Modeltime* modeltime = scenario->getModeltime();
        double adjInt = SectorUtils::calcPriceRatio(mCachedMarket, aRegionName, mTrackingMarketName, modeltime->getFinalCalibrationPeriod(), aPeriod) * mInterestRate;
        double fcr = FunctionUtils::calcFCR(adjInt, mPaybackYears);
        double levelizedCapitalCost = fcr * mOvernightCapitalPerOutput;
        const_cast<InputCapital*>(this)->mAdjustedCosts = levelizedCapitalCost;
    }
    return mAdjustedCosts;
}

void InputCapital::setPrice( const gcamstr& aRegionName,
                             const double aPrice,
                             const int aPeriod ) 
{
}

double InputCapital::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

void InputCapital::setPhysicalDemand( double aPhysicalDemand,
                                      const gcamstr& aRegionName,
                                      const int aPeriod )
{
    if(mIsActive) {
        mCapitalValue = mOvernightCapitalPerOutput * aPhysicalDemand;
        mCachedMarket.addToDemand(mTrackingMarketName, aRegionName, mCapitalValue, aPeriod, false);
    }
}

double InputCapital::getCO2EmissionsCoefficient( const gcamstr& aGHGName,
                                                 const int aPeriod ) const
{
    // Capital cost inputs cannot have emissions coefficients.
    return 0;
}

double InputCapital::getCoefficient( const int aPeriod ) const {
    return 1.0;
}

void InputCapital::setCoefficient( const double aCoefficient,
                                   const int aPeriod )
{
    assert(false);
    // not available
}

void InputCapital::tabulateFixedQuantity( const gcamstr& aRegionName,
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
    
    // interpolate the costs
    mCapitalOvernight = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                  prevCapInput->mCapitalOvernight,
                                                  nextCapInput->mCapitalOvernight );
    mOvernightCapitalPerOutput = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                      prevCapInput->mOvernightCapitalPerOutput,
                                                      nextCapInput->mOvernightCapitalPerOutput );
}
