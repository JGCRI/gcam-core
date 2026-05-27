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
 * \file non_energy_input.cpp
 * \ingroup Objects
 * \brief The NonEnergyInput class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include "functions/include/non_energy_input.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/function_utils.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string NonEnergyInput::XML_REPORTING_NAME = "input-non-energy";

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
const string& NonEnergyInput::getXMLNameStatic() {
    const static string XML_NAME = "minicam-non-energy-input";
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
const string& NonEnergyInput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& NonEnergyInput::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
NonEnergyInput::NonEnergyInput()
{
}

/*! \brief Constructor that sets name attribute.
*
* This allows non-energy input objects to be created by technology
* objects.
* \author Steve Smith
*/
NonEnergyInput::NonEnergyInput( const std::string& aName )
{
    mName = aName;
}

//! Clone the input.
NonEnergyInput* NonEnergyInput::clone() const {
    NonEnergyInput* clone = new NonEnergyInput();
    clone->copy( *this );
    return clone;
}

bool NonEnergyInput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

void NonEnergyInput::copy( const NonEnergyInput& aOther ) {
    MiniCAMInput::copy( aOther );
    
    mCost = aOther.mCost;
    
    // calculated parameters are not copied.
}

void NonEnergyInput::copyParam( const IInput* aInput,
                                const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void NonEnergyInput::copyParamsInto( NonEnergyInput& aInput,
                                     const int aPeriod ) const
{
}

void NonEnergyInput::toDebugXML( const int aPeriod,
                                 ostream& aOut,
                                 Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );
    XMLWriteElement( mCost, "input-cost", aOut, aTabs );
    XMLWriteElement( mAdjustedCosts, "adjusted-cost", aOut, aTabs );
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void NonEnergyInput::completeInit( const gcamstr& aRegionName,
                                   const gcamstr& aSectorName,
                                   const gcamstr& aSubsectorName,
                                   const gcamstr& aTechName,
                                   const IInfo* aTechInfo )
{
    // Initialize the adjusted costs in all periods to the base read-in costs.
    // These costs may be adjusted by the Technology, for instance for capture
    // penalties.
    mAdjustedCosts = mCost;
}

void NonEnergyInput::initCalc( const gcamstr& aRegionName,
                               const gcamstr& aSectorName,
                               const bool aIsNewInvestmentPeriod,
                               const bool aIsTrade,
                               const IInfo* aTechInfo,
                               const int aPeriod )
{
}

double NonEnergyInput::getPrice( const gcamstr& aRegionName,
                                 const int aPeriod ) const
{
    assert( mAdjustedCosts.isInited() );
    return mAdjustedCosts;
}

void NonEnergyInput::setPrice( const gcamstr& aRegionName,
                               const double aPrice,
                               const int aPeriod ) 
{
    mAdjustedCosts = aPrice;
}

double NonEnergyInput::getPhysicalDemand( const int aPeriod ) const {
    return 0;
}

void NonEnergyInput::setPhysicalDemand( double aPhysicalDemand,
                                        const gcamstr& aRegionName,
                                        const int aPeriod )
{
    // Does not add to the marketplace.
}

double NonEnergyInput::getCO2EmissionsCoefficient( const gcamstr& aGHGName,
                                                const int aPeriod ) const
{
    // Non-energy inputs cannot have emissions coefficients.
    return 0;
}

double NonEnergyInput::getCoefficient( const int aPeriod ) const {
    return 1.0;
}

void NonEnergyInput::setCoefficient( const double aCoefficient,
                                     const int aPeriod )
{
    assert(false);
    // not available
}

double NonEnergyInput::getCalibrationQuantity( const int aPeriod ) const
{
    // Non-energy inputs are not calibrated.
    return -1;
}

bool NonEnergyInput::hasTypeFlag( const int aTypeFlag ) const {
    return (  ( aTypeFlag & ~( IInput::CAPITAL | IInput::MATERIAL ) ) == 0 );
}

double NonEnergyInput::getIncomeElasticity( const int aPeriod ) const {
    return 0;
}

double NonEnergyInput::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

void NonEnergyInput::doInterpolations( const int aYear, const int aPreviousYear,
                                       const int aNextYear, const IInput* aPreviousInput,
                                       const IInput* aNextInput )
{
    const NonEnergyInput* prevNonEneInput = static_cast<const NonEnergyInput*>( aPreviousInput );
    const NonEnergyInput* nextNonEneInput = static_cast<const NonEnergyInput*>( aNextInput );
    
    /*!
     * \pre We are given a valid NonEnergyInput for the previous input.
     */
    assert( prevNonEneInput );
    
    /*!
     * \pre We are given a valid NonEnergyInput for the next input.
     */
    assert( nextNonEneInput );
    
    // interpolate the base cost
    mCost.set( util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                         prevNonEneInput->mCost, nextNonEneInput->mCost ) );
}

TrackingNonEnergyInput::TrackingNonEnergyInput():
mTrackingMarketName("capital"),
mIsActive(false)
{
}

TrackingNonEnergyInput::~TrackingNonEnergyInput() {
}

NonEnergyInput* TrackingNonEnergyInput::clone() const {
    TrackingNonEnergyInput* clone = new TrackingNonEnergyInput();
    clone->mCapitalRatio = mCapitalRatio;
    clone->mInterestRate = mInterestRate;
    clone->mPaybackYears = mPaybackYears;
    clone->mInvestUnitConversion = mInvestUnitConversion;
    clone->mTrackingMarketName = mTrackingMarketName;
    clone->mDepreciationRate = mDepreciationRate;
    clone->copy( *this );
    return clone;
}

const string& TrackingNonEnergyInput::getXMLNameStatic() {
    const static string XML_NAME = "tracking-non-energy-input";
    return XML_NAME;
}

const string& TrackingNonEnergyInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

const string& TrackingNonEnergyInput::getXMLName() const {
    return getXMLNameStatic();
}

void TrackingNonEnergyInput::completeInit( const gcamstr& aRegionName,
                                   const gcamstr& aSectorName,
                                   const gcamstr& aSubsectorName,
                                   const gcamstr& aTechName,
                                   const IInfo* aTechInfo )
{
    NonEnergyInput::completeInit(aRegionName, aSectorName,
            aSubsectorName, aTechName, aTechInfo);
    mOvernightCap = mCost / FunctionUtils::calcFCR(mInterestRate, mPaybackYears) * mCapitalRatio;
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( aSectorName, aRegionName, mTrackingMarketName, aRegionName);
}

void TrackingNonEnergyInput::initCalc( const gcamstr& aRegionName,
                               const gcamstr& aSectorName,
                               const bool aIsNewInvestmentPeriod,
                               const bool aIsTrade,
                               const IInfo* aTechInfo,
                               const int aPeriod )
{
    mIsActive = aTechInfo->getBoolean("new-vintage-tech", true);
    mPrevOutput = mIsActive ? aTechInfo->getDouble("prev-output-for-investment", 0.0) : 0.0;
    NonEnergyInput::initCalc(aRegionName, aSectorName, aIsNewInvestmentPeriod,
            aIsTrade, aTechInfo, aPeriod);
    
    // Ideally we only need to locate the market once, however during completeInit
    // all markets may have not yet been set up.  So, instead we avoid re-lookups
    // if the market has been found.  Unfortunately, this means if the market will
    // never be found we will continue to try to look it up each model period.
    if(!mCachedMarket.hasLocatedMarket()) {
        mCachedMarket = scenario->getMarketplace()->locateMarket( mTrackingMarketName, aRegionName );
    }
}

double TrackingNonEnergyInput::getPrice( const gcamstr& aRegionName,
                                 const int aPeriod ) const
{
    if(mIsActive) {
    const Modeltime* modeltime = scenario->getModeltime();
    double nonCapCost = mCost * (1.0 - mCapitalRatio);
    double adjInt = SectorUtils::calcPriceRatio(mCachedMarket, aRegionName, mTrackingMarketName, modeltime->getFinalCalibrationPeriod(), aPeriod) * mInterestRate;
    double fcr = FunctionUtils::calcFCR(adjInt, mPaybackYears);
    double adjCapCost = mOvernightCap * fcr;
    const_cast<TrackingNonEnergyInput*>(this)->mAdjustedCosts = nonCapCost + adjCapCost;
    }
    return mAdjustedCosts;
}

/*!
 * \brief A public stand alone method to calculate how a cost would change due to capital
 *        market price changes.
 * \details This method is required due to resource production which needs to bridge the
 *          gap between costs being captured in the supply curves but need to know how
 *          to adjust costs before we can look up those supply curves.  Note this implimentation
 *          is slightly different then getPrice since we can't use the precomputed mOvernightCap.
 * \param aCost The total cost to adjust for capital market prices.
 * \param aRegionName The region this input is contained in.
 * \param aPeriod The model period.
 * \return The adjusted cost from capital market price changes.
 */
double TrackingNonEnergyInput::calcAdjustedPrice(const double aCost,
                         const gcamstr& aRegionName,
                         const int aPeriod) const
{
    const Modeltime* modeltime = scenario->getModeltime();
    double nonCapCost = aCost * (1.0 - mCapitalRatio);
    double overnightCap = aCost / FunctionUtils::calcFCR(mInterestRate, mPaybackYears) * mCapitalRatio;
    double adjInt = SectorUtils::calcPriceRatio(aRegionName, mTrackingMarketName, modeltime->getFinalCalibrationPeriod(), aPeriod) * mInterestRate;
    double fcr = FunctionUtils::calcFCR(adjInt, mPaybackYears);
    double adjCapCost = overnightCap * fcr;
    return nonCapCost + adjCapCost;
}

void TrackingNonEnergyInput::setPhysicalDemand(const double aValue,
        const gcamstr& aRegionName,
        const int aPeriod )
{
    if(mIsActive && mName == gcamstr("resource-investment")) {
        // special case for resources due to the simultanies nature of needing to know
        // capital market price effects before being able to determine total cost
        // here mAdjustedCosts will be unadjusted for capital market effects
        mCapitalValue = mAdjustedCosts / FunctionUtils::calcFCR(mInterestRate, mPaybackYears) * aValue * mCapitalRatio;
        mCachedMarket.addToDemand(mTrackingMarketName, aRegionName, mCapitalValue, aPeriod, false);
    }
    else if(mIsActive) {
        double depreciation = mDepreciationRate * scenario->getModeltime()->gettimestep(aPeriod);
        mCapitalValue = std::max(aValue - mPrevOutput + mPrevOutput * depreciation, 0.0) * mOvernightCap * mInvestUnitConversion;
        mCachedMarket.addToDemand(mTrackingMarketName, aRegionName, mCapitalValue, aPeriod, false);

    }
}


