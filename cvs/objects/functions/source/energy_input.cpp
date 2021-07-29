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
 * \file energy_input.cpp
 * \ingroup Objects
 * \brief The EnergyInput class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cmath>

#include "functions/include/energy_input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "technologies/include/icapture_component.h"
#include "functions/include/icoefficient.h"
#include "functions/include/efficiency.h"
#include "functions/include/intensity.h"
#include "containers/include/iinfo.h"
#include "functions/include/function_utils.h"
#include "marketplace/include/cached_market.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

// static initialize.
const string EnergyInput::XML_REPORTING_NAME = "input-energy";

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
const string& EnergyInput::getXMLNameStatic() {
    const static string XML_NAME = "minicam-energy-input";
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
const string& EnergyInput::getXMLReportingName() const{
    return XML_REPORTING_NAME;
}

const string& EnergyInput::getXMLName() const{
    return getXMLNameStatic();
}

//! Constructor
EnergyInput::EnergyInput() :
    mCachedMarket( 0 )
{
    
    mCoefficient = 0;
    mPriceUnitConversionFactor = 1;
}

/*!
 * \brief Destructor.
 * \note An explicit constructor must be defined to avoid the compiler inlining
 *       it in the header file before the header file for the type contained in
 *       the auto_ptr is included.
 */
EnergyInput::~EnergyInput() {
    delete mCoefficient;
}

/*!
 * \brief Copy constructor.
 * \note This class requires a copy constructor because it has dynamically
 *          allocated memory.
 * \param aOther Energy input from which to copy.
 */
EnergyInput::EnergyInput( const EnergyInput& aOther )
{
    MiniCAMInput::copy( aOther );
    /*!
     * \warning Copying the coefficient here could break technical change.  We
     *          have done it this way because it is currently unused (coefficients
     *          are exogenously specified).  There should be some disscussion on
     *          on how we want to treat technical change with interpolated
     *          technologies.  Also note that when reconsidering this change we
     *          must also consider CCS and the way it makes adjustments to the
     *          coefficient.
     */
    mCoefficient = aOther.mCoefficient ? aOther.mCoefficient->clone() : 0;

    // Do not copy calibration values into the future
    // as they are only valid for one period.
    mName = aOther.mName;
    mIncomeElasticity = aOther.mIncomeElasticity;
    mTechChange = aOther.mTechChange;
    mPriceUnitConversionFactor = aOther.mPriceUnitConversionFactor;
    
    // copy keywords
    mKeywordMap = aOther.mKeywordMap;
    
    mMarketName = aOther.mMarketName;
}

EnergyInput* EnergyInput::clone() const {
    return new EnergyInput( *this );
}

bool EnergyInput::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

bool EnergyInput::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if( nodeName == Efficiency::getXMLNameStatic() ) {
        delete mCoefficient;
        mCoefficient = new Efficiency( XMLParseHelper::getValue<double>( aNode ) );
    }
    else if( nodeName == Intensity::getXMLNameStatic() ){
        delete mCoefficient;
        mCoefficient = new Intensity( XMLParseHelper::getValue<double>( aNode ) );
    }
    else if( nodeName == "flag" ) {
        setFlagsByName( XMLParseHelper::getValue<string>( aNode ) );
    }
    else {
        return false;
    }
    return true;
}

void EnergyInput::toDebugXML( const int aPeriod,
                               ostream& aOut,
                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    // Write out the coefficient if there is one.
    if( mCoefficient ){
        mCoefficient->toDebugXML( aPeriod, aOut, aTabs );
    }

    if( hasTypeFlag(IInput::RESOURCE) ) {
        XMLWriteElement( getFlagName(IInput::RESOURCE), "flag", aOut, aTabs );
    }
    else if( hasTypeFlag(IInput::BACKUP_ENERGY) ) {
        XMLWriteElement( getFlagName(IInput::BACKUP_ENERGY), "flag", aOut, aTabs );
    }
    
    XMLWriteElement( mIncomeElasticity, "income-elasticity", aOut, aTabs );
    XMLWriteElement( mCalibrationInput.isInited() ? mCalibrationInput.get() : -1,
                     "calibrated-value", aOut, aTabs );
    XMLWriteElement( mTechChange.isInited() ? mTechChange.get() : -1,
                     "tech-change", aOut, aTabs );
    XMLWriteElement( mAdjustedCoefficients[ aPeriod ], "current-coef", aOut, aTabs );
    XMLWriteElement( mCO2Coefficient.isInited() ? mCO2Coefficient.get() : -1,
                     "cached-co2-coef", aOut, aTabs );
    XMLWriteElement( mPhysicalDemand[ aPeriod ], "physical-demand", aOut, aTabs );
    //XMLWriteElement( mCarbonContent[ aPeriod ], "carbon-content", aOut, aTabs );
    XMLWriteElement( mPriceUnitConversionFactor, "price-unit-conversion", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void EnergyInput::completeInit( const string& aRegionName,
                                const string& aSectorName,
                                const string& aSubsectorName,
                                const string& aTechName,
                                const IInfo* aTechInfo )
{
    if( mMarketName.empty() ) {
        // Just a regional market.
        mMarketName = aRegionName;
    }
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( aSectorName, aRegionName, mName, mMarketName );

    initializeTypeFlags();
}

void EnergyInput::initCalc( const string& aRegionName,
                            const string& aSectorName,
                            const bool aIsNewInvestmentPeriod,
                            const bool aIsTrade,
                            const IInfo* aTechInfo,
                            const int aPeriod )
{
    // There must be a valid region name.
    assert( !aRegionName.empty() );

    mPhysicalDemand[ aPeriod ].set( 0 );// initialize to 0 shk
    // Initialize the C coefficient from the marketplace if one wasn't
    // explicitly provided during XML parse.
    if( !mCO2Coefficient.isInited() ) {
        mCO2Coefficient = FunctionUtils::getCO2Coef( mMarketName, mName, aPeriod );
    }

    // Set the coefficient for the current period if there is an explicit
    // coefficient read-in, or it was not initialized from the previous period.
    if( mCoefficient && !mAdjustedCoefficients[ aPeriod ].isInited() ){
        mAdjustedCoefficients[ aPeriod ] = mCoefficient->getCoefficient();
    }
    else if( !mAdjustedCoefficients[ aPeriod ].isInited() ){
        mAdjustedCoefficients[ aPeriod ] = 1;
    }
    
    mCachedMarket = scenario->getMarketplace()->locateMarket( mName, mMarketName, aPeriod );
}

/*! \brief Initialize the type flags.
 * \see setFlagsByName
 * \param aRegionName Name of the region.
 */
void EnergyInput::initializeTypeFlags() {
    
    // Set the flag to Energy.
    mTypeFlags |= IInput::ENERGY;
}

const string& EnergyInput::getMarketName( const string& aRegionName ) const {
    return mMarketName;
}

void EnergyInput::copyParam( const IInput* aInput,
                             const int aPeriod )
{
    aInput->copyParamsInto( *this, aPeriod );
}

void EnergyInput::copyParamsInto( EnergyInput& aInput,
                                  const int aPeriod ) const
{
    assert( aPeriod > 0 );
    // If the current input did not explicitly read in a coefficient, copy
    // forward the coefficient from the previous period. This results in any
    // technical change from the previous periods being applied.
    // TODO: This has some strange consequences. See the comment in copy.
    if( !aInput.mCoefficient ){
        aInput.mCoefficient = mCoefficient ? mCoefficient->clone() : new Intensity( 1 );
    }
}

double EnergyInput::getCO2EmissionsCoefficient( const string& aGHGName,
                                             const int aPeriod ) const
{
    // Check that the CO2 coefficient is initialized.
    assert( mCO2Coefficient.isInited() );
    return mCO2Coefficient;
}

double EnergyInput::getPhysicalDemand( const int aPeriod ) const {
    assert( mPhysicalDemand[ aPeriod ].isInited() );
    return mPhysicalDemand[ aPeriod ];
}

double EnergyInput::getCarbonContent( const int aPeriod ) const {
    return mPhysicalDemand[ aPeriod ] * mCO2Coefficient;
}

void EnergyInput::setPhysicalDemand( double aPhysicalDemand,
                                     const string& aRegionName,
                                     const int aPeriod )
{
    mPhysicalDemand[ aPeriod ].set( aPhysicalDemand );
    mCachedMarket->addToDemand( mName, mMarketName,
                                       mPhysicalDemand[ aPeriod ],
                                       aPeriod, true );
}

double EnergyInput::getCoefficient( const int aPeriod ) const {
    // Check that the coefficient has been initialized.
    assert( mAdjustedCoefficients[ aPeriod ].isInited() );

    return mAdjustedCoefficients[ aPeriod ];
}

void EnergyInput::setCoefficient( const double aCoefficient,
                                  const int aPeriod )
{
    // Coefficients must be positive. */
    assert( aCoefficient >= 0 );

    // Store the adjusted coefficient locally.
    mAdjustedCoefficients[ aPeriod ] = aCoefficient;
}

double EnergyInput::getPrice( const string& aRegionName,
                              const int aPeriod ) const
{
    return mPriceUnitConversionFactor *
        mCachedMarket->getPrice( mName, mMarketName, aPeriod );
}

void EnergyInput::setPrice( const string& aRegionName,
                            const double aPrice,
                            const int aPeriod )
{
    // Not hooking this up yet, it could work.
}

double EnergyInput::getCalibrationQuantity( const int aPeriod ) const
{
    // return -1 if no calibration value is read in
    return mCalibrationInput.isInited() ? mCalibrationInput.get() : -1;
}

double EnergyInput::getIncomeElasticity( const int aPeriod ) const {
    return mIncomeElasticity;
}

double EnergyInput::getPriceElasticity( const int aPeriod ) const {
    return 0;
}

double EnergyInput::getTechChange( const int aPeriod ) const
{
    return mTechChange;
}

void EnergyInput::doInterpolations( const int aYear, const int aPreviousYear,
                                    const int aNextYear, const IInput* aPreviousInput,
                                    const IInput* aNextInput )
{
    const EnergyInput* prevEneInput = static_cast<const EnergyInput*>( aPreviousInput );
    const EnergyInput* nextEneInput = static_cast<const EnergyInput*>( aNextInput );
    
    /*!
     * \pre We are given a valid EnergyInput for the previous input.
     */
    assert( prevEneInput );
    
    /*!
     * \pre We are given a valid EnergyInput for the next input.
     */
    assert( nextEneInput );
    
    // Interpolate the coefficient if it exisits in the previous technology.  It
    // may not if it was just the default in which case no interpolations are 
    // necessary.
    if( prevEneInput->mCoefficient ) {
        double newCoef = util::linearInterpolateY( aYear, aPreviousYear, aNextYear,
                                                   prevEneInput->mCoefficient->getCoefficient(),
                                                   nextEneInput->mCoefficient->getCoefficient() );
        delete mCoefficient;
        mCoefficient = new Intensity( newCoef );
    }
}


