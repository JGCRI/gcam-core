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
* \file food_demand_input.cpp
* \ingroup Objects
* \brief The FoodDemandInput class source file.
* \author Pralit Patel
* \author Robert Link
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <cmath>

#include "functions/include/food_demand_input.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/market_dependency_finder.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/function_utils.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
FoodDemandInput::FoodDemandInput()
{
}

//! Destructor
FoodDemandInput::~FoodDemandInput() {
}

void FoodDemandInput::completeInit( const string& aRegionName,
                             const string& aSectorName,
                             const string& aSubsectorName,
                             const string& aTechName,
                             const IInfo* aTechInfo)
{
    // Indicate that this sector depends on the service this input represents.
    // Note tech name is the name of the consumer which in GCAM is called directly
    // and so should be the name used in dependency tracking.
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( aTechName, aRegionName, mName, aRegionName );
    
    // Set up trial share markets
    string trialShareMarketName = SectorUtils::getTrialMarketName( getTrialShareMarketName() );
    bool isNew = SectorUtils::createTrialSupplyMarket( aRegionName, getTrialShareMarketName(),
                                                       "unitless", aRegionName );
    if( !isNew ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Created duplicate trial share market in " << aRegionName << " for "
                << trialShareMarketName << endl;
    }
    
    
    for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ) {
        // We set some initial trial share guess to ensure the solver starts in a
        // reasonable range which helps speed up solution in the historical years.
        // We could attempt to do better by calculating what the actual shares would
        // be in the data system and read them in.  However, it turned out to be fairly
        // involved to do and ultimately didn't help with solution much so we just stuck
        // with the constant initial guess.
        const double INITIAL_PRICE_GUESS = 0.1;
        scenario->getMarketplace()->setPrice( trialShareMarketName, aRegionName, INITIAL_PRICE_GUESS, period) ;
        // Set meta data to let the solver know the trial share values should be between 0 and 1
        // not that it explicitly respects this but the preconditioner will.
        SectorUtils::setSupplyBehaviorBounds( trialShareMarketName, aRegionName, 0, 1, period );
    }
    
    depFinder->addDependency( aTechName, aRegionName, trialShareMarketName, aRegionName );
    
}

void FoodDemandInput::initCalc( const string& aRegionName,
                         const string& aSectorName,
                         const bool aIsNewInvestmentPeriod,
                         const bool aIsTrade,
                         const IInfo* aTechInfo,
                         const int aPeriod )
{
    /*! \pre There must be a valid region name. */
    assert( !aRegionName.empty() );
    
    // Get the subregional population and income from the info object which is where
    // the consumer stored them.
    // We need to save these values here as we won't have access to the socioeconomic
    // drivers when calculating demands. We will need to income to run the food demand
    // equations and the population to convert from per capita demands to total.
    mSubregionalPopulation[ aPeriod ] = aTechInfo->getDouble( "subregional-population", true );
    mCurrentSubregionalIncome = aTechInfo->getDouble( "subregional-income-ppp", true );
    
    if( aPeriod == ( scenario->getModeltime()->getFinalCalibrationPeriod() + 1 ) ) {        
        // Fill in regional bias values for future model periods which may just copy
        // forward the calibrated value if no values were parsed or linearly converge
        // them otherwise.
        SectorUtils::fillMissingPeriodVectorInterpolated( mRegionalBias );
    }
}

void FoodDemandInput::copyParam( const IInput* aInput,
                          const int aPeriod )
{
    /*!
     * \warning The ability to copyParams has been left unimplemented for GCAM consumers.
     */
    assert( false );
}

void FoodDemandInput::copy( const FoodDemandInput& aInput ) {
    mName = aInput.mName;
    mFoodDemandQuantity = aInput.mFoodDemandQuantity;
    mScaleParam = aInput.mScaleParam;
    mSelfPriceElasticity = aInput.mSelfPriceElasticity;
    mRegionalBias = aInput.mRegionalBias;
}

bool FoodDemandInput::isSameType( const string& aType ) const {
    return aType == getXMLName();
}

//! Output debug info to XML
void FoodDemandInput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    XMLWriteElement( mFoodDemandQuantity[ aPeriod ], "service", aOut, aTabs );
    XMLWriteElement( mFoodDemandQuantity[ aPeriod ] / getAnnualDemandConversionFactor( aPeriod ), "food-demand-percap", aOut, aTabs );
    XMLWriteElement( mSubregionalPopulation[ aPeriod ], "subregional-population", aOut, aTabs );
    XMLWriteElement( mCurrentSubregionalIncome, "subregional-income-ppp", aOut, aTabs );
    XMLWriteElement( mRegionalBias[ aPeriod ], "regional-bias", aOut, aTabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

//! Get the name of the input
const string& FoodDemandInput::getName() const {
    return mName;
}

/*! 
 * \brief Get the Physical Demand.
 * \param aPeriod Model period.
 * \return Physical demand.
 */
double FoodDemandInput::getPhysicalDemand( const int aPeriod ) const {
    /*!
     * \pre The service demand has been calculated for this period.
     * \note The buildings model does not run in period 0 so we will
     *       allow an uninitialized value for that period.
     */
    assert( aPeriod == 0 || mFoodDemandQuantity[ aPeriod ].isInited() );
    
    return mFoodDemandQuantity[ aPeriod ];
}

//! Set Physical Demand.
void FoodDemandInput::setPhysicalDemand( double aPhysicalDemand, const string& aRegionName, const int aPeriod )
{
    // Quantity conversion factor on output.  Output quantities are in
    // Kcal/day (per capita).  We need to convert to Pcal/year (also per
    // capita).  This factor will be applied just before output.
    // We didn't just work in converted units all the way through because
    // the food demand model parameters were calibrated using *these* units.
    aPhysicalDemand *= getAnnualDemandConversionFactor( aPeriod );
    
    // We are storing the results in the same vector as the calibration data
    // generally the calculated value should match however it may not if the
    // solver throws us negative prices.  We must explictly gaurd against
    // reseting these values in calibration years.
    if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        mFoodDemandQuantity[ aPeriod ].set( aPhysicalDemand );
    }
    else {
        // In the calibration periods we calibrate the regional bias the
        // difference between the actual demand versus the modeled value.
        // Note the regional bias will be applied prior to annualization so
        // we need to convert the difference back from Pcal/year to
        // Kcal/day (per capita)
        mRegionalBias[ aPeriod ] = ( mFoodDemandQuantity[ aPeriod ] - aPhysicalDemand ) / getAnnualDemandConversionFactor( aPeriod );
    }
    
    scenario->getMarketplace()->addToDemand( mName, aRegionName,
        mFoodDemandQuantity[ aPeriod ], aPeriod );
}

/*!
 * \brief Return the market price, or unadjusted price, for the building service.
 * \param aRegionName Region containing the input.
 * \param aPeriod Period to find the price in.
 * \return The market or unadjusted price.
 */
double FoodDemandInput::getPrice( const string& aRegionName, const int aPeriod ) const {
    // Price conversion factor.  Input prices are in 1975$ per Mcal.
    // In the food demand equations the Price * Quantity should be in units of
    // thousands of dollars per year. Since Quantity is in Mcal/day, that means
    // we have to absorb the 365 day/year conversion factor into the prices.
    const double priceUnitConversionFactor =
        0.365 / FunctionUtils::DEFLATOR_1975_PER_DEFLATOR_2005();
    return scenario->getMarketplace()->getPrice( mName, aRegionName, aPeriod ) *
        priceUnitConversionFactor;
}

void FoodDemandInput::setPrice( const string& aRegionName,
                         const double aPrice,
                         const int aPeriod )
{
    // The service price is set by the supply sector and so can not be set here.
}

/*! \brief Returns the price paid for each FoodDemandInput.
 * \param aRegionName Name of the containing region.
 * \param aPeriod Model period.
 */
double FoodDemandInput::getPricePaid( const string& aRegionName, const int aPeriod ) const{
    return getPrice( aRegionName, aPeriod );
}

/*! \brief Set the price paid for each FoodDemandInput.
 *
 * \param aPricePaid new price paid value
 * \param aPeriod Model period.
 */
void FoodDemandInput::setPricePaid( double aPricePaid, const int aPeriod ) {
    // The service price is set by the supply sector and so can not be set here.
}

bool FoodDemandInput::hasTypeFlag( const int aTypeFlag ) const {
    return false;
}

void FoodDemandInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitFoodDemandInput( this, aPeriod );
    aVisitor->endVisitFoodDemandInput( this, aPeriod );
}

/*!
 * \brief Get the conversion factor to convert from the food demand units to the
 *        annualized units the supply sectors expect.
 * \details The food demand system will calculate demands in term of Kcal / person / day
 *          the supply sectors are expecting Pcal / year.  This method provides the conversion.
 * \return The appropriate conversion factor.
 */
double FoodDemandInput::getAnnualDemandConversionFactor( const int aPeriod ) const {
    // Quantity conversion factor on output.  Output quantities are in
    // Kcal/day (per capita).  We need to convert to Pcal/year (also per
    // capita).  This factor will be applied just before output.
    // We didn't just work in converted units all the way through because
    // the food demand model parameters were calibrated using *these* units.
    const double quantityUnitConversionFactor = 365.0 * 1e-9;
    return quantityUnitConversionFactor * mSubregionalPopulation[ aPeriod ] * 1000.0;
}

/*!
 * \brief Get the currently set Subregional income.  Note that this
 *          is the PPP per capita income.
 * \return Subregional income that has been set from
 *           the consumer.
 */
Value FoodDemandInput::getSubregionalIncome() const {
    return mCurrentSubregionalIncome;
}

/*!
 * \brief Generates an appropriate name to use for the trial share market name.
 */
std::string FoodDemandInput::getTrialShareMarketName() const {
    return mName + "-budget-fraction";
}

/*!
 * \brief Get the current trial share for this food demand input.
 * \details Retrieves the current trial share value from the marketplace.
 * \param aRegionName The region name used to look up the market.
 * \param aPeriod The current model period.
 * \return The trial share.
 */
double FoodDemandInput::getTrialShare( const string& aRegionName,
                                       const int aPeriod ) const
{
    // ensure the trial share is between zero and one
    return std::min(std::max( SectorUtils::getTrialSupply( aRegionName, getTrialShareMarketName(), aPeriod ), 0.0), 1.0);
}

/*!
 * \brief Set the actual share for this food demand input.
 * \details Updates the trial value market to let it know what the actual share value was.
 * \param aRegionName The region name used to look up the market.
 * \param aPeriod The current model period.
 */
void FoodDemandInput::setActualShare( double aShare,
                                      const string& aRegionName,
                                      const int aPeriod )
{
    mShare[ aPeriod ] = aShare;
    
    SectorUtils::addToTrialDemand( aRegionName, getTrialShareMarketName(),
                                   mShare[ aPeriod ], aPeriod );
}

/*!
 * \brief Get the additive regional bias correction.
 * \details The regional bias is an additive calibration term.
 * \param aPeriod The current model period.
 * \return The value for the current regional bias value.
 */
double FoodDemandInput::getRegionalBias( const int aPeriod ) const {
    // In the calibration periods we calibrate the regional bias
    // so it should not be applied here.
    return aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ?
        0.0 : mRegionalBias[ aPeriod ];
}

/*!
 * \brief Get the scale parameter (A) for this food demand input.
 * \details This just gets the multiplicative term in the food demand equation.
 */
double FoodDemandInput::getScaleParam() const {
    return mScaleParam;
}

/*!
 * \brief Calculate the cross price exponent (e_ij(x))
 * \details The cross price exponent is calculated as:
 *          e_ij = g_ij - alpha_j * f(x)
 *          Where:
 *          g_ij: The price elasticity; either self or cross as appropriate for the type of aOther.
 *          alpha_j: The share of total budget for j.
 *          f(x): The derivative of the income term, \sa calcIncomeTermDerivative
 * \param aOther The instance of FoodDemandInput for j.
 * \param aAdjIncome The adjusted income (x).
 * \param aRegionName The region name used to look up the trial share market.
 * \param aPeriod The current model period.
 * \return The value for the price exponent equation.
 */
double FoodDemandInput::calcPriceExponent( const FoodDemandInput* aOther,
                                           double aAdjIncome,
                                           const string& aRegionName,
                                           const int aPeriod ) const
{
    return getPriceElasticity( aOther, aRegionName, aPeriod ) -
        aOther->getTrialShare( aRegionName, aPeriod ) * calcIncomeTermDerivative( aAdjIncome );
}

StaplesFoodDemandInput::StaplesFoodDemandInput()
{
}

StaplesFoodDemandInput::~StaplesFoodDemandInput() {
}

const string& StaplesFoodDemandInput::getXMLNameStatic() {
    const static string XML_NAME = "staples-food-demand-input";
    return XML_NAME;
}

const string& StaplesFoodDemandInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

const string& StaplesFoodDemandInput::getXMLName() const {
    return getXMLNameStatic();
}

IInput* StaplesFoodDemandInput::clone() const {
    StaplesFoodDemandInput* clone = new StaplesFoodDemandInput();
    clone->copy( *this );
    return clone;
}

void StaplesFoodDemandInput::copy( const StaplesFoodDemandInput& aInput ) {
    mCrossPriceElasticity = aInput.mCrossPriceElasticity;
    mIncomeElasticity = aInput.mIncomeElasticity;
    mIncomeMaxTerm = aInput.mIncomeMaxTerm;
    FoodDemandInput::copy( aInput );
}
    
/*!
 * \brief Get the price elasticity (g_ij)
 * \details If this == aOther then returns self price elasticity otherwise the cross both of which are just read in from input.
 * \param aOther The instance of FoodDemandInput for j.
 * \param aRegionName The region name used to look up the trial share market.
 * \param aPeriod The current model period.
 * \return The value for the price elasticity.
 */
double StaplesFoodDemandInput::getPriceElasticity( const FoodDemandInput* aOther,
                                                   const string& aRegionName,
                                                   const int aPeriod ) const
{
    return this == aOther ? mSelfPriceElasticity : mCrossPriceElasticity;
}

/*!
 *\brief Get the appropriate price scaler.
 */
double StaplesFoodDemandInput::getPriceScaler() const {
    // This scale factor alters the price of food from what is specified in
    // the marketplace. The adjusted price is equal to the market price divided by
    // the price of material times this price scaler.
    // The scaler affects the shape of the demand curve. Different values
    // for this scaler will alter the amount of food consumed as income increases
    // for a given set of prices. The value below was estimated in the
    // offline food demand model; however, the prior specified in that model
    // has a strong effect on the resulting values of the price scalers.
    // The priors chosen here maximized the likelihood over the set of priors
    // tested; however, these were not systematically tested.
    // Note that if this number is changed the food demand parameters
    // would need to be reestimated. Also note that this scaler is both
    // read into the data system and specified here.
    // Both the absolute value and the ratio to the non-staple scaler
    // affect food demand.
    const double psscl = 100.0;
    return psscl;
}

/*!
 * \brief Calculate the income term (x^h(x)).
 * \details The income term for staples is calculated as:
 *          Qi = x^(lam/x)*(1+kappa / ln(x))) * scale
 *          Where:
 *          x: adjusted income
 *          lam: income elasticity
 *          kappa: income at which the maximum quantity is demanded
 *          scale: calculated such that at an income level of 1 the above equation returns 1; Qi(1) = 1.
 * \param aAdjIncome The adjusted income at which to calculate this term (x).
 * \return The income term to use in the food demand system for a staple good.
 */
double StaplesFoodDemandInput::calcIncomeTerm( double aAdjIncome ) const {
    double k = exp( mIncomeMaxTerm );        // k-value from the R version of the model
    // The limit as x-> 0 of the logarithmic derivative of this function is not
    // well behaved.  However, the quantity is very small for k*x < ~1e-3, so
    // we can replace this segment with a linear ramp and get essentially the same
    // behavior.  The parameters below facilitate that.
    double x1 = 1.0e-3 / k;
    double scale = pow(k, -mIncomeElasticity); // Normalization factor so that Qi(1) == 1
    double qis;
    
    if(aAdjIncome > x1) {
        qis = scale * pow(k*aAdjIncome, mIncomeElasticity/aAdjIncome);
    }
    else {
        qis = scale * pow(k*x1, mIncomeElasticity/x1) / x1 * aAdjIncome;
    }
    
    return qis;
}

/*!
 * \brief Calculate the derivative of the income term f(x).
 * \details The derivative of the income term for staples is calculated as:
 *          f(x) = lam * (1 - log(x*e^kappa)) / x
 * \param aAdjIncome The adjusted income at which to calculate this term (x).
 * \return The derivative of the income term to use in the food demand system for a staple good.
 */
double StaplesFoodDemandInput::calcIncomeTermDerivative( double aAdjIncome ) const {
    double k = exp( mIncomeMaxTerm );        // k-value from the R version of the model
    // The limit as x-> 0 of the logarithmic derivative of this function is not
    // well behaved.  However, the quantity is very small for k*x < ~1e-3, so
    // we can replace this segment with a linear ramp and get essentially the same
    // behavior.  The parameters below facilitate that.
    double x1 = 1.0e-3 / k;
    double etas;
    
    if(aAdjIncome > x1) {
        etas = mIncomeElasticity*(1-log(k*aAdjIncome)) / aAdjIncome;
    }
    else {
        etas = 1.0;
    }
    return etas;
}

NonStaplesFoodDemandInput::NonStaplesFoodDemandInput()
{
}

NonStaplesFoodDemandInput::~NonStaplesFoodDemandInput() {
}

const string& NonStaplesFoodDemandInput::getXMLNameStatic() {
    const static string XML_NAME = "non-staples-food-demand-input";
    return XML_NAME;
}

const string& NonStaplesFoodDemandInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

const string& NonStaplesFoodDemandInput::getXMLName() const {
    return getXMLNameStatic();
}

IInput* NonStaplesFoodDemandInput::clone() const {
    NonStaplesFoodDemandInput* clone = new NonStaplesFoodDemandInput();
    clone->copy( *this );
    return clone;
}

void NonStaplesFoodDemandInput::copy( const NonStaplesFoodDemandInput& aInput ) {
    mIncomeElasticity = aInput.mIncomeElasticity;
    FoodDemandInput::copy( aInput );
}

/*!
 * \brief Get the price elasticity (g_ij)
 * \details If this == aOther then returns mSelfPriceElasticity which is just read in from input otherwise calculate
 *          g_nonstaples = (alpha_nonstaples / alpha_staples) * g_staples
 * \param aOther The instance of FoodDemandInput for j.
 * \param aRegionName The region name used to look up the trial share market.
 * \param aPeriod The current model period.
 * \return The value for the price elasticity.
 */
double NonStaplesFoodDemandInput::getPriceElasticity( const FoodDemandInput* aOther,
                                                      const string& aRegionName,
                                                      const int aPeriod ) const
{
    if( this == aOther ) {
        return mSelfPriceElasticity;
    }
    else {
        // Get the trial budget fractions.  These will be used to calculate
        // price exponents in the demand equations.
        double alphaStaple = aOther->getTrialShare( aRegionName, aPeriod );
        double alphaNonstaple = getTrialShare( aRegionName, aPeriod );

        double alphaMin = 0.1;      // For stability, we limit how small the alphas
                                    // can be when calculating the condition for the
                                    // cross-elasticity
        return std::max(alphaNonstaple, alphaMin) / std::max(alphaStaple, alphaMin) * aOther->getPriceElasticity( this, aRegionName, aPeriod );
    }
}

/*!
 *\brief Get the appropriate price scaler.
 */
double NonStaplesFoodDemandInput::getPriceScaler() const {
    // This scale factor alters the price of food from what is specified in
    // the marketplace. The adjusted price is equal to the market price divided by
    // the price of material times this price scaler.
    // The scaler affects the shape of the demand curve. Different values
    // for this scaler will alter the amount of food consumed as income increases
    // for a given set of prices. The value below was estimated in the
    // offline food demand model; however, the prior specified in that model
    // has a strong effect on the resulting values of the price scalers.
    // The priors chosen here maximized the likelihood over the set of priors
    // tested; however, these were not systematically tested.
    // Note that if this number is changed the food demand parameters
    // would need to be reestimated. Also note that this scaler is both
    // read into the data system and specified here.
    // Both the absolute value and the ratio to the staple scaler
    // affect food demand.
    const double pnscl = 20.1;
    return pnscl;
}

/*!
 * \brief Calculate the income term (x^h(x)).
 * \details The income term for non-staples is calculated as:
 *          Qi = x^(nu / (1 - x)) * scale
 *          Where:
 *          x: adjusted income
 *          nu: income elasticity
 *          scale: calculated such that at an income level of 1 the above equation returns 1; Qi(1) = 1.
 * \param aAdjIncome The adjusted income at which to calculate this term (x).
 * \return The income term to use in the food demand system for a non-staple good.
 */
double NonStaplesFoodDemandInput::calcIncomeTerm( double aAdjIncome ) const {
    double enu = exp(-mIncomeElasticity);
    double delta = 1.0-aAdjIncome;
    double scale = 1.0 / enu;   // normalization factor
    double qin;

    if(fabs(delta) > 1.0e-3/mIncomeElasticity) {
        qin = scale * pow(aAdjIncome, mIncomeElasticity / delta);
    }
    else {
        // Represent q near x==1 as a Taylor series
        qin = scale * (enu -
                       0.5 * mIncomeElasticity * enu * delta +
                       1.0/24.0 * enu * mIncomeElasticity*(3.0*mIncomeElasticity-8.0) * pow(delta, 2));
    }
    return qin;
}

/*!
 * \brief Calculate the derivative of the income term f(x).
 * \details The derivative of the income term for non-staples is calculated as:
 *          f(x) = 1/(1-x) + x * log(x) / (1-x)^2
 * \param aAdjIncome The adjusted income at which to calculate this term (x).
 * \return The derivative of the income term to use in the food demand system for a non-staple good.
 */
double NonStaplesFoodDemandInput::calcIncomeTermDerivative( double aAdjIncome ) const {
    double delta = 1.0-aAdjIncome;
    double etan;

    if(fabs(delta) > 1.0e-3/mIncomeElasticity) {
        // lim_x->0 etan = 1
        etan = aAdjIncome < 1.0e-4 ? 1 : 1/delta + aAdjIncome*log(aAdjIncome)/pow(delta, 2);
    }
    else {
        // Represent the income term derivative near x==1 as a Taylor series
        etan = mIncomeElasticity * (0.5 +
                     1.0/6.0 * delta +
                     1.0/12.0 * pow(delta, 2) +
                     1.0/20.0 * pow(delta, 3));
    }
    return etan;
}

