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
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
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
using namespace xercesc;

extern Scenario* scenario;

//! Default Constructor
FoodDemandInput::FoodDemandInput()
{
}

//! Destructor
FoodDemandInput::~FoodDemandInput() {
}

void FoodDemandInput::XMLParse( const DOMNode* aNode ) {
    /*! \pre make sure we were passed a valid node. */
    assert( aNode );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    // get all child nodes.
    const DOMNodeList* nodeList = aNode->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() == DOMNode::TEXT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if ( nodeName == "base-service" ) {
            XMLHelper<Value>::insertValueIntoVector( curr, mFoodDemandQuantity, scenario->getModeltime() );
        }
        else if( nodeName == "scale-param" ) {
            mScaleParam = XMLHelper<Value>::getValue( curr );
        }
        else if( nodeName == "self-price-elasticity" ) {
            mSelfPriceElasticity = XMLHelper<Value>::getValue( curr );
        }
        else if( nodeName == "regional-bias" ) {
            mRegionalBias = XMLHelper<Value>::getValue( curr );
        }
        else if( !XMLDerivedClassParse( nodeName, curr ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLReportingName() << "." << endl;
        }
    }
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
    
    
    // TODO: hard wire some default prices for now since this really helps solution, what to do in the long run?
    for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ) {
        scenario->getMarketplace()->setPrice( trialShareMarketName, aRegionName, 0.1, period) ;
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
    mSubregionalPopulation[ aPeriod ] = aTechInfo->getDouble( "subregional-population", true );
    mCurrentSubregionalIncome = aTechInfo->getDouble( "subregional-income-ppp", true );
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
    return aType == getXMLReportingName();
}

//! Output debug info to XML
void FoodDemandInput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLReportingName(), aOut, aTabs, mName );

    XMLWriteElement( mFoodDemandQuantity[ aPeriod ], "service", aOut, aTabs );
    XMLWriteElement( mFoodDemandQuantity[ aPeriod ] / getAnnualDemandConversionFactor( aPeriod ), "food-demand-percap", aOut, aTabs );
    XMLWriteElement( mSubregionalPopulation[ aPeriod ], "subregional-population", aOut, aTabs );
    XMLWriteElement( mCurrentSubregionalIncome, "subregional-income-ppp", aOut, aTabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLReportingName(), aOut, aTabs );
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
    // Mcal/day (per capita).  We need to convert to Pcal/year (also per
    // capita).  This factor will be applied just before output.
    // Importantly, it will be applied *after* the budget fraction is
    // calculated.  In light of this, one might wonder why we didn't just
    // work in converted units all the way through.  It's because the food
    // demand model parameters were calibrated using *these* units, and
    // rejiggering to conform to GCAM unit conventions is a bit of a
    // pain.  Converting on input and again on output seems like the
    // least bad solution.
    aPhysicalDemand *= getAnnualDemandConversionFactor( aPeriod );
    
    // We are storing the results in the same vector as the calibration data
    // generally the calculated value should match however it may not if the
    // solver throws us negative prices.  We must explictly gaurd against
    // reseting these values in calibration years.
    if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        mFoodDemandQuantity[ aPeriod ].set( aPhysicalDemand );
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
    // The price units in the food demand model are kind of a mess.  Price
    // * Quantity should be in units of thousands of dollars per year.
    // Since Quantity is in Mcal/day (and we don't want to convert it to
    // Mcal/yr), that means we have to absorb the 365 day/year conversion
    // factor into the prices.
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
    //aVisitor->startVisitFoodDemandInput( this, aPeriod );
    //aVisitor->endVisitFoodDemandInput( this, aPeriod );
}

/*!
 * \brief Get the conversion factor to convert from the food demand untis to the
 *        annualized units the supply sectors expect.
 * \details The food demand system will calculate demands in term os Mcal / person / day
 *          the supply sectors are expecting Pcal / year.  This method provides the conversion..
 * \return The appropriate conversion factor.
 */
double FoodDemandInput::getAnnualDemandConversionFactor( const int aPeriod ) const {
    // Quantity conversion factor on output.  Output quantities are in
    // Mcal/day (per capita).  We need to convert to Pcal/year (also per
    // capita).  This factor will be applied just before output.
    // Importantly, it will be applied *after* the budget fraction is
    // calculated.  In light of this, one might wonder why we didn't just
    // work in converted units all the way through.  It's because the food
    // demand model parameters were calibrated using *these* units, and
    // rejiggering to conform to GCAM unit conventions is a bit of a
    // pain.  Converting on input and again on output seems like the
    // least bad solution.
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
    return SectorUtils::getTrialSupply( aRegionName, getTrialShareMarketName(), aPeriod );
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
 * \brief Get the scale term (A) for this food demand input.
 */
double FoodDemandInput::getScaleTerm() const {
    return mScaleParam * mRegionalBias;
}

/*!
 * \brief Calculate the self price exponent (e_ii(x))
 * \details The self price exponent is calculated as:
 *          e_ii = g_ii - alpha_i * f(x)
 * \param aAdjIncome The adjusted income (x).
 * \param aRegionName The region name used to look up the trial share market.
 * \param aPeriod The current model period.
 * \return The value fo the self price exponent equation.
 */
double FoodDemandInput::calcSelfPriceExponent( double aAdjIncome,
                                               const string& aRegionName,
                                               const int aPeriod ) const
{
    return mSelfPriceElasticity -
        getTrialShare( aRegionName, aPeriod ) * calcIncomeTermDerivative( aAdjIncome );
}

/*!
* \brief Calculate the cross price exponent (e_ij(x))
* \details The cross price exponent is calculated as:
*          e_ij = g_ij - alpha_j * f(x)
* \param aAdjIncome The adjusted income (x).
* \param aRegionName The region name used to look up the trial share market.
* \param aPeriod The current model period.
* \return The value fo the cross price exponent equation.
*/
double FoodDemandInput::calcCrossPriceExponent( const FoodDemandInput* aOther,
                                                double aAdjIncome,
                                                const string& aRegionName,
                                                const int aPeriod ) const
{
    return aOther->getCrossPriceElasticity( this, aRegionName, aPeriod ) -
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
    
bool StaplesFoodDemandInput::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    if( aNodeName == "cross-price-elasticity" ) {
        mCrossPriceElasticity = XMLHelper<Value>::getValue( aNode );
    }
    else if( aNodeName == "income-elasticity" ) {
        mIncomeElasticity = XMLHelper<Value>::getValue( aNode );
    }
    else if( aNodeName == "income-max-term" ) {
        mIncomeMaxTerm = XMLHelper<Value>::getValue( aNode );
    }
    else {
        return false;
    }
    return true;
}

/*!
 * \brief Get the cross price elasticity (g_ij)
 * \details The cross price elasticity for staples is given as a read in parameter.
 * \param aRegionName The region name used to look up the trial share market.
 * \param aPeriod The current model period.
 * \return The value for the cross price elasticity.
 */
double StaplesFoodDemandInput::getCrossPriceElasticity( const FoodDemandInput* aOther,
                                                        const string& aRegionName,
                                                        const int aPeriod ) const
{
    return mCrossPriceElasticity;
}

/*!
 *\brief Get the appropriate price scaler.
 */
double StaplesFoodDemandInput::getPriceScaler() const {
    // This scale factor below give us a little extra control over the shape of
    // the demand functions.  Unlike the other model parameters, we didn't fit
    // these; we fixed them ahead of time and then fit the other model
    // parameters subject to those assumed values.  As such, they shouldn't be
    // changed without re-fitting the model.  However, you shouldn't do that.
    // If we ever want to consider changing these, we should add a scale
    // parameter to the nonstaple demand and fit the model with that as an
    // additional parameter.  Note that although each the staple and non-staple
    // have their own values, only their ratio is significant.  Differences in
    // the absolute levels will be absorbed into Pm during the fitting process.
    const double psscl = 100.0;
    return psscl;
}

/*!
 * \brief Calculate the income term (x^h(x)).
 * \details The income term for staples is calculated as:
 *          Qi = x^(lam/x)*(1+kappa / ln(x))) * scale
 *          where scale is calculated such that Qi(1) == 1
 * \param aAdjIncome The adjusted income at which to calculate this term (x).
 * \return The income term to use in the food demand system for a staple good.
 */
double StaplesFoodDemandInput::calcIncomeTerm( double aAdjIncome ) const {
    double k = exp( mIncomeMaxTerm );        // k-value from the R version of the model
    // TODO: worry about the follow given we are unlikey to get such small incomes in GCAM:
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
* \brief Calculate the derivative of thre income term f(x).
* \details The derivative of the income term for staples is calculated as:
*          f(x) = lam * (1 - log(e^kappa)) / x
* \param aAdjIncome The adjusted income at which to calculate this term (x).
* \return The derivative of the income term to use in the food demand system for a staple good.
*/
double StaplesFoodDemandInput::calcIncomeTermDerivative( double aAdjIncome ) const {
    double k = exp( mIncomeMaxTerm );        // k-value from the R version of the model
    // TODO: worry about the follow given we are unlikey to get such small incomes in GCAM:
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

IInput* NonStaplesFoodDemandInput::clone() const {
    NonStaplesFoodDemandInput* clone = new NonStaplesFoodDemandInput();
    clone->copy( *this );
    return clone;
}

void NonStaplesFoodDemandInput::copy( const NonStaplesFoodDemandInput& aInput ) {
    mIncomeElasticity = aInput.mIncomeElasticity;
    FoodDemandInput::copy( aInput );
}
    
bool NonStaplesFoodDemandInput::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    if( aNodeName == "income-elasticity" ) {
        mIncomeElasticity = XMLHelper<Value>::getValue( aNode );
    }
    else {
        return false;
    }
    return true;
}

/*!
* \brief Get the cross price elasticity (g_ij)
* \details The cross price elasticity for non-staples is not a free parameter and it can be derived as:
               g_nonstaples = (alpha_nonstaples / alpha_staples) * g_staples
* \param aRegionName The region name used to look up the trial share market.
* \param aPeriod The current model period.
* \return The value for the cross price elasticity.
*/
double NonStaplesFoodDemandInput::getCrossPriceElasticity( const FoodDemandInput* aOther,
                                                          const string& aRegionName,
                                                          const int aPeriod ) const
{
    // Get the trial budget fractions.  These will be used to calculate
    // price exponents in the demand equations.
    double alphas = aOther->getTrialShare( aRegionName, aPeriod );
    double alphan = getTrialShare( aRegionName, aPeriod );

    double amin = 0.1;          // For stability, we limit how small the alphas
                                // can be when calculating the condition for the
                                // cross-elasticity
    return std::max(alphan, amin) / std::max(alphas, amin) * aOther->getCrossPriceElasticity( this, aRegionName, aPeriod );
}

/*!
*\brief Get the appropriate price scaler.
*/
double NonStaplesFoodDemandInput::getPriceScaler() const {
    // This scale factor below give us a little extra control over the shape of
    // the demand functions.  Unlike the other model parameters, we didn't fit
    // these; we fixed them ahead of time and then fit the other model
    // parameters subject to those assumed values.  As such, they shouldn't be
    // changed without re-fitting the model.  However, you shouldn't do that.
    // If we ever want to consider changing these, we should add a scale
    // parameter to the nonstaple demand and fit the model with that as an
    // additional parameter.  Note that although each the staple and non-staple
    // have their own values, only their ratio is significant.  Differences in
    // the absolute levels will be absorbed into Pm during the fitting process.
    const double pnscl = 20.0;
    return pnscl;
}

/*!
* \brief Calculate the income term (x^h(x)).
* \details The income term for non-staples is calculated as:
*          Qi = x^(nu / (1 - x)) * scale
*          where scale is calculated such that Qi(1) == 1
* \param aAdjIncome The adjusted income at which to calculate this term (x).
* \return The income term to use in the food demand system for a non-staple good.
*/
double NonStaplesFoodDemandInput::calcIncomeTerm( double aAdjIncome ) const {
    double enu = exp(-mIncomeElasticity);
    double delta = 1.0-aAdjIncome;
    double delta2 = delta*delta;
    double scale = 1.0 / enu;   // normalization factor
    double qin;

    // TODO: worry about the follow given we are unlikey to get such small incomes in GCAM:
    if(fabs(delta) > 1.0e-3/mIncomeElasticity) {
        qin = scale * pow(aAdjIncome, mIncomeElasticity / delta);
    }
    else {
        // Represent q and eta near x==1 as a Taylor series
        qin = scale * (enu -
                       0.5 * mIncomeElasticity * enu * delta +
                       1.0/24.0 * enu * mIncomeElasticity*(3.0*mIncomeElasticity-8.0) * delta2);
    }
    return qin;
}

/*!
* \brief Calculate the derivative of thre income term f(x).
* \details The derivative of the income term for non-staples is calculated as:
*          f(x) = 1/(1-x) + x * log(x) / (1-x)^2
* \param aAdjIncome The adjusted income at which to calculate this term (x).
* \return The derivative of the income term to use in the food demand system for a non-staple good.
*/
double NonStaplesFoodDemandInput::calcIncomeTermDerivative( double aAdjIncome ) const {
    double delta = 1.0-aAdjIncome;
    double delta2 = delta*delta;
    double etan;

    // TODO: worry about the follow given we are unlikey to get such small incomes in GCAM:
    if(fabs(delta) > 1.0e-3/mIncomeElasticity) {
        // lim_x->0 etan = 1
        etan = aAdjIncome < 1.0e-4 ? 1 : 1/delta + aAdjIncome*log(aAdjIncome)/delta2;
    }
    else {
        // Represent q and eta near x==1 as a Taylor series
        double delta3 = delta2*delta;
        etan = mIncomeElasticity * (0.5 +
                     1.0/6.0 * delta +
                     1.0/12.0 * delta2 +
                     1.0/20.0 * delta3);
    }
    return etan;
}
