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
* \file nested_ces_production_function_macro.cpp
* \ingroup Objects
* \brief The NestedCESProductionFunction class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cmath>
#include <cassert>

#include "functions/include/nested_ces_production_function_macro.h"
#include "containers/include/national_account.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/util.h"
#include "util/base/include/model_time.h"
#include "functions/include/function_utils.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/configuration.h"
#include "containers/include/market_dependency_finder.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"

using namespace std;
using namespace objects;

extern Scenario* scenario;

/*!
 * \brief Currency conversions used through out macro calculations.
 * \details GCAM utils are billion 1975$ and macro uses million 1990$
 * \return The conversion from billion 1975$ to million 1990$
 */
inline double CURRENCY_CONVERSION() {
    return 1000.0 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975();
}


//! Default Constructor
FactorInputLeaf::FactorInputLeaf():
mIsLabor(false),
mIsCapital(false),
mIsEnergy(false),
mProductivity(Value(1.0)),
mScaler(0),
mOutputMrkName("energy service")
{
}

//! Get the name of the NodeInput
const gcamstr& FactorInputLeaf::getName() const {
    return mName;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author Sonny Kim
 * \return The constant XML_NAME as a static.
 */
const gcamstr& FactorInputLeaf::getXMLNameStatic() {
    static const gcamstr XML_NAME = "factor-input-leaf";
    return XML_NAME;
}

const gcamstr& FactorInputLeaf::getXMLName() const {
    return getXMLNameStatic();
}

//! complete initializations
void FactorInputLeaf::completeInit( const gcamstr& aRegionName, const gcamstr& aGDPActName ){
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency(aGDPActName, aRegionName, mOutputMrkName, aRegionName);
}

//! complete initializations for each period
void FactorInputLeaf::initCalc(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod){
    const Modeltime* modeltime = scenario->getModeltime();
    if(mIsLabor || mIsCapital) {
        if(aPeriod <= modeltime->getFinalCalibrationPeriod()) {
            // ensure the price driven inputs (capital and labor) are not solved in the
            // calibration periods and are marked "fully-calibrated" so that we can be
            // sure to use our calibration prices
            Marketplace* marketplace = scenario->getMarketplace();
            marketplace->unsetMarketToSolve( mOutputMrkName, aRegionName, aPeriod );
            IInfo* marketInfo = marketplace->getMarketInfo( mOutputMrkName, aRegionName, aPeriod, true );
            const gcamstr FULLY_CAL_KEY("fully-calibrated");
            marketInfo->setBoolean( FULLY_CAL_KEY, true );
        }
        
        // set supply curve bounds
        double upperbound = mIsCapital ? 1.0 : util::getLargeNumber();
        SectorUtils::setSupplyBehaviorBounds(mOutputMrkName, aRegionName, 0.0, upperbound, aPeriod);
    }
}

void FactorInputLeaf::updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod) {
    Marketplace* marketplace = scenario->getMarketplace();
    if(mIsLabor) {
        mDemand = getCalibrationQuantity(aRegionName, aNationalAccount, aPeriod);
        marketplace->addToDemand(mOutputMrkName, aRegionName, mDemand, aPeriod);
    }
    // going to leave capital as zero as there is no way to know the gcam-investment
    // and so we could not attempt to balance the market
}

double FactorInputLeaf::getCalShares(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const int aPeriod)
{
    double baseValue;
    if(mIsLabor) {
        baseValue = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_LABOR_WAGES);
    }
    else if(mIsCapital) {
        baseValue = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_CAPITAL_VALUE);
    }
    else if(mIsEnergy) {
        baseValue = -1.0;
    }
    else {
        const Marketplace* marketplace = scenario->getMarketplace();
        baseValue = marketplace->getPrice(mOutputMrkName, aRegionName, aPeriod)
                    * CURRENCY_CONVERSION();
    }
    return baseValue / aGrossOutput;
}

double FactorInputLeaf::getCalibrationQuantity(const gcamstr& aRegionName,
                                       NationalAccount* aNationalAccount,
                                       int aPeriod) const
{
    double quantity;
    if(mIsLabor) {
        quantity = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_LABOR_FORCE);
    }
    else if(mIsCapital) {
        quantity = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_CAPITAL_STOCK);
    }
    else {
        Marketplace* marketplace = scenario->getMarketplace();
        quantity = marketplace->getPrice(mOutputMrkName, aRegionName, aPeriod)
                    * CURRENCY_CONVERSION();
    }
    return quantity;
}

void FactorInputLeaf::calcCoef(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const double aShareAdj, const double aSlackShare, const double aParentValue, const double aParentPrice, const double aGamma, const int aPeriod)
{
    Marketplace* marketplace = scenario->getMarketplace();
    double value = mIsEnergy ? aSlackShare * aGrossOutput :
        (getCalShares(aRegionName, aNationalAccount, aGrossOutput, aPeriod) * aShareAdj) * aGrossOutput;
    double quantity = getCalibrationQuantity(aRegionName, aNationalAccount, aPeriod);

    double price = value / quantity;
    if(mIsLabor || mIsCapital) {
        marketplace->setPrice(mOutputMrkName, aRegionName, price, aPeriod);
    }
    double share = value / aParentValue;
    mScaler = pow(share, (-1.0/aGamma)) * price / aParentPrice;
}

double FactorInputLeaf::getCoefficient(const double aTFP, const int aPeriod) const {
    return mScaler * mProductivity[aPeriod] * aTFP;
}

double FactorInputLeaf::getPrice(const gcamstr& aRegionName, const double aTFP, const int aPeriod) {
    assert(mIsLabor || mIsCapital);
    const Marketplace* marketplace = scenario->getMarketplace();
    
    double price = marketplace->getPrice(mOutputMrkName, aRegionName, aPeriod);
    return max( price, SectorUtils::getDemandPriceThreshold() );
}

void FactorInputLeaf::calcQuantity(const gcamstr& aRegionName,
        NationalAccount* aNationalAccount,
        const double aTFP,
        const double aParentQuantity,
        const double aParentPrice,
        const double aGamma,
        const bool aSaveResults,
        const int aPeriod)
{
    double sigma = 1.0-aGamma;
    double price = getPrice(aRegionName, aTFP, aPeriod);
    double quantity = pow(mScaler*mProductivity[aPeriod]*aTFP, (sigma - 1.0)) * pow((aParentPrice/ price),sigma) * aParentQuantity;
    Marketplace* marketplace = scenario->getMarketplace();
    if(mIsLabor) {
        mDemand = quantity;
    }
    else if(mIsCapital) {
        double capitalQ = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_CAPITAL_STOCK);
        double capStockChange = quantity - capitalQ;
        mDemand = max(capStockChange, 0.0) / CURRENCY_CONVERSION();
        if(aSaveResults) {
            // cover the edge case when we actually want less capital stock
            // than we already have in which case we need to record the
            // extra depreciation
            aNationalAccount->setAccount(NationalAccount::MATERIALS_CAPITAL_STOCK, quantity);
            if(capStockChange < 0.0) {
                // annual depreciation
                double timestep = scenario->getModeltime()->gettimestep(aPeriod);
                double depreciation = aNationalAccount->getAccountValue(NationalAccount::DEPRECIATION);
                depreciation -= capStockChange / timestep;
                aNationalAccount->setAccount(NationalAccount::DEPRECIATION, depreciation);
            }
        }
    }
    if(price <= SectorUtils::getDemandPriceThreshold()) {
        const Marketplace* marketplace = scenario->getMarketplace();
        double unadjPrice = marketplace->getPrice(mOutputMrkName, aRegionName, aPeriod);
        mDemand = SectorUtils::adjustDemandForNegativePrice( mDemand, unadjPrice );
    }
    if(!aSaveResults) {
    marketplace->addToDemand(mOutputMrkName, aRegionName, mDemand, aPeriod);
    }
}

double FactorInputLeaf::getQuantity(const gcamstr& aRegionName, const int aPeriod) {
    assert(!mIsLabor && !mIsCapital);

    const Marketplace* marketplace = scenario->getMarketplace();
    // stash it so we can use it for reporting later
    mDemand = marketplace->getPrice(mOutputMrkName, aRegionName, aPeriod)
        * CURRENCY_CONVERSION();
    return mDemand;
}

void FactorInputLeaf::calcPricesForReporting(NationalAccount* aNationalAccount, const double aTFP, const double aQuantityAbove, const double aPriceAbove, const double aGamma, const int aPeriod) {
    assert(!mIsLabor && !mIsCapital);
    double sigma = 1.0 - aGamma;
    double price = pow(getCoefficient(aTFP, aPeriod), (1.0-1.0/sigma)) * pow((aQuantityAbove/mDemand),(1.0/sigma)) * aPriceAbove;
    double value = price * mDemand;
    aNationalAccount->setAccount(mIsEnergy ? NationalAccount::ENERGY_SERVICE_VALUE : NationalAccount::AG_NONFOOD_SERVICE_VALUE, value);
}

void FactorInputLeaf::reportResults(const gcamstr& aRegionName,
        const double aTFP,
        const double aGrossOutput,
        NationalAccount* aNationalAccount,
        const int aPeriod)
{
    if(mIsLabor) {
        double price = getPrice(aRegionName, aTFP, aPeriod);
        double value = mDemand * price;
        double fr_share = value / aGrossOutput;
        aNationalAccount->setAccount(NationalAccount::FR_SHARE_LABOR, fr_share);
        aNationalAccount->setAccount(NationalAccount::MATERIALS_LABOR_WAGES, value);
        aNationalAccount->setAccount(NationalAccount::MATERIALS_LABOR_FORCE, mDemand);
    }
    else if(mIsCapital) {
        double capInvTimestep = mDemand * CURRENCY_CONVERSION();
        const double annualizationFactor = scenario->getModeltime()->gettimestep(aPeriod);
        double capInvAnnual = capInvTimestep / annualizationFactor;
        double capStockTot = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_CAPITAL_STOCK);
        double price = getPrice(aRegionName, aTFP, aPeriod);
        double value = capStockTot * price;
        double fr_share = value / aGrossOutput;
        aNationalAccount->setAccount(NationalAccount::FR_SHARE_CAPITAL, fr_share);
        aNationalAccount->setAccount(NationalAccount::CAPITAL_PRICE, price);
        aNationalAccount->setAccount(NationalAccount::MATERIALS_CAPITAL_INV, capInvAnnual);
    }
    else {
        double value = aNationalAccount->getAccountValue(mIsEnergy ?
            NationalAccount::ENERGY_SERVICE_VALUE :
            NationalAccount::AG_NONFOOD_SERVICE_VALUE);
        double quantity = mDemand;
        double price = value / quantity;
        double fr_share = value / aGrossOutput;
        if(mIsEnergy) {
            aNationalAccount->setAccount(NationalAccount::ENERGY_SERVICE, quantity);
            aNationalAccount->setAccount(NationalAccount::ENERGY_SERVICE_PRICE, price);
            aNationalAccount->setAccount(NationalAccount::FR_SHARE_ENERGY, fr_share);
        }
        else {
            aNationalAccount->setAccount(NationalAccount::AG_NONFOOD_SERVICE, quantity);
            aNationalAccount->setAccount(NationalAccount::AG_NONFOOD_SERVICE_PRICE, price);
            aNationalAccount->setAccount(NationalAccount::FR_SHARE_AG, fr_share);
        }
    }
}

//! post calcualtions for each period
void FactorInputLeaf::postCalc(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod){
    // do nothing
}

// for reporting FactorInputLeaf information
void FactorInputLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitFactorInputLeaf( this, aPeriod );
    
    aVisitor->endVisitFactorInputLeaf( this, aPeriod );
}

//******* FactorInputNode *******

//! Default Constructor
FactorInputNode::FactorInputNode():
mFactorNodeGamma(1)
{
}

// static initialize.


//! Get the name of the NodeInput
const gcamstr& FactorInputNode::getName() const {
    return mName;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author Sonny Kim
 * \return The constant XML_NAME as a static.
 */
const gcamstr& FactorInputNode::getXMLNameStatic() {
    static const gcamstr XML_NAME = "factor-input-node";
    return XML_NAME;
}

const gcamstr& FactorInputNode::getXMLName() const {
    return getXMLNameStatic();
}

//! complete initializations
void FactorInputNode::completeInit( const gcamstr& aRegionName, const gcamstr& aGDPActName ) {
    mFactorNodeGamma = mFactorNodeGamma/(mFactorNodeGamma-1.0);
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->completeInit( aRegionName, aGDPActName );
    }
}

//! complete initializations for each period
void FactorInputNode::initCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ) {
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->initCalc( aRegionName, aNationalAccount, aPeriod );
    }
}

void FactorInputNode::updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod) {
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->updateMarkets( aRegionName, aNationalAccount, aPeriod );
    }
}

void FactorInputNode::getCalShares(std::vector<double>& aShares, const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const int aPeriod)
{
    for(auto leaf : mFactorInputLeaf) {
        double share = leaf->getCalShares(aRegionName, aNationalAccount, aGrossOutput, aPeriod);
        aShares.push_back(share);
    }
}

void FactorInputNode::calcCoef(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const double aGrossOutput, const double aShareAdj, const double aSlackShare, const double aParentValue, const double aParentPrice, const double aGamma, const int aPeriod)
{
    double childShareTotal = 0.0;
    for(auto leaf : mFactorInputLeaf) {
        double share = leaf->getCalShares(aRegionName, aNationalAccount, aGrossOutput, aPeriod);
        share *= aShareAdj;
        childShareTotal += share;
    }
    double value = childShareTotal * aGrossOutput;
    double price = 1.0;
    double share = value / aParentValue;
    mScaler = pow(share, (-1.0/aGamma)) * price / aParentPrice;
    for(auto leaf : mFactorInputLeaf) {
        leaf->calcCoef(aRegionName, aNationalAccount, aGrossOutput, aShareAdj, aSlackShare, value, price, mFactorNodeGamma, aPeriod);
    }
}

double FactorInputNode::getPrice(const gcamstr& aRegionName, const double aTFP, const int aPeriod)
{
    double childPriceSum = 0.0;
    for(auto leaf : mFactorInputLeaf) {
        double price = leaf->getPrice(aRegionName, aTFP, aPeriod);
        childPriceSum += pow(price / leaf->getCoefficient(aTFP, aPeriod), mFactorNodeGamma);
    }
    return pow(childPriceSum, 1.0 / mFactorNodeGamma);
}

void FactorInputNode::calcQuantity(const gcamstr& aRegionName,
        NationalAccount* aNationalAccount,
        const double aTFP,
        const double aParentQuantity,
        const double aParentPrice,
        const double aGamma,
        const bool aSaveResults,
        const int aPeriod)
{
    double sigma = 1.0 - aGamma;
    double price = getPrice(aRegionName, aTFP, aPeriod);
    double quantity = pow(mScaler,(sigma - 1)) * pow((aParentPrice/price),sigma) * aParentQuantity;
    if(aSaveResults) {
        mPrice = price;
        mDemand = quantity;
    }
    for(auto leaf : mFactorInputLeaf) {
        leaf->calcQuantity(aRegionName, aNationalAccount, aTFP, quantity, price, mFactorNodeGamma, aSaveResults, aPeriod);
    }
}


void FactorInputNode::reportResults(const gcamstr& aRegionName,
        const double aTFP,
        const double aGrossOutput,
        NationalAccount* aNationalAccount,
        const int aPeriod)
{
    double value = mPrice * mDemand;
    aNationalAccount->setAccount(NationalAccount::MATERIALS_VALUE_ADDED, value);
    for(auto leaf : mFactorInputLeaf) {
        leaf->reportResults(aRegionName, aTFP, aGrossOutput, aNationalAccount, aPeriod);
    }
}

//! post calcualtions for each period
void FactorInputNode::postCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ){
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->postCalc(aRegionName, aNationalAccount, aPeriod);
    }
}

// for reporting FactorInputNode information
void FactorInputNode::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitFactorInputNode( this, aPeriod );

    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->accept(aVisitor, aPeriod);
    }
    
    aVisitor->endVisitFactorInputNode( this, aPeriod );
}


//*********** NestedCESProductionFunctionMacro

//! Default Constructor
NestedCESProductionFunctionMacro::NestedCESProductionFunctionMacro():mFactorInputNode(0)
{
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
 *
 * This public function accesses the private constant string, XML_NAME.
 * This way the tag is always consistent for both read-in and output and can be easily changed.
 * The "==" operator that is used when parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author Sonny Kim
 * \return The constant XML_NAME as a static.
 */
const gcamstr& NestedCESProductionFunctionMacro::getXMLNameStatic() {
    static const gcamstr XML_NAME = "gdp-macro-function";
    return XML_NAME;
}

const gcamstr& NestedCESProductionFunctionMacro::getXMLName() const {
    return getXMLNameStatic();
}

/*!
 * \brief Get the name of this contianer.
 * \details The NestedCESProductionFunctionMacro isn't named by the user so we just return the XML name.
 * \return The name of this container.
 */
const gcamstr& NestedCESProductionFunctionMacro::getName() const {
    static const gcamstr NAME(getXMLNameStatic());
    return NAME;
}

//! Writes data members to debugging data stream in XML format.
void NestedCESProductionFunctionMacro::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

//! Complete initializations and scaler initializations
void NestedCESProductionFunctionMacro::completeInit(const gcamstr& aRegionName, const gcamstr& aGDPActName ){

    mRho = mRho/(mRho-1.0);
    // complete initialization for nodes and leaves and establish input dependencies
    mFactorInputNode->completeInit( aRegionName, aGDPActName );
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
            mFactorInputLeaf[i]->completeInit( aRegionName, aGDPActName );
    }
}

//! complete initializations for each period
void NestedCESProductionFunctionMacro::initCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ){
    
    // initialization of factor reward shares and base scalers are done in
    // postCalc as dynamic calculations of energy value are required
    
    mFactorInputNode->initCalc( aRegionName, aNationalAccount, aPeriod );
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->initCalc( aRegionName, aNationalAccount, aPeriod );
    }
}

void NestedCESProductionFunctionMacro::setTotalFactorProductivity(const double aTotalFactorProd) {
    const double MIN_TFP = 0.001;
    mTotalFactorProd = std::max(aTotalFactorProd, MIN_TFP);
}

void NestedCESProductionFunctionMacro::updateMarkets(const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod) {
    mFactorInputNode->updateMarkets(aRegionName, aNationalAccount, aPeriod);
    for(auto leaf : mFactorInputLeaf) {
        leaf->updateMarkets(aRegionName, aNationalAccount, aPeriod);
    }
}

//! calculate Gross Output
double NestedCESProductionFunctionMacro::calcGrossOutput( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod, const bool aSaveResults ){
    const Modeltime* modeltime = scenario->getModeltime();
    Marketplace* marketplace = scenario->getMarketplace();

    // the price of Z is the numeraire (fixed)
    const double priceZ = 1.0;

    // if this is a historical period then we need to first calibrate the CES scalers
    if(aPeriod <= modeltime->getFinalCalibrationPeriod()) {
        double grossOutput = aNationalAccount->getAccountValue(NationalAccount::MATERIALS_GROSS_OUTPUT);
        vector<double> shares;
        mFactorInputNode->getCalShares(shares, aRegionName, aNationalAccount, grossOutput, aPeriod);
        for(auto leaf : mFactorInputLeaf) {
            double share = leaf->getCalShares(aRegionName, aNationalAccount, grossOutput, aPeriod);
            shares.push_back(share);
        }
        double shareSum = 0.0;
        for(int i = 0; i < shares.size(); ++i) {
            // the slack share uses a negative value as sentinal, do not include
            // it in the sum
            if(shares[i] > 0.0) {
                shareSum += shares[i];
            }
        }
        double slackShare = 1.0 - shareSum;
        double shareAdj = 1.0;
        // calculate the "slack" share
        // Given the energy value is being pulled out of GCAM without adjustment
        // there is a chance for inconsistency and send the remaining value (which is
        // all attributed to capital) to <= zero.  Instead we will enforce a minimum
        // capital share and adjust both labor and energy to make up for the shortfall
        const double MIN_SLACK_SHARE = 0.05;
        if(slackShare < MIN_SLACK_SHARE) {
            // adjust labor and energy uniformily to meet the minimum threshold
            shareAdj = (1.0 - MIN_SLACK_SHARE) / shareSum;
            slackShare = MIN_SLACK_SHARE;
        }

        mFactorInputNode->calcCoef(aRegionName, aNationalAccount, grossOutput, shareAdj, slackShare, grossOutput, priceZ, mRho, aPeriod);
        for(auto leaf : mFactorInputLeaf) {
            leaf->calcCoef(aRegionName, aNationalAccount, grossOutput, shareAdj, slackShare, grossOutput, priceZ, mRho, aPeriod);
        }
    }

    // given the mix of price driven (value added nest) and quantity driven inputs
    // (ag and energy) we will need to calculate our CES equations is a specific order
    // first aggregate the prices within the value added nest
    double priceVA = mFactorInputNode->getPrice(aRegionName, mTotalFactorProd, aPeriod);
    // next calculate the quantity sum of the direct inputs into the top level nest
    double sigma = 1.0-mRho;
    double quantDenom = 0.0;
    for(auto leaf : mFactorInputLeaf) {
        double coef = leaf->getCoefficient(mTotalFactorProd, aPeriod);
        double quant = leaf->getQuantity(aRegionName, aPeriod);
        quantDenom += 1.0/pow(coef * quant, 1.0/sigma - 1.0);
    }
    // we can now mix the quantities and prices (given the output price "Z" as numeraire)
    // to calculate the total gross output quantity
    double grossOutput = pow((1.0-pow((priceVA/(mFactorInputNode->getCoefficient()*priceZ)),(1-sigma)))/quantDenom,(sigma/(1-sigma)));
    // with all the quantities now know the last set is to back calculate the implied
    // quantities within the value added nest (capital and labor)
    mFactorInputNode->calcQuantity(aRegionName, aNationalAccount, mTotalFactorProd, grossOutput, priceZ, mRho, aSaveResults, aPeriod);

    // Set the supply for investment which generally is just fixed to the INVESTMENT account
    // value.  However, for solution stability we will scale down to zero when prices are _low_
    const double LOW_CAP_PRICE_THRESHOLD = 0.001;
    gcamstr capMarketName("");
    for(auto leaf : mFactorInputNode->mFactorInputLeaf) {
        if(leaf->mIsCapital) {
            capMarketName = leaf->mOutputMrkName;
        }
    }
    assert(!capMarketName.empty());
    
    double priceK = marketplace->getPrice(capMarketName, aRegionName, aPeriod);
    double capSupplyAdj = 1.0;
    if(priceK < LOW_CAP_PRICE_THRESHOLD) {
        capSupplyAdj = priceK / LOW_CAP_PRICE_THRESHOLD;
    }
    // INVESTMENT is an annual supply of savings for investment in terms of million 1990$
    // the capital market is cumulative investment over a timestep in terms of billion 1975$
    mCapitalSupply = aNationalAccount->getAccountValue(NationalAccount::INVESTMENT)
        / CURRENCY_CONVERSION()
        * modeltime->gettimestep(aPeriod) * capSupplyAdj;
    
    if(!aSaveResults) {
        marketplace->addToSupply(capMarketName, aRegionName, mCapitalSupply, aPeriod);
    }

    // during post calc we do another call with aSaveResults set to true so that we can store
    // some additional information for reporting purposes
    if(aSaveResults) {
        aNationalAccount->setAccount(NationalAccount::MATERIALS_GROSS_OUTPUT, grossOutput);
        mFactorInputNode->reportResults(aRegionName, mTotalFactorProd, grossOutput, aNationalAccount, aPeriod);
        for(auto leaf : mFactorInputLeaf) {
            leaf->calcPricesForReporting(aNationalAccount, mTotalFactorProd, grossOutput, priceZ, mRho, aPeriod);
            leaf->reportResults(aRegionName, mTotalFactorProd, grossOutput, aNationalAccount, aPeriod);
        }
    }

    return grossOutput;
}

void NestedCESProductionFunctionMacro::postCalc( const gcamstr& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ) {
    mFactorInputNode->postCalc(aRegionName, aNationalAccount, aPeriod);
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->postCalc(aRegionName, aNationalAccount, aPeriod);
    }
}

// for reporting NestedCESProductionFunctionMacro information
void NestedCESProductionFunctionMacro::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNestedCESProductionFunctionMacro( this, aPeriod );
    mFactorInputNode->accept(aVisitor, aPeriod);
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->accept(aVisitor, aPeriod);
    }
    
    aVisitor->endVisitNestedCESProductionFunctionMacro( this, aPeriod );
}
