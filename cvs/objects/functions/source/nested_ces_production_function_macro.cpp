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

using namespace std;
using namespace objects;

extern Scenario* scenario;


//! Default Constructor
FactorInputLeaf::FactorInputLeaf():
mIsPrimaryFactor(false),
mIsLabor(false),
mIsCapital(false),
mIsEnergy(false),
mProductivity(Value(1.0)),
mScaler(0),
mOutputMrkName("energy service")

{
}

//! Get the name of the NodeInput
const string& FactorInputLeaf::getName() const {
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
const string& FactorInputLeaf::getXMLNameStatic() {
    static const std::string XML_NAME = "factor-input-leaf";
    return XML_NAME;
}

const string& FactorInputLeaf::getXMLName() const {
    return getXMLNameStatic();
}

//! Return boolean to determine if primary factor
bool FactorInputLeaf::isPrimaryFactor() const {
    return mIsPrimaryFactor;
}

//! complete initializations
void FactorInputLeaf::completeInit( const string& aRegionName, const string& aGDPActName ){
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency(aGDPActName, aRegionName, mOutputMrkName, aRegionName);
}

//! complete initializations for each period
void FactorInputLeaf::initCalc(const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod){
    // do nothing
}



//! post calcualtions for each period
void FactorInputLeaf::postCalc(const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod){
    // do nothing
}

// for reporting FactorInputLeaf information
void FactorInputLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitFactorInputLeaf( this, aPeriod );
    
    aVisitor->endVisitFactorInputLeaf( this, aPeriod );
}

void FactorInputLeaf::grabInputs(FactorInputLeaf *&aEnergyInput, FactorInputLeaf *&aLaborInput, FactorInputLeaf *&aCapitalInput)
{
    if(mIsEnergy) {
        aEnergyInput = this;
    }
    else if(mIsLabor) {
        aLaborInput = this;
    }
    else if(mIsCapital) {
        aCapitalInput = this;
    }
}

//******* FactorInputNode *******

//! Default Constructor
FactorInputNode::FactorInputNode():
mFactorNodeGamma(1)
{
}

// static initialize.


//! Get the name of the NodeInput
const string& FactorInputNode::getName() const {
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
const string& FactorInputNode::getXMLNameStatic() {
    static const string XML_NAME = "factor-input-node";
    return XML_NAME;
}

const string& FactorInputNode::getXMLName() const {
    return getXMLNameStatic();
}

//! Return boolean to determine if primary factor
bool FactorInputNode::isPrimaryFactor() const {
    // node is primary if it contains a single primary leaf
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        if( mFactorInputLeaf[i]->isPrimaryFactor() ){
            return true;
        }
    }
    return false;
}

//! complete initializations
void FactorInputNode::completeInit( const string& aRegionName, const string& aGDPActName ) {
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->completeInit( aRegionName, aGDPActName );
    }
}

//! complete initializations for each period
void FactorInputNode::initCalc( const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ) {
    for( unsigned int i = 0; i < mFactorInputLeaf.size(); i++ ){
        mFactorInputLeaf[i]->initCalc( aRegionName, aNationalAccount, aPeriod );
    }
}

//! post calcualtions for each period
void FactorInputNode::postCalc( const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ){
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
NestedCESProductionFunctionMacro::NestedCESProductionFunctionMacro()
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
const string& NestedCESProductionFunctionMacro::getXMLNameStatic() {
    static const string XML_NAME = "gdp-macro-function";
    return XML_NAME;
}

const string& NestedCESProductionFunctionMacro::getXMLName() const {
    return getXMLNameStatic();
}

/*!
 * \brief Get the name of this contianer.
 * \details The NestedCESProductionFunctionMacro isn't named by the user so we just return the XML name.
 * \return The name of this container.
 */
const string& NestedCESProductionFunctionMacro::getName() const {
    return getXMLNameStatic();
}

//! Writes data members to debugging data stream in XML format.
void NestedCESProductionFunctionMacro::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

//! Complete initializations and scaler initializations
void NestedCESProductionFunctionMacro::completeInit(const string& aRegionName, const string& aGDPActName ){

    // complete initialization for nodes and leaves and establish input dependencies
    for( unsigned int i = 0; i < mFactorInputNode.size(); i++ ){
            mFactorInputNode[i]->completeInit( aRegionName, aGDPActName );
    }
}

//! complete initializations for each period
void NestedCESProductionFunctionMacro::initCalc( const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ){
    
    // initialization of factor reward shares and base scalers are done in
    // postCalc as dynamic calculations of energy value are required
    
    for( unsigned int i = 0; i < mFactorInputNode.size(); i++ ){
        mFactorInputNode[i]->initCalc( aRegionName, aNationalAccount, aPeriod );
    }
}

void NestedCESProductionFunctionMacro::setTotalFactorProductivity(const double aTotalFactorProd) {
    mTotalFactorProd = aTotalFactorProd;
}

//! calculate Gross Output
double NestedCESProductionFunctionMacro::calcGrossOutput( const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod, const bool aSaveResults ){
    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();
    
    // grab inputs and parameters from the nesting structure
    double gamma = 0.0;
    for( unsigned int i = 0; i < mFactorInputNode.size(); i++ ){
        if(mFactorInputNode[i]->isPrimaryFactor()){
            gamma = mFactorInputNode[i]->getNodeGamma();
        }
    }
    FactorInputLeaf* energyInput;
    FactorInputLeaf* laborInput;
    FactorInputLeaf* capitalInput;
    for(auto node : mFactorInputNode) {
        node->grabInputs(energyInput, laborInput, capitalInput);
    }
    
    // get the quantities to feed into the CES
    double energyQ = marketplace->getPrice(energyInput->mOutputMrkName, aRegionName, aPeriod)
        * 1000.0 * FunctionUtils::DEFLATOR_1990_PER_DEFLATOR_1975();
    double laborQ = aNationalAccount->getAccountValue(NationalAccount::LABOR_FORCE);
    double capitalQ = aNationalAccount->getAccountValue(NationalAccount::CAPITAL_STOCK);
    
    // if this is a historical period then we need to first calibrate the CES scalers
    if(aPeriod <= modeltime->getFinalCalibrationPeriod()) {
        double grossOutput = aNationalAccount->getAccountValue(NationalAccount::GROSS_OUTPUT);
        
        // calcualte value shares
        // Note: in principle the energy value and quantity could be different but given
        // energy the Q is just a service index weighted by prices these are indeed the same
        double energyV = energyQ;
        double laborV = aNationalAccount->getAccountValue(NationalAccount::LABOR_WAGES);
        
        double shareE = energyV / grossOutput;
        double shareL = laborV / grossOutput;
        double shareK = (1.0 - shareE - shareL);
        // Given the energy value is being pulled out of GCAM without adjustment
        // there is a chance for inconsistency and send the remaining value (which is
        // all attributed to capital) to <= zero.  Instead we will enforce a minimum
        // capital share and adjust both labor and energy to make up for the shortfall
        const double MIN_CAPITAL_SHARE = 0.05;
        if(shareK < MIN_CAPITAL_SHARE) {
            double shareAdj = MIN_CAPITAL_SHARE - shareK;
            // adjust labor and energy uniformily to meet the minimum threshold
            shareE -= shareAdj / 2.0;
            shareL -= shareAdj / 2.0;
            shareK = MIN_CAPITAL_SHARE;
        }
        
        // calibrate and store scalers
        double c = shareE * pow(grossOutput / energyQ, mRho);
        double b = pow(pow(grossOutput, mRho) - c * pow(energyQ, mRho), (gamma / mRho)) /
            ((1 + shareL / shareK) * pow(capitalQ, gamma));
        double a = pow(pow(grossOutput, mRho) - c * pow(energyQ, mRho), (gamma / mRho)) /
            ((1 + shareK / shareL) * pow(laborQ, gamma));
        laborInput->mScaler = a;
        capitalInput->mScaler = b;
        energyInput->mScaler = c;
    }
    
    // get factor productivity adjustments
    double laborProd = 1.0;
    double capitalProd = 1.0;
    double energyProd = 1.0;
    if(aPeriod > modeltime->getFinalCalibrationPeriod()) {
        laborProd = laborInput->mProductivity[aPeriod];
        capitalProd = capitalInput->mProductivity[aPeriod];
        energyProd = energyInput->mProductivity[aPeriod];
    }
    // calculate the nested CES starting with the value added nest then the top level nest
    double a = laborInput->mScaler;
    double b = capitalInput->mScaler;
    double c = energyInput->mScaler;
    double V = pow(a * pow(laborProd * laborQ, gamma) + b * pow(capitalProd * capitalQ, gamma), 1.0 / gamma);
    double Y = pow(pow(V, mRho) + c * pow(energyProd * energyQ, mRho), 1.0 / mRho);
    // finally apply total factor productivity to get the final gross output
    double grossOutput = Y * mTotalFactorProd;

    // during post calc we do another call with aSaveResults set to true so that we can store
    // some additional information for reporting purposes
    if(aSaveResults) {
        aNationalAccount->setAccount(NationalAccount::GROSS_OUTPUT, grossOutput);
        
        double facRewardEnergy = c * pow(energyProd * energyQ, mRho) / pow(Y, mRho);
        double facRewardCapital = b * pow(capitalProd * capitalQ, gamma) / pow(V, gamma) * pow(V, mRho) / pow(Y, mRho);
        double facRewardLabor = a * pow(laborProd * laborQ, gamma) / pow(V, gamma) * pow(V, mRho) / pow(Y, mRho);
        
        double valueAdded = grossOutput * (1.0 - facRewardEnergy);
        aNationalAccount->setAccount(NationalAccount::VALUE_ADDED, valueAdded);
        aNationalAccount->setAccount(NationalAccount::FR_SHARE_ENERGY, facRewardEnergy);
        aNationalAccount->setAccount(NationalAccount::FR_SHARE_CAPITAL, facRewardCapital);
        aNationalAccount->setAccount(NationalAccount::FR_SHARE_LABOR, facRewardLabor);
        aNationalAccount->setAccount(NationalAccount::LABOR_WAGES, (facRewardLabor*grossOutput));
        aNationalAccount->setAccount(NationalAccount::ENERGY_SERVICE_VALUE, (facRewardEnergy*grossOutput));
    }
    return grossOutput;
}

void NestedCESProductionFunctionMacro::postCalc( const string& aRegionName, NationalAccount* aNationalAccount, const int aPeriod ) {
    for( unsigned int i = 0; i < mFactorInputNode.size(); i++ ){
        mFactorInputNode[i]->postCalc(aRegionName, aNationalAccount, aPeriod);
    }
}

// for reporting NestedCESProductionFunctionMacro information
void NestedCESProductionFunctionMacro::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitNestedCESProductionFunctionMacro( this, aPeriod );
    for( unsigned int i = 0; i < mFactorInputNode.size(); i++ ){
        mFactorInputNode[i]->accept(aVisitor, aPeriod);
    }
    
    aVisitor->endVisitNestedCESProductionFunctionMacro( this, aPeriod );
}
