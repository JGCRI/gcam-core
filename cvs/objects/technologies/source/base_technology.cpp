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
* \file base_technology.cpp
* \ingroup Objects
* \brief The BaseTechnology class source file.
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <vector>

#include "technologies/include/base_technology.h"
#include "functions/include/iinput.h"
#include "technologies/include/ioutput.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/co2_emissions.h"
#include "functions/include/function_utils.h"
#include "functions/include/node_input.h"
#include "technologies/include/icapture_component.h"
#include "containers/include/info_factory.h"


using namespace std;

extern Scenario* scenario;

typedef vector<IInput*>::iterator InputIterator;
typedef vector<IInput*>::const_iterator CInputIterator;
typedef vector<AGHG*>::const_iterator CGHGIterator;
typedef vector<AGHG*>::iterator GHGIterator;

//!< Default Constructor
BaseTechnology::BaseTechnology():
  mShareWeight( 1.0 ),
  mIsInitialYear( false ),
  mSequestrationDevice( 0 ),
  doCalibration( false )
{
    mNestedInputRoot = 0 ;
}

//!< Destructor
BaseTechnology::~BaseTechnology() {
    clear();
}

void BaseTechnology::copy( const BaseTechnology& baseTechIn ) {
    name = baseTechIn.name;
    categoryName = baseTechIn.categoryName;
    mGhgNameMap = baseTechIn.mGhgNameMap;
    mShareWeight = baseTechIn.mShareWeight;
    mNestedInputRoot = static_cast<INestedInput*>( baseTechIn.mNestedInputRoot->clone() );
    mLeafInputs = FunctionUtils::getLeafInputs( mNestedInputRoot );

    // copies can not be an initial year
    mIsInitialYear = false;

    // clone the baseTechIn sequestration device if it had one
    if( baseTechIn.mSequestrationDevice.get() ) {
        mSequestrationDevice.reset( baseTechIn.mSequestrationDevice.get()->clone() );
    }

    for ( unsigned int i = 0; i < baseTechIn.mOutputs.size(); i++) {
        mOutputs.push_back( baseTechIn.mOutputs[i]->clone() );
    }
    for( CGHGIterator ghg = baseTechIn.mGhgs.begin(); ghg != baseTechIn.mGhgs.end(); ++ghg ){
        mGhgs.push_back( (*ghg)->clone() );
    }
}

void BaseTechnology::copyParam( const BaseTechnology* baseTechIn,
                                const int aPeriod )
{
    name = baseTechIn->name;
    categoryName = baseTechIn->categoryName;

    // only copy the share weight if it was not already read in
    // TODO: use a Value or something else for this
    if( mShareWeight == 1 ) {
        mShareWeight = baseTechIn->mShareWeight;
    }

    // only clone if we don't already have a sequestration device and 
    // basTechIn did
    if( baseTechIn->mSequestrationDevice.get() && !mSequestrationDevice.get() ) {
        mSequestrationDevice.reset( baseTechIn->mSequestrationDevice.get()->clone() );
    }
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int initialYear = max( modeltime->getStartYear(), year );
    const int initialPeriod = modeltime->getyr_to_per( initialYear );
    if( mNestedInputRoot ) {
        mNestedInputRoot->copyParam( baseTechIn->mNestedInputRoot, aPeriod );
    }
    else {
        mNestedInputRoot = static_cast<INestedInput*>( baseTechIn->mNestedInputRoot->clone() );
        mLeafInputs = FunctionUtils::getLeafInputs( mNestedInputRoot );
    }
    
    // For each Ghg check if it exists in the current technology.
    for ( CGHGIterator ghg = baseTechIn->mGhgs.begin(); ghg != baseTechIn->mGhgs.end(); ++ghg ) {
        if( !util::hasValue( mGhgNameMap, (*ghg)->getName() ) ){
            mGhgs.push_back( (*ghg)->clone() );
            // Add it to the map.
            mGhgNameMap[ (*ghg)->getName() ] = static_cast<int>( mGhgs.size() ) - 1;
        }
        // It already exists, we need to copy into.
        // For now just leave the current one. It should already be in the map since it was parsed.
        else {
            // TODO: Add copy into code here.
        }
    } // end for
}

void BaseTechnology::clear() {
    delete mNestedInputRoot;

    for( vector<IOutput*>::iterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ) {
        delete *iter;
    }
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        delete *ghg;
    }
}

//! Output debug info to XML data
void BaseTechnology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name, year );

    XMLWriteElement( mShareWeight, "share-weight", out, tabs );
    mNestedInputRoot->toDebugXML( period, out, tabs );

    for( unsigned int iter = 0; iter < mOutputs.size(); iter++ ){
        mOutputs[ iter ]->toDebugXML( period, out, tabs );
    }
    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->toDebugXML( period, out, tabs );
    }
    //expenditures[ period ].toDebugXML( period, out, tabs );

    toDebugXMLDerived( period, out, tabs );

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Complete the initialization of the BaseTechnology object.
void BaseTechnology::completeInit( const string& aRegionName,
                                   const string& aSectorName,
                                   const string& aSubsectorName )
{
    const Modeltime* modeltime = scenario->getModeltime();
    const int initialYear = max( modeltime->getStartYear(), year );
    
    mTechInfo.reset( InfoFactory::constructInfo( 0, name ) );

    // technologies before the base year will not have inputs yet
    if( mNestedInputRoot ) {
        mNestedInputRoot->completeInit( aRegionName, aSectorName, aSubsectorName, name, mTechInfo.get() );
    }

    // Check if CO2 is missing. 
    if( !util::hasValue( mGhgNameMap, CO2Emissions::getXMLNameStatic() ) ){
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        mGhgs.push_back( new CO2Emissions() ); // at least CO2 must be present
        mGhgNameMap[ "CO2" ] = static_cast<int>( mGhgs.size() ) - 1;
    }
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        // (*ghg)->completeInit();
    }

}

void BaseTechnology::initCalc( const string& aRegionName,
                               const string& aSectorName, NationalAccount& nationalAccount,
                               const Demographic* aDemographics, const double aCapitalStock, const int aPeriod )
{
    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->initCalc( aRegionName, 0, aPeriod );
    }
    
    // Initialize the inputs.
    // TODO: derived classes will need to initialize the node input
    // themselves until I can figure out how to get this correct for
    // all of them here
    /*
    const Modeltime* modeltime = scenario->getModeltime();
    const bool isInitialYear = modeltime->getper_to_yr( aPeriod ) == year;
    for( InputIterator curr = input.begin(); curr != input.end(); ++curr ){
        (*curr)->initCalc( aRegionName, aSectorName, isInitialYear, isTrade(), aPeriod );
    }
    */

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->initCalc( aRegionName, aSectorName, aPeriod );
    }
    mPricePaidCached = false;
}

/*! \brief Return whether a technology is new investment for the current period.
* \param aPeriod The current period.
* \return Whether the technology is new investment in the period.
* \author Josh Lurz, Sonny Kim
*/
bool BaseTechnology::isNewInvestment( const int aPeriod ) const {
    // Return false for base technology.
    return false;
}

/*! \brief Clear out empty inputs.
* \details Loop through the set of inputs for a technology and remove all inputs
*          which have a currency demand of zero.
*/
void BaseTechnology::removeEmptyInputs(){
    // Technologies before the base year will not have inputs yet.
    if( mNestedInputRoot ) {
        // This method is recursive for all nodes.
        mNestedInputRoot->removeEmptyInputs();
    }
}

//! get technology name
const string& BaseTechnology::getName() const {
    return name;
}

//! get technology year
int BaseTechnology::getYear() const {
    return year;
}

void BaseTechnology::setYear( int newYear ) {
    year = newYear;
}

/*! \brief Get the physical output of the technology in a given period.
* \param aPeriod Period to get output for.
* \return The output for the given period.
* \author Josh Lurz
* \note Currently the output vector is calculated differently depending on whether the new 
* investments are being included in subsector level output. 
*/
double BaseTechnology::getOutput( const int aPeriod ) const {
    return mOutputs[ 0 ]->getPhysicalOutput( aPeriod );
}

/*! \brief Calculates and sets the price paid for each input of the the
*          technology.
* \param aRegionName The name of the region.
* \param aSectorName The name of the sector.
* \param aPeriod The period for which to calculate expected price paid.
* \author Sonny Kim
*/
void BaseTechnology::calcPricePaid( const string& aRegionName,
                                    const string& aSectorName, const int aPeriod, const int aLifetimeYears ) const
{
    // the leaves must calculate their price paid first, then the nesting structure can calculate
    // node prices through the calcLevelizedCost method
    // Note the hack on the sequestration device, this is because MiniCAM uses a getLargeNumber
    // for the storage cost when there is no policy market to keep CCS out of a reference.  In
    // SGM we would have calibrated with that large number which leads to incorrect behavior so
    // we just need to make sure when we calibrate (aPeriod is zero) that value is not included.
    for( vector<IInput*>::const_iterator it = mLeafInputs.begin(); it != mLeafInputs.end(); ++it ) {
        (*it)->calcPricePaid( aRegionName, aSectorName, mGhgs,
            aPeriod > 0 ? mSequestrationDevice.get() : 0, // hack to avoid getLargeNumber when calibrating
            aLifetimeYears, aPeriod );
    }
}

void BaseTechnology::updateMarketplace( const string& sectorName, const string& regionName, const int period ) {
    // need to create the list here so that marketplaces get set up correctly
    // TODO: I could just create an updateMarketplace in the node input
    mLeafInputs = FunctionUtils::getLeafInputs( mNestedInputRoot );
    Marketplace* marketplace = scenario->getMarketplace();
    double totalDemand = 0;
    for( InputIterator curr = mLeafInputs.begin(); curr != mLeafInputs.end(); ++curr ) {
        // really currency
        double tempDemand = (*curr)->getPhysicalDemand( period );
        //marketplace->addToDemand( (*curr)->getName(), regionName, tempDemand, period );
        totalDemand += tempDemand;
    }
    //marketplace->addToSupply( sectorName, regionName, totalDemand, period );
    mLeafInputs.clear();
}

void BaseTechnology::accept( IVisitor* aVisitor, const int aPeriod ) const
{
    aVisitor->startVisitBaseTechnology( this, aPeriod );

    mNestedInputRoot->accept( aVisitor, aPeriod );

    /*
    if( aPeriod == -1 ) {
        int currPeriod = 0;
        for( vector<Expenditure>::const_iterator cExpenditure = expenditures.begin(); cExpenditure !=
            expenditures.end(); ++cExpenditure )
        {
            (*cExpenditure).accept( aVisitor, currPeriod );
            ++currPeriod;
        }
    } else {
        expenditures[ aPeriod].accept( aVisitor, aPeriod );
    }
    */
    for( unsigned int iter = 0; iter < mOutputs.size(); iter++ ){
        mOutputs[ iter ]->accept( aVisitor, aPeriod );
    }
    
    for( vector<AGHG*>::const_iterator cGHG = mGhgs.begin(); cGHG != mGhgs.end(); ++cGHG ) {
        (*cGHG)->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitBaseTechnology( this, aPeriod );
}

const string BaseTechnology::getIdentifier() const {
    return createIdentifier( name, year );
}

const string BaseTechnology::createIdentifier( const string& aName, int aYear ){
    return aName + util::toString( aYear );
}

bool BaseTechnology::hasCalibrationMarket() const {
    return doCalibration;
}

double BaseTechnology::getShareWeight( const int aPeriod ) const {
    // maybe check that the period is equivalent to this year

    return mShareWeight;
}

/*! \breif Whether this is the first year for this kind of technology.
 * \details This flag is used to be able to determine which year 
 *           of a technology will be used to calibrate coefficients.
 * \return True if this is the first year for this technology.
 */
bool BaseTechnology::isInitialYear() const {
    return mIsInitialYear;
}
