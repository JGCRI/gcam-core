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
* \file subresource.cpp
* \ingroup Objects
* \brief SubResource class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <vector>
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "resources/include/subresource.h"
#include "resources/include/grade.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/sector_utils.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/itechnology.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
SubResource::SubResource():
mAvailable( Value( 0.0 ) ),
mAnnualProd( Value( 0.0 ) ),
mCumulProd( Value( 0.0 ) ),
mCumulativeTechChange( 1.0 ),
mEffectivePrice( Value( -1.0 ) ),
mCalProduction( -1.0 ),
mTechnology( 0 )
{
}

//! Destructor.
SubResource::~SubResource() {
    for ( vector<Grade*>::iterator outerIter = mGrade.begin(); outerIter != mGrade.end(); outerIter++ ) {
        delete *outerIter;
    }
    
    delete mTechnology;
}

//! Initialize member variables from xml data
void SubResource::XMLParse( const DOMNode* aNode ){
    // make sure we were passed a valid node.
    assert( aNode );

    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, "name" );
    
    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == Grade::getXMLNameStatic() ){
            parseContainerNode( curr, mGrade, new Grade );
        }
        else if( nodeName == "annualprod" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mAnnualProd, modeltime );
        }
        else if( nodeName == "techChange" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mTechChange, modeltime );
        }
        else if( nodeName == "cal-production" ){
            XMLHelper<double>::insertValueIntoVector( curr, mCalProduction, modeltime );
        }
        else if( nodeName == "price-adder" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mPriceAdder, modeltime );
        }
        else if( TechnologyContainer::hasTechnologyType( nodeName ) ) {
            parseSingleNode( curr, mTechnology, new TechnologyContainer() );
        }
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName 
                    << " found while parsing " << getXMLName() << "." << endl;
        }
    }

}

bool SubResource::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aNode ) {
    return false;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz, Sonny Kim
* \warning markets are not necessarily set when completeInit is called
*/
void SubResource::completeInit( const std::string& aRegionName, const std::string& aResourceName,
                                const IInfo* aResourceInfo ) {
    mSubresourceInfo.reset( InfoFactory::constructInfo( aResourceInfo, mName ) ); 
    // update the available resource for period 0
    // this function must be called after all the grades have been parsed and nograde set
    updateAvailable( 0 );

    // call completeInit for grades
    for( vector<Grade*>::iterator gradeIter = mGrade.begin(); gradeIter != mGrade.end(); gradeIter++ ) {
        ( *gradeIter )->completeInit( mSubresourceInfo.get() );
    }
    
    if( !mTechnology ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Missing technology in " << getXMLName() << ": "
                << aRegionName << ", " << aResourceName << ", " << mName << endl;
        abort();
    } else {
        mTechnology->completeInit( aRegionName, aResourceName, mName, mSubresourceInfo.get(), 0 );
    }

    // Interpolate and initialized exogenous resource variables created dynamically.
    const Modeltime* modeltime = scenario->getModeltime();

    // If unitialize, initialize following variables to null for final calibration period.
    const int FinalCalPer = modeltime->getFinalCalibrationPeriod();
    if( !mTechChange[ FinalCalPer ].isInited() ) {
        mTechChange[ FinalCalPer ] = 0;
    }

    // decrement from terminal period to copy backward the technical change for missing periods
    for( int per = modeltime->getmaxper() - 1; per > modeltime->getFinalCalibrationPeriod(); --per ) {
        if( !mTechChange[ per ].isInited() ){
            // Copy backwards.
            if( per < modeltime->getmaxper() - 1 ){
                mTechChange[ per ] = mTechChange[ per + 1 ];
            }
            // For unitialized terminal period.
            // This may occur if running GCAM time periods beyond read-in dataset and
            // fillout is not used.
            else{
                mTechChange[ per ] = 0;
            }
        }
    }
}

/*! \brief Perform any initializations needed for each period.
* \details Any initializations or calculations that only need to be done once per
*          period(instead of every iteration) should be placed in this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param aPeriod Model aPeriod
*/
void SubResource::initCalc( const string& aRegionName, const string& aResourceName,
                            const IInfo* aResourceInfo, const int aPeriod )
{
    // call grade initializations
    for (unsigned int i = 0; i < mGrade.size(); i++) {
        mGrade[i]->initCalc( aRegionName, aResourceName, aPeriod );
    }
    
    // Fill price added after it is calibrated.  This will interpolate to any
    // price adders read in the future or just copy forward if there is nothing
    // to interpolate to.
    const int finalCalPeriod = scenario->getModeltime()->getFinalCalibrationPeriod();
    if( aPeriod == finalCalPeriod + 1 ) {
        SectorUtils::fillMissingPeriodVectorInterpolated( mPriceAdder );
    }

    // If we are in a calibration period and a calibrated value was read in set the
    // flag on the market that this resource is fully calibrated.
    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* productInfo = marketplace->getMarketInfo( aResourceName, aRegionName, aPeriod, false );
    if( aPeriod <= finalCalPeriod && mCalProduction[ aPeriod ] > 0 && productInfo ) {
        productInfo->setBoolean( "fully-calibrated", true );
    }
    
    // Note we have to reset the CO2coefficient for the resource to zero before
    // the technology / output calls initCalc to avoid undesriable carbon accounting
    // (positive carbon in the output but no inputs = negative emissions).
    // we will then reset the value to what it was before after the call
    double resCCoef = productInfo ? productInfo->getDouble( "CO2coefficient", false ) : 0;
    if( productInfo ) {
        productInfo->setDouble( "CO2coefficient", 0.0 );
    }
    mTechnology->initCalc( aRegionName, aResourceName, aResourceInfo, 0, aPeriod );
    if( productInfo ) {
        productInfo->setDouble( "CO2coefficient", resCCoef );
    }
    
    // calculate total extraction cost for each grade
    for ( unsigned int gr=0; gr< mGrade.size(); gr++) {
        if ( aPeriod > 0) {
            const Modeltime* modeltime = scenario->getModeltime();
            mCumulativeTechChange[ aPeriod ] = mCumulativeTechChange[ aPeriod - 1 ] * 
                pow( ( 1.0 + mTechChange[ aPeriod ] ), modeltime->gettimestep( aPeriod ) );
        }
        // Determine cost
        mGrade[gr]->calcCost( mCumulativeTechChange[ aPeriod ], aPeriod );
    }
    
}

/*! \brief Perform any initializations needed after each period.
* \details Any initializations or calculations that only need to be done once
*          after each period(instead of every iteration) should be placed in
*          this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param period Model aPeriod
*/
void SubResource::postCalc( const string& aRegionName, const string& aResourceName, const int aPeriod ) {

    // Available is the total resource (stock) initialized in initCalc and
    // is the initial amount at the beginning of the period.
    // It does not subtract the amount used in that period.
    updateAvailable( aPeriod ); // reinitialize available amount
    if( aPeriod > 0 ) {
        mAvailable[ aPeriod ] -= mCumulProd[ aPeriod - 1 ];
        mAvailable[ aPeriod ] = max( mAvailable[ aPeriod ].get(), 0.0 );
    }

    // call grade post calculations.
    for( unsigned int i = 0; i < mGrade.size(); i++ ) {
        mGrade[i]->postCalc( aRegionName, aResourceName, aPeriod );
    }
    
    mTechnology->postCalc( aRegionName, aPeriod );
    
    // reset the supply curve bounds, the lower bound in particular
    // as the rule for setting the lower bound is the lowest price wins
    // (to accommodate global markets) but this can be problematic when target
    // in which case the amount depleted will vary between dispatches
    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( aResourceName, aRegionName, aPeriod, true );
    const string LOWER_BOUND_KEY = "lower-bound-supply-price";
    marketInfo->setDouble(LOWER_BOUND_KEY, util::getLargeNumber());
}

void SubResource::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, mName );

    // Write out data for the period we are in from the vectors.
    XMLWriteElement( mAvailable[ period ], "available", out, tabs );
    XMLWriteElement( mAnnualProd[ period ], "annualprod", out, tabs );
    XMLWriteElement( mCumulProd[ period ], "cumulprod", out, tabs );
    XMLWriteElement( mTechChange[ period ], "techChange", out, tabs );
    XMLWriteElement( mCalProduction[ period ], "cal-production", out, tabs );
    XMLWriteElement( mEffectivePrice[ period ], "effective-price", out, tabs );
    XMLWriteElement( mPriceAdder[ period ], "price-adder", out, tabs );

    // write out the grade objects.
    for( int i = 0; i < static_cast<int>( mGrade.size() ); i++ ){    
        mGrade[ i ]->toDebugXML( period, out, tabs );
    }
    
    mTechnology->toDebugXML( period, out, tabs );

    // finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& SubResource::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& SubResource::getXMLNameStatic() {
    static const string XML_NAME = "subresource";
    return XML_NAME;
}

//! return SubResource name
const std::string& SubResource::getName() const {
    return mName;
}

void SubResource::cumulsupply( const string& aRegionName, const string& aResourceName,
                               double aPrice, int aPeriod )
{
    // Always calculate the effective price
    ITechnology* currTech = mTechnology->getNewVintageTechnology( aPeriod );
    currTech->calcCost( aRegionName, aResourceName, aPeriod );
    mEffectivePrice[ aPeriod ] = aPrice + mPriceAdder[ aPeriod ] - currTech->getCost( aPeriod );
    
    double prevCumul = aPeriod != 0 ? mCumulProd[ aPeriod - 1 ] : 0.0;

    // Case 1
    // if market price is less than cost of first grade, then zero cumulative
    // production
    if ( mEffectivePrice[ aPeriod ] <= mGrade[0]->getCost( aPeriod )) {
        mCumulProd[ aPeriod ] = prevCumul;
    }
    
    // Case 2
    // if market price is in between cost of first and last grade, then calculate
    // cumulative production in between those grades
    if ( mEffectivePrice[ aPeriod ] > mGrade[0]->getCost( aPeriod ) && mEffectivePrice[ aPeriod ] <= mGrade[ mGrade.size() - 1 ]->getCost( aPeriod )) {
        mCumulProd[ aPeriod ] = 0;
        int i = 0;
        int iL = 0;
        int iU = 0;
        while ( mGrade[ i ]->getCost( aPeriod ) < mEffectivePrice[ aPeriod ] ) {
            iL=i; i++; iU=i;
        }
        // add subrsrcs up to the lower grade
        for ( i = 0; i <= iL; i++ ) {
            mCumulProd[ aPeriod ] += Value( mGrade[i]->getAvail() );
        }
        // price must reach upper grade cost to produce all of lower grade
        double slope = mGrade[iL]->getAvail()
            / ( mGrade[iU]->getCost( aPeriod ) - mGrade[iL]->getCost( aPeriod ) );
        mCumulProd[ aPeriod ] -= Value( slope * ( mGrade[iU]->getCost( aPeriod ) - mEffectivePrice[ aPeriod ] ) );
    }
    
    // Case 3
    // if market price greater than the cost of the last grade, then
    // cumulative production is the amount in all grades
    if ( mEffectivePrice[ aPeriod ] > mGrade[ mGrade.size() - 1 ]->getCost( aPeriod ) ) {
        mCumulProd[ aPeriod ] = 0;
        for ( unsigned int i = 0; i < mGrade.size(); i++ ) {
            mCumulProd[ aPeriod ] += Value( mGrade[i]->getAvail() );
        }
    }
    
    mCumulProd[ aPeriod ] = std::max( mCumulProd[ aPeriod ].get(), prevCumul );
}

double SubResource::getCumulProd( const int aPeriod ) const {
    return mCumulProd[ aPeriod ];
}

/*! Update the sub-resource availability for a period
* Resource depletion by grade is not calculated.
* This function only returns the maximum amount of resource
* available by grade.  
*
*/
void SubResource::updateAvailable( const int aPeriod ){
    mAvailable[ aPeriod ] = 0;
    for ( unsigned int i = 0; i < mGrade.size(); ++i ) {
        mAvailable[ aPeriod ] += Value( mGrade[ i ]->getAvail() );
    }
}

//! calculate annual supply
/*! Takes into account short-term capacity limits.
Note that cumulsupply() must be called before calling this function. */
void SubResource::annualsupply( const string& aRegionName, const string& aResourceName,
                                int aPeriod, const GDP* aGdp, double aPrice )
{
    const Modeltime* modeltime = scenario->getModeltime();
    // For period 0 use initial annual supply. Cumulative production is 0 for period 0
    if ( aPeriod >= 1) {
        // Calculate the annual production given that the cumulative production
        // for the period is known. Cumulative production for the current period
        // is equal to the cumulative production of the previous period plus the
        // current annual production times the timestep.  This assumes that
        // the production in the current period is the average production for
        // the whole timestep.
        mAnnualProd[ aPeriod ] = ( mCumulProd[ aPeriod ] - mCumulProd[ aPeriod - 1 ] ) 
                                / modeltime->gettimestep( aPeriod );
        
        if( mAnnualProd[ aPeriod ] <= 0) {
            mCumulProd[ aPeriod ] = mCumulProd[ aPeriod - 1 ];
            mAnnualProd[ aPeriod ] = 0.0;
        }
        
        mTechnology->getNewVintageTechnology( aPeriod )->
            production( aRegionName, aResourceName, mAnnualProd[ aPeriod ], 1.0, aGdp, aPeriod );

        // mAvailable is the total resource (stock) remaining
        mAvailable[ aPeriod ] = mAvailable[ aPeriod - 1 ] - ( mAnnualProd[ aPeriod ] * modeltime->gettimestep( aPeriod ) );
        mAvailable[ aPeriod ] = max( mAvailable[ aPeriod ].get(), 0.0 );
    }
}

//! return annual production for period
double SubResource::getAnnualProd( int aPeriod ) const {
    return mAnnualProd[ aPeriod ];
}

/*! \brief Update an output container for a SubResource.
* \param aVisitor Output container to update.
* \param aPeriod Period to update.
*/
void SubResource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSubResource( this, aPeriod );

    // Update the output container for the subresources.
    for( unsigned int i = 0; i < mGrade.size(); ++i ){
        mGrade[ i ]->accept( aVisitor, aPeriod );
    }
    mTechnology->accept( aVisitor, aPeriod );
    
    aVisitor->endVisitSubResource( this, aPeriod );
}

//! return available resource for period
double SubResource::getAvailable(int per) const {
    return mAvailable[per];
}

//! get variance
/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
double SubResource::getVariance() const {
    return 0.0;
}

//! get resource capacity factor
/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
double SubResource::getAverageCapacityFactor() const {
    return 0.0;
}

/*!
 * \brief Calculate the highest price for which a price change produces a nonzero supply response 
 * \details This is simply the cost of the highest grade for the
 *          subresource, adjusted by the price adder. 
 * \param aPeriod The current model period.
 * \author Robert Link
 */
double SubResource::getHighestPrice( const int aPeriod ) const
{
    if( mGrade.size() > 0 ) {
        double cost = mGrade[ mGrade.size() - 1 ]->getCost( aPeriod );
        return cost - mPriceAdder[ aPeriod ];
    }
    else {
        // We may have subresources with no grades in the case where we are simply
        // attempting to add a region to a global market and that region does not
        // have any supply potential.
        // In that case we return some small number and let another region set the
        // high price for the market.
        return -util::getLargeNumber();
    }
}

/*!
 * \brief Calculate the lowest price for which a price change produces a nonzero supply response 
 * \details This one is slightly more complicated than the upper
 *          bound.  For a depletable resource we have to back out the
 *          production that has occurred in previous time periods so
 *          that we produce the minimum price that will produce a
 *          change in the annual supply.
 * \param aPeriod The current model period.
 */
double SubResource::getLowestPrice( const int aPeriod ) const
{
    if(mGrade.size() == 0) {
        // We may have subresources with no grades in the case where we are simply
        // attempting to add a region to a global market and that region does not
        // have any supply potential.
        // In that case we return some large number and let another region set the
        // low price for the market.
        return util::getLargeNumber();
    }

    double depleted = aPeriod != 0 ? mCumulProd[ aPeriod - 1 ] : 0;


    // figure out what's the lowest grade that hasn't been fully
    // depleted, and how much has been used from that grade.
    unsigned i;
    double avail = 0.0;
    for( i=0; i < mGrade.size(); ++i ) {
        avail = mGrade[i]->getAvail();
        if( avail > 0 && avail >= depleted ) {
            // this grade still has some left in it
            break;
        }
        else {
            depleted -= avail;
        }
    }

    // check to see if we're on the highest grade.  This shouldn't
    // happen because the highest grade should be a dummy with
    // availability zero, but you never know.
    if( i >= mGrade.size() - 1 ) {
        // the resource is wholly depleted (or the dummy is missing,
        // which will cause similar problems).  That means there isn't
        // any price where we will have a real supply response.
        // Arbitrarily pick 95% of the last grade cost.
        double value = 0.95 * mGrade[ mGrade.size() - 1 ]->getCost( aPeriod );
        return value;
    }
    else {
        // reverse-interpolate the price at which annual supply will
        // be > 0.
        double priceLow = mGrade[ i ]->getCost( aPeriod );
        double priceHigh = mGrade[ i + 1 ]->getCost( aPeriod );
        double deltaPrice = priceHigh - priceLow;
        double value = priceLow + deltaPrice * depleted / avail - mPriceAdder[aPeriod];

        return value;
    }
}
