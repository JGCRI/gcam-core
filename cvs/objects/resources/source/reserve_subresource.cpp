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
* \file reserve_subresource.cpp
* \ingroup Objects
* \brief ReserveSubResource class source file.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include <vector>
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

//#include "containers/include/scenario.h"
//#include "util/base/include/model_time.h"
//#include "marketplace/include/marketplace.h"
#include "resources/include/reserve_subresource.h"
#include "resources/include/grade.h"
#include "util/base/include/xml_helper.h"
//#include "containers/include/info_factory.h"
//#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
//#include "sectors/include/sector_utils.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/technology_container.h"
#include "technologies/include/resource_reserve_technology.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Default constructor.
ReserveSubResource::ReserveSubResource():
mTechnology( 0 )
{
}

//! Destructor.
ReserveSubResource::~ReserveSubResource() {
    delete mTechnology;
}


/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz, Sonny Kim
* \warning markets are not necessarily set when completeInit is called
*/
void ReserveSubResource::completeInit( const std::string& aRegionName, const std::string& aResourceName,
                                       const IInfo* aResourceInfo )
{
    SubResource::completeInit( aRegionName, aResourceName, aResourceInfo );

    if( !mTechnology ) {
        abort();
    } else {
        mTechnology->completeInit( aRegionName, aResourceName, mName, aResourceInfo, 0 );
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
void ReserveSubResource::initCalc( const string& aRegionName, const string& aResourceName,
                                   const IInfo* aResourceInfo, const int aPeriod )
{
    SubResource::initCalc( aRegionName, aResourceName, aResourceInfo, aPeriod );

    mTechnology->initCalc( aRegionName, aResourceName, aResourceInfo, 0, aPeriod );
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
#include "util/base/include/auto_file.h"
void ReserveSubResource::postCalc( const string& aRegionName, const string& aResourceName, const int aPeriod ) {
    SubResource::postCalc( aRegionName, aResourceName, aPeriod );
    
    mTechnology->postCalc( aRegionName, aPeriod );
    
    if( aRegionName == "European Free Trade Association" && mName == "crude oil") {
    AutoOutputFile temp("temp_debug_"+util::toString(aPeriod)+".txt");
    Tabs tabs;
    SubResource::toDebugXML( aPeriod, *temp, &tabs);
    mTechnology->toDebugXML( aPeriod, *temp, &tabs);
    }
}

bool ReserveSubResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node ) {
    if( nodeName == ResourceReserveTechnology::getXMLNameStatic() ) {
        parseSingleNode( node, mTechnology, new TechnologyContainer() );
        return true;
    }
    else if( nodeName == "cal-reserve" ) {
        XMLHelper<Value>::insertValueIntoVector( node, mCalReserve, scenario->getModeltime() );
        return true;
    }
    return false;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& ReserveSubResource::getXMLName() const {
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
const std::string& ReserveSubResource::getXMLNameStatic() {
    static const string XML_NAME = "reserve-subresource";
    return XML_NAME;
}

void ReserveSubResource::cumulsupply( double aPrice, int aPeriod ) {
    // TODO: not ovverride this method?
    if( mCalReserve[ aPeriod ].isInited() ) {
        calCumulsupply( aPrice, aPeriod );
    }
    else if( aPeriod == 0 ) {
        mCumulProd[ aPeriod ] = 0;
        return;
    }

    double prevCumul = aPeriod > 0 ? mCumulProd[ aPeriod - 1 ] : 0.0;
    mEffectivePrice[ aPeriod ] = aPrice + mPriceAdder[ aPeriod ];

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
    
    /*if( mCalReserve[ aPeriod ].isInited() ) {
        mCumulProd[ aPeriod ] = prevCumul + mCalReserve[ aPeriod ];
    }
    else {*/
        mCumulProd[ aPeriod ] = std::max( mCumulProd[ aPeriod ].get(), prevCumul );
    //}
}

void ReserveSubResource::calCumulsupply( double aPrice, int aPeriod ) {
    double prevCumul = aPeriod > 0 ? mCumulProd[ aPeriod - 1 ] : 0.0;
    
    // First, calculate the cumulative production.  This is equal to
    // cumulative production in the previous period plus the calibrated
    // production times the timestep.  Note: this assumes constant production
    // in all years with in a timestep. This is necessary to prevent erratic
    // production in the future.
    
    double tempCumulProd = prevCumul + mCalReserve[ aPeriod ];
    
    // Next, determine which grade of resource is produced to get to the
    // cumulative production needed.
    double temp_cumulative = 0.0;
    int gr_ind = 0;
    double gr_avail = 0.0;
    while ( temp_cumulative < tempCumulProd && gr_ind < mGrade.size() ) {
        gr_avail = mGrade[ gr_ind ]->getAvail();
        temp_cumulative += gr_avail;
        gr_ind++;
    }
    
    // not enough in supply curve to meet calibration
    if( gr_ind == mGrade.size() ) {
        mPriceAdder[ aPeriod ] = mGrade[ gr_ind - 1 ]->getCost( aPeriod ); // TODO?
        return;
    }
    
    // Then, calculate the fraction of the next grade that will be produced
    double fractGrade = 0.0;
    if ( gr_avail > 0.0 ) {
        fractGrade = ( tempCumulProd - ( temp_cumulative - gr_avail ) )
        / gr_avail;
    }
    
    // Next, calculate the effective price.  This is the price needed
    // to produce the calibrated production quantity in this period.
    // mEffectivePrice = cost of the next highest grade -
    // ( 1 - fractGrade )*( cost of higher grade - cost of lower grade )
    double low_cost = 0.0;
    if ( gr_ind > 0 ) {
        low_cost = mGrade[ gr_ind - 1 ]->getCost( aPeriod );
    }
    double tempEffectivePrice = mGrade[ gr_ind ]->getCost( aPeriod ) -
        ( 1 - fractGrade ) * ( mGrade[ gr_ind ]->getCost( aPeriod ) - low_cost );
    
    double mktPrice = aPrice;
    
    // Finally, calculate the price adder. This is the difference between the
    // effective price and the global price
    mPriceAdder[ aPeriod ] = tempEffectivePrice - mktPrice;
}

/*
double ReserveSubResource::getCumulProd( const int aPeriod ) const {
    return mCumulProd[ aPeriod ];
}
*/

/*! Update the sub-resource availability for a period
* Resource depletion by grade is not calculated.
* This function only returns the maximum amount of resource
* available by grade.  
*
*/
/*
void ReserveSubResource::updateAvailable( const int aPeriod ){
    mAvailable[ aPeriod ] = 0;
    for ( unsigned int i = 0; i < mGrade.size(); ++i ) {
        mAvailable[ aPeriod ] += Value( mGrade[ i ]->getAvail() );
    }
}
*/

//! calculate annual supply
/*! Takes into account short-term capacity limits.
Note that cumulsupply() must be called before calling this function. */
void ReserveSubResource::annualsupply( const string& aRegionName, const string& aResourceName,
                                       int aPeriod, const GDP* aGdp, double aPrice, double aPrev_price )
{
    double prevCumul = aPeriod > 0 ? mCumulProd[ aPeriod - 1] : 0.0;
    double newReserves = mCumulProd[ aPeriod ] - prevCumul;
    
    // get fixed output as we may scale to match calibration
    // note we still need to call Technology::getFixedOutput to set the correct marginal revenue
    // even when not calibrating
    double fixedScaleFactor = 1.0;
    double fixedOutput = 0;
    for( auto techIter = mTechnology->getVintageBegin( aPeriod ); techIter != mTechnology->getVintageEnd( aPeriod ); ++techIter ) {
        fixedOutput += (*techIter).second->getFixedOutput( aRegionName, aResourceName, false, "", mEffectivePrice[ aPeriod ], aPeriod );
    }
    if( mCalProduction[ aPeriod ] != -1 ) {
        fixedScaleFactor = mCalProduction[ aPeriod ] == 0.0 ? 0.0 : mCalProduction[ aPeriod ] / ( fixedOutput + /* TOOD: move into tech? */ mCalReserve[ aPeriod ] / static_cast<double>( mTechnology->getNewVintageTechnology( aPeriod )->getLifetimeYears() ) );
    }

    /*if( aRegionName == "USA" ) {
        cout << "fixedOutput: " << fixedOutput << ", New inv: " << (mCalReserve[ aPeriod ] / 60.0) << ", scale: " << fixedScaleFactor << endl;
    }*/
    for( auto techIter = mTechnology->getVintageBegin( aPeriod ); techIter != mTechnology->getVintageEnd( aPeriod ); ++techIter ) {
        (*techIter).second->production( aRegionName, aResourceName, newReserves, fixedScaleFactor, aGdp, aPeriod );
    }
    
    // The technologies will add output to the market and the resource would want to as well.
    // we will avoid that by just telling the resource there was zero annual production.
    mAnnualProd[ aPeriod ] = 0;
}

//! return annual production for period
/*
double ReserveSubResource::getAnnualProd( int aPeriod ) const {
    return mAnnualProd[ aPeriod ];
}
*/

/*! \brief Update an output container for a ReserveSubResource.
* \param aVisitor Output container to update.
* \param aPeriod Period to update.
*/
void ReserveSubResource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSubResource( this, aPeriod );

    // Update the output container for the subresources.
    for( unsigned int i = 0; i < mGrade.size(); ++i ){
        mGrade[ i ]->accept( aVisitor, aPeriod );
    }
    
    mTechnology->accept( aVisitor, aPeriod );
    
    aVisitor->endVisitSubResource( this, aPeriod );
}

//! return available resource for period
/*
double ReserveSubResource::getAvailable(int per) const {
    return mAvailable[per];
}
*/

/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
/*
double ReserveSubResource::getVariance() const {
    return 0.0;
}
*/

//! get resource capacity factor
/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
/*
double ReserveSubResource::getAverageCapacityFactor() const {
    return 0.0;
}
*/

/*!
 * \brief Calculate the highest price for which a price change produces a nonzero supply response 
 * \details This is simply the cost of the highest grade for the
 *          subresource, adjusted by the price adder. 
 * \param aPeriod The current model period.
 * \author Robert Link
 */
double ReserveSubResource::getHighestPrice( const int aPeriod ) const
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
double ReserveSubResource::getLowestPrice( const int aPeriod ) const
{
    // TODO
    return 0.0;
}

