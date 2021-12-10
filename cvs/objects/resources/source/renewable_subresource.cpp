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
* \file renewable_subresource.cpp
* \ingroup Objects
* \brief SubRenewableResource class source file.
* \author Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "resources/include/grade.h"
#include "resources/include/renewable_subresource.h"
#include "resources/include/subresource.h"
#include "containers/include/gdp.h"
#include "containers/include/iinfo.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/ivisitor.h"
#include "technologies/include/itechnology_container.h"
#include "technologies/include/itechnology.h"

using namespace std;

extern Scenario* scenario;

//! Constructor
SubRenewableResource::SubRenewableResource(void):
mMaxAnnualSubResource( 0.0 ),
mGdpSupplyElasticity( 0 )
{
}

SubRenewableResource::~SubRenewableResource(){
}

const std::string& SubRenewableResource::getXMLName() const{
    return getXMLNameStatic();
}

const std::string& SubRenewableResource::getXMLNameStatic(){
    static const std::string XMLName = "sub-renewable-resource";
    return XMLName;
}

//! Do any initializations needed for this resource
/*! Renewable resources should have only grades with well defined cost curves. 
\todo The extra elements in the vector should be removed. 
Also remove any grades with zero available by resetting the parameter nograde. */
void SubRenewableResource::completeInit( const string& aRegionName, const string& aResourceName,
                                         const IInfo* aSectorInfo ) {

    SubResource::completeInit( aRegionName, aResourceName, aSectorInfo );
    
    double lastAvailable = 0;
    
    for( vector<Grade*>::iterator i = mGrade.begin(); i != mGrade.end(); ++i ){
        if( i != mGrade.begin() && (*i)->getAvail() <= lastAvailable ){
            // Remove the bad grade.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Removing invalid grade in subresource " << mName << "." << endl;
            delete *i;
            mGrade.erase( i-- ); 
        }
        else {
            lastAvailable = (*i)->getAvail();
        }
    }
    
    if( !mGrade.empty() && mGrade[ 0 ]->getAvail() != 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Non-zero initial grade available is ignored in " << getXMLNameStatic()
                << " " << mName << "." << endl;
    }
}

//! Cumulative Production
/*! Cumulative production Is not needed for renewable resources. But still do
*   any preliminary calculations that need to be done before calculating
*   production 
*/

void SubRenewableResource::cumulsupply( const string& aRegionName, const string& aResourceName,
                                        double aPrice, int aPeriod )
{
    // Cumulative supply is not utilize for annual production.
    // Calculate cumulative production for reporting after calling annual supply for this
    // subresource.
}

//! calculate annual supply 
/*! Annual production (supply) is placed into variable (into variable annualprod[]).
* For renewable resources interprets parameters as a cost curve.
* Technological change is applied if present. 
* Note that the cost curve needs to be in the form of price, and cumulative fraction available.
*/
void SubRenewableResource::annualsupply( const string& aRegionName, const string& aResourceName,
                                         int aPeriod, const GDP* aGdp, double aPrice )
{
    ITechnology* currTech = mTechnology->getNewVintageTechnology( aPeriod );
    currTech->calcCost( aRegionName, aResourceName, aPeriod );
    double fractionAvailable = -1;
    const double effectivePrice = aPrice + mPriceAdder[ aPeriod ] - currTech->getCost( aPeriod );

    // Move up the cost curve until a point is found above the current price.
    for ( unsigned int i = 0; i < mGrade.size(); ++i ) {
        if( effectivePrice <= mGrade[ i ]->getCost( aPeriod ) ) {
            if( i == 0 ) {
                // Below the bottom of the supply curve which means the fraction
                // available is zero.
                fractionAvailable = 0;
            }
            else {
                // Determine the cost and available for the previous
                // point. 
                double prevGradeCost = mGrade[ i - 1 ]->getCost( aPeriod );
                double prevGradeAvailable = mGrade[ i - 1 ]->getAvail();

                // This should not be able to happen because the above if
                // statement would fail.
                assert( mGrade[ i ]->getCost( aPeriod ) > prevGradeCost );
                double gradeFraction = ( effectivePrice - prevGradeCost )
                    / ( mGrade[ i ]->getCost( aPeriod ) - prevGradeCost );
                // compute production as fraction of total possible
                fractionAvailable = prevGradeAvailable + gradeFraction
                    * ( mGrade[ i ]->getAvail() - prevGradeAvailable ); 
            }

            break;
        }
    }

    // If the fraction available has not been set there is not a point with a
    // cost greater than the price. This means the price is above the curve.
    if( fractionAvailable == -1 ){
        // Calculate the total fraction of the max subresource to use. Note that
        // the max fraction available can be more than 100 percent.
        double maxFraction = mGrade[ mGrade.size() - 1 ]->getAvail();
        fractionAvailable = maxFraction;
    }

    // Calculate the amount of resource expansion due to GDP increase.
    double resourceSupplyIncrease = pow( aGdp->getApproxGDP( aPeriod ) / aGdp->getApproxGDP( 0 ),
                                         mGdpSupplyElasticity );

    // now convert to absolute value of production
    mAnnualProd[ aPeriod ] = fractionAvailable * mMaxAnnualSubResource[aPeriod] * resourceSupplyIncrease;
    
    currTech->production( aRegionName, aResourceName, mAnnualProd[ aPeriod ], 1.0, aGdp, aPeriod );

    // This subresource does not utilize a cumualtive supply curve.
    // Calculate cumulative production from annunal production values.
    if ( aPeriod == 0 ) {
        mCumulProd[ aPeriod ] = 0.0;
    }
    else {
        mCumulProd[ aPeriod ] = ( mAnnualProd[aPeriod] + mAnnualProd[aPeriod - 1] ) / 2
        * scenario->getModeltime()->gettimestep( aPeriod ) + mCumulProd[aPeriod - 1];
    }
}

double SubRenewableResource::getMaxAnnualSubResource( const int aPeriod ) const {
    return mMaxAnnualSubResource[ aPeriod ];
}

/*!
 * \brief Calculate the lowest price for which a price change produces a nonzero supply response
 * \details For renewable resources, it's simple; it's just the cost of the lowest grade.
 * \param aPeriod The current model period.
 */
double SubRenewableResource::getLowestPrice( const int aPeriod ) const
{
    double cost = 0.0;
    if( mGrade.size() > 0 ) {
        cost = mGrade[0]->getCost( aPeriod );
    }
    else {
        // We may have subresources with no grades in the case where we are simply
        // attempting to add a region to a global market and that region does not
        // have any supply potential.
        // In that case we return some large number and let another region set the
        // low price for the market.
        cost = util::getLargeNumber();
    }
    return cost;
}

/*! \brief Update an output container for a SubRenewableResource.
 * \param aVisitor Output container to update.
 * \param aPeriod Period to update.
 */
void SubRenewableResource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSubRenewableResource( this, aPeriod );
    
    // Update the output container for the subresources.
    for( unsigned int i = 0; i < mGrade.size(); ++i ){
        mGrade[ i ]->accept( aVisitor, aPeriod );
    }
    mTechnology->accept( aVisitor, aPeriod );
    
    aVisitor->endVisitSubRenewableResource( this, aPeriod );
}
