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

#include "resources/include/reserve_subresource.h"
#include "resources/include/grade.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
#include "technologies/include/resource_reserve_technology.h"
#include "technologies/include/technology_container.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor.
ReserveSubResource::ReserveSubResource():
mAvgProdLifetime( 0 )
{
}

//! Destructor.
ReserveSubResource::~ReserveSubResource() {
}

void ReserveSubResource::completeInit( const std::string& aRegionName, const std::string& aResourceName,
                                       const IInfo* aResourceInfo )
{
    if( mAvgProdLifetime <= 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "No average production lifetime set in " << getXMLName() << ": "
                << aRegionName << ", " << aResourceName << ", " << mName << endl;
        abort();
    }
    IInfo* currInfo = InfoFactory::constructInfo( aResourceInfo, mName );
    currInfo->setDouble( "average-production-lifetime", mAvgProdLifetime );
    SubResource::completeInit( aRegionName, aResourceName, currInfo );
    delete currInfo;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
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
* \return The constant XML_NAME as a static.
*/
const std::string& ReserveSubResource::getXMLNameStatic() {
    static const string XML_NAME = "reserve-subresource";
    return XML_NAME;
}

//! calculate annual supply
/*! Takes into account short-term capacity limits.
Note that cumulsupply() must be called before calling this function. */
void ReserveSubResource::annualsupply( const string& aRegionName, const string& aResourceName,
                                       int aPeriod, const GDP* aGdp, double aPrice )
{
    double prevCumul = aPeriod > 0 ? mCumulProd[ aPeriod - 1] : 0.0;
    double newReserves = mCumulProd[ aPeriod ] - prevCumul;
    
    // get fixed output as we may need scale to match calibration
    // note we still need to call Technology::getFixedOutput to set the correct marginal revenue
    // even when not calibrating
    double fixedScaleFactor = 1.0;
    double fixedOutput = 0;
    double nonTechInvestmentCost = mEffectivePrice[ aPeriod ] - mTechnology->getNewVintageTechnology( aPeriod )->getCost( aPeriod );
    for( auto techIter = mTechnology->getVintageBegin( aPeriod ); techIter != mTechnology->getVintageEnd( aPeriod ); ++techIter ) {
        fixedOutput += (*techIter).second->getFixedOutput( aRegionName, aResourceName, false, "", nonTechInvestmentCost, aPeriod );
    }
    if( mCalProduction[ aPeriod ] != -1 ) {
        // Calculate the ratio from the actual production to the calibrated production
        // to use as the fixedScaleFactor which we can then use in Technology::production
        // to scale and ensure production matches calibration.
        fixedScaleFactor = mCalProduction[ aPeriod ] == 0.0 ? 0.0 :
            mCalProduction[ aPeriod ] /
                ( fixedOutput + mCalReserve[ aPeriod ] / static_cast<double>( mAvgProdLifetime ) );
    }

    // Calculate production from all vintages
    double totalProduction = 0.0;
    for( auto techIter = mTechnology->getVintageBegin( aPeriod ); techIter != mTechnology->getVintageEnd( aPeriod ); ++techIter ) {
        (*techIter).second->production( aRegionName, aResourceName, newReserves, fixedScaleFactor, aGdp, aPeriod );
        totalProduction += (*techIter).second->getOutput( aPeriod );
    }
    mAnnualProd[ aPeriod ] = totalProduction;
}

/*! \brief Update an output container for a ReserveSubResource.
* \param aVisitor Output container to update.
* \param aPeriod Period to update.
*/
void ReserveSubResource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitReserveSubResource( this, aPeriod );

    // Update the output container for the subresources.
    for( unsigned int i = 0; i < mGrade.size(); ++i ){
        mGrade[ i ]->accept( aVisitor, aPeriod );
    }
    
    mTechnology->accept( aVisitor, aPeriod );
    
    aVisitor->endVisitReserveSubResource( this, aPeriod );
}

/*!
 * \brief Calculate the lowest price for which a price change produces a nonzero supply response 
 * \details This one is not straight forward due to vintaging.  In principal we could
 *          have supply with a price all the way down to zero so that is the best we
 *          can give for an estimate without doing a lot of extra work.
 * \param aPeriod The current model period.
 */
double ReserveSubResource::getLowestPrice( const int aPeriod ) const
{
    // TODO: potentially could do better if, for instance, there were no vintages
    // that are producing
    return 0.0;
}

