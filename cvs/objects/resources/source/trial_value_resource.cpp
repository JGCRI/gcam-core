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
 * \file unlimited_resource.cpp
 * \ingroup Objects
 * \brief UnlimitedResource class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"

#include <string>
#include <vector>
#include <cassert>

#include "resources/include/trial_value_resource.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/imarket_type.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/sector_utils.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Get the XML name of the class.
 * \return The XML name of the class.
 */
const string& TrialValueResource::getXMLNameStatic(){
    static const string XML_NAME = "trial-value-resource";
    return XML_NAME;
}

//! Constructor.
TrialValueResource::TrialValueResource()
{
}

//! Destructor.
TrialValueResource::~TrialValueResource() {
}

const string& TrialValueResource::getXMLName() const {
    return getXMLNameStatic();
}

void TrialValueResource::toDebugXML( const int aPeriod,
                                    ostream& aOut,
                                    Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );

    // Write the xml for the class members.
    // Write out the market string.
    XMLWriteElement( mOutputUnit, "output-unit", aOut, aTabs );
    XMLWriteElement( mPriceUnit, "price-unit", aOut, aTabs );
    XMLWriteElement( mMarket, "market", aOut, aTabs );
    XMLWriteElement( mMinPrice, "min-price", aOut, aTabs );
    XMLWriteElement( mMinPrice, "max-price", aOut, aTabs );

    // finished writing xml for the class members.

    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void TrialValueResource::completeInit( const string& aRegionName,
                                      const IInfo* aRegionInfo )
{
    // default unit to EJ
    if ( mOutputUnit.empty() ) {
        mOutputUnit = "EJ"; 
    }
    // default unit to $/GJ
    if ( mPriceUnit.empty() ) {
        mPriceUnit = "1975$/GJ"; 
    }

    // Setup markets for this resource.
    setMarket( aRegionName );
}

void TrialValueResource::initCalc( const string& aRegionName,
                                  const int aPeriod )
{
}

void TrialValueResource::postCalc( const string& aRegionName,
                                  const int aPeriod )
{
}


const string& TrialValueResource::getName() const {
    return mName;
}

void TrialValueResource::calcSupply( const string& aRegionName,
                                    const int aPeriod )
{
}

double TrialValueResource::getAnnualProd( const string& aRegionName,
                                         const int aPeriod ) const
{
    // Return the market supply.
    return 0.0;
}

//! Return price of resources.
double TrialValueResource::getPrice( const int aPeriod ) const {
    return 0.0;
}

/*
* \brief Create the resource market.
* \details The unlimited resource creates a single unsolved market for the
*          resource. The object will ensure that supply is always equal to
*          demand.
* \param aRegionName Region name.
*/
void TrialValueResource::setMarket( const string& aRegionName ) {
    // Setup the market for the resource. This market will not be solved. Note
    // that in a standard Resource setMarketToSolve would be called here.
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    marketplace->createMarket( aRegionName, mMarket, mName, IMarketType::TRIAL_VALUE );

    // Set price and output units for period 0 market info
    IInfo* marketInfo = marketplace->getMarketInfo( mName, aRegionName, 0, true );
    marketInfo->setString( "price-unit", mPriceUnit );
    marketInfo->setString( "output-unit", mOutputUnit );
    
    // if we have a min/max value set that otherwise set to NaN
    // so it will be ignored in SectorUtils::setSupplyBehaviorBounds
    double minValue = mMinPrice.isInited() ? mMinPrice : std::numeric_limits<double>::signaling_NaN();
    double maxValue = mMaxPrice.isInited() ? mMaxPrice : std::numeric_limits<double>::signaling_NaN();
    
    for( int period = 1; period < modeltime->getmaxper(); ++period ) {
        marketplace->setMarketToSolve( mName, aRegionName, period );
        SectorUtils::setSupplyBehaviorBounds( mName, aRegionName, minValue, maxValue, period );
    }
}

void TrialValueResource::accept( IVisitor* aVisitor,
                                const int aPeriod ) const
{
    aVisitor->startVisitResource( this, aPeriod );
    aVisitor->endVisitResource( this, aPeriod );
}
