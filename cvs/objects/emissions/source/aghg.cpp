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
* \file ghg.cpp
* \ingroup Objects
* \brief Ghg class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"

#include <cassert>

#include "emissions/include/aghg.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "functions/include/iinput.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/icapture_component.h"
#include "marketplace/include/cached_market.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

//! Default constructor.
AGHG::AGHG()
{
}

//! Destructor
AGHG::~AGHG(){
}

//! Copy helper function.
void AGHG::copy( const AGHG& aOther ){
    mName = aOther.mName;
    mEmissionsUnit = aOther.mEmissionsUnit;

    // Note results (such as emissions) are never copied.
}

//! Writes datamembers to debugging datastream in XML format.
void AGHG::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag( getXMLName(), aOut, aTabs, getName() );

    // write xml for data members
    XMLWriteElement( mEmissions[ aPeriod ], "emission", aOut, aTabs );

    toDebugXMLDerived( aPeriod, aOut, aTabs );
    // done writing xml for data members.

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*!
 * \brief Returns the name of ghg gas.
 * \return A string representing the name of the ghg gas.
 */
const string& AGHG::getName() const {
    return mName;
}

/*!
 * \brief Complete the initialization of the ghg object.
 * \note This routine is only called once per model run
 * \param aRegionName Region name.
 * \param aSectorName Sector name, also the name of the product.
 * \param aTechInfo Technology information object.
 * \author Pralit Patel
 * \warning Markets are not necessarily set when completeInit is called
 */
void AGHG::completeInit( const string& aRegionName, const string& aSectorName,
                         const IInfo* aTechInfo )
{
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                      aRegionName,
                                                                      getName(),
                                                                      aRegionName,
                                                                      false );
}

/*!
 * \brief Perform initializations that only need to be done once per period.
 * \param aRegionName Region name.
 * \param aLocalInfo The local information object.
 * \param aPeriod Model period.
 */
void AGHG::initCalc( const string& aRegionName, const IInfo* aLocalInfo, const int aPeriod ) {
    mCachedMarket = scenario->getMarketplace()->locateMarket( getName(), aRegionName, aPeriod );
}

/*!
 * \brief Sets the emissions as the demand side of the gas market.
 * \param aRegionName the region to set
 * \param aPeriod the period
 */
void AGHG::addEmissionsToMarket( const string& aRegionName, const int aPeriod ){
    // set emissions as demand side of gas market
    mCachedMarket->addToDemand( getName(), aRegionName,
                                mEmissions[ aPeriod ],
                                aPeriod, false );
}

/*! Second Method: Convert GHG tax and any storage costs into energy units using
*   GHG coefficients and return the value or cost of the tax and storage for the
*   GHG. Apply taxes only if emissions occur. Emissions occur if there is a
*   difference in the emissions coefficients.
*  \param aInput Input for which to calculate the carbon tax.
*  \param aRegionName The name of the current region.
*  \param aGoodName The name of the output product.
*  \param aSequestrationDevice A capture component which will adjust the cost.
*  \param aPeriod The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*  \todo Collapsing two methods.
*  \note This method is only used by SGM.
*/
double AGHG::getGHGValue( const IInput* aInput, const string& aRegionName,
                          const string& aGoodName,
                          const ICaptureComponent* aSequestrationDevice,
                          const int aPeriod ) const
{
    // Determine if there is a tax.
    double ghgTax = mCachedMarket->getPrice( getName(), aRegionName, aPeriod, false );
    if( ghgTax == Marketplace::NO_MARKET_PRICE ){
        ghgTax = 0;
    }

    // Get the emissions coef for the input.
    double currInputGasCoef = aInput->getCO2EmissionsCoefficient( getName(), aPeriod );

    // Get the remove fraction
    double removeFract = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( getName() ) : 0;

    // Get the storage cost of sequestered emissions
    double storageCost = aSequestrationDevice ? aSequestrationDevice->getStorageCost( aRegionName, getName(), 
        aPeriod ) : 0;

    // Return the rate.
    return ( ( 1 - removeFract ) * ghgTax + removeFract * storageCost ) * currInputGasCoef;
}

/*! Second Method: Convert GHG tax and any storage costs into energy units using
*   GHG coefficients and return the value or cost of the tax and storage for the
*   GHG. Apply taxes only if emissions occur. Emissions occur if there is a
*   difference in the emissions coefficients.
*  \param aOutput Output for which to calculate the carbon tax.
*  \param aRegionName The name of the current region.
*  \param aGoodName The name of the output product.
*  \param aSequestrationDevice A capture component which will adjust the cost.
*  \param aPeriod The period in which this calculation is occurring. 
*  \return Generalized cost or value of the GHG
*  \todo Collapsing two methods.
*  \note This method is only used by SGM.
*/
double AGHG::getGHGValue( const IOutput* aOutput, const string& aRegionName,
                          const string& aGoodName,
                          const ICaptureComponent* aSequestrationDevice,
                          const int aPeriod ) const
{
    // Determine if there is a tax.
    double ghgTax = mCachedMarket->getPrice( getName(), aRegionName, aPeriod, false );
    if( ghgTax == Marketplace::NO_MARKET_PRICE ){
        ghgTax = 0;
    }

    // Get the emissions coef for the output.
    double currOutputGasCoef = aOutput->getEmissionsPerOutput( getName(), aPeriod );

    // Get the remove fraction
    double removeFract = aSequestrationDevice ? aSequestrationDevice->getRemoveFraction( getName() ) : 0;

    // Get the storage cost of sequestered emissions
    double storageCost = aSequestrationDevice ? aSequestrationDevice->getStorageCost( aRegionName, getName(), 
        aPeriod ) : 0;

    // Return the rate.
    return ( ( 1 - removeFract ) * ghgTax + removeFract * storageCost ) * currOutputGasCoef;
}

/*!
 * \brief Returns GHG emissions.
 * \param aPeriod The model period.
 * \return GHG emissions amount.
 */
double AGHG::getEmission( const int aPeriod ) const {
    assert( aPeriod < static_cast<int>( mEmissions.size() ) );
    return mEmissions[ aPeriod ];
}

/*!
 * \brief Update a visitor with information from a GHG for a given period.
 * \param aVisitor The visitor to update.
 * \param aPeriod The period for which to update.
 */
void AGHG::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitGHG( this, aPeriod );
    aVisitor->endVisitGHG( this, aPeriod );
}

/*!
 * \brief Hook for a ghg to do interpolations to fill in any data that
 *        should be interpolated to a newly created ghg for the missing
 *        technology.
 * \param aYear the year to be filled in.
 * \param aPreviousYear The year of the last parsed ghg.
 * \param aNextYear The year of the next closest parsed ghg.
 * \param aPreviousGHG The previous parsed ghg.
 * \param aNextGHG The next parsed ghg.
 */
void AGHG::doInterpolations( const int aYear, const int aPreviousYear,
                             const int aNextYear, const AGHG* aPreviousGHG,
                             const AGHG* aNextGHG )
{
    // the default is to not interpolate anything
}
