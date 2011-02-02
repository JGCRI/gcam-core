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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */

/*!
 * \file forest_land_leaf.cpp
 * \ingroup Objects
 * \brief ForestLandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "land_allocator/include/forest_land_leaf.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Default constructor.
 * \param aParent Pointer to this leafs's parent.
 * \param aName String representing product's name.
 * \author James Blackwood
 */
ForestLandLeaf::ForestLandLeaf( const ALandAllocatorItem* aParent,
                                const string& aName  ):
LandLeaf( aParent, aName ),
mSteps( -1 )
{
}

//! Destructor
ForestLandLeaf::~ForestLandLeaf() {
}

const string& ForestLandLeaf::getXMLName() const {
    const static string XML_NAME = "ForestLandAllocatorLeaf";
    return XML_NAME;
}

void ForestLandLeaf::completeInit( const string& aRegionName, 
                                   const IInfo* aRegionInfo )
{
    const Modeltime* modeltime = scenario->getModeltime();

    // rotationPeriod is passed through regionInfo which is then used to
    // calculate the number of steps.
    int rotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
    mRotationPeriod = rotationPeriod;
    
    // reset yield and land variables to be larger
    int maxper = modeltime->getmaxper();
    // Note: this is assuming constant steps equal to that of the last model period
    // for periods after the last model period.
    mSteps = rotationPeriod / modeltime->gettimestep( maxper -1  );
    mYield.resize( maxper + mSteps );
    
    // these may or may not have been sized by now
    if( ( maxper + mSteps ) > mMaxYield.size() ) {
        // resize and don't forget the fill new periods with -1
        mMaxYield.resize( maxper + mSteps, -1 );
    }
    if( ( maxper + mSteps ) > mLandToBeHarvested.size() ) {
        mLandToBeHarvested.resize( maxper + mSteps );
    }

    LandLeaf::completeInit( aRegionName, aRegionInfo );

    // Resize to account for extra forest periods
    mAgProdChange.resize( maxper + mSteps );
}

/*!
* \brief Calculates the land allocated to a particular type
* \details Land allocation is the product of the land 
*          allocated to the parent node and the share 
*          of land specified for this land leaf which is calculated
*          previously using the logit function. Note: this method
*          is called in every time period including calibration
*          periods. Thus, land in a calibration period is not 
*          necessarily equal to read in values
* \param aRegionName Region.
* \param aLandAllocationAbove Land allocated to the parent node
* \param aPeriod Model period
* \note mLandToBeHarvested stores the land needed for a single year (not period)
*/
void ForestLandLeaf::calcLandAllocation( const string& aRegionName,
                                         const double aLandAllocationAbove,
                                         const int aPeriod )
{
    // Check that the number of time steps is initialized.
    assert( mSteps != -1 );

    // Call standard land allocation.
    // This determines the total land allocated to this leaf, which is all the land
    // set aside for harvest over a a time of one rotation period into the future
    LandLeaf::calcLandAllocation( aRegionName, aLandAllocationAbove, aPeriod );

    // Calculate the land allocated for the harvest period defined by this
    // period and the constant rotation period.
    
    // First calculate land already allocated to be harvested in 
    // the intermediate periods between now and the future
    // market period mSteps into the future.
    // This land cannot be used for harvest in the future market period
    // because it is already designated for intermediate period harvest
    mSteps = numPeriodsForRotation( aPeriod );
    // Cumulative forest land set aside.
    double forestLandAside = 0;
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = aPeriod + 1; i < aPeriod + mSteps; ++i ){
        // Use the time step from the last model period if the period is greater
        // than the maximum model period.
        int modelPeriod = min( modeltime->getmaxper() - 1, static_cast<int>( i ) );
        forestLandAside += mLandToBeHarvested[ i ] * modeltime->gettimestep( modelPeriod );
    }
    
    // Calculate the land that can be harvested in the harvest period by subtracting the cumulative
    // land allocated in this period by the cumulative land set aside for inbetween harvest periods.
    // We can then annualize that land amount by dividing by the timestep in the harvest period.
    int modelPeriod = min( modeltime->getmaxper() - 1, static_cast<int>( aPeriod + mSteps ) );
    double annualizedLand = ( mLandAllocation[ aPeriod ] - forestLandAside ) / modeltime->gettimestep( modelPeriod );

    // Store the land to be harvested in the future period.
    // This is land harvested to produce one year of production.
    mLandToBeHarvested[ aPeriod + mSteps ] = max( annualizedLand, 0.0 );

    // TODO -- could improve forest allocation calculations by linearly interpolating
    // instead of using averages by model period, although an estimate of the difference  
    // indicates this would not have a large impact on the results.  It does have an
    // effect on the land necessary for calibration particularly with changing time-steps
    
}

double ForestLandLeaf::getLandAllocation( const string& aLandType,
                                          const string& aProductName,
                                          const int aPeriod ) const 
{
    assert( aProductName == mName );

    return mLandToBeHarvested[ aPeriod ];
}

double ForestLandLeaf::getTotalLandAllocation( const LandAllocationType aType,
                                               const int aPeriod ) const
{
    // All forestry land is production.
    if( aType == eAnyLand || aType == eManaged ){
        return mLandAllocation[ aPeriod ];
    }
    return 0;
}

/*!
* \brief Sets the calibrated land allocation
* \details This method is called during completeInit and set to the 
*          read in value ( if it exists ). Land allocation can vary
*          from the calibrated value in a calibration period however.
*          This method differs from LandLeaf because the land allocation
*          computed is the land needed for all years in the rotation period
*          which typically spans 45 years.
* \param aLandType Type of land.
* \param aProductName Name of the land leaf
* \param aCalLandUsed Calibrated land used
* \param aHarvestPeriod Harvest period
* \param aCurrentPeriod Current period.
*/
void ForestLandLeaf::setCalLandAllocation( const string& aLandType,
                                           const string& aProductName,
                                           const double aCalLandUsed,
                                           const int aHarvestPeriod, 
                                           const int aCurrentPeriod )
{
    assert( aProductName == mName );
    const Modeltime* modeltime = scenario->getModeltime();
    
    // Since setCalLandAllocation is called before completeInit we must make sure
    // that there is enough space in the mLandToBeHarvested vector and expand if not.
    // Note that rotation period is also not available yet.
    if( aHarvestPeriod >= mLandToBeHarvested.size() ) {
        mLandToBeHarvested.resize( aHarvestPeriod + 1 );
    }

    mLandToBeHarvested[ aHarvestPeriod ] = aCalLandUsed;

    // Since calc assumes harvested land values are zero for all future periods, zero
    // these out.
    for( unsigned int aPeriod = aHarvestPeriod + 1; aPeriod < mLandToBeHarvested.size(); ++aPeriod ){
        mLandToBeHarvested[ aPeriod ] = 0;
    }
    
    // Set the land allocation to be all land that is allocated to future
    // periods(current land is now free for reallocation).
    double totalAllocatedLand = 0;

    // TODO: It would be better to use mSteps to determining how for into the future
    // to sum but that variable is not set until complete init, which is before
    // this method.
    
    // This calculation is duplicated each time this is call for a given modelPeriod,
    // so this depends on the last call being the call with all land harvest data filled
    // out. (i.e., must be called in order)
    for( unsigned int aPeriod = aCurrentPeriod + 1; aPeriod < mLandToBeHarvested.size(); ++aPeriod ){
        // Use the time step from the last model period if the period is greater
        // than the maximum model period.
        int modelPeriod = min( modeltime->getmaxper() - 1, static_cast<int>( aPeriod ) );
        totalAllocatedLand += mLandToBeHarvested[ aPeriod ]
                              * modeltime->gettimestep( modelPeriod );
    }
    LandLeaf::setCalLandAllocation( aLandType, aProductName, totalAllocatedLand,
                                    aHarvestPeriod, aCurrentPeriod );
}

void ForestLandLeaf::setCalObservedYield( const string& aLandType,
                                          const string& aProductName,
                                          const double aCalObservedYield,
                                          const int aPeriod )
{
    assert( aProductName == mName );

    // This sets the current yield and the observed yield. The land leaf
    // only sets the observed yield.
    // TODO: Change the way this is done and remove this function.
    mCalObservedYield[ aPeriod ] = mYield[ aPeriod ] = aCalObservedYield;

}

/*!
* \brief Calculates a the intrinsic rate of a land leaf
* \details This method converts the intrinsic rate passed in from 
*          the food production technology from $/GCal to $/kHa
*          Additionally, we add the carbon value of the land to
*          the intrinsic rate if the ag subsidy is active and a 
*          carbon price exists
* \param aRegionName Region.
* \param aLandType Type of land
* \param aProductName Name of land leaf
* \param aIntrinsicRate Profit rate of the food production technology
*          Equal to price minus variable cost
* \param aPeriod Period.
*/
void ForestLandLeaf::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate,
                                 const int aPeriod )
{
    assert( aProductName == mName );
    assert( mIntrinsicYieldMode[ aPeriod ].isInited() &&
            mIntrinsicYieldMode[ aPeriod ] >= 0 );

    // forest intrinsic rate needs to have additional prod change applied as these
    // are forests that will be harvested in the future
    // KVC: are we doing this? This method seems to be identical to LandLeaf::setIntrinsicRate
     
    // aIntrinsicRate [$/GCal] * intrinsicYieldMode [GCal/kHa] = [$/kHa]
    // The intrinsicRate that is passed in is $/Gcal. 
    // It is multiplied by intrinsicYieldMode (GCal/kHa) to convert it to $/kHa.   
    // Add carbon value to intrinsic rate.
    // We are assuming that producers expect the carbon price to grow at the interest rate
    // We then discount the future carbon price back to this period when determining
    // the intrinsic rate of future forests. The result is that we add the whole carbon value
    // to the future forest's intrinsic rate.
    mIntrinsicRate[ aPeriod ] = max( aIntrinsicRate * mIntrinsicYieldMode[ aPeriod ] * mIntrinsicYieldModeAgProdMultiplier[ aPeriod ] 
                                    + getCarbonValue( aRegionName, aPeriod ), 0.0 );
}

void ForestLandLeaf::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    LandLeaf::toDebugXMLDerived( aPeriod, aOut, aTabs );
    // Write out land to be harvested into the future as well as it may change.
    const Modeltime* modeltime = scenario->getModeltime();
    int theYear = modeltime->getper_to_yr( aPeriod );
    for( int i = aPeriod; i < mLandToBeHarvested.size(); ++i ){
        if ( i < modeltime->getmaxper() ) {
            theYear = modeltime->getper_to_yr( i );
        }
        else {
            theYear += modeltime->gettimestep( modeltime->getmaxper() - 1 );
        }
        XMLWriteElement( mLandToBeHarvested[ i ], "landToBeHarvested", aOut, aTabs, theYear );
    }
}

/*!
* \brief Calculates the yield of a managed land leaf
* \details This method assumes the yield of a forest land leaf
*          is determined by the yield in the previous period 
*          and the exogenously specified technical change. 
*          The current calculation does not adjust the yield
*          based on the amount of land allocated to forests.
* \param aLandType Type of land.
* \param aProductName Product produced on this parcel of land.
* \param aRegionName Region.
* \param aProfitRate Profit rate of the food production technology ($/GCal)
* \param aAvgIntrinsicRate Profit rate of all land ($/kHa)
* \param aHarvestPeriod Harvest period
* \param aCurrentPeriod Current period
*/
void ForestLandLeaf::calcYieldInternal( const string& aLandType,
                                  const string& aProductName,
                                  const string& aRegionName,
                                  const double aProfitRate,
                                  const double aAvgIntrinsicRate,
                                  const int aHarvestPeriod,
                                  const int aCurrentPeriod )
{
    // TODO: yield in forest should be calculated internally to account for
    //       expansion into less productive land. 
    assert( aHarvestPeriod >= aCurrentPeriod );
    assert( mIntrinsicYieldMode[ aCurrentPeriod ].isInited() );

//  Code to call endogenous yield calcuation, except for current period.
    if ( aHarvestPeriod > aCurrentPeriod ) {
        LandLeaf::calcYieldInternal( aLandType, aProductName, aRegionName, aProfitRate,
                                    aAvgIntrinsicRate, aHarvestPeriod,  aCurrentPeriod );
    }
    else {
        mYield[ aHarvestPeriod ] = mCalObservedYield[ aHarvestPeriod ];
    }

/*     
    // Override internal calculation and set forest yield equal to exogenous specification
    if ( aHarvestPeriod > aCurrentPeriod ) { // check if forest leaf
        if ( mCalDataExists[ aCurrentPeriod ] ) {
           mYield[ aHarvestPeriod ] = mCalObservedYield[ aHarvestPeriod ];
        }
        else {
             mYield[ aHarvestPeriod ] = mYield[ aHarvestPeriod - 1 ];
            if ( mIntrinsicYieldModeAgProdMultiplier[ aCurrentPeriod ]  > util::getSmallNumber() ) {
                mYield[ aHarvestPeriod ] *= mIntrinsicYieldModeAgProdMultiplier[ aCurrentPeriod ] / 
                                            mIntrinsicYieldModeAgProdMultiplier[ aCurrentPeriod - 1 ];
            }
        }

    } */
}

int ForestLandLeaf::numPeriodsForRotation( const int aPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    if( aPeriod >= modeltime->getmaxper() ) {
        return ceil( static_cast<double>( mRotationPeriod ) / modeltime->gettimestep( modeltime->getmaxper() - 1 ) );
    }
    const int rotationYear = modeltime->getper_to_yr( aPeriod ) + mRotationPeriod;
    int numPeriods = aPeriod;
    for( ; numPeriods < modeltime->getmaxper() && modeltime->getper_to_yr( numPeriods ) < rotationYear; ++numPeriods ) {
    }
    
    if( numPeriods == modeltime->getmaxper() ) {
        const int finalPeriod = modeltime->getmaxper() - 1;
        numPeriods += ceil( static_cast<double>( rotationYear - modeltime->getper_to_yr( finalPeriod ) )
                           / modeltime->gettimestep( finalPeriod ) ) - 1;
    }
    return numPeriods - aPeriod;
}

void ForestLandLeaf::setMaxYield( const string& aLandType,
                                  const string& aProductName,
                                  const double aMaxYield,
                                  const int aPeriod )
{
    // Since setMaxYield is called before completeInit we must make sure that there
    // is enough space in the mMaxYield vector and expand if not.
    // Note that rotation period is also not available yet.
    if( aPeriod >= mMaxYield.size() ) {
        // resize and don't forget the fill new periods with -1
        mMaxYield.resize( aPeriod + 1, -1 );
    }
    LandLeaf::setMaxYield( aLandType, aProductName, aMaxYield, aPeriod );
}
