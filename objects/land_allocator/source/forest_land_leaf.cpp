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
mSteps( -1 ),
// size to some reasonable maximum number for future forest rotation period
// Will be re-sized later to appropriate number. Used 15 since could, at some point,
// use this with smaller (say 5 year) time steps. This results in a slight oversize
// of the forest leaf, but there are not many of these.
mLandToBeHarvested( scenario->getModeltime()->getmaxper() + 15 )
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
    mSteps = rotationPeriod / modeltime->gettimestep( 0 );
    
    // reset yield and land variables to be larger
    int maxper = modeltime->getmaxper();
    mYield.resize( maxper + mSteps );
    mLandToBeHarvested.resize( maxper + mSteps );

    LandLeaf::completeInit( aRegionName, aRegionInfo );

    // Resize to account for extra forest periods
    mAgProdChange.resize( maxper + mSteps );
}

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
    double forestLandAside = 0;
    for( int i = aPeriod + 1; i < aPeriod + mSteps; ++i ){
        forestLandAside += mLandToBeHarvested[ i ];
    }
    
    // Calculate the amount of land to be harvested per year.
    const Modeltime* modeltime = scenario->getModeltime();
    double annualizedLand = mLandAllocation[ aPeriod ] / modeltime->gettimestep( aPeriod )
                            - forestLandAside;

    // Store the land to be harvested in the future period.
    // This is land harvested to produce one year of production.
    mLandToBeHarvested[ aPeriod + mSteps ] = max( annualizedLand, 0.0 );

    //TODO -- could improve forest allocation calculations by linearly interpolating
    // instead of using averages by model period, although an estimate of the difference  
    // indicates this would not have a large impact on the results.
    
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

void ForestLandLeaf::setCalLandAllocation( const string& aLandType,
                                           const string& aProductName,
                                           const double aCalLandUsed,
                                           const int aHarvestPeriod, 
                                           const int aCurrentPeriod )
{
    assert( aProductName == mName );
    const Modeltime* modeltime = scenario->getModeltime();

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
     
    // aIntrinsicRate [$/GCal] * intrinsicYieldMode [GCal/kHa] = [$/kHa]
    // The intrinsicRate that is passed in is $/Gcal. 
    // It is multiplied by intrinsicYieldMode (GCal/kHa) to convert it to $/kHa.   
    // For biomass [$/GJ] * [GJ/kHa] = [$/kHa]
    // Add carbon value to intrinsic rate.
    // TODO -- check if this is correct. Carbonvalue is applied to profit rate AND to mIntrinsicRate
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
