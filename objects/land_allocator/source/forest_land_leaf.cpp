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
// TODO: This needs improvement. This vector is resized again later, but values
// are set before the resize.
mLandToBeHarvested( scenario->getModeltime()->getmaxper() )
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
}

void ForestLandLeaf::calcLandAllocation( const string& aRegionName,
                                         const double aLandAllocationAbove,
                                         const int aPeriod )
{
    // Check that the number of time steps is initializes.
    assert( mSteps != -1 );

    // Call standard land allocation.
    LandLeaf::calcLandAllocation( aRegionName, aLandAllocationAbove, aPeriod );

    // Calculate the land allocation for the harvest period defined by this
    // period and the constant rotation period. First calculate land already
    // allocated to be harvested in the future.
    double forestLandAside = 0;
    for( int i = aPeriod + 1; i < aPeriod + mSteps; ++i ){
        forestLandAside += mLandToBeHarvested[ i ];
    }
    
    // Calculate the amount of land to be harvested per year.
    const Modeltime* modeltime = scenario->getModeltime();
    double annualizedLand = mLandAllocation[ aPeriod ] / modeltime->gettimestep( aPeriod );

    // Store the land to be harvested in the future.
    mLandToBeHarvested[ aPeriod + mSteps ] = max( annualizedLand - forestLandAside, 0.0 );
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
 
    // Set the land allocation to be all land that is allocated to future
    // periods(current land is now free for reallocation).
    double totalAllocatedLand = 0;

    // TODO: It would be better to use mSteps to determining how for into the future
    // to sum but that variable is not set until complete init, which is before
    // this method.
    for( unsigned int i = aCurrentPeriod + 1; i < mLandToBeHarvested.size(); ++i ){
        // Use the time step from the last model period if the period is greater
        // than the maximum model period.
        int modelPeriod = min( modeltime->getmaxper() - 1, static_cast<int>( i ) );
        totalAllocatedLand += mLandToBeHarvested[ i ]
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

void ForestLandLeaf::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    LandLeaf::toDebugXMLDerived( aPeriod, aOut, aTabs );
    // Write out land to be harvested into the future as well as it may change.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = aPeriod; i < modeltime->getmaxper(); ++i ){
        XMLWriteElement( mLandToBeHarvested[ i ], "landToBeHarvested", aOut, aTabs, modeltime->getper_to_yr( i ) );
    }
}
