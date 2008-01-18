/*! 
 * \file unmanaged_land_leaf.cpp
 * \ingroup Objects
 * \brief UnmanagedLandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include "land_allocator/include/unmanaged_land_leaf.h"
#include "land_allocator/include/land_use_history.h"
#include "util/base/include/xml_helper.h"
#include "emissions/include/unmanaged_carbon_calc.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/ghg_factory.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Default constructor.
 * \param aParent Pointer to this leafs's parent.
 * \author James Blackwood
 */
UnmanagedLandLeaf::UnmanagedLandLeaf( const ALandAllocatorItem* aParent ):
// Default the name to the empty string. It will be read in during XML parsing.
LandLeaf( aParent, "" )
{
}

//! Destructor
UnmanagedLandLeaf::~UnmanagedLandLeaf() {
}

void UnmanagedLandLeaf::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    // Call parent
    LandLeaf::completeInit( aRegionName, aRegionInfo );
    
    // If land value was not set for some period, set for all periods.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < modeltime->getmaxper(); period++ ) {
        if ( !mLandAllocation[ period ].isInited() ) {
            if ( period > 0 && mLandAllocation[ period - 1 ].isInited() ) {
                mLandAllocation[ period ] = mLandAllocation[ period - 1 ];
            }
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::NOTICE );
                mainLog << "Land for unmanged Land-Leaf " << getName() 
                        << " was not allocated in period " << period << endl;
            }
        }
        
       if ( !mBaseLandAllocation[ period ].isInited() ) {
          mBaseLandAllocation[ period ] = mLandAllocation[ period ] ;
      }
    }
    
}

bool UnmanagedLandLeaf::XMLDerivedClassParse( const string& aNodeName,
                                              const DOMNode* aCurr )
{
    if( aNodeName == "landAllocation" ){
        XMLHelper<Value>::insertValueIntoVector( aCurr, mLandAllocation,
                                                 scenario->getModeltime() );
    }
    else if( aNodeName == "intrinsicRate" ){
        XMLHelper<Value>::insertValueIntoVector( aCurr, mBaseIntrinsicRate,
                                                 scenario->getModeltime() );
    }
    else if( aNodeName == LandUseHistory::getXMLNameStatic() ){
        parseSingleNode( aCurr, mLandUseHistory, new LandUseHistory );
    }
    else if( aNodeName == UnmanagedCarbonCalc::getXMLNameStatic() ) {
        parseSingleNode( aCurr, mCarbonContentCalc, new UnmanagedCarbonCalc );
    }
    else {
        return false;
    }
    return true;
}

void UnmanagedLandLeaf::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( mBaseIntrinsicRate, "intrinsicRate", aOut, aTabs, modeltime, Value( 0.0 ) );
    XMLWriteVector( mLandAllocation, "landAllocation", aOut, aTabs, modeltime );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( aOut, aTabs );
    }

    mCarbonContentCalc->toInputXML( aOut, aTabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void UnmanagedLandLeaf::toDebugXMLDerived( const int aPeriod,
                                           ostream& aOut,
                                           Tabs* aTabs ) const
{
    LandLeaf::toDebugXMLDerived( aPeriod, aOut, aTabs );
    XMLWriteElement( mBaseIntrinsicRate[ aPeriod ], "baseIntrinsicRate", aOut, aTabs );
    XMLWriteElement( mBaseLandAllocation[ aPeriod ], "baseLandAllocation", aOut, aTabs );

    // Don't write out land allocation because ALandAllocatorItem writes it.
    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( aPeriod, aOut, aTabs );
    }
}

const string& UnmanagedLandLeaf::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& UnmanagedLandLeaf::getXMLNameStatic() {
    const static string XML_NAME = "UnmanagedLandLeaf";
    return XML_NAME;
}

void UnmanagedLandLeaf::initCarbonCycle(){
    if( !mCarbonContentCalc.get() ){
        mCarbonContentCalc.reset( new UnmanagedCarbonCalc );
    }
}

void UnmanagedLandLeaf::initLandUseHistory( const double aParentHistoryShare,
                                            const LandUseHistory* aParentHistory,
                                            const int aFirstCalibratedPeriod )
{
    assert( aParentHistoryShare >= 0 && aParentHistoryShare <= 1 );

    // Check that the share has been normalized.
    assert( mShare[ aFirstCalibratedPeriod ].isInited() &&
            mShare[ aFirstCalibratedPeriod ] <= 1 );

    // Use the unmanaged land leaf's history if one exists,
    // otherwise use the parent.
    double historyShare = 1;
    const LandUseHistory* history = mLandUseHistory.get();
    if( !history ){
        history = aParentHistory;
        historyShare = aParentHistoryShare * mShare[ aFirstCalibratedPeriod ];
    }

    mCarbonContentCalc->initLandUseHistory( history, historyShare );
}

void UnmanagedLandLeaf::setUnmanagedLandAllocation( const string& aRegionName,
                                                    const double aNewUnmanaged,
                                                    const int aPeriod )
{
    // Unmanaged land leaves should always have a positive land allocation.
    assert( aNewUnmanaged >= 0 );

    mLandAllocation[ aPeriod ] = mBaseLandAllocation[ aPeriod ] = aNewUnmanaged;
}

void UnmanagedLandLeaf::setUnmanagedLandValues( const string& aRegionName,
                                                const int aPeriod )
{
    assert( mBaseIntrinsicRate[ aPeriod ].isInited() );
    mIntrinsicRate[ aPeriod ] = mBaseIntrinsicRate[ aPeriod ]
                                + getCarbonValue( aRegionName, aPeriod );   
}

double UnmanagedLandLeaf::calcLandShares( const string& aRegionName,
                                          const double aSigmaAbove,
                                          const double aTotalBaseLand,
                                          const int aPeriod )
{
    double unnormalizedShare = LandLeaf::calcLandShares( aRegionName,
                                                         aSigmaAbove,
                                                         0,
                                                         aPeriod );

    // This is an unmanaged land leaf, so adjust the share as calculated in
    // LandLeaf::calcLandShares proportional to the base land allocated to this
    // leaf (relative to the total land in this unmanaged land node)
    if ( aTotalBaseLand > util::getSmallNumber() ) {
        assert( mBaseLandAllocation[ aPeriod ].isInited() );
        unnormalizedShare *= mBaseLandAllocation[ aPeriod ] / aTotalBaseLand;
    }
    return unnormalizedShare;
}

void UnmanagedLandLeaf::resetToCalLandAllocation( const int aPeriod ) {
   // Do nothing, since unmanagd land does not have a calibrated land allocation
}

bool UnmanagedLandLeaf::isUnmanagedNest() const {
    // This is a hack to help find the top node of the unmanaged land nest.
    return 1;
}

double UnmanagedLandLeaf::getBaseLandAllocation( const int aPeriod ) const {
    assert( mBaseLandAllocation[ aPeriod ].isInited() );
    return mBaseLandAllocation[ aPeriod ];
}

double UnmanagedLandLeaf::getTotalLandAllocation( const LandAllocationType aType,
                                                  const int aPeriod ) const
{
    // Check if unmanaged land should be returned.
    if( aType == eAnyLand || aType == eUnmanaged ){
        assert( mLandAllocation[ aPeriod ].isInited() );
        return mLandAllocation[ aPeriod ];
    }
    return 0;
}
void UnmanagedLandLeaf::checkCalObservedYield( const int aPeriod ) const {
    // Unmanaged land leaves do not have to have a calibrated observed yield, so
    // no checking is done.
}
