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
#include "ccarbon_model/include/carbon_box_model.h"
#include "emissions/include/aghg.h"
#include "util/base/include/summary.h"
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
        // Correct and warn if negative values
        if( mLandAllocation[ period ].isInited() && mLandAllocation[ period ] < 0 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Negative land allocation of " << mLandAllocation[ period ] << 
                       " read in for leaf " << getName() << ". Resetting to zero." 
                    << endl;
            mLandAllocation[ period ] = 0;
        }

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


bool UnmanagedLandLeaf::XMLDerivedClassParse( const std::string& aNodeName,
                                              const xercesc::DOMNode* aCurr )
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
    else if( aNodeName == CarbonBoxModel::getXMLNameStatic() ){
        parseSingleNode( aCurr, mCarbonContentCalc, new CarbonBoxModel );
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
   double newLandValue = aNewUnmanaged;
   if( aNewUnmanaged < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Negative land allocation being set for leaf " << getName() << ". Resetting to zero." << endl;
        newLandValue = 0;
    }
    mLandAllocation[ aPeriod ] = mBaseLandAllocation[ aPeriod ] = newLandValue;
}

/*!
* \brief Sets a the intrinsic rate of a land leaf
* \details This method adjusts the intrinsic rate of an unmanaged land leaf
*          to account for the carbon value of land if the ag subsidy is
*          is active and a carbon price exists. 
* \param aRegionName Region.
* \param aPeriod Period.
*/
void UnmanagedLandLeaf::setUnmanagedLandValues( const string& aRegionName,
                                                const int aPeriod )
{
    // The base intrinsic rate that is read in should be equal to 
    // the average profit rate of all land in the region. To convert from
    // this rate for all land to the intrinsic rate for the unmanaged land
    // leaf we must multiply by the intrinsic yield multiplier.
    // TODO: check that the baseIntrinsicRate read-in is equal to the avgProfitRate of
    // the tree land allocator OR pass that rate down to the leaf to initialize mBaseIntrinsicRate
    assert( mBaseIntrinsicRate[ aPeriod ].isInited() );
    mIntrinsicRate[ aPeriod ] = mBaseIntrinsicRate[ aPeriod ]*mIntrinsicYieldMult[ aPeriod ]
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

    // result should be > 0.
    assert( unnormalizedShare >= 0 );

    return unnormalizedShare;
}

void UnmanagedLandLeaf::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    LandLeaf::calcLandAllocation( aRegionName, aLandAllocationAbove, aPeriod);

    //sjsTEMP - these are hacks, need to add initCalc() method
    // Set base land allocation for next period
    const Modeltime* modeltime = scenario->getModeltime();
    if (aPeriod > 2 && modeltime->getper_to_yr( aPeriod ) > mLandUseHistory->getMaxYear() ) {
        mBaseLandAllocation[ aPeriod ] = mLandAllocation[ aPeriod - 1 ];
    }
    if ( mLandUseHistory.get() ) {
        if ( modeltime->getper_to_yr( aPeriod ) <= mLandUseHistory->getMaxYear() ) {
            mBaseLandAllocation[ aPeriod ] = mLandUseHistory->getAllocation( modeltime->getper_to_yr( aPeriod ) );
        }
    }
}

void UnmanagedLandLeaf::resetToCalLandAllocation( const int aPeriod ) {
   // Set to land-use history value if this exists
   if( mLandUseHistory.get() ){
      const Modeltime* modeltime = scenario->getModeltime();
      if ( modeltime->getper_to_yr( aPeriod ) <= mLandUseHistory->getMaxYear() ) {
         mLandAllocation[ aPeriod ] = mBaseLandAllocation[ aPeriod ] = mLandUseHistory->getAllocation( modeltime->getper_to_yr( aPeriod ) );
      }
   }
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
