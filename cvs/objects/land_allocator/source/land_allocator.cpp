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
 * \file land_allocator.cpp
 * \ingroup Objects
 * \brief LandAllocator class source file.
 * \author James Blackwood, Kate Calvin
 */

#include "util/base/include/definitions.h"
#include "util/base/include/xml_helper.h"

#include "land_allocator/include/land_allocator.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "containers/include/info_factory.h"
#include "util/base/include/model_time.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/configuration.h"
#include "functions/include/idiscrete_choice.hpp"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \author James Blackwood
 */
LandAllocator::LandAllocator()
: LandNode( 0 )
{
    mCarbonPriceIncreaseRate.assign( mCarbonPriceIncreaseRate.size(), 0.0 );
    mSoilTimeScale = CarbonModelUtils::getSoilTimeScale();
}

//! Destructor
LandAllocator::~LandAllocator() {
}

const string& LandAllocator::getXMLName() const {
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
const string& LandAllocator::getXMLNameStatic() {
    const static string XML_NAME = "LandAllocatorRoot";    // original XML tag
    return XML_NAME;
}

void LandAllocator::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    // Call the node toDebugXML
    ALandAllocatorItem::toDebugXML( aPeriod, aOut, aTabs );  
}

void LandAllocator::initCalc( const string& aRegionName, const int aPeriod )
{
    // In calibration periods, check land area and set calibration values
    if ( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
        checkLandArea( aRegionName, aPeriod);
        calibrateLandAllocator( aRegionName, aPeriod );
    }

    // Call land node's initCalc
    LandNode::initCalc( aRegionName, aPeriod );
    
    // Ensure that carbon price increase rate is positive
    if ( mCarbonPriceIncreaseRate[ aPeriod ] < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Carbon price increase rate is negative in region "
                << aRegionName << endl;
        exit( -1 );
    }
    setCarbonPriceIncreaseRate( mCarbonPriceIncreaseRate[ aPeriod ] , aPeriod );
}

void LandAllocator::completeInit( const string& aRegionName, 
                                  const IInfo* aRegionInfo )
{
    // Ensure the parent nodes are set through out the tree.  The root has no
    // parent so just set it to null.
    setParent( 0 );
    
    // create a land-info from the region info so we can pass the
    // negative emissions market name
    IInfo* landInfo = InfoFactory::constructInfo( aRegionInfo, mName );
    landInfo->setString( "negative-emiss-market", mNegEmissMarketName );


    // Call generic node method (since LandAllocator is just a specialized node)
    LandNode::completeInit( aRegionName, landInfo );

    // clean up landInfo as we are done with it
    delete landInfo;

    // Ensure that soil time scale is positive
    if ( mSoilTimeScale < 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Soil time scale is negative in region "
                << aRegionName << endl;
        exit( -1 );
    }

    // Set the soil time scale
    setSoilTimeScale( mSoilTimeScale );
}


/*!
 * \brief Ensures consistency in land area
 * \details Checks if the sum of land leafs equals the total land allocation
 * \param aRegionName Region name.
 * \param aPeriod model period.
 */
void LandAllocator::checkLandArea( const string& aRegionName, const int aPeriod ) {

    // Check to make sure that the sum of the area of the leafs is equal
    // to the total land area read in.
    double excessLand = getCalLandAllocation( eManaged, aPeriod )
        + getCalLandAllocation( eUnmanaged, aPeriod )
        - mLandAllocation[ aPeriod ];

    // If the difference between the total and the sum of the leafs
    // is too large, print an error message and exit. 
    double fractionDiff = excessLand / mLandAllocation[ aPeriod ];
    if ( abs( fractionDiff ) > 0.001 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "The sum of land areas in region " << aRegionName 
            << " exceeds the land allocation by " 
            << excessLand << " (" << fractionDiff * 100 << "%) "
            << " in period " << aPeriod
            << endl;
		if ( abs( fractionDiff ) > 0.05 ) {
			exit( -1 );
		}
    }
}


/*!
 * \brief Calibrate the land allocator.
 * \details Sets initial land shares, sets unmanaged land profit rates,
 *          calculate calibration prfot rates, then calculates share profit scalers
 *          that will be used for scaling the profit rates for future year sharing
 * \param aRegionName Region name.
 * \param aPeriod model period.
 * \author Marshall Wise and Katherine V Calvin
 */
void LandAllocator::calibrateLandAllocator( const string& aRegionName, const int aPeriod ) {
    
/*  Step 1. Calculate and set initial land shares based on read in data for a 
    calibration period. */
    
    setInitShares( aRegionName,
                   0, // No land allocation above this node.
                   aPeriod );


/* Step 2. Set the profit rate of unmanaged land leafs equal to the read in land price
   (which is also the marginal profit rate) for that region or land node subregion. This
   is the only way the unmanaged land will have a profit rate at this point. It is implied
   by the read in price of land.  */

    setUnmanagedLandProfitRate( aRegionName, mUnManagedLandValue, aPeriod );

/* For these steps to work, the profit rates of managed land leaves will have been computed before
   this method call (i.e., calibrateLandAllocator) in the initCalc() of the agTechnology Class
   and set in the leafs based on read in calibration prices, yields, and non-land variable 
   costs. These might also be called "observed profits" since they are based on this simple 
   calculation. Also, the node profits do not need to be calculated here as they are not a 
   part of the calibration.  All the info needed is in the leaves. */


/* Step 3. Set the profit rates to use at each node in the land allocation tree.  Note the choice
   of profit rate used here is arbitrary.  Thus we can choose values which we find convenient:
   the profit rate of the dominant child in each nest.  Choosing node profit rates in this way also
   gives us an appropriate value to use if we are making sharing decisions using absolute profit
   rate differences then we can use the node profit rate as it will already be in the appropriate
   range of values. */
	
    calculateNodeProfitRates( aRegionName, aPeriod );

/* Step 4. Calculate share-weights with in each sub nest of the land allocation tree.  The scale
   of these share-weights matter since we will use IDiscreteChoice::calcAverageValue method for
   calculating.  Thus it is important that this method is called after calculateNodeProfitRates
   so that the node profit rate can be used to set that scale.  Exactly how it will affect the
   calculated share-weights depends on the discrete choice function configured in each nest but
   generally it is related to it's share and the ratio of the node profit rate and it's
   profit rate. */

    calculateShareWeights( aRegionName, mChoiceFn, aPeriod, false );
}

double LandAllocator::getLandAllocation( const string& aProductName,
                                         const int aPeriod ) const
{
    const ALandAllocatorItem* node = findChild( aProductName, eLeaf );

    if( node ){
        return node->getLandAllocation( aProductName, aPeriod );
    }
    return 0;
}

void LandAllocator::setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate, const int aPeriod )
{
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setCarbonPriceIncreaseRate( aCarbonPriceIncreaseRate, aPeriod );
    }
}

/*!
* \brief Set the number of years needed to for soil carbons emissions/uptake
* \details This method sets the soil time scale into the carbon calculator
*          for each land leaf.
* \param aTimeScale soil time scale (in years)
* \author Kate Calvin
*/
void LandAllocator::setSoilTimeScale( const int aTimeScale ) {

    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setSoilTimeScale( aTimeScale );
    }

}

void LandAllocator::setProfitRate( const string& aRegionName,
                                   const string& aProductName,
                                   const double aProfitRate,
                                   const int aPeriod )
{
    ALandAllocatorItem* node = findChild( aProductName, eLeaf );
    if( node ){
        node->setProfitRate( aRegionName, aProductName,
                                aProfitRate, aPeriod );
    } 
}

void LandAllocator::setInitShares( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    // Calculating the shares
    for ( unsigned int i = 0; i < mChildren.size(); i++ ) {
        mChildren[ i ]->setInitShares( aRegionName,
                                       mLandAllocation[ aPeriod ],
                                       aPeriod );
    }

    // This is the root node so its share is 100%.
    mShare[ aPeriod ] = 1;
}

double LandAllocator::calcLandShares( const string& aRegionName,
                                      IDiscreteChoice* aChoiceFnAbove,
                                      const int aPeriod ){

    // First set value of unmanaged land leaves
    setUnmanagedLandProfitRate( aRegionName, mUnManagedLandValue, aPeriod );

    LandNode::calcLandShares( aRegionName, aChoiceFnAbove, aPeriod );
 
    // This is the root node so its share is 100%.
    mShare[ aPeriod ] = 1;
    return 1;
}

void LandAllocator::calcLandAllocation( const string& aRegionName,
                                            const double aLandAllocationAbove,
                                            const int aPeriod ){
    for ( unsigned int i = 0; i < mChildren.size(); ++i ){
        mChildren[ i ]->calcLandAllocation( aRegionName, mLandAllocation[ aPeriod ], aPeriod );
    }
}

void LandAllocator::calcLUCEmissions( const string& aRegionName, const int aPeriod,
                                      const int aEndYear, const bool aStoreFullEmiss )
{
    // Calculate emissions for all years in this model period.  Note that in
    // period 0 historical emissions are also calculated.
    for ( unsigned int i = 0; i < mChildren.size(); ++i ){
        mChildren[ i ]->calcLUCEmissions( aRegionName, aPeriod, aEndYear, aStoreFullEmiss );
    } 
}


void LandAllocator::calcFinalLandAllocation( const string& aRegionName,
                                                 const int aPeriod ){

    // In calibration periods, check land area and set calibration values
    if ( aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ) {
       calibrateLandAllocator( aRegionName, aPeriod );
    } 
	
    // Calculate land shares
    calcLandShares( aRegionName,
                    mChoiceFn,
                    aPeriod );

    // Calculate land allocation
    calcLandAllocation( aRegionName,
                        0, // No land allocation above the root.
                        aPeriod );

    // Calculate land-use change emissions but only to the end of this model
    // period for performance reasons.
    calcLUCEmissions( aRegionName,
                      aPeriod,
                      scenario->getModeltime()->getper_to_yr( aPeriod ), false );
}

void LandAllocator::postCalc( const string& aRegionName, const int aPeriod ) {
    // In the final calibration year re-calculate the share-weights this time
    // calculating the future share-weights as well.
    if( scenario->getModeltime()->getFinalCalibrationPeriod() == aPeriod ) {
        setInitShares( aRegionName,
                      0, // No land allocation above this node.
                      aPeriod );
        calculateNodeProfitRates( aRegionName, aPeriod );
        calculateShareWeights( aRegionName, mChoiceFn, aPeriod, true );
    }
    
    // Calculate land-use change emissions for the entire model time horizon.
    calcLUCEmissions( aRegionName, aPeriod, CarbonModelUtils::getEndYear(), true );
}

ALandAllocatorItem* LandAllocator::findProductLeaf( const string& aProductName ) {
    return findChild( aProductName, eLeaf );
}

void LandAllocator::accept( IVisitor* aVisitor, const int aPeriod ) const {
    LandNode::accept( aVisitor, aPeriod );
}
