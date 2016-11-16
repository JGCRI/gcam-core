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
 * \file land_leaf.cpp
 * \ingroup Objects
 * \brief LandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "land_allocator/include/land_leaf.h"
#include "util/base/include/ivisitor.h"
#include "ccarbon_model/include/land_carbon_densities.h"
#include "ccarbon_model/include/no_emiss_carbon_calc.h"
#include "containers/include/iinfo.h"
#include "land_allocator/include/land_use_history.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/configuration.h"
#include "containers/include/market_dependency_finder.h"
#include "functions/include/idiscrete_choice.hpp"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*!
 * \brief Constructor.
* \author James Blackwood
 * \param aParent Pointer to this leafs's parent.
 * \param aName Product name.
*/
LandLeaf::LandLeaf( const ALandAllocatorItem* aParent, const std::string &aName ):
    ALandAllocatorItem( aParent, eLeaf ),
    mCarbonContentCalc( 0 ),
    mMinAboveGroundCDensity( 0.0 ),
    mMinBelowGroundCDensity( 0.0 ),
    mCarbonPriceIncreaseRate( 0.0 ),
    mLandUseHistory( 0 ),
    mReadinLandAllocation( 0.0 ),
    mLastCalcCO2Value( 0.0 ),
    mLastCalcExpansionValue( 0.0 )
{
    // Can't use initializer because mName is a member of ALandAllocatorItem,
    // not LandLeaf.
    mName = aName;
}

//! Destructor
LandLeaf::~LandLeaf() {
}

const string& LandLeaf::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Kate Calvin
* \return The constant XML_NAME as a static.
*/
const string& LandLeaf::getXMLNameStatic() {
    const static string XML_NAME = "LandLeaf";
    return XML_NAME;
}

size_t LandLeaf::getNumChildren() const {
    return 0;
}

const ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) const {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

bool LandLeaf::XMLParse( const xercesc::DOMNode* aNode ){
    const Modeltime* modeltime = scenario->getModeltime();

    // assume we are passed a valid node.
    assert( aNode );
    
    // Set the node name.
    mName = XMLHelper<string>::getAttr( aNode, "name" );

    // get all the children.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "landAllocation" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mReadinLandAllocation,
                                                     modeltime );
        }
        else if( nodeName == "minAboveGroundCDensity" ){
            mMinAboveGroundCDensity = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "minBelowGroundCDensity" ){
            mMinBelowGroundCDensity = XMLHelper<double>::getValue( curr );
        }
		else if( nodeName == "ghost-unnormalized-share" ){
            XMLHelper<Value>::insertValueIntoVector( curr, mGhostUnormalizedShare, modeltime );
        }
        else if( nodeName == LandUseHistory::getXMLNameStatic() ){
            parseSingleNode( curr, mLandUseHistory, new LandUseHistory );
        }
        else if( nodeName == LandCarbonDensities::getXMLNameStatic() ) {
            parseSingleNode( curr, mCarbonContentCalc, new LandCarbonDensities );
        }
        else if( nodeName == NoEmissCarbonCalc::getXMLNameStatic() ) {
            parseSingleNode( curr, mCarbonContentCalc, new NoEmissCarbonCalc );
        }
        else if( nodeName == "landConstraintCurve" ) {
            mLandExpansionCostName = XMLHelper<string>::getValue( curr );
            mIsLandExpansionCost = true;
        }

        else if ( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLName() << "." << endl;
        }
    }

    return true;
}

bool LandLeaf::XMLDerivedClassParse( const std::string& aNodeName, const xercesc::DOMNode* aCurr ){
    // Allow derived classes to override.
    return false;
}

void LandLeaf::completeInit( const string& aRegionName,
                             const IInfo* aRegionInfo )
{
    // Store the interest rate from the region.
    mSocialDiscountRate = aRegionInfo->getDouble( "social-discount-rate", true );
    const double privateDiscountRateLand = aRegionInfo->getDouble( "private-discount-rate-land", true );

    // Set the carbon cycle object if it has not already been initialized. Use a
    // virtual function so that derived leaves may use a different default type.
    if( !mCarbonContentCalc.get() ){
        mCarbonContentCalc.reset( new LandCarbonDensities );
    }

    // Initialize the carbon-cycle object
    mCarbonContentCalc->completeInit( privateDiscountRateLand );

    // Ensure that a carbon cycle object has been setup.
    assert( mCarbonContentCalc.get() );

    // Ensure that any land allocation read in was positive
    // Note: zero land allocation is allowed
    const Modeltime* modeltime = scenario->getModeltime();
    for( int period = 0; period < modeltime->getmaxper(); period++ ) {
        if( mLandAllocation[ period ] < 0 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Negative land allocation of " << mLandAllocation[ period ] 
                    << " read in for leaf " << getName() << " in " 
                    << aRegionName << "." << endl;
            abort();
        }
    }
    
    // Initialize the land use history.
    initLandUseHistory( aRegionName );
    
    // Add dependency for to expansion constraint market if it is being used.
    if( mIsLandExpansionCost ) {
        scenario->getMarketplace()->getDependencyFinder()->addDependency( "land-allocator",
                                                                          aRegionName,
                                                                          mLandExpansionCostName,
                                                                          aRegionName );
    }
}

void LandLeaf::initCalc( const string& aRegionName, const int aPeriod )
{
    // TODO: error checking
    if ( aPeriod > 1 ) {
        // If leaf is a "new tech" get the scaler from its parent
        if ( !mShareWeight[ aPeriod ].isInited()) {
            mShareWeight[ aPeriod ] = mShareWeight[ aPeriod - 1 ];
        }
    }
}

/*!
* \brief Initializes the share of land of a leaf
* \details Calculates the share of land allocated to a leaf.
*          This method is called during the calibration process 
*          so the shares set are prior to any calculations
*          of share weights.
* \param aRegionName Region.
* \param aLandAllocationAbove Land allocation of the parent node
* \param aPeriod Model period
*/
void LandLeaf::setInitShares( const string& aRegionName,
                              const double aLandAllocationAbove,
                              const int aPeriod )
{
    if ( aLandAllocationAbove > 0.0 ) {
        mShare[ aPeriod ] = mReadinLandAllocation[ aPeriod ] / aLandAllocationAbove;
    }
    else {
        mShare[ aPeriod ] = 0;
    }
}

/*!
 * \brief Initialize the land use history for the leaf.
 * \param aRegionName Region name used for printing error messages.
 */
void LandLeaf::initLandUseHistory( const string& aRegionName )
{
    if( !mLandUseHistory.get() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No land use history read in for leaf "
                << getName() << " in region " << aRegionName << endl;
        abort();
    }
    mCarbonContentCalc->initLandUseHistory( mLandUseHistory.get() );
}

void LandLeaf::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );
    const Modeltime* modeltime = scenario->getModeltime();
    const Value defaultValue;
    XMLWriteVector( mReadinLandAllocation, "landAllocation", aOut, aTabs, modeltime, defaultValue );
    XMLWriteVector( mGhostUnormalizedShare, "ghost-share-node", aOut, aTabs, modeltime, defaultValue );  
    XMLWriteElement( mMinAboveGroundCDensity, "minAboveGroundCDensity", aOut, aTabs );
    XMLWriteElement( mMinBelowGroundCDensity, "minBelowGroundCDensity", aOut, aTabs );
    XMLWriteElementCheckDefault( mLandExpansionCostName, "landConstraintCurve", aOut, aTabs, string() );
    

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( aOut, aTabs );
    }

    mCarbonContentCalc->toInputXML( aOut, aTabs );

    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

void LandLeaf::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mReadinLandAllocation[ period ], "read-in-land-allocation", out, tabs );
    XMLWriteElement( mMinAboveGroundCDensity, "minAboveGroundCDensity", out, tabs );
    XMLWriteElement( mMinBelowGroundCDensity, "minBelowGroundCDensity", out, tabs );
    XMLWriteElement( mSocialDiscountRate, "social-discount-rate", out, tabs );
    XMLWriteVector( mCarbonPriceIncreaseRate, "carbon-price-increase-rate", out, tabs, scenario->getModeltime() );
    XMLWriteElementCheckDefault( mLandExpansionCostName, "landConstraintCurve", out, tabs, string() );
    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( period, out, tabs );
    }
    
    mCarbonContentCalc->toDebugXML( period, out, tabs );
}

/*!
* \brief Sets a the profit rate of a land leaf
* \details This method stores the profit rate passed in from 
*          the ag production technology in $/kHa
*          Additionally, we add the carbon value of the land to
*          the profit rate if the ag subsidy is active and a 
*          carbon price exists
* \param aRegionName Region.
* \param aProductName Name of land leaf
* \param aProfitRate Profit rate of the ag production technology
* \param aPeriod Period.
*/
void LandLeaf::setProfitRate( const string& aRegionName,
                                 const string& aProductName,
                                 const double aProfitRate,
                                 const int aPeriod )
{
    // adjust profit rate for land expnasion costs if applicable
    double adjustedProfitRate = aProfitRate;
    const Marketplace* marketplace = scenario->getMarketplace();

    if ( mIsLandExpansionCost ) {
        //subtract off expansion cost from profit rate
        double expansionCost = marketplace->getPrice( mLandExpansionCostName, aRegionName, aPeriod );
        adjustedProfitRate = aProfitRate - expansionCost;
    }

    mProfitRate[ aPeriod ] = adjustedProfitRate + getCarbonSubsidy( aRegionName, aPeriod );
}


/*!
* \brief Calculates the carbon subsidy per hectare for this land leaf.
* \details Uses the carbon content of the land and the carbon
*          price to compute a subsidy on land. Land parcels with higher
*          carbon contents receive higher subsidies.
* \author Kate Calvin
* \param aRegionName Region name.
* \param aPeriod Model period.
*/
double LandLeaf::getCarbonSubsidy( const string& aRegionName, const int aPeriod ) const {
    const double dollar_conversion_75_90 = 2.212;
    // Check if a carbon market exists and has a non-zero price.
    const Marketplace* marketplace = scenario->getMarketplace();
    double carbonPrice = marketplace->getPrice( "CO2_LUC", aRegionName, aPeriod, false );

    // If a carbon price exists, calculate the subsidy
    if( carbonPrice != Marketplace::NO_MARKET_PRICE && carbonPrice > 0.0 ){
        // Carbon price is in 1990$, but land value is in 1975$ so we need to convert
        carbonPrice /= dollar_conversion_75_90;

        // Carbon content is in kgC/m2. Land profit rate is in $/billion m2 (or $/ thous km2).
        // We need to multiply by 1e9 to go from $/m2 to $/billion m2
        // We need to divide by 1e3 to go from kgC to tC
        const double conversionFactor = 1000000.0;

        // We are only subsidizing for carbon contents above the read in minimum
        const int year = scenario->getModeltime()->getper_to_yr( aPeriod );
        double incrementalAboveCDensity = mCarbonContentCalc->getActualAboveGroundCarbonDensity( year )
                                            - mMinAboveGroundCDensity;
        double incrementalBelowCDensity = mCarbonContentCalc->getActualBelowGroundCarbonDensity( year )
                                            - mMinBelowGroundCDensity;

        // Calculate the carbon value as the total carbon content of the land
        // multiplied by the carbon price and the interest rate.
        double carbonSubsidy = ( incrementalAboveCDensity * mCarbonContentCalc->getAboveGroundCarbonSubsidyDiscountFactor()
            + incrementalBelowCDensity * mCarbonContentCalc->getBelowGroundCarbonSubsidyDiscountFactor() )
            * carbonPrice * ( mSocialDiscountRate - mCarbonPriceIncreaseRate[ aPeriod ] )* conversionFactor;

        assert( carbonSubsidy >= 0.0 );

        return carbonSubsidy;
    }

    // If no market price
    return 0.0;
}

void LandLeaf::setUnmanagedLandProfitRate( const string& aRegionName,  
                                           double aAverageProfitRate, const int aPeriod ) {
    // Does nothing for production (managed) leaves.
	// Only takes action on unmanaged leaves, which is a type derived from LandLeaf
}

void LandLeaf::resetCalLandAllocation( const string& aRegionName,
                                          double aNewLandAllocation, const int aPeriod ) {
    mReadinLandAllocation[ aPeriod ] = aNewLandAllocation;
}



void LandLeaf::calculateNodeProfitRates( const string& aRegionName, 
                                         double aAverageProfitRateAbove, 
                                         IDiscreteChoice* aChoiceFnAbove,
                                         const int aPeriod ) {
    // This value is not used by the leaf, although we could calculate it for
    // comparison..
}


void LandLeaf::setCarbonPriceIncreaseRate( const double aCarbonPriceIncreaseRate,
                                    const int aPeriod )
{
    mCarbonPriceIncreaseRate[ aPeriod ] = aCarbonPriceIncreaseRate;
}

/*!
* \brief Set the number of years needed to for soil carbons emissions/uptake
* \details This method sets the soil time scale into the carbon calculator
*          for each land leaf.
* \param aTimeScale soil time scale (in years)
* \author Kate Calvin
*/
void LandLeaf::setSoilTimeScale( const int aTimeScale ) {
    mCarbonContentCalc->setSoilTimeScale( aTimeScale );
}

/*!
* \brief Calculates the share of land allocated to a particular type
* \details This method implements the logit function. A land type's
*          share of land is based on its profit rate and the
*          distribution assumed for the parent node ( aLogitExpAbove )
* \param aRegionName Region.
* \param aLogitExpAbove Distribution parameter for the parent node
* \param aChoiceFnAbove The discrete choice function from the level above.
* \param aPeriod Model period
*/
double LandLeaf::calcLandShares( const string& aRegionName,
                                 IDiscreteChoice* aChoiceFnAbove,
                                 const int aPeriod )
{
    // Calculate the unnormalized share for this leaf
    // The unnormalized share is used by the parent node to 
    // calculate the leaf's share of the parent's land
    double unnormalizedShare = aChoiceFnAbove->calcUnnormalizedShare( mShareWeight[ aPeriod ], mProfitRate[ aPeriod ], aPeriod );
    
    // result should be > 0 if we have a non-zero share-weight (it is -infinity when zero)
    assert( mShareWeight[ aPeriod ] == 0.0 || unnormalizedShare >= 0.0 );

    return unnormalizedShare; 
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
* \param aRegionName Region name.
* \param aLandAllocationAbove Land allocated to the parent node
* \param aPeriod Model period
*/
void LandLeaf::calcLandAllocation( const string& aRegionName,
                                   const double aLandAllocationAbove,
                                   const int aPeriod )
{
    assert( mShare[ aPeriod ] >= 0 &&
            mShare[ aPeriod ] <= 1 );

    if ( aLandAllocationAbove > 0.0 ) {
        mLandAllocation[ aPeriod ] = aLandAllocationAbove * mShare[ aPeriod ];
    }
    else {
        mLandAllocation[ aPeriod ] = 0.0;
    }

    // Set the land use in the carbon content calculator.
    mCarbonContentCalc->setTotalLandUse( mLandAllocation[ aPeriod ], aPeriod );

    // compute any demands for land use constraint resources
    if ( mIsLandExpansionCost ) {
        Marketplace* marketplace = scenario->getMarketplace();
        mLastCalcExpansionValue = marketplace->addToDemand( mLandExpansionCostName, aRegionName, 
            mLandAllocation[ aPeriod ], mLastCalcExpansionValue, aPeriod, true );      
    }

}

/*!
* \brief Calls the simple carbon calculator to calculate land use emissions
* \param aRegionName Region.
* \param aPeriod Current model period.
* \param aEndYear The year to calculate LUC emissions to.
*/
void LandLeaf::calcLUCEmissions( const string& aRegionName,
                                 const int aPeriod, const int aEndYear )
{
    // Calculate the amount of emissions attributed to land use change in the current period
    mCarbonContentCalc->calc( aPeriod, aEndYear );

    // Add emissions to the carbon market. 
    const Modeltime* modeltime = scenario->getModeltime();
    if ( ( aEndYear != CarbonModelUtils::getEndYear() || aPeriod == modeltime->getmaxper() - 1 ) ) {
        double LUCEmissions = mCarbonContentCalc
            ->getNetLandUseChangeEmission( modeltime->getper_to_yr( aPeriod ) );
        Marketplace* marketplace = scenario->getMarketplace();
        mLastCalcCO2Value = marketplace->addToDemand( "CO2_LUC", aRegionName, LUCEmissions,
                                                      mLastCalcCO2Value, aPeriod, false );
    }  
}

/*!
* \brief Returns the land allocation of this leaf
* \param aProductName Product name.
* \param aPeriod Model Period
*/
double LandLeaf::getLandAllocation( const string& aProductName,
                                    const int aPeriod ) const
{
    assert( aProductName == mName || aProductName == "" ); // Residue output object calls this without product information

    return mLandAllocation[ aPeriod ];
}

/*!
* \brief Returns the land allocation of this leaf if it is the specified type
* \param aType Land type.
* \param aPeriod Model Period
*/
double LandLeaf::getCalLandAllocation( const LandAllocationType aType,
                                       const int aPeriod ) const
{
    // If the requested type is managed or "any", return land allocation.
    if( aType == eAnyLand || aType == eManaged ){
        return mReadinLandAllocation[ aPeriod ];
    }

    return 0;
}

void LandLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitLandLeaf( this, aPeriod );

    acceptDerived( aVisitor, aPeriod );

    if( mCarbonContentCalc.get() ){
        mCarbonContentCalc->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitLandLeaf( this, aPeriod );
}

void LandLeaf::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // does nothing for the base class
}

ICarbonCalc* LandLeaf::getCarbonContentCalc() const{
    return ( mCarbonContentCalc.get() );
}

bool LandLeaf::isUnmanagedLandLeaf( )  const 
{
    return false;
}

