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
* \file building_node_input.cpp
* \ingroup Objects
* \brief The BuildingNodeInput class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"

#include "functions/include/building_node_input.h"
#include "functions/include/ifunction.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "functions/include/function_manager.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "functions/include/building_service_input.h"
#include "functions/include/thermal_building_service_input.h"
#include "sectors/include/sector_utils.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/satiation_demand_function.h"
#include "functions/include/building_gompertz_function.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
BuildingNodeInput::BuildingNodeInput()
{
    mSatiationDemandFunction = 0;
    mIsFixedBuildingSize = false;
}

//! Destructor
BuildingNodeInput::~BuildingNodeInput() {
    for( CNestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        delete *it;
    }
    delete mSatiationDemandFunction;
}

void BuildingNodeInput::completeInit( const string& aRegionName, const string& aSectorName,
                                      const string& aSubsectorName, const string& aTechName,
                                      const IInfo* aTechInfo)
{
    // create internal gains market for this type of building
    auto_ptr<IInfo> internalGainsInfo( InfoFactory::constructInfo( aTechInfo, mInternalGainsMarketname ) );
    internalGainsInfo->setString( "output-unit", mInternalGainsUnit );
    if( SectorUtils::createTrialSupplyMarket( aRegionName, mInternalGainsMarketname, internalGainsInfo.get() ) ){
        // set initial trial supplies from the parsed vector
        Marketplace* marketplace = scenario->getMarketplace();
        const string trialMarketName = SectorUtils::getTrialMarketName( mInternalGainsMarketname );
        // Note tech name is the name of the consumer which in GCAM is called
        // directly and so should be the name used in dependency tracking.
        marketplace->getDependencyFinder()->addDependency( aTechName, aRegionName, trialMarketName, aRegionName );
    }

    // create the function by getting it from the function manager
    if( !mFunctionType.empty() ) {
        mFunction = FunctionManager::getFunction( mFunctionType );
    }

    // we sort the child inputs now to make things easier on us when it comes time to merge
    // node inputs in copyParamsInto.
    util::NameComparator<IInput> comp;
    sort( mNestedInputs.begin(), mNestedInputs.end(), comp );

    // have all contained inputs do completeInit as well
    for( NestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->completeInit( aRegionName, aSectorName, aSubsectorName, aTechName, aTechInfo );
    }
    
    // Note initializing the fixed building size flag before interpolations implies
    // we will assume the user wanted to use calculated values instead of interpolated
    // in those missing periods.
    for (size_t period = 0; period < mBuildingSize.size(); ++period) {
        mIsFixedBuildingSize[period] = mBuildingSize[period].isInited();
    }

    // Interpolate parameters.  Note this will copy the last value to extrapolate
    // if necessary.
    SectorUtils::fillMissingPeriodVectorInterpolated( mBuildingSize );
    SectorUtils::fillMissingPeriodVectorNextAvailable( mPriceExponent );
    SectorUtils::fillMissingPeriodVectorInterpolated( mShellConductance );
    SectorUtils::fillMissingPeriodVectorInterpolated( mFloorToSurfaceRatio );
}

void BuildingNodeInput::initCalc( const string& aRegionName,
                                  const string& aSectorName,
                                  const bool aIsNewInvestmentPeriod,
                                  const bool aIsTrade,
                                  const IInfo* aTechInfo,
                                  const int aPeriod )
{
    /*!
     * \pre We must have a demand fn by now.
     */
    assert( !mFunctionType.empty() && mFunction );

    // Get the subregional population and income from the info object which is where
    // the consumer stored them.
    mCurrentSubregionalPopulation = aTechInfo->getDouble( "subregional-population", true );
    mCurrentSubregionalIncome = aTechInfo->getDouble( "subregional-income", true );

    // create a cache of the child INestedInput* as IInput* since the production function
    // needs a vector of those and the compiler can not be conviced it is safe to use a
    // vector of the subclass INestedInput* instead of IInput*
    mChildInputsCache.clear();
    mChildInputsCache.reserve( mNestedInputs.size() );
    for( NestedInputIterator nestedInputIter = mNestedInputs.begin();
        nestedInputIter != mNestedInputs.end(); ++nestedInputIter )
    {
        (*nestedInputIter)->initCalc( aRegionName, aSectorName, aIsNewInvestmentPeriod, 
                                      aIsTrade, aTechInfo, aPeriod );
        mChildInputsCache.push_back( *nestedInputIter );
    }
}

void BuildingNodeInput::copyParam( const IInput* aInput,
                                   const int aPeriod )
{

    /*!
     * \warning The ability to copyParams has been left unimplemented for GCAM consumers.
     */
    assert( false );
}

IInput* BuildingNodeInput::clone() const {
    BuildingNodeInput* retNodeInput = new BuildingNodeInput;
    retNodeInput->copy( *this );
    return retNodeInput;
}



void BuildingNodeInput::copy( const BuildingNodeInput& aNodeInput ) {
    mName = aNodeInput.mName;
    mFunctionType = aNodeInput.mFunctionType;
    mFunction = aNodeInput.mFunction;
    mBuildingSize = aNodeInput.mBuildingSize;
    mIsFixedBuildingSize = aNodeInput.mIsFixedBuildingSize;
    mPriceExponent = aNodeInput.mPriceExponent;
    mShellConductance = aNodeInput.mShellConductance;
    mFloorToSurfaceRatio = aNodeInput.mFloorToSurfaceRatio;
    mInternalGainsMarketname = aNodeInput.mInternalGainsMarketname;
    mInternalGainsUnit = aNodeInput.mInternalGainsUnit;


    delete mSatiationDemandFunction;
    mSatiationDemandFunction = aNodeInput.mSatiationDemandFunction->clone();



    // copy children
    for( CNestedInputIterator it = aNodeInput.mNestedInputs.begin(); it != aNodeInput.mNestedInputs.end(); ++it ) {
        mNestedInputs.push_back( static_cast<INestedInput*>( (*it)->clone() ) );
    }
}

bool BuildingNodeInput::isSameType( const string& aType ) const {
    return aType == getXMLName();
}

bool BuildingNodeInput::hasTypeFlag( const int aTypeFlag ) const {
    return false;
}

//! Output debug info to XML
void BuildingNodeInput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    XMLWriteElement( mBuildingSize[ aPeriod ], "building-size", aOut, aTabs );
    XMLWriteElement(mIsFixedBuildingSize[aPeriod], "is-building-size-fixed", aOut, aTabs);
    XMLWriteElement( mPriceExponent[ aPeriod ], "price-exponent", aOut, aTabs );
    XMLWriteElement( mShellConductance[ aPeriod ], "shell-conductance", aOut, aTabs );
    XMLWriteElement( mFloorToSurfaceRatio[ aPeriod ], "floor-to-surface-ratio", aOut, aTabs );
    XMLWriteElement( mPrice[ aPeriod ], "price", aOut, aTabs );
    XMLWriteElement(mUnadjustSatiation, "unadjust-satiation", aOut, aTabs);
    XMLWriteElement(mHabitableLand, "habitable-land", aOut, aTabs);
    XMLWriteElement(mBasepcFlsp, "base-pcFlsp", aOut, aTabs);
    XMLWriteElement(mLandDensityParam, "land-density-param", aOut, aTabs);
    XMLWriteElement(mbParam, "b-param", aOut, aTabs);
    XMLWriteElement(mIncomeParam, "income-param", aOut, aTabs);
    XMLWriteElement(mBiasAdjustParam, "bias-adjust-param", aOut, aTabs);
    XMLWriteElement(mCurrentSubregionalPopulation, "subregional-population", aOut, aTabs);
    XMLWriteElement(mCurrentSubregionalIncome, "subregional-income", aOut, aTabs);

    XMLWriteElement( mFunctionType, "prodDmdFnType", aOut, aTabs );
    for( CNestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->toDebugXML( aPeriod, aOut, aTabs );
    }

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& BuildingNodeInput::getXMLName() const {
    return getXMLNameStatic();
}

const string& BuildingNodeInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
* \details This public function accesses the private constant string, XML_NAME.
*          This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& BuildingNodeInput::getXMLNameStatic() {
    const static string XML_NAME = "building-node-input";
    return XML_NAME;
}

//! Get the name of the NodeInput
const string& BuildingNodeInput::getName() const {
    return mName;
}

/*!
 * \brief Get the currently set Subregional Population.
 * \return Subregional population that has been set from
 *           the consumer.
 */
Value BuildingNodeInput::getSubregionalPopulation() const {
    return mCurrentSubregionalPopulation;
}

/*!
 * \brief Get the currently set Subregional income.  Note that this
 *          is the per capita income.
 * \return Subregional income that has been set from
 *           the consumer.
 */
Value BuildingNodeInput::getSubregionalIncome() const {
    return mCurrentSubregionalIncome;
}

/*!
 * \brief Get the shell conductance.
 * \param aPeriod The current model period.
 * \return Shell conductance at the given period.
 */
Value BuildingNodeInput::getShellConductance( const int aPeriod ) const {
    return mShellConductance[ aPeriod ];
}

/*!
 * \brief Get the floor to surface ratio.
 * \param aPeriod The current model period.
 * \return Floor to surface ratio at the given period.
 */
Value BuildingNodeInput::getFloorToSurfaceRatio( const int aPeriod ) const {
    return mFloorToSurfaceRatio[ aPeriod ];
}

/*!
 * \brief Get the total internal gains for this type of building.
 * \param aRegionName The name of the containing region.
 * \param aPeriod Model period.
 * \return Internal gains in the given period.
 */
double BuildingNodeInput::getInternalGains( const string& aRegionName, const int aPeriod ) const {
    return SectorUtils::getTrialSupply( aRegionName, mInternalGainsMarketname, aPeriod );
}

/*!
 * \brief Get the satiation demand function to be used in demand calculations.
 * \return The satiation demand function.
 */
SatiationDemandFunction* BuildingNodeInput::getSatiationDemandFunction() const {
    return mSatiationDemandFunction;
}


void BuildingNodeInput::removeEmptyInputs() {
    // this functionality has not been implemented
}

void BuildingNodeInput::initialize() {
}

void BuildingNodeInput::calcCoefficient( const std::string& aRegionName, const std::string& aSectorName,
        const int aTechPeriod )
{
    // have child inputs calculate their children's coefficients first
    for( NestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->calcCoefficient( aRegionName, aSectorName, aTechPeriod );
    }

    // now have the production function calculate the coefficient for our direct children
    mFunction->calcCoefficient( mChildInputsCache,
        0, aRegionName, aSectorName, aTechPeriod, 0, 0, 0, this );
}

void BuildingNodeInput::changeElasticity( const std::string& aRegionName, const int aPeriod, const double aAlphaZero ) {
    // have children adjust their children's coefficients first
    for( NestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->changeElasticity( aRegionName, aPeriod, aAlphaZero );
    }
    
    // calculate new alpha coefficients for our direct children
    mFunction->changeElasticity( mChildInputsCache, aRegionName, 0, 0, 0, aPeriod, aAlphaZero,
        0, 0 );
}

void BuildingNodeInput::calcLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aAlphaZero ) 
{
    // have children calculate their levelized costs first
    // the leaves are assumed to already have calculated their appropriate price paid
    for( NestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->calcLevelizedCost( aRegionName, aSectorName, aPeriod, aAlphaZero );
    }

    // use the function to calculate our levelized costs
    double weightedEnergyPrice = mFunction->calcLevelizedCost( mChildInputsCache, aRegionName, aSectorName, aPeriod,
        aAlphaZero, 0, this );

    setPricePaid( weightedEnergyPrice, aPeriod );
}

double BuildingNodeInput::calcInputDemand( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod, const double aPhysicalOutput, const double aUtilityParameterA,
        const double aAlphaZero )
{
    // first calculate the demands for the direct children
    double retDemand = mFunction->calcDemand( mChildInputsCache, aPhysicalOutput, aRegionName, aSectorName, 1,
        aPeriod, aUtilityParameterA, aAlphaZero, 0, 0, this );

    // have all of the children calculate demands for their children
    for( NestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->calcInputDemand( aRegionName, aSectorName, aPeriod, (*it)->getPhysicalDemand( aPeriod ),
            aUtilityParameterA, aAlphaZero );
    }
    return retDemand;
}

const IFunction* BuildingNodeInput::getFunction() const {
    assert( mFunction );

    return mFunction;
}

double BuildingNodeInput::getLevelizedCost( const std::string& aRegionName, const std::string& aSectorName,
        const int aPeriod ) const
{
    return mPrice[ aPeriod ];
}

double BuildingNodeInput::getPhysicalDemand( const int aPeriod ) const {
    return mBuildingSize[ aPeriod ];
}
    
void BuildingNodeInput::setPhysicalDemand( const double aPhysicalDemand,
                                    const std::string& aRegionName, 
                                    const int aPeriod )
{
    // We are storing the results in the same vector as the calibration data
    // generally the calculated value should match however it may not if the
    // solver throws us negative prices.  We must explictly gaurd against
    // reseting these values in calibration years.
    if( aPeriod > scenario->getModeltime()->getFinalCalibrationPeriod() ) {
         // Only reset the building size to a calculated value when not
        // running with a fixed path.
        if (!mIsFixedBuildingSize[aPeriod]) {
           mBuildingSize[ aPeriod ].set( aPhysicalDemand );
        }
    }
}

double BuildingNodeInput::getPrice( const std::string& aRegionName,
                             const int aPeriod ) const
{
    return mPrice[ aPeriod ];
}

void BuildingNodeInput::setPrice( const std::string& aRegionName,
                           const double aPrice,
                           const int aPeriod )
{
    mPrice[ aPeriod ] = aPrice;
}

double BuildingNodeInput::getPricePaid( const std::string& aRegionName,
                                 const int aPeriod ) const
{
    return mPrice[ aPeriod ];
}

void BuildingNodeInput::setPricePaid( const double aPricePaid,
                               const int aPeriod )
{
    mPrice[ aPeriod ] = aPricePaid;
}

double BuildingNodeInput::getPriceElasticity( const int aPeriod ) const {
    return mPriceExponent[ aPeriod ];
}

void BuildingNodeInput::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitBuildingNodeInput( this, aPeriod );

    // visit the nested inputs
    for( CNestedInputIterator it = mNestedInputs.begin(); it != mNestedInputs.end(); ++it ) {
        (*it)->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitBuildingNodeInput( this, aPeriod );
}
