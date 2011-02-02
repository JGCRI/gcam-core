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
* \file forest_production_technology.cpp
* \ingroup Objects
* \brief ForestProductionTechnology class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include "technologies/include/forest_production_technology.h"
#include "land_allocator/include/iland_allocator.h"
#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
ForestProductionTechnology::ForestProductionTechnology( const string& aName, const int aYear )
:FoodProductionTechnology( aName, aYear ){
    // TODO: 0.02 should not be a default value.
    interestRate = 0.05;
    mRotationPeriod = 0;
}

// ! Destructor
ForestProductionTechnology::~ForestProductionTechnology() {
}

//! Parses any input variables specific to derived classes
bool ForestProductionTechnology::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    if( nodeName == "interestRate" ) {
        interestRate = XMLHelper<double>::getValue( curr );
    }
    else if( nodeName == "futureProduction" ) {
        mFutureProduction = XMLHelper<double>::getValue( curr );
    }
    else if( !FoodProductionTechnology::XMLDerivedClassParse(nodeName, curr)) {
        return false;
    }
    return true;
}

/*! \brief Derived class visitor.
*
* This may have already been implemented in multi-inputs version, so replace with that if so.
* \author Steve Smith
*/
void ForestProductionTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    
    // Call derived visit for this technology.
    aVisitor->startVisitForestProductionTechnology( this, aPeriod );
    // End the derived class visit.
    aVisitor->endVisitForestProductionTechnology( this, aPeriod );
}

//! write object to xml output stream
void ForestProductionTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    FoodProductionTechnology::toInputXMLDerived( out, tabs);
    if( mFutureProduction.isInited() ){
        XMLWriteElement( mFutureProduction, "futureProduction", out, tabs );
    }
    XMLWriteElement( interestRate, "interestRate", out, tabs );
}

//! write object to xml output stream
void ForestProductionTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    FoodProductionTechnology::toDebugXMLDerived( period, out, tabs);
    XMLWriteElement( mFutureProduction, "futureProduction", out, tabs );
    XMLWriteElement( interestRate, "interestRate", out, tabs );
    XMLWriteElement( mFutureYield, "future-yield", out, tabs );
    XMLWriteElement( mFutureLand, "future-land-allocation", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& ForestProductionTechnology::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& ForestProductionTechnology::getXMLNameStatic() {
    const static string XML_NAME = "ForestProductionTechnology";
    return XML_NAME;
}

//! Clone Function. Returns a deep copy of the current technology.
ForestProductionTechnology* ForestProductionTechnology::clone() const {
    return new ForestProductionTechnology( *this );
}

void ForestProductionTechnology::initCalc( const string& aRegionName,
                                           const string& aSectorName,
                                           const IInfo* aSubsectorInfo,
                                           const Demographic* aDemographics,
                                           PreviousPeriodInfo& aPrevPeriodInfo,
                                           const int aPeriod )
{
    // Ideally this would use the production state but it isn't setup yet for
    // this period.
    if( year == scenario->getModeltime()->getper_to_yr( aPeriod ) ){
        // Set calibrated values to land allocator in case these were disrupted
        // in previous period
        setCalLandValues();
    }

    FoodProductionTechnology::initCalc( aRegionName, aSectorName, aSubsectorInfo,
                                        aDemographics, aPrevPeriodInfo, aPeriod );

    // Apply technical change for forests
    // This function needs to be called again because forest tech change must be applied from now until the rotation period
    mLandAllocator->applyAgProdChange( landType, mName, agProdChange, aPeriod + getNRotationPeriodSteps( scenario->getModeltime()->getyr_to_per( year ) ) , aPeriod );
    
}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \author Josh Lurz
* \warning Markets are not necessarily set when completeInit is called
* \author James Blackwood
* \warning This may break if timestep is not constant for each time period.
*/
void ForestProductionTechnology::completeInit( const std::string& aRegionName,
                                               const std::string& aSectorName,
                                               const std::string& aSubsectorName,
                                               DependencyFinder* aDepFinder,
                                               const IInfo* aSubsectorInfo,
                                               ILandAllocator* aLandAllocator )
{
    // Setup the land allocators for the secondary outputs
    if ( mOutputs.size() ) {
        // Technology::completeInit() will add the primary output.
        // At this point, all are secondary outputs
        for ( vector<IOutput*>::iterator outputIter = mOutputs.begin(); outputIter != mOutputs.end(); ++outputIter ) {
           ( *outputIter )->setLandAllocator( aLandAllocator, mName, landType );
        }
    }

    // Unlike food tech's, forest techs can have ag prod change in a calibration period -- as this indicates 
    // future productivity change. Set to zero so that is not actually applied in base period, but save value 
    // for application to forest production leaf.
    mSavedAgProdChange = agProdChange;
    
    // TODO: Change to be able to call the parent function.
    // Right now doesn't work since two classes aren't derived from common parent.
    // FoodProductionTechnology::completeInit has operations not appropriate for forests 
    // To do this, likely need a     ILandAllocator::LandUsageType getLandType() function so as to
    // create the proper land leaf type.
    
    Technology::completeInit( aRegionName, aSectorName, aSubsectorName,
                              aDepFinder, aSubsectorInfo,
                              aLandAllocator );

    // Store away the land allocator.
    mLandAllocator = aLandAllocator;

    // Set rotation period variable so this can be used throughout object
    mRotationPeriod = aSubsectorInfo->getInteger( "rotationPeriod", true );

    // Setup the land usage for this production.
    int techPeriod = scenario->getModeltime()->getyr_to_per( year );
    mLandAllocator->addLandUsage( landType, mName, ILandAllocator::eForest, techPeriod );

    setCalLandValues();
}

int ForestProductionTechnology::getNRotationPeriodSteps( const int aPeriod ) const {
   const Modeltime* modeltime = scenario->getModeltime();
    
    int ret = 0;
    if( aPeriod >= modeltime->getmaxper() ) {
        ret = ceil( static_cast<double>( mRotationPeriod ) / modeltime->gettimestep( modeltime->getmaxper() - 1 ) );
    }
    const int rotationYear = modeltime->getper_to_yr( aPeriod ) + mRotationPeriod;
    int numPeriods = aPeriod;
    for( ; numPeriods < modeltime->getmaxper() && modeltime->getper_to_yr( numPeriods ) < rotationYear; ++numPeriods ) {
    }
    
    if( numPeriods == modeltime->getmaxper() ) {
        const int aFinalPeriod = modeltime->getmaxper() - 1;
        numPeriods += ceil( static_cast<double>( rotationYear - modeltime->getper_to_yr( aFinalPeriod ) )
                           / modeltime->gettimestep( aFinalPeriod ) ) - 1;
    }
    ret = numPeriods - aPeriod;
    if ( !mFutureProduction.isInited() ) {
        ret = 0;
    }
   
   return ret;
}

/*! \brief Sets calibrated land values to land allocator.
*
* For forests, this utility function is called twice. Once in completeInit so that initial
* shares can be set throughout the land allocator and again in initCalc()
* in case shares have been disrupted by a previous call to calc() and to overwrite future calibration 
* values with new values.  
*
* \author Steve Smith
*/
void ForestProductionTechnology::setCalLandValues() {

    // -1 means not read in
    if ( mCalValue.get() && ( calYield != -1 )) {
        const Modeltime* modeltime = scenario->getModeltime();

        // Operating period of this technology
        int thisPeriod = modeltime->getyr_to_per(year);
        // Number of model timesteps for rotation period
        int nRotPeriodSteps = getNRotationPeriodSteps( thisPeriod );
        int finalRotPeriodSteps = getNRotationPeriodSteps( modeltime->getmaxper() - 1 );
        
        // Make sure that calibrated land-use for periods beyond rotation period is set to zero.
        // Land allocator does not know rotation period information at this point, so it sums over all
        // periods to determine calibrated land use, so zero out information for all these periods.
        for ( int aHarvestPeriod = thisPeriod + nRotPeriodSteps + 1; aHarvestPeriod < modeltime->getmaxper() + finalRotPeriodSteps; aHarvestPeriod++ ) {
            mLandAllocator->setCalLandAllocation( landType, mName, 0.0, aHarvestPeriod, thisPeriod );
        }
        
        // Start with current period values
        double calProductionTemp = mCalValue->getCalOutput(); // Pass in 1 since efficiency is always 1 for forests
        double calYieldTemp = calYield;
        
        // Set value of calObservedYield, so that FoodProductionTechnology::initCalc will set variable costs
        calObservedYield = calYield;
        // Loop from current period to rotation period time steps from current period to set current and 
        // future land allocation and production from forests. Operation of setCalLandAllocation in the forest
        // leaf depends on all cal land-use data being present on the last call.
        for ( int aHarvestPeriod = thisPeriod; aHarvestPeriod <= thisPeriod + nRotPeriodSteps; aHarvestPeriod++ ) {
            // TODO: Need to do be able to somehow get productivity change from other
            // periods. Or demand that productivity change is the same for all
            // calibration + future harvest periods (could test in applyAgProdChange)
            if ( aHarvestPeriod > thisPeriod ) {
                // increment production linearly between current and future production years
                calProductionTemp += ( mFutureProduction - mCalValue->getCalOutput() )
                    / ( static_cast<double>( mRotationPeriod ) / modeltime->gettimestep( aHarvestPeriod ) );
                // Apply ag productivity change to yield assuming same productivity applies to all years
                // Apply ag prod change relative to this period
                calYieldTemp = calYield * pow( 1 + mSavedAgProdChange, 
                    double( modeltime->getper_to_yr( aHarvestPeriod ) - modeltime->getper_to_yr( thisPeriod ) ) );
            }
            // Calculate land harvested in the harvest period
            calLandUsed = calProductionTemp / calYieldTemp;
            mLandAllocator->setCalLandAllocation( landType, mName, calLandUsed, aHarvestPeriod, thisPeriod );
            if ( thisPeriod == aHarvestPeriod ){
                mLandAllocator->setCalObservedYield( landType, mName, calYieldTemp, aHarvestPeriod );   
            }
            else if ( aHarvestPeriod > modeltime->getFinalCalibrationPeriod() ) {
                mLandAllocator->setCalObservedYield( landType, mName, calYieldTemp, aHarvestPeriod );   
            }
        }  
    }
}

void ForestProductionTechnology::calcCost( const string& aRegionName,
                                           const string& aSectorName,
                                           const int aPeriod )
{
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

    // If yield is GCal/Ha and prices are $/GCal, then rental rate is $/Ha
    // Passing in rate as $/GCal and setIntrinsicRate will set it to  $/Ha.
    double profitRate = calcProfitRate( aRegionName, getFutureMarket( aSectorName ), aPeriod );
    mLandAllocator->setIntrinsicRate( aRegionName, landType, mName, profitRate, aPeriod );

    // Override costs to a non-zero value as the cost for a food production
    // technology is not used for the shares.
    mCosts[ aPeriod ] = 1;
}

/*! \brief Calculates the output of the technology.
* \details Calculates the amount of current forestry output based on the amount
*          of planted forestry land and it's yield. Forestry production
*          technologies are profit based and determine their supply
*          independently of the passed in subsector demand. However, since this
*          is a solved market, in equilibrium the sum of the production of
*          technologies within a sector will equal the demand for the sector.
*          For forestry this supply is fixed because trees were planted several
*          periods before. Since the supply is inelastic, demand must adjust to
*          reach equilibrium.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aVariableDemand Subsector demand for output.
* \param aFixedOutputScaleFactor Fixed output scale factor.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void ForestProductionTechnology::production( const string& aRegionName,
                                             const string& aSectorName,
                                             const double aVariableDemand,
                                             const double aFixedOutputScaleFactor,
                                             const GDP* aGDP,
                                             const int aPeriod )
{
    if( !mProductionState[ aPeriod ]->isOperating() ){
            // Set physical output to zero.
        mOutputs[ 0 ]->setPhysicalOutput( 0, aRegionName,
                                          mCaptureComponent.get(),
                                          aPeriod );
        return;
    }

    // Calculate profit rate.
    double profitRate = calcProfitRate( aRegionName, getFutureMarket( aSectorName ), aPeriod );

    // Calculating the yield for future forest.
    const int harvestPeriod = getHarvestPeriod( aPeriod );
    mLandAllocator->calcYield( landType, mName, aRegionName,
                               profitRate, harvestPeriod, aPeriod );
    
    // Add the supply of future forestry to the future market.
    double futureSupply = calcSupply( aRegionName, aSectorName, harvestPeriod );
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( getFutureMarket( aSectorName ), aRegionName, futureSupply, aPeriod );

    // Save values for debugging
    mFutureLand = mLandAllocator->getLandAllocation( landType, mName, aPeriod );
    mFutureYield = futureSupply / mFutureLand;

    // now calculate the amount to be consumed this period (ie. planted steps
    // periods ago).
    double primaryOutput = calcSupply( aRegionName, aSectorName, aPeriod );

    calcEmissionsAndOutputs( aRegionName, primaryOutput, aGDP, aPeriod );
}

/*! \brief Calculate the profit rate for the technology.
* \details Calculates the profit rate for the forestry technology. This is equal
*          to the net present value of the market price minus the variable cost 
*          Profit rate can be negative.
* \param aRegionName Region name.
* \param aProductName Name of the product for which to calculate the profit
*        rate. Must be an output of the technology.
* \return The profit rate.
*/
double ForestProductionTechnology::calcProfitRate( const string& aRegionName,
                                                   const string& aProductName,
                                                   const int aPeriod ) const
{
    // Calculate the future profit rate.
    // TODO: If a ForestProductionTechnology had emissions this would not be correct as the 
    // emissions cost would be calculated for the present year and the emissions would be 
    // charged in a future year.
    double profitRate = FoodProductionTechnology::calcProfitRate( aRegionName, aProductName, aPeriod );

    // Calculate the net present value.
    double netPresentValue = profitRate * calcDiscountFactor();

    return netPresentValue;
}

/*! \brief Calculate the factor which discounts the future value of the forest
*          harvest between the future harvest period and the current period and
*          levels across the number of years during which the trees are
*          grown.
* \return The discount factor.
*/
double ForestProductionTechnology::calcDiscountFactor() const {
    assert( mRotationPeriod > 0 );
    return interestRate / ( pow( 1 + interestRate, static_cast<int>( mRotationPeriod ) ) - 1 );
}

/*! \brief Get the period in which the crop will be harvested if planted in the
*          current period.
* \param aCurrentPeriod Current period.
* \return The harvest period.
*/
int ForestProductionTechnology::getHarvestPeriod( const int aCurrentPeriod ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int rotationYear = modeltime->getper_to_yr( aCurrentPeriod ) + mRotationPeriod;
    int numPeriods = aCurrentPeriod;
    for( ; numPeriods < modeltime->getmaxper() && modeltime->getper_to_yr( numPeriods ) < rotationYear; ++numPeriods ) {
    }
    
    if( numPeriods == modeltime->getmaxper() ) {
        const int aFinalPeriod = modeltime->getmaxper() - 1;
        numPeriods += ceil( static_cast<double>( rotationYear - modeltime->getper_to_yr( aFinalPeriod ) )
                           / modeltime->gettimestep( aFinalPeriod ) ) - 1;
    }
    return numPeriods;
}

/*! \brief Get the future market for a given product name.
* \param aProductName Name of the product.
* \return Name of the future market.
*/
const string ForestProductionTechnology::getFutureMarket( const string& aProductName ) const {
    return "Future" + aProductName;
}
