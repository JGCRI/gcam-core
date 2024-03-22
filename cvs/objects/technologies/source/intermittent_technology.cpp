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
* \file intermittent_technology.cpp
* \ingroup Objects
* \brief IntermittentTechnology class source file.
* \author Marshall Wise, Sonny Kim, Matthew Binsted, Matt Mowers
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <vector>

#include "technologies/include/intermittent_technology.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"
#include "containers/include/info_factory.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/ibackup_calculator.h"
#include "sectors/include/value_factor_calculator.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/iinput.h"
#include "functions/include/non_energy_input.h"
#include "technologies/include/iproduction_state.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor.
 * \author Marshall Wise, Sonny Kim
 */

IntermittentTechnology::IntermittentTechnology( const string& aName, const int aYear ) 
:Technology( aName, aYear )
{
    mElectricSectorName = "electricity";
    mElecReserveMargin = 0.15;
    
    mValueFactorCalculator = 0;
    
    mResourceInput = mInputs.end();
    mTechCostInput = mInputs.end();
}

IntermittentTechnology::IntermittentTechnology() {
    mElectricSectorName = "electricity";
    mElecReserveMargin = 0.15;
    
    mValueFactorCalculator = 0;
    
    mResourceInput = mInputs.end();
    mTechCostInput = mInputs.end();
}

/*!
 * \brief Destructor.
 */
IntermittentTechnology::~IntermittentTechnology() {
    delete mValueFactorCalculator;
}
    
IntermittentTechnology* IntermittentTechnology::clone() const {
    IntermittentTechnology* clone = new IntermittentTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void IntermittentTechnology::copy( const IntermittentTechnology& aOther ) {
    Technology::copy( aOther );
    mElectricSectorName = aOther.mElectricSectorName;
    mElectricSectorMarket = aOther.mElectricSectorMarket;
    mTrialMarketNameParsed = aOther.mTrialMarketNameParsed;
    
    if( aOther.mValueFactorCalculator) {
        delete mValueFactorCalculator;
        mValueFactorCalculator = aOther.mValueFactorCalculator->clone();
    }
    
    /*!
     * \warning Only copy member variables that are read-in. The rest will be filled in by
     *          initialization methods.
     */
}

const string& IntermittentTechnology::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& IntermittentTechnology::getXMLNameStatic() {
    const static string XML_NAME = "intermittent-technology";
    return XML_NAME;
}


/*! \brief Return name to be used for input object containing technology costs.
*
* This input object will contain technology capital, operation, and any other costs
* exclusive of fuel costs. Setting to blank indicates that this object does
* not use this cost.
*
* \author Steve Smith
* \return The constant XML_NAME as a static.
*/
const string& IntermittentTechnology::getTechCostName( ) const {
   const static string TECH_COST_NAME = ""; // no tech cost implmented in this class
   return TECH_COST_NAME;
}

void IntermittentTechnology::toDebugXMLDerived( const int period, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mElectricSectorName, "electric-sector-name", aOut, aTabs);
    XMLWriteElement( mElectricSectorMarket, "electric-sector-market", aOut, aTabs);
    XMLWriteElementCheckDefault( mTrialMarketNameParsed, "trial-market-name", aOut, aTabs, string("") );
}

/*! \brief Create a trial market for the technology and complete the initialization.
*
* This routine is only called once per model run
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aSubsectorName Subsector name.
* \param aSectorInfo Sector information object.
* \param aLandAllocator Regional land allocator.
* \author Marshall Wise, Sonny Kim
* \detail A trial market for the intermittent technology is created here,  
*         as well as the completetion of technology initialization. 
*         A trial market is created for each intermittent technology if common
*         trial market name is not given.
*         Use techInfo to pass infomation to SectorUtils for setting
*         market parameters, such as units.
* \todo Member constant values (Electricity Reserve Margin, and Ave grid
*       Capacity Factor) could be dynamically calculated and utilized by
*       intermittent technology.
*/
void IntermittentTechnology::completeInit( const string& aRegionName,
                                           const string& aSectorName,
                                           const string& aSubsectorName,
                                           const IInfo* aSubsectorInfo,
                                           ILandAllocator* aLandAllocator )
{
	// The parent method must be called first due to sequence issues
    Technology::completeInit( aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
							 aLandAllocator );	
	
	// Initialize electric reserve margin and average grid capacity factor from the Sector.
    mElecReserveMargin = aSubsectorInfo->getDouble( "electricity-reserve-margin", true );
    mAveGridCapacityFactor = aSubsectorInfo->getDouble( "average-grid-capacity-factor", true );

    // Inititalize info object
    mIntermittTechInfo.reset( InfoFactory::constructInfo( 0, getName() ) );
    // Output unit for intermittent technology to be for market.
    mIntermittTechInfo->setString( "output-unit", "share" );

    // If trial market name has not been readin use technology name as default trial market name.
    if( mTrialMarketNameParsed.empty() ){
        mTrialMarketName = getName(); // technology name
    }
    else {
        mTrialMarketName = mTrialMarketNameParsed;
    }
    
    if( mElectricSectorMarket.empty() ) {
        mElectricSectorMarket = aRegionName;
    }

    // Create trial market for intermittent technology
    if(mValueFactorCalculator){
        SectorUtils::createTrialSupplyMarket( aRegionName, mTrialMarketName, mIntermittTechInfo.get(), mElectricSectorMarket );
        MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
        depFinder->addDependency( aSectorName, aRegionName,
                                  SectorUtils::getTrialMarketName( mTrialMarketName ),
                                  aRegionName );
        if( aSectorName != mElectricSectorName ) {
            // This dependency can not be removed since it is inherently different
            // than sector dependencies.
            depFinder->addDependency( mElectricSectorName, mElectricSectorMarket, aSectorName, aRegionName, false );
        }
    }

    // Warn if a value factor calculator was not read-in.
    if( !mValueFactorCalculator){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "Intermittent technology " << mName << " in sector " << aSectorName
                << " in region " << aRegionName
                << " did not read in a value factor calculator."
                << "Results will not reflect decreasing value of intermittent generation as technology shares increase." << endl;
    }
    
    initializeInputLocations( aRegionName, aSectorName, 0 );
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
    depFinder->addDependency( aSectorName, aRegionName, (*mResourceInput)->getName(), aRegionName );
}

void IntermittentTechnology::initCalc( const string& aRegionName,
                                       const string& aSectorName,
                                       const IInfo* aSubsectorInfo,
                                       const Demographic* aDemographics,
                                       PreviousPeriodInfo& aPrevPeriodInfo,
                                       const int aPeriod )
{
    // Note: initCalc is called for all past, current and future technologies.
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo,
        aDemographics, aPrevPeriodInfo, aPeriod );
    if (mValueFactorCalculator) {
        mValueFactorCalculator->initCalc( mIntermittTechInfo.get() );

        // The renewable trial market is a share calculation so we can give the
        // solver some additional hints that the range should be between 0 and 1.
        SectorUtils::setSupplyBehaviorBounds( SectorUtils::getTrialMarketName( mTrialMarketName ),
                                              aRegionName, 0, 1, aPeriod );
    }
    initializeInputLocations( aRegionName, aSectorName, aPeriod );
}

void IntermittentTechnology::postCalc( const string& aRegionName,
                                      const int aPeriod )
{
    // Use the base class postCalc.
    Technology::postCalc( aRegionName, aPeriod );
}

/*! \brief Set intermittent technology outputs and inputs and shares for trial market.
* \author Sonny Kim
* \param aRegionName name of the region.
* \param aSectorName name of the sector.
* \param aVariableDemand current demand for the technology.
* \param aFixedOutputScaleFactor
* \param aGDP GDP
* \param aPeriod Model period
*/
void IntermittentTechnology::production( const string& aRegionName,
                                         const string& aSectorName, 
                                         double aVariableDemand,
                                         double aFixedOutputScaleFactor,
                                         const int aPeriod )
{
    // Use the base class production to set outputs and inputs.
    Technology::production( aRegionName, aSectorName, aVariableDemand,
                            aFixedOutputScaleFactor, aPeriod );
    
    // For the trial intermittent technology market, set the trial supply amount to
    // the ratio of intermittent-technology output to the electricity output.
    double dependentSectorOutput = scenario->getMarketplace()->getDemand( mElectricSectorName, mElectricSectorMarket, aPeriod );

    if ( dependentSectorOutput > 0 ){
        mIntermitOutTechRatio = std::min( getOutput( aPeriod ) / dependentSectorOutput, 1.0 );
    }
    else {
        mIntermitOutTechRatio = 0.0;
    }

    // Multiple vintaged intermittent technology ratios are additive. This gives one 
    // share for value factor calculation and proper behavior for vintaging intermittent technologies.
    SectorUtils::addToTrialDemand( aRegionName, mTrialMarketName, mIntermitOutTechRatio, aPeriod );
}

/*! \brief Return amount of resource needed per unit of energy output.
*  This method should be used when a technology uses a resource that is
*  not in energy units.
* \author Steve Smith
* \param aPeriod Model period.
*/
double IntermittentTechnology::getResourceToEnergyRatio(const string& aRegionName,
    const string& aSectorName,
    const int aPeriod)
{
    // Default assumpion is that resource is in energy units
    return 1.0;
}

/*! \brief Computes weighted cost of all technologies in Subsector,
*          adjusted by value factor.
* \details Computes a total cost of the subsector by adjusting the weighted
*          technology costs by value factor. The result is
*          "profitability-adjusted LCOE" (PLCOE = LCOE/VF).
* \author Marshall Wise, Sonny Kim, Matthew Binsted, Matt Mowers
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void IntermittentTechnology::calcCost( const string& aRegionName,
                                       const string& aSectorName,
                                       const int aPeriod )
{
    if (mProductionState[aPeriod]->isOperating()) {

        double cost = getTotalInputCost(aRegionName, aSectorName, aPeriod)
            * mPMultiplier -
            calcSecondaryValue(aRegionName, aPeriod);

        double valueFactor = 1;

        if ( mValueFactorCalculator ) {
            valueFactor = mValueFactorCalculator->getValueFactor(mTrialMarketName, mElectricSectorMarket, aRegionName, aPeriod);
        }

        mCosts[aPeriod] = cost / valueFactor;

        assert(util::isValidNumber(mCosts[aPeriod]));
    }
}

/*! 
 * \brief Initialize the cached locations of the resource input.
 * \details Determines and caches the locations of the resource
 *          input. The resource input is assumed to be the input with a
 *          variance.
 * \param aRegionName Name of the containing region.
 * \param aSectorName Name of the containing sector.
 * \param aPeriod Period.
 * \warning If the input vector changes size, these positions will not be valid.
 */
void IntermittentTechnology::initializeInputLocations( const string& aRegionName,
                                                       const string& aSectorName,
                                                       const int aPeriod )
{
    // Set the inputs to the error value.
    mTechCostInput = mResourceInput = mInputs.end();

    for( InputIterator i = mInputs.begin(); i != mInputs.end(); ++i ){
        // Parse location for energy inputs.
        if( ( *i )->hasTypeFlag( IInput::ENERGY | IInput::RESOURCE ) ){
            if( mResourceInput != mInputs.end() ){
                // There already was a resource input.
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::NOTICE );
                mainLog << "Intermittent technology " << mName << " in sector " << aSectorName
                << " in region " << aRegionName << " has more than one variable resource input." << endl;
            }
            else {
                mResourceInput = i;
            }
        }

        // Parse location for non-energy inputs.
        else{
            if ( ( *i )->getName() == getTechCostName() && ( getTechCostName() != "" ) ) {
               mTechCostInput = i;
               continue;
            }
        }
    }

    // Check that the resource input was set.
    if( mResourceInput == mInputs.end() ){
        // There already was a resource input.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Intermittent technology " << mName << " in sector " << aSectorName
                << " in region " << aRegionName << " does not have the required resource inputs."
                << endl;
        abort();
    }
}

