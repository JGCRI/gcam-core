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
* \file resource_reserve_technology.cpp
* \ingroup Objects
* \brief ResourceReserveTechnology class source file.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include "technologies/include/resource_reserve_technology.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/xml_helper.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/ioutput.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"

using namespace std;

extern Scenario* scenario;

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
ResourceReserveTechnology::ResourceReserveTechnology(const string& aName, const int aYear) :
Technology(aName, aYear),
mTotalReserve( 0.0 ),
mInvestmentCost( 0.0 ),
mBuildupYears( 0 ),
mDeclinePhasePct( 1.0 )
{
}

ResourceReserveTechnology::ResourceReserveTechnology() :
mTotalReserve( 0.0 ),
mInvestmentCost( 0.0 ),
mBuildupYears( 0 ),
mDeclinePhasePct( 1.0 )
{
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech ResourceReserveTechnology to copy.
*/
void ResourceReserveTechnology::copy(const ResourceReserveTechnology& aTech) {
    Technology::copy( aTech );
    
    mBuildupYears = aTech.mBuildupYears;
    mDeclinePhasePct = aTech.mDeclinePhasePct;
}

// ! Destructor
ResourceReserveTechnology::~ResourceReserveTechnology() {
}

//! write object to xml output stream
void ResourceReserveTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const {
	XMLWriteElement(mTotalReserve, "total-resource-reserve", aOut, aTabs);
    XMLWriteElement(mInvestmentCost, "investment-cost", aOut, aTabs);
	XMLWriteElement(mCumulProd[ aPeriod ], "cumulative-production", aOut, aTabs);
	XMLWriteElement(mBuildupYears, "buildup-years", aOut, aTabs);
    XMLWriteElement(mDeclinePhasePct, "decline-phase-percent", aOut, aTabs);
    XMLWriteElement(mMarginalRevenue, "marginal-revenue", aOut, aTabs);
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \return The constant XML_NAME.
*/
const string& ResourceReserveTechnology::getXMLName() const {
	return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \return The constant XML_NAME as a static.
*/
const string& ResourceReserveTechnology::getXMLNameStatic() {
	const static string XML_NAME = "resource-reserve-technology";
	return XML_NAME;
}

//! Clone Function. Returns a deep copy of the current technology.
ResourceReserveTechnology* ResourceReserveTechnology::clone() const {
    ResourceReserveTechnology* clone = new ResourceReserveTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void ResourceReserveTechnology::completeInit(const std::string& aRegionName,
	const std::string& aSectorName,
	const std::string& aSubsectorName,
	const IInfo* aSubsectorInfo,
	ILandAllocator* aLandAllocator)
{
    mAvgProdLifetime = aSubsectorInfo->getDouble( "average-production-lifetime", true );
    
	Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
		aLandAllocator);
}

void ResourceReserveTechnology::initCalc( const string& aRegionName,
                                          const string& aSectorName,
                                          const IInfo* aSubsectorInfo,
                                          const Demographic* aDemographics,
                                          PreviousPeriodInfo& aPrevPeriodInfo,
                                          const int aPeriod )
{
    // determine if the resource is currently calibrating so we can disable any "shutdown"
    // behavior and ensure calibration values match
    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* productInfo = marketplace->getMarketInfo( aSectorName, aRegionName, aPeriod, false );
    mIsResourceCalibrating = productInfo->getBoolean( "fully-calibrated", false );
    
    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPrevPeriodInfo, aPeriod);
}


/*!
 * \brief Calculates the output of the technology.
 * \details For the case of ResourceReserveTechnology this method will be called
 *          by the containing resource and aVariableDemand will actually be the
 *          new investment in terms of cumulative resource.  If this is a new vintage
 *          technology then we can go ahead and set that value.  We can then just use
 *          the base class production method if we annualize aVariableDemand using
 *          the expected average lifetime already set from the containing resource.
 * \param aRegionName Region name.
 * \param aSectorName Sector name, aka resource name.
 * \param aVariableDemand The cumulative resource base to use as new investment.
 * \param aFixedOutputScaleFactor A factor that may scale down *all* production (new
 *                                and existing vintages).  For ResourceReserveTechnology
 *                                would only potentially be necessary during calibration.
 * \param aGDP Regional GDP container.
 * \param aPeriod Model period.
 */
void ResourceReserveTechnology::production(const string& aRegionName,
	const string& aSectorName,
	const double aVariableDemand,
	const double aFixedOutputScaleFactor,
	const GDP* aGDP,
	const int aPeriod)
{
    // Set the total resource reserve base if this is a new investment technology
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        mTotalReserve = aVariableDemand;
    }
    
    // Just call the base class production with the annualized production value for
    // aVariableDemand.  Note we also need to be sure to scale the annualized production
    // by aFixedOutputScaleFactor to ensure all production is being scaled incase we
    // need to do so in order to match calibration.
    double annualizedProd = aVariableDemand / mAvgProdLifetime * aFixedOutputScaleFactor;
    Technology::production( aRegionName, aSectorName, annualizedProd,
                            aFixedOutputScaleFactor, aGDP, aPeriod );
}

/*!
 * \brief Set up the production state object for this period which is responsible
 *        for properly operating this vintage.
 * \details For ResourceReserveTechnology we may need to adjust the "base" output
 *          levels to account for the buildup or decline phase other than that this
 *          method operates mostly the same as the base class version.
 * \param aPeriod The current model period.
 */
void ResourceReserveTechnology::setProductionState( const int aPeriod ) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if( mProductionState[ aPeriod ] ) {
        delete mProductionState[ aPeriod ];
    }

    const Modeltime* modeltime = scenario->getModeltime();
    const int currYear = modeltime->getper_to_yr( aPeriod );
    
    double annualAvgProd = mTotalReserve / mAvgProdLifetime;
    double productionPhaseScaler;
    bool active = true;
    if( mYear >= currYear || mTotalReserve == 0.0 || ( mYear + mLifetimeYears ) <= currYear ) {
        // variable, retired, or future production
        productionPhaseScaler = 1.0;
        active = false;
    }
    else if((currYear - mYear) < mBuildupYears) {
        // phase in production linearly if we are within the buildup years
        productionPhaseScaler = (currYear - mYear + 1) / mBuildupYears;
    }
    else if(!mIsResourceCalibrating && (mTotalReserve - mCumulProd[ aPeriod - 1]) <= (mTotalReserve * mDeclinePhasePct)) {
        // phase out production linearly if we have depleted enough of the reserve to be
        // in the decline phase
        productionPhaseScaler = max((mTotalReserve - mCumulProd[ aPeriod - 1]) /
                                    (mTotalReserve * mDeclinePhasePct), 0.0);
    }
    else {
        productionPhaseScaler = 1.0;
    }

    double initialOutput = annualAvgProd * productionPhaseScaler;
    if( active ) {
        // guard against producing more than the total reserve by backing out the
        // annual production that would depelete the rest of the reserve and
        // only producing the that amount if it is less that the adjusted average
        // annual production
        double maxAvail = std::max(
                                   (( mTotalReserve - mCumulProd[ aPeriod - 1 ]) - modeltime->gettimestep(aPeriod) * mOutputs[0]->getPhysicalOutput(aPeriod - 1)) * 2 /
                                   modeltime->gettimestep( aPeriod) + mOutputs[0]->getPhysicalOutput( aPeriod - 1),
                                   0.0);
        initialOutput = std::min(initialOutput, maxAvail);
    }
    
    mProductionState[ aPeriod ] =
        ProductionStateFactory::create( mYear, mLifetimeYears, mFixedOutput,
                                   initialOutput, aPeriod ).release();
}

/*!
 * \brief Return fixed Technology output
 * \details For ResourceReserveTechnology we use this oportunity to set the investment
 *          cost if this is a new vintage technology.  In addition we adjust the marginal
 *          revenue to include the investment cost.
 * \param aRegionName Region name.
 * \param aSectorName Sector name.
 * \param aHasRequiredInput Whether the technology should check what the required
 *        input is.
 * \param aRequiredInput The input the technology is required to have if it
 *        returns a fixed output value.
 * \param aMarginalRevenue The marginal revenue that may be used when calculating
 *                         the profit rate of this technology.
 * \param aPeriod model period
 * \return Value of fixed output for this Technology
 */
double ResourceReserveTechnology::getFixedOutput( const string& aRegionName,
                                  const string& aSectorName,
                                  const bool aHasRequiredInput,
                                  const string& aRequiredInput,
                                  const double aMarginalRevenue,
                                  const int aPeriod ) const
{
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        const_cast<ResourceReserveTechnology*>(this)->mInvestmentCost = aMarginalRevenue;
    }
    
    double margRevTest = mIsResourceCalibrating ? 50.0 : aMarginalRevenue + mInvestmentCost;

    return Technology::getFixedOutput( aRegionName, aSectorName, aHasRequiredInput, aRequiredInput,
                                       margRevTest, aPeriod );
}

/*!
 * \brief Return the total variable input costs which includes energy, taxes, etc.
 * \details For ResourceReserveTechnology we simply tack on the investment cost.
 * \param aRegionName The region containing the Technology.
 * \param aSectorName The sector containing the Technology.
 * \param aPeriod Period in which to calculate the energy cost.
 * \return A calculated energy cost for the Technology.
 */
double ResourceReserveTechnology::getEnergyCost( const string& aRegionName,
                                  const string& aSectorName,
                                  const int aPeriod ) const
{
    // Calculates the energy cost by first calculating the total cost including
    // all inputs and then removing the non-energy costs.
    double cost = Technology::getEnergyCost( aRegionName, aSectorName, aPeriod ) +
        mInvestmentCost;

    return cost;
}

void ResourceReserveTechnology::doInterpolations(const Technology* aPrevTech, const Technology* aNextTech) {
	Technology::doInterpolations(aPrevTech, aNextTech);

	const ResourceReserveTechnology* prevTech = static_cast<const ResourceReserveTechnology*> (aPrevTech);
	const ResourceReserveTechnology* nextTech = static_cast<const ResourceReserveTechnology*> (aNextTech);

	/*!
	* \pre We were given a valid previous ag production technology.
	*/
	assert(prevTech);

	/*!
	* \pre We were given a valid next ag production technology.
	*/
	assert(nextTech);
}

void ResourceReserveTechnology::postCalc( const string& aRegionName, const int aPeriod ) {
    Technology::postCalc( aRegionName, aPeriod );
    
    if( !mProductionState[ aPeriod ]->isOperating() ) {
        return;
    }
    
    // update cumulative production to account for resource depletion
    const Modeltime* modeltime = scenario->getModeltime();
    double currProd = mOutputs[ 0 ]->getPhysicalOutput( aPeriod );
    int timeStep = modeltime->gettimestep(aPeriod);
    int currYear = modeltime->getper_to_yr( aPeriod );
    if( mYear == currYear ) {
        // assume constant production
        mCumulProd[ aPeriod ] = currProd * timeStep;
    }
    else {
        // simplify by assuming linear change in annual production from the previous
        // period
        double prevProd = mOutputs[ 0 ]->getPhysicalOutput( aPeriod - 1 );
        double periodCumulProd = prevProd * timeStep + 0.5 * ( currProd - prevProd) * timeStep;
        mCumulProd[ aPeriod ] = aPeriod > 0 ? mCumulProd[ aPeriod - 1 ] + periodCumulProd : 0.0;
    }
}
