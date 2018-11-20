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
#include "emissions/include/aghg.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/generic_output.h"
#include "util/base/include/ivisitor.h"
//#include "containers/include/market_dependency_finder.h"
//#include "sectors/include/sector_utils.h"
#include "technologies/include/ishutdown_decider.h"
#include "util/base/include/initialize_tech_vector_helper.hpp"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const int ADDITIONAL_PRODUCTION_LIFETIME = 20;

/*!
* \brief Constructor.
* \param aName Technology name.
* \param aYear Technology year.
*/
ResourceReserveTechnology::ResourceReserveTechnology(const string& aName, const int aYear) :
Technology(aName, aYear)
{
    mTotalReserve = 0.0;
}

/*!
* \brief Copy the technology paramaters.
* \details Does not copy variables which should get initialized through normal
*          model operations.
* \param aTech Tech ResourceReserveTechnology to copy.
*/
void ResourceReserveTechnology::copy(const ResourceReserveTechnology& aTech) {
    Technology::copy( aTech );
    
    //mEORCoef = aTech.mEORCoef;
}

// ! Destructor
ResourceReserveTechnology::~ResourceReserveTechnology() {
}

//! Parses any input variables specific to derived classes
bool ResourceReserveTechnology::XMLDerivedClassParse(const string& aNodeName, const DOMNode* aCurrNode) {
    bool success = false;
    /*if( aNodeName == "eor-coef" ) {
        mEORCoef = XMLHelper<Value>::getValue( aCurrNode );
        success = true;
    }*/
	return success;
}

//! write object to xml output stream
void ResourceReserveTechnology::toDebugXMLDerived(const int aPeriod, ostream& aOut, Tabs* aTabs) const {
	XMLWriteElement(mTotalReserve, "total-resource-reserve", aOut, aTabs);
	XMLWriteElement(mCumulProd[ aPeriod ], "cumulative-production", aOut, aTabs);
	XMLWriteElement(mProductionPhaseScaler, "production-phase-scaler", aOut, aTabs);
    XMLWriteElement(mMarginalRevenue, "marginal-revenue", aOut, aTabs);
    /*double shutdown = 1.0;
    for( auto sd : mShutdownDeciders ) {
        shutdown *= sd->calcShutdownCoef(0, (mMarginalRevenue - mInputs[0]->getPrice("USA", aPeriod))/mInputs[0]->getPrice("USA", aPeriod), "USA", "alternative crude oil", mYear, aPeriod);
    }
    XMLWriteElement(shutdown, "profit-shutdown", aOut, aTabs);*/
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
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
* \author Josh Lurz, James Blackwood
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
	// Note: Technology::completeInit() loops through the outputs.
	//       Therefore, if any of the outputs need the land allocator,
	//       the call to Technology::completeInit() must come afterwards
	Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo,
		aLandAllocator);
    
    // replace the primary output with a generic output (does not add supply to market)
    //delete mOutputs[ 0 ];
    //mOutputs[ 0 ] = new GenericOutput( aSectorName );
    for( auto ghg : mGHG ) {
        delete ghg;
    }
    mGHG.clear();
}

void ResourceReserveTechnology::initCalc(const string& aRegionName,
	const string& aSectorName,
	const IInfo* aSubsectorInfo,
	const Demographic* aDemographics,
	PreviousPeriodInfo& aPrevPeriodInfo,
	const int aPeriod)
{
	Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo,
		aDemographics, aPrevPeriodInfo, aPeriod);
    
    // we have to re-do setProductionState because it will override input coefficients but
    // EnegyInput::initCalc resets it back again, so we call re-do setProductionState to
    // reset it yet again.
    setProductionState( aPeriod );
    
    if( !isOperating( aPeriod ) ) {
        return;
    }

    if( aPeriod > 0 && isOperating( aPeriod -1 ) ) {
        
        for( auto currInput : mInputs ) {
            if( currInput->isSameType( "minicam-non-energy-input" ) ) {
                currInput->setPrice( aRegionName, currInput->getPrice( aRegionName, aPeriod -1 ), aPeriod );
            }
        }
    }
    else {
        mCumulProd[ aPeriod ] = 0.0;
    }
}

/*! \brief Calculates the output of the technology.
* \details Calculates the amount of current ag output based on the amount
*          land and it's yield.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aVariableDemand Subsector demand for output.
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
    if( !mProductionState[ aPeriod ]->isOperating() ) {
        return;
    }
    
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        mTotalReserve = aVariableDemand;
    }
    
    Technology::production( aRegionName, aSectorName, aVariableDemand / mLifetimeYears * aFixedOutputScaleFactor, aFixedOutputScaleFactor, aGDP, aPeriod );
}

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
    //int adjTechYear = mYear - modeltime->gettimestep( modeltime->getyr_to_per( mYear ) ) + 1;
    
    const double BUILDUP_YEARS = 0.0;
    const double DECLINE_PCT = 0.3;
    const double ABANDONMENT_PCT = 0.10;
    
    //double eorCoef = 0.0;
    
    double annualAvgProd = mTotalReserve / mLifetimeYears;
    if( mYear >= currYear || mTotalReserve == 0.0 || ( mYear + mLifetimeYears + ADDITIONAL_PRODUCTION_LIFETIME ) <= currYear ) {
        // variable, retired, or future production
        mProductionPhaseScaler = 1.0;
    }
    else if((currYear - mYear) < BUILDUP_YEARS) {
        mProductionPhaseScaler = (currYear - mYear + 1) / BUILDUP_YEARS;
    }
    else if((mTotalReserve - mCumulProd[ aPeriod - 1]) <= (mTotalReserve * ABANDONMENT_PCT)) {
        mProductionPhaseScaler = max((mTotalReserve - mCumulProd[ aPeriod - 1]) / (mTotalReserve * ABANDONMENT_PCT), 0.0);
        //eorCoef = mEORCoef;
    }
    else if((mTotalReserve - mCumulProd[ aPeriod - 1]) <= (mTotalReserve * DECLINE_PCT)) {
        mProductionPhaseScaler = 1.0;//max((mTotalReserve - mCumulProd[ aPeriod - 1]) / (mTotalReserve * DECLINE_PCT), 0.0);
        //eorCoef = mEORCoef;
    }
    else {
        mProductionPhaseScaler = 1.0;
    }
    
    /*for( auto currInput : mInputs ) {
        if( currInput->getName() == "CO2_For_EOR" ) {
            currInput->setCoefficient( eorCoef, aPeriod );
        }
    }*/


    double initialOutput = annualAvgProd * mProductionPhaseScaler;
    
    mProductionState[ aPeriod ] =
        ProductionStateFactory::create( mYear, mLifetimeYears + ADDITIONAL_PRODUCTION_LIFETIME, mFixedOutput,
                                   initialOutput, aPeriod ).release();
}

double ResourceReserveTechnology::getMarginalRevenue( const string& aRegionName,
                                      const string& aSectorName,
                                      const int aPeriod ) const
{
    //return scenario->getMarketplace()->getPrice( aSectorName, aRegionName, aPeriod ) + mInputs[0]->getPrice( aRegionName, aPeriod );
    return mMarginalRevenue;
}

double ResourceReserveTechnology::getFixedOutput( const string& aRegionName,
                                  const string& aSectorName,
                                  const bool aHasRequiredInput,
                                  const string& aRequiredInput,
                                  const double aMarginalRevenue,
                                  const int aPeriod ) const
{
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        for( auto currInput : mInputs ) {
            if( currInput->isSameType( "minicam-non-energy-input" ) ) {
                currInput->setPrice( aRegionName, aMarginalRevenue, aPeriod );
            }
        }
    }
    mMarginalRevenue = aMarginalRevenue + mInputs[0]->getPrice( aRegionName, aPeriod );
    
    // Construct a marginal profit calculator. This allows the calculation of
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc( this );
    return mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                       aSectorName,
                                                       0, // No variable output.
                                                       &marginalProfitCalc,
                                                       1, // Not shutting down any fixed output using
                                                       // the scale factor.
                                                       mShutdownDeciders,
                                                       aPeriod );
}

/*! \brief Return the total variable input costs which includes energy, taxes, etc.
 * \todo This assumes a leontief production function.
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
    double cost = getTotalInputCost( aRegionName, aSectorName, aPeriod );

    return cost;
}

double ResourceReserveTechnology::getCalibrationOutput( const bool aHasRequiredInput,
                                                 const string& aRequiredInput,
                                                 const int aPeriod ) const
{
    double techCalOutput = Technology::getCalibrationOutput( aHasRequiredInput, aRequiredInput, aPeriod );
    return techCalOutput ;//== -1 ? techCalOutput : techCalOutput / mResourceReserveFactor;
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
    
    const Modeltime* modeltime = scenario->getModeltime();
    double currProd = mOutputs[ 0 ]->getPhysicalOutput( aPeriod );
    int timeStep = modeltime->gettimestep(aPeriod);
    int currYear = modeltime->getper_to_yr( aPeriod );
    if( mYear == currYear ) {
        // assume constant production (consistent if BUILDUP_YEARS == 0)
        mCumulProd[ aPeriod ] = currProd * timeStep;
    }
    else {
        // simplify by using trapizoidal (maybe could do better since we know shape of "fixed"
        // production profile)
        double prevProd = mOutputs[ 0 ]->getPhysicalOutput( aPeriod - 1 );
        double periodCumulProd = prevProd * timeStep + 0.5 * ( currProd - prevProd) * timeStep;
        mCumulProd[ aPeriod ] = aPeriod > 0 ? mCumulProd[ aPeriod - 1 ] + periodCumulProd : 0.0;
    }
    
    /*if( aRegionName == "USA" && mYear == 1975 && aPeriod <= 4 ) {
        mCumulProd[ aPeriod ] -= aPeriod == 4 ? 30 : 100.0;
    }*/
}

void ResourceReserveTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    // Derived visit.
    //aVisitor->startVisitResourceReserveTechnology( this, aPeriod );
    // End the derived class visit.
    //aVisitor->endVisitResourceReserveTechnology( this, aPeriod );
}

void ResourceReserveTechnology::initTechVintageVector() {
    const Modeltime* modeltime = scenario->getModeltime();
    int numPeriodsActive = 0;
    int startPer = modeltime->getyr_to_per( getYear() );
    int currPer = startPer;
    for( int year = getYear(); currPer < modeltime->getmaxper() && year < (getYear() + mLifetimeYears + ADDITIONAL_PRODUCTION_LIFETIME); ) {
        ++numPeriodsActive;
        ++currPer;
        if( currPer < modeltime->getmaxper() ) {
            year = modeltime->getper_to_yr( currPer );
        }
    }
    
    objects::InitializeTechVectorHelper helper( startPer, numPeriodsActive );
    helper.initializeTechVintageVector( this );
}

