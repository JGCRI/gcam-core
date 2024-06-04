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
 * \file ag_storage_technology.cpp
 * \ingroup Objects
 * \brief AgStorageTechnology class source file.
 * \author Ellie Lochner
 */

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include "technologies/include/ag_storage_technology.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"


using namespace std;

extern Scenario* scenario;

AgStorageTechnology::AgStorageTechnology()
{
}

AgStorageTechnology::AgStorageTechnology( const string& aName, const int aYear ):
Technology(aName, aYear)
{
}

AgStorageTechnology::~AgStorageTechnology() {
}

AgStorageTechnology* AgStorageTechnology::clone() const {
    AgStorageTechnology* clone = new AgStorageTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void AgStorageTechnology::copy( const AgStorageTechnology& aOther ) {
    Technology::copy( aOther );
    
    mLogitExponent = aOther.mLogitExponent;
    mLossCoefficient = aOther.mLossCoefficient;
}

const string& AgStorageTechnology::getXMLNameStatic() {
    const static string XML_NAME = "food-storage-technology";

    return XML_NAME;
}

const string& AgStorageTechnology::getXMLName() const {
    return getXMLNameStatic();
}

void AgStorageTechnology::completeInit(const string& aRegionName,
    const string& aSectorName,
    const string& aSubsectorName,
    const IInfo* aSubsectorInfo,
    ILandAllocator* aLandAllocator)
{

    Technology::completeInit(aRegionName, aSectorName, aSubsectorName, aSubsectorInfo, aLandAllocator);

    // AgStorageTechnology should have a lifetime just long enough to last two model periods, double
    // check that now
    const Modeltime* modeltime = scenario->getModeltime();
    if (mYear < modeltime->getEndYear()) {
        const int period = modeltime->getyr_to_per(mYear);
        const int prevYear = period == 0 ?
            (mYear - modeltime->gettimestep(period)) : modeltime->getper_to_yr(period - 1);
        const int nextYear = (period+1) == modeltime->getmaxper() ?
            (mYear + modeltime->gettimestep(period)) : modeltime->getper_to_yr(period+1);
        if(mLifetimeYears != (nextYear - prevYear)) {
            ILogger& mainLog = ILogger::getLogger("main_log");
            mainLog.setLevel(ILogger::SEVERE);
            mainLog << "Lifetime for " << getXMLNameStatic()
                    << " must last exatly 2 periods, instead has lifetime: "
                    << mLifetimeYears << " in " << aRegionName << ", " << aSectorName
                    << ", " << mName << " year " << mYear << endl;
            abort();
        }
    }

}

void AgStorageTechnology::setProductionState(const int aPeriod) {
    // Check that the state for this period has not already been initialized.
    // Note that this is the case when the same scenario is run multiple times
    // for instance when doing the policy cost calculation.  In which case
    // we must delete the memory to avoid a memory leak.
    if (mProductionState[aPeriod]) {
        delete mProductionState[aPeriod];
    }

    double initialOutput = 0;
    const Modeltime* modeltime = scenario->getModeltime();

    // initialOutput is only used when technology is "vintaged" and is ignored when it's a new investment
    if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {
        initialOutput = mOpeningStock; // calibrated opening-stock
    }
    else {
        initialOutput = mStoredValue * mLossCoefficient; //stored from last*loss
    }

    mProductionState[aPeriod] =
        ProductionStateFactory::create(mYear, mLifetimeYears, mFixedOutput,
            initialOutput, aPeriod).release();
}


void AgStorageTechnology::initCalc(const string& aRegionName,
    const string& aSectorName,
    const IInfo* aSubsectorInfo,
    const Demographic* aDemographics,
    PreviousPeriodInfo& aPrevPeriodInfo,
    const int aPeriod) {

    Technology::initCalc(aRegionName, aSectorName, aSubsectorInfo, aDemographics, aPrevPeriodInfo, aPeriod);

    // we are using lagged expectation scheme so we can just look that up now to save time
    if (aPeriod > 0) {
        // the profit expectation should be reduced by the storage cost and losses
        mAdjExpectedPrice = (scenario->getMarketplace()->getPrice(aSectorName, aRegionName, aPeriod - 1) - mStorageCost) * mLossCoefficient;
    }
    else {
        mAdjExpectedPrice = 1;
    }
}

double AgStorageTechnology::getFixedOutput(const string& aRegionName,
    const string& aSectorName,
    const bool aHasRequiredInput,
    const string& aRequiredInput,
    const double aMarginalRevenue,
    const int aPeriod) const
{
    // we do not want to report any fixed output as the stored supply
    // gets added to the supply of the solved Ag commodity, not adjusting
    // the regional demand
    return 0;
}

void AgStorageTechnology::production(const string& aRegionName,
    const string& aSectorName,
    double aVariableDemand,
    double aFixedOutputScaleFactor,
    const int aPeriod)
{
    int outputPosition;
    
    if (mProductionState[aPeriod]->isNewInvestment()) {
        // New vintage mode where we will use a simple logit to determine how much of the
        // "variable demand" will be put into storage vs passed on for consumption.
        // Once we have determined the share we simply set the IO coefficient for the Ag
        // commodity input accordingly.  From there we can rely on the standard methods to
        // calculate and set demands / outputs
        outputPosition = 0;

        const Modeltime* modeltime = scenario->getModeltime();
        mConsumption = aVariableDemand; 
        if (!util::isValidNumber(mConsumption)) {
            mConsumption = 1;
        }
        if (aPeriod <= modeltime->getFinalCalibrationPeriod()) {  
            // During calibration periods we are given observed closing stock (how much to store)
            // and total consumption.  We can use this to calibrate the share-weight

            // some error checking to ensure a zero share-weight if we have zeros for calibration
            // data
            if (mClosingStock == 0) {
                mShareWeight = 0;
            }
            else if (mConsumption == 0) {
                mShareWeight = 0;
                ILogger& mainLog = ILogger::getLogger("main_log");
                mainLog.setLevel(ILogger::WARNING);
                mainLog << "No consumption and positive closing stock in " << aRegionName << " " << aSectorName << endl;
            }
            else {
                // calibrate the share-weight
                mShareWeight = mClosingStock / ((pow(mAdjExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent)) * mConsumption);
            }
         }

        // Calculate the logit share for consumption vs storage using the current Ag commodity price
        // vs the expectation (lower than expected leads to more storage)
        mStoredValue = mShareWeight * (pow(mAdjExpectedPrice / mInputs[0]->getPrice(aRegionName, aPeriod), mLogitExponent))* mConsumption;
        double total = mStoredValue + mConsumption;
        // calculate the IO coefficient as the ratio of total to consumption
        double totalToVariableRatio = total == 0 ? 1.0 : total / mConsumption;
        // and set the IO coefficient into the input object
        mInputs[0]->setCoefficient(totalToVariableRatio, aPeriod);

        mTotal = total;
       
    }
    else if(mProductionState[aPeriod]->isOperating()){
        // in the vintage mode we are simply supplying from previous storage, not demanding anything
        // additional so set the IO coefficient to zero
        outputPosition = 1;
        mInputs[0]->setCoefficient(0, aPeriod);
        
    }

    // Early exit optimization to avoid running through the demand function and
    // emissions calculations for non-operating technologies.
    if (!mProductionState[aPeriod]->isOperating()) {
        return;
    }

    // Use standard methods to calculate outputs and demands

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc(this);

    // Use the production state to determine output.
    double primaryOutput =
        mProductionState[aPeriod]->calcProduction(aRegionName,
            aSectorName,
            aVariableDemand,
            &marginalProfitCalc,
            aFixedOutputScaleFactor,
            mShutdownDeciders,
            aPeriod);

    // Calculate input demand.
    mProductionFunction->calcDemand(mInputs, primaryOutput, aRegionName, aSectorName,
        1, aPeriod, 0, 1);

    // note we purposefully only set output one either the primary output (when new vintage) or secondary
    // output (when old vintaged) 
    mOutputs[outputPosition]->setPhysicalOutput(primaryOutput, aRegionName, mCaptureComponent, aPeriod);
    
}

void AgStorageTechnology::calcCost(const string& aRegionName,
    const string& aSectorName,
    const int aPeriod)
{
    // we didn't want the price signal to consumption to change due to storage
    // so we need to reset it to one here
    double origCoef = mInputs[0]->getCoefficient(aPeriod);
    mInputs[0]->setCoefficient(1.0, aPeriod);
    Technology::calcCost(aRegionName, aSectorName, aPeriod);
    // restore the previous value in case it is needed
    mInputs[0]->setCoefficient(origCoef, aPeriod);
}

//! write object to xml output stream
void AgStorageTechnology::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement(mStoredValue, "stored-value", aOut, aTabs);
    XMLWriteElement(mAdjExpectedPrice, "expected-price", aOut, aTabs);
    XMLWriteElement(mConsumption, "consumption", aOut, aTabs);
    XMLWriteElement(mClosingStock, "closing-stock", aOut, aTabs);
    XMLWriteElement(mTotal, "total-supply", aOut, aTabs);
    XMLWriteElement(mShareWeight, "share-weight", aOut, aTabs);
    XMLWriteElement(mOpeningStock, "opening-stock", aOut, aTabs);
    XMLWriteElement(mStorageCost, "storage-cost", aOut, aTabs);
}

