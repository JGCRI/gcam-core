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
* \file tran_technology.cpp
* \ingroup Objects
* \brief transporation technology class source file.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <cmath>
#include "technologies/include/tran_technology.h"
#include "emissions/include/aghg.h"
#include "util/base/include/xml_helper.h"
#include "util/logger/include/ilogger.h"
#include "functions/include/ifunction.h"
#include "containers/include/iinfo.h"
#include "technologies/include/ical_data.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "functions/include/function_utils.h"
#include "util/base/include/ivisitor.h"

using namespace std;

//! Constructor.
TranTechnology::TranTechnology( const string& aName, const int aYear ): Technology( aName, aYear ) {
    mLoadFactor = 1;
}

TranTechnology::TranTechnology() {
    mLoadFactor = 1;
}

TranTechnology* TranTechnology::clone() const {
    TranTechnology* clone = new TranTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void TranTechnology::copy( const TranTechnology& aOther ) {
    Technology::copy( aOther );
    
    mLoadFactor = aOther.mLoadFactor;
}

const std::string& TranTechnology::getXMLName() const {
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
const std::string& TranTechnology::getXMLNameStatic() {
    static const string XML_NAME = "tranTechnology";
    return XML_NAME;
}

void TranTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( mLoadFactor, "loadFactor", out, tabs );
    XMLWriteElement( getOutput( period ) / mLoadFactor, "vehicleOutput", out, tabs );
    XMLWriteElement( getOutput( period ), "serviceOutput", out, tabs );
}   


void TranTechnology::initCalc( const gcamstr& aRegionName,
                               const gcamstr& aSectorName, 
							   const IInfo* aSubsectorInfo,
                               const Demographic* aDemographics,
                               PreviousPeriodInfo& aPrevPeriodInfo,
							   const int aPeriod )   
{
    const ITechnology* prevTech = aPrevPeriodInfo.mPrevVintage;
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo,
                          aDemographics, aPrevPeriodInfo, aPeriod );

    // Check if illegal values have been read in
    if ( mLoadFactor == 0 ) {
        mLoadFactor = 1;
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "LoadFactor was zero in technology: " << mName << ". Reset to 1." << endl;
    }
    
    // we may need to reset the previous output for investment to adjust for the load factor
    // this means calling initCalc on inputs twice but this should be ok
    if(isOperating(aPeriod) && !(aPeriod == 0 || !prevTech || (isVintagingActive() &&
          // cover the case of transition from history with no vintaging
          prevTech->isVintagingActive())))
    {
        double prevOutputForInvestment = prevTech->getOutput(aPeriod -1) / mLoadFactor;
        mTechnologyInfo->setDouble( "prev-output-for-investment", prevOutputForInvestment );
        for(auto input : mInputs) {
            // reset the previous investment by calling initCalc again
            if(input->hasTypeFlag(IInput::CAPITAL)) {
                input->initCalc(aRegionName, aSectorName, mProductionState[ aPeriod ]->isNewInvestment(), false,
                                mTechnologyInfo.get(), aPeriod );
            }
        }
    }
}

double TranTechnology::getTotalInputCost( const gcamstr& aRegionName,
                                          const gcamstr& aSectorName,
                                          const int aPeriod ) const
{
    double inputCosts = Technology::getTotalInputCost(aRegionName, aSectorName, aPeriod);
    // inputCosts is in 1975$/GJ/veh-mi due to the vehicle intensity.
    // finally adjust by the load factor to ensure prices/costs are always in the same units
    return inputCosts / mLoadFactor * 1000.0;
}

/*! \brief This function calculates the sum of the Carbon Values for all GHG's
*          in this Technology.
* \details The function first checks if a carbon tax exists for the Technology,
*          and if it does loops through all GHGs to calculate a sum carbon
*          value. The GHG function which it calls, getGHGValue() calculates the
*          carbon equivalent of all GHG's contained in this Technology. The
*          totalGHGCost attribute of the Technology is then set to this new
*          value. This function is slightly modified from the base version
*          to account for the vehicle intensity unit.
* \author Sonny Kim, Josh Lurz
* \param aRegionName The region containing this Technology.
* \param aSectorName The sector containing this Technology.
* \param aPeriod The period to calculate this value for.
* \return The total emissions and storage cost of all ghgs.
*/
double TranTechnology::getTotalGHGCost( const gcamstr& aRegionName,
                                    const gcamstr& aSectorName,
                                    const int aPeriod ) const
{
    double totalGHGCost = Technology::getTotalGHGCost(aRegionName, aSectorName, aPeriod);
    // totalGHGCost is in 1975$/GJ/veh-mi due to the vehicle intensity.
    // finally adjust by the load factor to ensure prices/costs are always in the same units
    return totalGHGCost / mLoadFactor * 1000.0;
}

double TranTechnology::calcSecondaryValue( const gcamstr& aRegionName,
                                           const int aPeriod ) const
{
    double totalValue = Technology::calcSecondaryValue(aRegionName, aPeriod);
    // totalValue is in 1975$/GJ/veh-mi due to the vehicle intensity.
    // finally adjust by the load factor to ensure prices/costs are always in the same units
    return totalValue / mLoadFactor * 1000.0;
}

double TranTechnology::getEnergyCost( const gcamstr& aRegionName,
                                      const gcamstr& aSectorName,
                                      const int aPeriod ) const
{
    double cost = Technology::getEnergyCost(aRegionName, aSectorName, aPeriod);
    // finally adjust by the load factor to ensure prices/costs are always in the same units
    return cost / mLoadFactor * 1000.0;
}

void TranTechnology::production( const gcamstr& aRegionName, const gcamstr& aSectorName,
                                 double aVariableDemand, double aFixedOutputScaleFactor,
                                 const int aPeriod )
{
    // Can't have a scale factor and positive demand.
    assert( aFixedOutputScaleFactor == 1 || aVariableDemand == 0 );

    // Can't have negative variable demand.
    assert( aVariableDemand >= 0 && util::isValidNumber( aVariableDemand ) );

    // Check for positive variable demand and positive fixed output.
    assert( mFixedOutput == getFixedOutputDefault() || util::isEqual( aVariableDemand, 0.0 ) );

    // Check that a state has been created for the period.
    assert( mProductionState[ aPeriod ] );

    // Early exit optimization to avoid running through the demand function and
    // emissions calculations for non-operating technologies.
    if( !mProductionState[ aPeriod ]->isOperating() ) {
        return;
    }

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc( this );

    // Use the production state to determine output. This ensures the correct action
    // is taken when the technology is retired.
    double primaryOutput =
        mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                     aSectorName,
                                                     aVariableDemand,
                                                     &marginalProfitCalc,
                                                     aFixedOutputScaleFactor,
                                                     mShutdownDeciders,
                                                     aPeriod );

    // Convert from service demand (pass-km) to vehicle demand (vehicle-km)
    double vehicleOutput = primaryOutput / mLoadFactor;
        
    // we need to run input demands seperately such that fuel demands can be
    // set using fuelUsage while non-energy inputs will be using service demand
    for(auto input : mInputs) {
        // standard Leontief assumptions except for capital inputs, drive demands with
        // new instead of total
        double inputDemand = input->getCoefficient(aPeriod) * vehicleOutput;
        input->setPhysicalDemand(inputDemand, aRegionName, aPeriod);
    }

    calcEmissionsAndOutputs( aRegionName, aSectorName, primaryOutput, aPeriod );  
}

double TranTechnology::getCalibrationOutput( const bool aHasRequiredInput,
                                             const string& aRequiredInput,
                                             const int aPeriod ) const
{
    // TODO: Remove function and use the base class when units framework is
    //       available.

    /*! \pre If the caller requests only output for a specific fixed input, the
       *        specific input name must be passed. 
       */
    assert( !aHasRequiredInput || ( !aRequiredInput.empty() && aRequiredInput != "allInputs" ) );

    // Check if this is an existing vintage which cannot have a calibration value.
    if( !mProductionState[ aPeriod ]->isNewInvestment() ){
        return -1;
    }

    // If an input is required and the technology does not have it return early.
    if( aHasRequiredInput && !hasInput( aRequiredInput ) ) {
        return -1;
    }

    // Check if the technology has a calibrated output value.
    if( mCalValue ) {
        return mCalValue->getCalOutput() * mLoadFactor;
    }

    // Conversion from input to output.
    double totalCalOutput = -1;
    for( unsigned int i = 0; i < mInputs.size(); ++i ) {
        // Check if either the caller does not care whether this technology uses a
        // certain input, or it is used.
        if( !aHasRequiredInput || mInputs[ i ]->getName() == aRequiredInput ) {
            // Calibrated output uses the first calibrated coefficient found.
            // All coefficients are checked for consistency, so the input used
            // is arbitrary.
            double calInput = mInputs[ i ]->getCalibrationQuantity( aPeriod );
            if( calInput >= 0 ) {
                // TODO: Remove leontief assumption.
                totalCalOutput = calInput / mInputs[ i ]->getCoefficient( aPeriod )
                                 * mLoadFactor;
                break;
            }
        }
    }
    return totalCalOutput;
}

/*!
* \brief Return good or service price for converting quantity to dollar unit.
* \details This is used to convert quantitites of different goods or services
*          into common currency units. Currently use to convert to 1975 billion dollars
*          which is the native units given the price and quantity units used accross the
*          energy system.  Note, the macro calculations will be responsible for converting
*          to 1990 million dollars which are the units it operates in.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aPeriod Period.
*/
double TranTechnology::getCurrencyConversionPrice( const gcamstr& aRegionName,
                                                   const gcamstr& aSectorName,
                                                   const int aPeriod ) const
{
    // transportation costs will be in 1975$/service and the output units
    // will be million-service
    // given the value derived will need to be consistent with the rest of the
    // energy system we will need to convert million to billion
    return getCost( aPeriod ) / 1000.0;
}


void TranTechnology::acceptDerived( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitTranTechnology( this, aPeriod );
    aVisitor->endVisitTranTechnology( this, aPeriod );
}

void TranTechnology::doInterpolations( const Technology* aPrevTech, const Technology* aNextTech ) {
    Technology::doInterpolations( aPrevTech, aNextTech );
    
    const TranTechnology* prevTranTech = static_cast<const TranTechnology*> ( aPrevTech );
    const TranTechnology* nextTranTech = static_cast<const TranTechnology*> ( aNextTech );
    /*!
     * \pre We are given a valid TranTechnology for the previous tech.
     */
    assert( prevTranTech );
    
    /*!
     * \pre We are given a valid TranTechnology for the next tech.
     */
    assert( nextTranTech );
    
    // Interpolate load factors
    mLoadFactor = util::linearInterpolateY( mYear, prevTranTech->mYear, nextTranTech->mYear,
                                            prevTranTech->mLoadFactor, nextTranTech->mLoadFactor );
}
