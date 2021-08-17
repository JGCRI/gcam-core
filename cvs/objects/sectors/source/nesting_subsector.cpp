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
* \file nesting_subsector.cpp
* \ingroup Objects
* \brief NestingSubsector class source file.
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>

#include "util/base/include/configuration.h"
#include "sectors/include/nesting_subsector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/tran_subsector.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/idiscrete_choice.hpp"

using namespace std;
using namespace objects;

extern Scenario* scenario;

NestingSubsector::NestingSubsector():
    Subsector(),
    mNestingDepth( 0 )
{
}

/*! \brief Default destructor.
*
* deletes all subsector objects associated with this subsector.
*
*/
NestingSubsector::~NestingSubsector() {
    for( auto subsector : mSubsectors ) {
        delete subsector;
    }
}

void NestingSubsector::toDebugXMLDerived( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    for( auto subsector : mSubsectors ) {
        subsector->toDebugXML( aPeriod, aOut, aTabs );
    }
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& NestingSubsector::getXMLName() const {
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
const string& NestingSubsector::getXMLNameStatic() {
    static const string XML_NAME = "nesting-subsector";
    return XML_NAME;
}

void NestingSubsector::setNames( const string& aRegionName, const string& aSectorName ) {
    for( auto subsector : mSubsectors ) {
        subsector->setNames( aRegionName, aSectorName );
    }
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
* \param aSectorInfo The parent sector info object.
* \param aLandAllocator Regional land allocator.
* \author Josh Lurz
* \warning markets are not necessarily set when completeInit is called
*/
void NestingSubsector::completeInit( const IInfo* aSectorInfo,
                                     ILandAllocator* aLandAllocator )
{
    if( !mTechContainers.empty() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::SEVERE );
        mainLog << "Technology read into a " << getXMLNameStatic() << " in " << mRegionName << ", "
                << mSectorName << ", " << mName << ", depth " << mNestingDepth << endl;
        abort();
    }

    Subsector::completeInit( aSectorInfo, aLandAllocator );
    for( auto subsector : mSubsectors ) {
        if(subsector->getXMLName() == getXMLNameStatic()) {
            dynamic_cast<NestingSubsector*>(subsector)->mNestingDepth = mNestingDepth + 1;
        }
        subsector->completeInit( aSectorInfo, aLandAllocator );
    }
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calculations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period
*/
void NestingSubsector::initCalc( NationalAccount* aNationalAccount,
                          const Demographic* aDemographics,
                          const int aPeriod )
{
    // note the order of operations matter here as the base class initCalc will
    // call methods which will trigger recursion down the nest, therefore we must
    // call initCalc on the child subsectors first
    for( auto subsector : mSubsectors ) {
        subsector->initCalc( aNationalAccount, aDemographics, aPeriod );
    }
    Subsector::initCalc( aNationalAccount, aDemographics, aPeriod );
}

/*!
 * \brief calculate child subsector shares within this nest 
 *
 * Calls child subsectors to first calculate cost, then their share. Follows this by normalizing shares. 
 *
 * \param aGDP The GDP object in case of fuel preference elasticity is used.
 * \param aPeriod model period
 * \return A vector of subsector shares.
*/
const vector<double> NestingSubsector::calcChildShares( const GDP* aGDP, const int aPeriod ) const {
    // Calculate unnormalized shares.
    vector<double> subsecShares( mSubsectors.size() );
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        subsecShares[ i ] = mSubsectors[ i ]->calcShare( mDiscreteChoiceModel, aGDP, aPeriod );
    }

    // Normalize the shares.  After normalization they will be true shares, not log(shares).
    pair<double, double> shareSum = SectorUtils::normalizeLogShares( subsecShares );
    if( shareSum.first == 0.0 && !allOutputFixed( aPeriod ) ){
        // This should no longer happen, but it's still technically possible.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Shares for nesting-subsector " << mName << " in region " << mRegionName
                << " did not normalize correctly. Sum is " << shareSum.first << " * exp( "
                << shareSum.second << " ) "<< "." << endl;

        // All shares are zero likely due to underflow.  Give 100% share to the
        // minimum cost subsector.
        assert( subsec.size() > 0 );
        int minPriceIndex = 0;
        double minPrice = mSubsectors[ minPriceIndex ]->getPrice( aGDP, aPeriod );
        subsecShares[ 0 ] = 0.0;
        for( int i = 1; i < mSubsectors.size(); ++i ) {
            double currPrice = mSubsectors[ i ]->getPrice( aGDP, aPeriod );
            subsecShares[ i ] = 0.0;                  // zero out all subsector shares ...
            if( currPrice < minPrice ) {
                minPrice = currPrice;
                minPriceIndex = i;
            }
        }
        subsecShares[ minPriceIndex ] = 1.0;        // ... except the lowest price
    }
    /*! \post There is one share per subsector. */
    assert( subsecShares.size() == subsec.size() );
    return subsecShares;
}

/*! \brief Returns the subsector price.
* \details Calculates and returns share-weighted total price (subsectorprice)
*          and cost of fuel (fuelprice). 
* \author Sonny Kim
* \param aGDP Regional GDP object.
* \param aPeriod Model period
*/
double NestingSubsector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    double subsectorPrice = 0.0; // initialize to 0 for summing
    double sharesum = 0.0;
    const vector<double>& techShares = calcChildShares( aGDP, aPeriod );
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ) {
        double currCost = mSubsectors[i]->getPrice( aGDP, aPeriod );
        // calculate weighted average price for Subsector.
        /*!
         * \note Negative prices may be produced and are valid.
         */
        // Subsectors with no share cannot affect price.
        if( techShares[ i ] > util::getSmallNumber() ){
            subsectorPrice += techShares[ i ] * currCost;
            sharesum += techShares[i];
        }
    }

    if( sharesum < util::getSmallNumber() ) {
        // None of the technologies have a valid share.  Set the price
        // to NaN.  This gets tested in calcShare(), and any subsector
        // with a NaN price gets a share of zero.  Therefore, as long
        // as you use only subsectors with positive shares, you will
        // never see the NaN price.
        return numeric_limits<double>::signaling_NaN();
    }
    else {
        return subsectorPrice;
    }
}

/*! \brief returns Subsector fuel price times share
* \details Returns the share-weighted fuel price, which is later summed to get
*          the sector-weighted fuel price.
* \author Sonny Kim
* \param aGDP GDP container.
* \param aPeriod Model period.
* \return share-weighted fuel price
*/
double NestingSubsector::getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const {
    // Determine the average fuel price.
    double fuelPrice = 0;

    // The base period is not solved so the current shares can be calculated and
    // used. In future periods the previous period's shares must be used as the
    // current period's are unknown.
    const int sharePeriod = ( aPeriod == 0 ) ? aPeriod : aPeriod - 1;

    const vector<double>& techShares = calcChildShares( aGDP, sharePeriod );
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i) {
        // calculate weighted average price of fuel only
        // subsector shares are based on total cost
        fuelPrice += techShares[ i ] * mSubsectors[i]->getAverageFuelPrice( aGDP, aPeriod );
    }
    /*! \post Fuel price must be positive. */
    assert( fuelPrice >= 0 );
    return fuelPrice;
}

/*!
* \brief Calculate the cost of the Subsector.
* \details Instructs all technologies to calculate their costs. The subsector
*          can calculate it's costs dynamically once all Technologies have
*          calculated their costs, so the Subsector cost is not stored.
* \param aPeriod Model period.
*/
void NestingSubsector::calcCost( const int aPeriod ) {
    for( auto subsector : mSubsectors ) {
        subsector->calcCost( aPeriod );
    }
}


/*! \brief Return the total fixed Technology output for this subsector.
* \details Fixed output may come from vintaged production or exogenously 
*          specified.
* \author Steve Smith
* \param aPeriod model period
* \param aMarginalRevenue The marginal revenue from the sector which may be necessary
*                         for the technology to calculate it's level of fixed output.
*/
double NestingSubsector::getFixedOutput( const int aPeriod, const double aMarginalRevenue ) const {
    double fixedOutput = 0;
    for( auto subsector : mSubsectors ) {
        fixedOutput += subsector->getFixedOutput( aPeriod, aMarginalRevenue );
    }
    /*! \post Fixed output total must be positive. */
    assert( fixedOutput >= 0 );
    return fixedOutput;
}

/*! \brief The demand passed to this function is shared out at the subsector
*          level.
* \details Variable demand (could be energy or energy service) is passed to
*          child subsectors.
* \author Sonny Kim, Josh Lurz
* \param aSubsectorVariableDemand Total variable demand for this subsector.
* \param aFixedOutputScaleFactor Scale factor to scale down fixed output
*        technologies.
* \param aPeriod Model period
* \param aGDP Regional GDP container.
*/
void NestingSubsector::setOutput( const double aSubsectorVariableDemand, 
                           const double aFixedOutputScaleFactor,
                           const GDP* aGDP,
                           const int aPeriod )

{
    const vector<double>& subsecShares = calcChildShares( aGDP, aPeriod );
    for( size_t i = 0; i < mSubsectors.size(); ++i ) {
        mSubsectors[i]->setOutput( subsecShares[i] * aSubsectorVariableDemand,
                aFixedOutputScaleFactor, aGDP, aPeriod );
    }
}

/*! \brief Test to see if calibration worked for this subsector
* \author Josh Lurz
* \param aPeriod The model period.
* \param aCalAccuracy Accuracy (fraction) to check if calibrations are within.
* \param aPrintWarnings Whether to print a warning.
* \return Whether calibration was successful.
*/
bool NestingSubsector::isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const {
    for( auto subsector : mSubsectors ) {
        if( !subsector->isAllCalibrated( aPeriod, aCalAccuracy, aPrintWarnings ) ) {
            return false;
        }
    }
    return true;
}

/*! \brief returns the total calibrated output from this subsector.
*
* Routine adds up calibrated values from child subsectors. This returns only calibrated
* outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
double NestingSubsector::getTotalCalOutputs( const int period ) const {
    double sumCalValues = 0;

    for( auto subsector : mSubsectors ) {
        sumCalValues += subsector->getTotalCalOutputs( period );
    }
    /*! \post Total calibrated output is greater than or equal to zero. */
    assert( sumCalValues >= 0 );

    return sumCalValues;
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this Subsector or all technologies in this subsector returns true.
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
bool NestingSubsector::allOutputFixed( const int period ) const {
    // If there is no shareweight for this subsector than it cannot produce any
    // output, and so the output must be fixed.
    if( util::isEqual( mShareWeights[ period ].get(), 0.0 ) ){
        return true;
    }
    for( auto subsector : mSubsectors ) {
        if( !subsector->allOutputFixed( period ) ) {
            return false;
        }
    }
    return true;
}

/*!\brief Returns a boolean for whether the subsector contains only fixed output technologies
* or at least one technology that competes on the margin.
*\author Sonny Kim
*\return Boolean for determining whether subsector contains only fixed output technologies.
*/
bool NestingSubsector::containsOnlyFixedOutputTechnologies( const int aPeriod ) const {
    bool allFixed = true;
    for( auto subsector : mSubsectors ) {
        allFixed &= subsector->containsOnlyFixedOutputTechnologies( aPeriod );
    }
    return allFixed;
}

/*! \brief returns Subsector output
*
* output summed every time to ensure consistency
* this is never called for demand sectors!
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector output
*/
double NestingSubsector::getOutput( const int period ) const {
    double output = 0.0;
    for( auto subsector : mSubsectors ) {
        output += subsector->getOutput( period );
    }
    return output;
}

/*!
 * \brief Get the energy input for the Subsector.
 * \param aPeriod Period.
 * \return Total energy input.
 */
double NestingSubsector::getEnergyInput( const int aPeriod ) const {
    double input = 0.0;
    for( auto subsector : mSubsectors ) {
        input += subsector->getEnergyInput( aPeriod );
    }
    return input;
}


/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete. 
* \param aPeriod The period to finalize.
* \author Josh Lurz, Sonny Kim
*/
void NestingSubsector::postCalc( const int aPeriod ) {
    for( auto subsector : mSubsectors ) {
        subsector->postCalc( aPeriod );
    }
}

void NestingSubsector::accept( IVisitor* aVisitor, const int period ) const {
    aVisitor->startVisitNestingSubsector( this, period );
    
    for( auto subsector : mSubsectors ) {
        subsector->accept( aVisitor, period );
    }
            
    aVisitor->endVisitNestingSubsector( this, period );
}
