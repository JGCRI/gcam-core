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
* \file sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <fstream>
#include <cassert>
#include <algorithm>
#include <stack>

// xml headers
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/nesting_subsector.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "containers/include/world.h"
#include "util/base/include/util.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/info_factory.h"
#include "util/logger/include/logger.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"
#include "sectors/include/tran_subsector.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/idiscrete_choice.hpp"
#include "containers/include/market_dependency_finder.h"
#include "util/base/include/object_meta_info.h"

using namespace std;

extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector
* sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
Sector::Sector()
    :mObjectMetaInfo()
{
    mDiscreteChoiceModel = 0;
    mUseTrialMarkets = false;
}

/*! \brief Destructor
* \details Deletes all subsector objects associated  with this Sector.
* \author Josh Lurz
*/
Sector::~Sector() {
    clear();
}

//! Clear member variables
void Sector::clear(){
    for( SubsectorIterator subSecIter = mSubsectors.begin(); subSecIter != mSubsectors.end(); subSecIter++ ) {
        delete *subSecIter;
    }
    
    for(auto metaInfo: mObjectMetaInfo) {
        delete metaInfo;
    }
    
    delete mDiscreteChoiceModel;
}

/*! \brief Returns Sector name
*
* \author Sonny Kim
* \return Sector name as a string
*/
const string& Sector::getName() const {
    return mName;
}

void Sector::setNames( const string& aRegionName ) {
    mRegionName = aRegionName;
    for( vector<Subsector*>::iterator subSecIter = mSubsectors.begin(); subSecIter != mSubsectors.end(); subSecIter++ ) {
        (*subSecIter)->setNames( mRegionName, mName );
    }
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
* \param aTabs A tabs object responsible for printing the correct number of tabs.
*/
void Sector::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {

    XMLWriteOpeningTag ( getXMLName(), aOut, aTabs, mName );

    // write the xml for the class members.
    XMLWriteElement( mOutputUnit, "output-unit", aOut, aTabs );
    XMLWriteElement( mInputUnit, "input-unit", aOut, aTabs );
    XMLWriteElement( mPriceUnit, "price-unit", aOut, aTabs );
    XMLWriteElement( mUseTrialMarkets, "use-trial-market", aOut, aTabs );
    mDiscreteChoiceModel->toDebugXML( aPeriod, aOut, aTabs );

    // Write out the data in the vectors for the current period.
    XMLWriteElement( getOutput( aPeriod ), "output", aOut, aTabs );
    XMLWriteElement( getFixedOutput( aPeriod ), "fixed-output", aOut, aTabs );
    XMLWriteElement( outputsAllFixed( aPeriod ), "outputs-all-fixed", aOut, aTabs );
    XMLWriteElement( getCalOutput( aPeriod ), "cal-output", aOut, aTabs );


    for(auto metaInfo : mObjectMetaInfo) {
        metaInfo->toDebugXML( aPeriod, aOut, aTabs );
    }

    toDebugXMLDerived (aPeriod, aOut, aTabs);

    // write out the subsector objects.
    for( CSubsectorIterator j = mSubsectors.begin(); j != mSubsectors.end(); j++ ){
        ( *j )->toDebugXML( aPeriod, aOut, aTabs );
    }

    // finished writing xml for the class members.

    XMLWriteClosingTag( getXMLName(), aOut, aTabs );
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz
* \param aRegionInfo Regional information object.
* \param aLandAllocator Regional land allocator.
* \warning markets are not necessarily set when completeInit is called
*/
void Sector::completeInit( const IInfo* aRegionInfo, ILandAllocator* aLandAllocator )
{
    if( !mDiscreteChoiceModel ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "No Discrete Choice function set in " << mRegionName << ", " << mName << endl;
        abort();
    }

    // Allocate the sector info.
    // Do not reset if mSectorInfo contains information from derived sector classes.
    // This assumes that info from derived sector contains region info (parent).
    if( !mSectorInfo.get() ){
        mSectorInfo.reset( InfoFactory::constructInfo( aRegionInfo, mRegionName + "-" + mName ) );
    }

    // Set output and price unit of sector into sector info.
    mSectorInfo->setString( "output-unit", mOutputUnit );
    mSectorInfo->setString( "input-unit", mInputUnit );
    mSectorInfo->setString( "price-unit", mPriceUnit );

    for(auto metaInfo : mObjectMetaInfo) {
        mSectorInfo->setDouble( metaInfo->getName(), metaInfo->getValue() );
    }

    // Complete the subsector initializations.
    for( vector<Subsector*>::iterator subSecIter = mSubsectors.begin(); subSecIter != mSubsectors.end(); subSecIter++ ) {
        ( *subSecIter )->completeInit( mSectorInfo.get(), aLandAllocator );
    }

    if( mUseTrialMarkets ) {
        // Adding a self dependency will force the MarketDependencyFinder to create
        // solved trial price/demand markets for this sector.
        MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();
        depFinder->addDependency( mName, mRegionName, mName, mRegionName );
    }
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calculations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author Steve Smith
* \param aPeriod Model period
*/
void Sector::initCalc( NationalAccount* aNationalAccount,
                      const Demographic* aDemographics,
                      const int aPeriod )
{
    mDiscreteChoiceModel->initCalc( mRegionName, mName, false, aPeriod );
    
    // do any sub-Sector initializations
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        mSubsectors[ i ]->initCalc( aNationalAccount, aDemographics, aPeriod );
    }
}

/*! \brief Test to see if calibration worked for this sector
*
* Compares the sum of calibrated + fixed values to output of sector.
* Will optionally print warning to the screen (and eventually log file).
*
* If all outputs are not calibrated then this does not check for consistency.
*
* \author Steve Smith
* \param period Model period
* \param calAccuracy Accuracy (fraction) to check if calibrations are within.
* \param printWarnings if true prints a warning
* \return Boolean true if calibration is ok.
*/
bool Sector::isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const {
    bool isAllCalibrated = true;
    // Check if each subsector is calibrated.
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        isAllCalibrated &= mSubsectors[ i ]->isAllCalibrated( period, calAccuracy, printWarnings );
    }
    return isAllCalibrated;
}

/*!
 * \brief Calculate technology costs for the Sector.
 * \param aPeriod Period.
 * \todo Move to supply sector and make private once demand and supply sectors
 *       are separate.
 */
void Sector::calcCosts( const int aPeriod ){
    // Instruct all subsectors to calculate their costs. This must be done
    // before prices can be calculated.
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        mSubsectors[ i ]->calcCost( aPeriod );
    }
}

/*! \brief Calculate the shares for the subsectors.
* \details This routine calls subsector::calcShare for each subsector, which
*          calculated an unnormalized share, and then calls normShare to
*          normalize the shares for each subsector. Fixed subsectors are ignored
*          here as they do not have a share of the new investment.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return A vector of normalized shares, one per subsector, ordered by subsector.
*/
const vector<double> Sector::calcSubsectorShares( const GDP* aGDP, const int aPeriod ) const {
    // Calculate unnormalized shares.
    vector<double> subsecShares( mSubsectors.size() );
    for( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        subsecShares[ i ] = mSubsectors[ i ]->calcShare( mDiscreteChoiceModel, aGDP, aPeriod );
    }

    // Normalize the shares.  After normalization they will be true shares, not log(shares).
    pair<double, double> shareSum = SectorUtils::normalizeLogShares( subsecShares );
    if( shareSum.first == 0.0 && !outputsAllFixed( aPeriod ) ){
        // This should no longer happen, but it's still technically possible.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        mainLog << "Shares for sector " << mName << " in region " << mRegionName
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

/*! \brief Calculate and return weighted average price of subsectors.
* \param period Model period
* \return The weighted sector price.
* \author Sonny Kim, Josh Lurz, James Blackwood
* \param period Model period
* \return Weighted sector price.
*/
double Sector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    const vector<double>& subsecShares = calcSubsectorShares( aGDP, aPeriod );
    double sectorPrice = 0;
    double sumSubsecShares = 0;
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        // Subsectors with no share cannot affect price. The getPrice function
        // is constant so skipping it will not have any side effects.
        if( subsecShares[ i ] > util::getSmallNumber() ){
            sumSubsecShares += subsecShares[ i ];
            double currPrice = mSubsectors[ i ]->getPrice( aGDP, aPeriod );
            sectorPrice += subsecShares[ i ] * currPrice;
        }
    }
    
    return sectorPrice;
}

/*! \brief Returns true if all sub-Sector outputs are fixed or calibrated.
*
* Routine loops through all the subsectors in the current Sector. If output is
* calibrated, assigned a fixed output, or set to zero (because share weight is
* zero) then true is returned. If all ouptput is not fixed, then the Sector has
* at least some capacity to respond to a change in prices.
*
* \author Steve Smith
* \param period Model period
* \return Boolean that is true if entire Sector is calibrated or has fixed
*         output
*/
bool Sector::outputsAllFixed( const int period ) const {
    assert( period >= 0 );
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        if ( !( mSubsectors[ i ]->allOutputFixed( period ) ) ) {
            return false;
        }
    }
    return true;
}

/*!
 * \brief Calculate the total amount of fixed output in the Sector.
 * \details Fixed output is defined as infra-marginal output. This means that
 *          the production of this output is below the margin, and so does not
 *          affect the marginal cost of producing the Sector's output. Fixed
 *          output may be the output of vintages, or may be fixed investment
 *          that is determined by exogenous, non-cost based, factors. Fixed
 *          output should never be used to specify an investment or output
 *          pathway for a good that should be competitively determined. For
 *          example, investment in hydro-electricity is input as fixed output,
 *          because the investment is determined not be marginal cost but by
 *          government decisions. Since fixed output is infra-marginal, it is
 *          not included in the cost calculation. The total fixed output is
 *          removed from the desired output of the sector before the output is
 *          distributed to variable output technologies(which are on the
 *          margin).
 * \note Currently the price of a Sector with all fixed output is undefined. If
 *       the model encounters the condition, it will set variable output to zero
 *       and scale fixed output to equal total output. The price will be set as
 *       the previous period's price. This is not generally an issue in
 *       equilibrium, as the socio-economic scenarios used have increasing
 *       output, and depreciation of capital causes new capital to be required.
 *       If this became an issue, there are two potential solutions: A fraction
 *       of marginal output could be forced into the Sector, which would assist
 *       the model to solve but not be economically consistent. The economically
 *       consistent solution would be to back down the supply schedule and
 *       shutdown the marginal fixed output producer.
 *
 * \author Steve Smith, Josh Lurz
 * \param aPeriod Model period
 * \return Total fixed output.
 */
double Sector::getFixedOutput( const int aPeriod ) const {
    const double sectorPrice = scenario->getMarketplace()->getPrice( mName, mRegionName, aPeriod );
    double totalfixedOutput = 0;
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        totalfixedOutput += mSubsectors[ i ]->getFixedOutput( aPeriod, sectorPrice );
    }
    return totalfixedOutput;
}

/*! \brief Return subsector total calibrated outputs.
*
* Returns the total calibrated outputs from all subsectors and technologies.
* Note that any calibrated input values are converted to outputs and are included.
*
* This returns only calibrated outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return total calibrated outputs
*/
double Sector::getCalOutput( const int period  ) const {
    double totalCalOutput = 0;
    for ( unsigned int i = 0; i < mSubsectors.size(); ++i ){
        totalCalOutput += mSubsectors[ i ]->getTotalCalOutputs( period );
    }
    return totalCalOutput;
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete.
* \param aPeriod The period to finalize.
* \todo Finish this function.
* \author Josh Lurz, Sonny Kim
*/
void Sector::postCalc( const int aPeriod ){
    // Finalize sectors.
    for( SubsectorIterator subsector = mSubsectors.begin(); subsector != mSubsectors.end(); ++subsector ){
        (*subsector)->postCalc( aPeriod );
    }
    // Set member price vector to solved market prices
    if( aPeriod > 0 ){
        mPrice[ aPeriod ] = scenario->getMarketplace()->getPrice( mName, mRegionName, aPeriod, true );
    }
}

void Sector::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSector( this, aPeriod );
    for( unsigned int i = 0; i < mSubsectors.size(); i++ ) {
        mSubsectors[ i ]->accept( aVisitor, aPeriod );
    }
    
    aVisitor->endVisitSector( this, aPeriod );
}
