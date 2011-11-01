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
 * \file sector_activity.cpp
 * \ingroup Objects
 * \brief The SectorActivity class source file.
 * \author Pralit Patel
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "containers/include/sector_activity.h"
#include "sectors/include/sector.h"
#include "util/base/include/calibrate_share_weight_visitor.h"
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;

extern Scenario* scenario;

/*!
 * \brief Constructor, takes the sector to wrap and any additional information
 *        necessary to calculate the sector.
 * \param aSector The sector to wrap.
 * \param aGDP The regional gdp needed to calculate the sector.
 * \param aRegionName The name of the region in which this activity is contained.
 */
SectorActivity::SectorActivity( Sector* aSector, const GDP* aGDP, const string& aRegionName ):
mSector( aSector ),
mGDP( aGDP ),
mRegionName( aRegionName ),
mIsStale( true )
{
    // Create a shared pointer such that when both the price and supply activities
    // get deleted this object will also be deleted.
    boost::shared_ptr<SectorActivity> thisPtr( this );
    mPriceActivity = new SectorPriceActivity( thisPtr );
    mDemandActivity = new SectorDemandActivity( thisPtr );
}

/*!
 * \brief Destructor
 */
SectorActivity::~SectorActivity() {
    // Note that this object does not own any memory
}

/*!
 * \brief Calculate intermediate prices and set them into the marketplace.
 * \param aPeriod Model period to calculate.
 */
void SectorActivity::setPrices( const int aPeriod ) {
    const bool calibrationPeriod = aPeriod > 0 && aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod();
    static const bool calibrationActive = Configuration::getInstance()->getBool( "CalibrationActive" );
    if( calibrationActive && calibrationPeriod ) {
        CalibrateShareWeightVisitor calibrator( mRegionName, mGDP );
        mSector->accept( &calibrator, aPeriod );
    }
    mSector->calcFinalSupplyPrice( mGDP, aPeriod );
    // Calculating prices will reset the shares and thus the shares are no longer
    // stale.
    mIsStale = false;
}

/*!
 * \brief Calculate intermediate demand and set them into the marketplace.
 * \param aPeriod Model period to calculate.
 */
void SectorActivity::setDemands( const int aPeriod ) {
    // In case the sector shares where last set during a previous partial derivative
    // calc (stale) they will need to be reset before distributing the supply.
    if( mIsStale ) {
        setPrices( aPeriod );
    }
    mSector->supply( mGDP, aPeriod );
}

/*!
 * \brief Set that sector shares might be stale and require an extra price
 *        calculation.
 */
void SectorActivity::setStale() {
    mIsStale = true;
}

/*!
 * \brief Get a description of this activity which could be used in error
 *        and debug messages.
 * \return A description of this activity.
 */
string SectorActivity::getDescription() const {
    return mRegionName + " " + mSector->getName();
}

/*!
 * \brief Get the activity that will calculate the prices of this sector.
 * \return The associated price activity.
 */
IActivity* SectorActivity::getSectorPriceActivity() const {
    return mPriceActivity;
}

/*!
 * \brief Get the activity that will calculate the demands of this sector.
 * \return The associated demand activity.
 */
IActivity* SectorActivity::getSectorDemandActivity() const {
    return mDemandActivity;
}

/*!
 * \brief Constructor linking back to the sector activity which will do the work.
 * \param aSectorActivity The shared sector activity.
 */
SectorPriceActivity::SectorPriceActivity( boost::shared_ptr<SectorActivity> aSectorActivity )
:mSectorActivity( aSectorActivity )
{
}

/*!
 * \brief Destructor, memory of the sector activity is shared and is managed by
 *        the shared_ptr.
 */
SectorPriceActivity::~SectorPriceActivity() {
}

void SectorPriceActivity::calc( const int aPeriod ) {
    mSectorActivity->setPrices( aPeriod );
}

void SectorPriceActivity::setStale() {
    mSectorActivity->setStale();
}

string SectorPriceActivity::getDescription() const {
    return mSectorActivity->getDescription() + " Price";
}

/*!
 * \brief Constructor linking back to the sector activity which will do the work.
 * \param aSectorActivity The shared sector activity.
 */
SectorDemandActivity::SectorDemandActivity( boost::shared_ptr<SectorActivity> aSectorActivity )
:mSectorActivity( aSectorActivity )
{
}

/*!
 * \brief Destructor, memory of the sector activity is shared and is managed by
 *        the shared_ptr.
 */
SectorDemandActivity::~SectorDemandActivity() {
}

void SectorDemandActivity::calc( const int aPeriod ) {
    mSectorActivity->setDemands( aPeriod );
}

void SectorDemandActivity::setStale() {
    mSectorActivity->setStale();
}

string SectorDemandActivity::getDescription() const {
    return mSectorActivity->getDescription() + " Demand";
}
