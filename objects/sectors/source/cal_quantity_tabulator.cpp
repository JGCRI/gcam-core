/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file cal_quantity_tabulator.cpp
 * \ingroup Objects
 * \brief The CalQuantityTabulator class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "sectors/include/cal_quantity_tabulator.h"
#include "technologies/include/technology.h"
#include "containers/include/region.h"
#include "technologies/include/ioutput.h"
#include "resources/include/resource.h"
#include "util/base/include/util.h"
#include "sectors/include/subsector.h"
#include "sectors/include/sector.h"

using namespace std;

/*!
 * \brief Constructor
 * \param Name of the region in which tabulation is occurring. This only needs
 *        to be set if tabulation starts below the regional level.
 */
CalQuantityTabulator::CalQuantityTabulator( const std::string& aRegionName )
: mCurrentRegionName( aRegionName ),
mCurrentOutput( 0 ),
mTechState( eUnknown ),
mSubsectorState( eUnknown ),
mShouldTabulateSector( false )
{
}

void CalQuantityTabulator::startVisitRegion( const Region* aRegion,
                                             const int aPeriod )
{
    mCurrentRegionName = aRegion->getName();
}

void CalQuantityTabulator::startVisitSector( const Sector* aSector,
                                             const int aPeriod )
{
    mCurrentSectorName = aSector->getName();

    // Check if the visitor should tabulate this sector.
    mShouldTabulateSector = mSectorType.empty() ||
                            mSectorType == aSector->mSectorType;
}

void CalQuantityTabulator::endVisitSector( const Sector* aSector,
                                           const int aPeriod )
{
    mCurrentSectorName.clear();
}

void CalQuantityTabulator::startVisitResource( const AResource* aResource,
                                               const int aPeriod )
{
    mCalSupplies[ aResource->getName() ].mAllFixed = false;
}

void CalQuantityTabulator::startVisitSubsector( const Subsector* aSubsector,
                                                const int aPeriod )
{
    // Check that the state is initialized.
    assert( mSubsectorState == eUnknown );
    assert( !mCurrentSectorName.empty() );

    // Check if the subsector has a zero share weight which implies a fixed
    // output of zero.
    if ( util::isEqual( aSubsector->getShareWeight( aPeriod ), 0.0 ) ){
        mSubsectorState = eFixed;
        mCurrentOutput = 0;
    }
    else {
        mSubsectorState = eVariable;
    }
}

void CalQuantityTabulator::endVisitSubsector( const Subsector* aSubsector,
                                             const int aPeriod )
{
    assert( mSubsectorState != eUnknown );
    mSubsectorState = eUnknown;
}

void CalQuantityTabulator::startVisitTechnology( const Technology* aTechnology,
                                                const int aPeriod )
{
    // Check that the subsector state is already set and the technology state is
    // not set yet.
    assert( mSubsectorState != eUnknown && mTechState == eUnknown );

    // If the subsector state is fixed or calibrated set the Technology to have
    // the same state.
    if( mSubsectorState != eVariable ){
        mTechState = mSubsectorState;
        // Even if the subsector is fixed to zero the technology will attempt to
        // produce its fixed output.
        if( mTechState == eFixed ){
            mCurrentOutput = aTechnology->getFixedOutput( mCurrentRegionName,
                                                          mCurrentSectorName,
                                                          aPeriod );
            // Don't allow the current output to be set to the -1 flag. This
            // could occur if the subsector had a zero shareweight and the
            // technology had variable output.
            mCurrentOutput = max( mCurrentOutput, 0.0 );
        }
    }

    // Output is fixed, it could be calibrated or fixed.
    else if( aTechnology->isOutputFixed( aPeriod ) ) {
        // Check if the technology is calibrated.
        double calOutput = aTechnology->getCalibrationOutput( aPeriod );
        if( calOutput != -1 ) {
            mTechState = eCalibrated;
            // Add calibration outputs for all outputs.
            mCurrentOutput = calOutput;
        }
        else {
            mCurrentOutput = aTechnology->getFixedOutput( mCurrentRegionName,
                                                          mCurrentSectorName,
                                                          aPeriod );
            assert( mCurrentOutput != -1 );
            mTechState = eFixed;
        }
    }
    else {
        mTechState = eVariable;
        mCurrentOutput = 0;
    }
}

void CalQuantityTabulator::endVisitTechnology( const Technology* aTechnology,
                                               const int aPeriod )
{
    assert( mTechState != eUnknown );
    mTechState = eUnknown;
    mCurrentOutput = 0;
}

void CalQuantityTabulator::startVisitOutput( const IOutput* aOutput,
                                             const int aPeriod )
{
    if( !mShouldTabulateSector ){
        return;
    }

    // Make sure there is a stored region name.
    assert( !mCurrentRegionName.empty() );

    // Check that the current technology state is known.
    assert( mTechState != eUnknown );

    // Add calibrated and fixed supplies.
    if( mTechState == eCalibrated ){
        mCalSupplies[ aOutput->getName() ].mCalQuantity
            += aOutput->calcPhysicalOutput( mCurrentOutput,
                                            mCurrentRegionName,
                                            0, // Capture component only changes emissions.
                                            aPeriod );
    }
    else if( mTechState == eFixed ){
        mCalSupplies[ aOutput->getName() ].mFixedQuantity
            += aOutput->calcPhysicalOutput( mCurrentOutput,
                                            mCurrentRegionName,
                                            0, // Capture component only changes emissions.
                                            aPeriod );
    }
    // Set that at least one technology which produces the output good is not
    // all fixed.
    else {
        mCalSupplies[ aOutput->getName() ].mAllFixed = false;
    }
}

/*!
 * \brief Set the type of sector for which to tabulate calibrated output.
 * \details Instructs the object to only tabulate demands for sectors with a
 *          given type. If this function is not called the object will tabulate
 *          for all sector types. This function must be called before the
 *          visiting occurs.
 * \param aSectorType The sector type for which to tabulate calibrated output.
 */
void CalQuantityTabulator::setApplicableSectorType( const string& aSectorType ){
    mSectorType = aSectorType;
}

const CalQuantityTabulator::CalInfoMap& CalQuantityTabulator::getSupplyInfo() const {
    // Make sure the calibrated supplies map has been setup.
    assert( !mCalSupplies.empty() );
    return mCalSupplies;
}
