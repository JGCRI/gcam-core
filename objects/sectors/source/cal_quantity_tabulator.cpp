/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responisbility for the use of this software.
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

using namespace std;

/*!
 *\brief Constructor
 */
CalQuantityTabulator::CalQuantityTabulator()
:mCurrentOutput( 0 ),
mTechState( eUnknown ),
mSubsectorState( eUnknown )
{
}

void CalQuantityTabulator::startVisitRegion( const Region* aRegion,
                                             const int aPeriod )
{
    mCurrentRegionName = aRegion->getName();
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

void CalQuantityTabulator::startVisitTechnology( const technology* aTechnology,
                                                 const int aPeriod )
{
    // Check that the subsector state is already set and the technology state is
    // not set yet.
    assert( mSubsectorState != eUnknown && mTechState == eUnknown );

    // If the subsector state is fixed or calibrated set the Technology to have
    // the same state.
    if( mSubsectorState != eVariable ){
        mTechState = mSubsectorState;
    }
    // Check if the technology is calibrated.
    else if ( aTechnology->getCalibrationStatus() ) {
        mTechState = eCalibrated;
        // Add calibration outputs for all outputs.
        mCurrentOutput = aTechnology->getCalibrationOutput( aPeriod );
    }
    else if( aTechnology->outputFixed() ) {
        mTechState = eFixed;
        mCurrentOutput = aTechnology->getFixedOutput();
    }
    else {
        mTechState = eVariable;
        mCurrentOutput = 0;
    }
}

void CalQuantityTabulator::endVisitTechnology( const technology* aTechnology,
                                               const int aPeriod )
{
    assert( mTechState != eUnknown );
    mTechState = eUnknown;
    mCurrentOutput = 0;
}

void CalQuantityTabulator::startVisitOutput( const IOutput* aOutput,
                                             const int aPeriod )
{
    // Make sure there is a stored region name.
    assert( !mCurrentRegionName.empty() );

    // Check that the current technology state is known.
    assert( mTechState != eUnknown );

    // Add calibrated and fixed supplies.
    if( mTechState == eCalibrated ){
        mCalSupplies[ aOutput->getName() ].mCalQuantity
            += aOutput->calcPhysicalOutput( mCurrentOutput,
                                            mCurrentRegionName,
                                            aPeriod );
    }
    else if( mTechState == eFixed ){
        mCalSupplies[ aOutput->getName() ].mFixedQuantity
            += aOutput->calcPhysicalOutput( mCurrentOutput,
                                            mCurrentRegionName,
                                            aPeriod );
    }
    // Set that at least one technology which produces the output good is not
    // all fixed.
    else {
        mCalSupplies[ aOutput->getName() ].mAllFixed = false;
    }
}

const CalQuantityTabulator::CalInfoMap& CalQuantityTabulator::getSupplyInfo() const {
    // Make sure the calibrated supplies map has been setup.
    assert( !mCalSupplies.empty() );
    return mCalSupplies;
}
