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
 * \file pass_through_technology.cpp
 * \ingroup Objects
 * \brief PassThroughTechnology class source file.
 * \author Pralit Patel
 */

// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include "technologies/include/pass_through_technology.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/market_dependency_finder.h"

using namespace std;

extern Scenario* scenario;

PassThroughTechnology::PassThroughTechnology( const string& aName, const int aYear ):
Technology(aName, aYear )
{
}

PassThroughTechnology::PassThroughTechnology() {
}

PassThroughTechnology::~PassThroughTechnology() {
}

PassThroughTechnology* PassThroughTechnology::clone() const {
    PassThroughTechnology* clone = new PassThroughTechnology( mName, mYear );
    clone->copy( *this );
    return clone;
}

void PassThroughTechnology::copy( const PassThroughTechnology& aOther ) {
    Technology::copy( aOther );
    
    mPassThroughSectorName = aOther.mPassThroughSectorName;
    mPassThroughMarketName = aOther.mPassThroughMarketName;
    mPassThroughFixedOutput = aOther.mPassThroughFixedOutput;
}

const string& PassThroughTechnology::getXMLNameStatic() {
    const static string XML_NAME = "pass-through-technology";

    return XML_NAME;
}

const string& PassThroughTechnology::getXMLName() const {
    return getXMLNameStatic();
}

void PassThroughTechnology::completeInit( const string& aRegionName,
                                          const string& aSectorName,
                                          const string& aSubsectorName,
                                          const IInfo* aSubsectorInfo,
                                          ILandAllocator* aLandAllocator )
{
    // The PassThroughTechnology should not have any vintaging.  All vintaging should be
    // in the associated pass-through sector
    if( mLifetimeYears != -1 && mLifetimeYears != calcDefaultLifetime() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Reseting lifetime " << mLifetimeYears << " to a single timestep for "
                << getXMLNameStatic() << " " << mName << " in year " << mYear << "." << endl;
        mLifetimeYears = -1;
    }

    Technology::completeInit( aRegionName, aSectorName, aSubsectorName, aSubsectorInfo, aLandAllocator );

    // Find the name of the pass-through sector so that we can get the fixed output
    for( vector<IInput*>::const_iterator iter = mInputs.begin(); iter != mInputs.end(); ++iter ) {
        if( (*iter)->hasTypeFlag( IInput::ENERGY ) ) {
            /*!
             *\pre There should be only one input to the pass-through sector.
             */
            assert( mPassThroughSectorName.empty() );

            mPassThroughSectorName = (*iter)->getName();
            mPassThroughMarketName = (*iter)->getMarketName( aRegionName );
        }
    }

    /*!
     *\pre There should be only one input to the pass-through sector.
     */
    assert( !mPassThroughSectorName.empty() );

    // Add dependencies for a calc item to gather up the fixed demands from this
    // pass through sector and make that available for the downstream sector
    const string fixedDemandActivityName = mPassThroughSectorName + "-fixed-output";
    MarketDependencyFinder* depFinder = scenario->getMarketplace()->getDependencyFinder();

    // Ensure we gather the fixed demands after we calculate prices / before we
    // set supplies
    depFinder->addDependency( fixedDemandActivityName, mPassThroughMarketName, aSectorName, aRegionName );
}

void PassThroughTechnology::production( const string& aRegionName,
                                        const string& aSectorName,
                                        double aVariableDemand,
                                        double aFixedOutputScaleFactor,
                                        const GDP* aGDP,
                                        const int aPeriod )
{
    // The total output that should be passed on to the pass-through sector is the
    // fixed (potentially scaled) + variable
    double totalDemand = aVariableDemand + mPassThroughFixedOutput * aFixedOutputScaleFactor;
    Technology::production( aRegionName, aSectorName, totalDemand, 1, aGDP, aPeriod );
}

double PassThroughTechnology::getFixedOutput( const string& aRegionName,
                                              const string& aSectorName,
                                              const bool aHasRequiredInput,
                                              const string& aRequiredInput,
                                              const double aMarginalRevenue,
                                              const int aPeriod ) const
{
    // Retrieve the fixed output from the pass-through sector which will store this
    // information in a unsolved trial market.
    const string fixedDemandActivityName = mPassThroughSectorName + "-fixed-output";
    const_cast<PassThroughTechnology*>( this )->mPassThroughFixedOutput = scenario->getMarketplace()->getPrice( fixedDemandActivityName, mPassThroughMarketName, aPeriod );
    return mPassThroughFixedOutput;
}

double PassThroughTechnology::getCalibrationOutput( const bool aHasRequiredInput,
                                                    const string& aRequiredInput, 
                                                    const int aPeriod ) const
{
    double calOutput = Technology::getCalibrationOutput( aHasRequiredInput, aRequiredInput, aPeriod );
    // If there is a calibration value we must adjust it by the fixed output amount to ensure
    // share-weights are only calibrated on the variable amounts.
    if( calOutput != -1 ) {
        // TODO: check the ordering of this to make sure it is retreived before the calibration
        // value is requested
        calOutput -= mPassThroughFixedOutput;
    }
    return calOutput;
}

//! write object to xml output stream
void PassThroughTechnology::toDebugXMLDerived( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteElement( mPassThroughSectorName, "pass-through-name", aOut, aTabs );
    XMLWriteElement( mPassThroughMarketName, "pass-through-market-name", aOut, aTabs );
    XMLWriteElement( mPassThroughFixedOutput, "pass-through-fixed-output", aOut, aTabs );
}

