/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
 * \file indirect_emissions_calculator.cpp
 * \ingroup Objects
 * \brief The IndirectEmissionsCalculator source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "reporting/include/indirect_emissions_calculator.h"
#include "sectors/include/sector.h"
#include "technologies/include/technology.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/util.h"

// Remove when modeltime is no longer required.
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
extern Scenario* scenario;

using namespace std;

/*! 
 * \brief Constructor
 */
IndirectEmissionsCalculator::IndirectEmissionsCalculator():
mCurrTotalEmissions( 0 ),
mCurrIndirectEmissions( 0 ),
mCurrOutput( 0 ){
}

/*!
 * \brief Get the upstream emissions coefficient for a Sector.
 * \details Returns the upstream emissions coefficient for the Sector. The
 *          upstream emissions coefficient includes both direct emissions and
 *          upstream emissions.
 * \param aSector Name of the sector for which to get the upstream emissions
 *        coefficient.
 * \return The upstream emissions coefficient for a sector.
 */
double IndirectEmissionsCalculator::getUpstreamEmissionsCoefficient( const string& aSector,
                                                                     const int aPeriod ) const
{
    DoubleMap::const_iterator upstreamEmissionsCoef = mUpstreamEmissionsCoefficients.find( aSector );
    if( mUpstreamEmissionsCoefficients.end() == upstreamEmissionsCoef
        || !upstreamEmissionsCoef->second[ aPeriod ].isInited() )
    {
        // Upstream emissions coefficients do not exist for resources, renewable
        // fuels and the 'none' fuel.
        return 0;
    }
    return upstreamEmissionsCoef->second[ aPeriod ];
}

/*!
 * \brief Get the total indirect emissions for a Sector.
 * \details Returns the total indirect emissions for the Sector. The indirect
 *          emissions include all upstream emissions, but does not include
 *          direct emissions for the sector.
 * \param aSector Name of the sector for which to get the total indirect
 *        emissions.
 * \return The total indirect emissions.
 * \todo Remove this function once the Access database is removed.
 */
double IndirectEmissionsCalculator::getIndirectEmissions( const string& aSector,
                                                          const int aPeriod ) const
{
    DoubleMap::const_iterator indirectEmissions = mIndirectEmissions.find( aSector );
    if( mIndirectEmissions.end() == indirectEmissions || !indirectEmissions->second[ aPeriod ].isInited() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "No indirect emissions were calculated for sector " << aSector << "." << endl;
        return 0;
    }
    return indirectEmissions->second[ aPeriod ];
}

void IndirectEmissionsCalculator::startVisitSector( const Sector* aSector,
                                                    const int aPeriod )
{
    // Check that aPeriod is not all period mode.
    assert( aPeriod != -1 );

    // Ensure all current variables have been cleared.
    assert( mCurrSectorName.empty() && mCurrTotalEmissions == 0 && mCurrIndirectEmissions == 0
            && mCurrOutput == 0 );

    // Ensure values have not already been calculated.
    assert( mUpstreamEmissionsCoefficients.find( mCurrSectorName ) == mUpstreamEmissionsCoefficients.end() );
    assert( mIndirectEmissions.find( mCurrSectorName ) == mIndirectEmissions.end() );

    // Set the current sector name.
    mCurrSectorName = aSector->getName();
}

void IndirectEmissionsCalculator::endVisitSector( const Sector* aSector, const int aPeriod ){
    // Check that aPeriod is not all period mode.
    assert( aPeriod != -1 );

    // Ensure the sector name has been set.
    assert( !mCurrSectorName.empty() );

    // Calculate the indirect emissions coefficient.
    mUpstreamEmissionsCoefficients[ mCurrSectorName ][ aPeriod ] = mCurrOutput > util::getSmallNumber()
                                                                   ? mCurrTotalEmissions / mCurrOutput : 0;

    // Store the indirect emissions.
    mIndirectEmissions[ mCurrSectorName ][ aPeriod ] = mCurrIndirectEmissions;

    // Clear the state variables.
    mCurrSectorName.clear();
    mCurrTotalEmissions = 0;
    mCurrIndirectEmissions = 0;
    mCurrOutput = 0;
}

void IndirectEmissionsCalculator::startVisitTechnology( const technology* aTechnology,
                                                        const int aPeriod )
{
    // TODO: Fix this for vintaging.
    if( scenario->getModeltime()->getper_to_yr( aPeriod ) != aTechnology->year ){
        return;
    }

    // Ensure the sector name has been set.
    assert( !mCurrSectorName.empty() );

    double upstreamCoef = getUpstreamEmissionsCoefficient( aTechnology->getFuelName(), aPeriod );

    // Calculate indirect and total emissions. Total emissions are required to
    // calculate the indirect emissions coefficient for this sector which will
    // include direct emissions.
    double indirectEmissions = upstreamCoef * aTechnology->getInput();

    double totalEmissions = aTechnology->get_emissmap_second( "CO2" )
                            + indirectEmissions;

    // Add to the regional lists.
    mCurrIndirectEmissions += indirectEmissions;
    mCurrTotalEmissions += totalEmissions;
    mCurrOutput += aTechnology->getOutput( aPeriod );
}
