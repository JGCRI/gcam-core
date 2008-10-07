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
* \file emissions_summer.cpp
* \ingroup Objects
* \brief The EmissionsSummer class source file.
*
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <cassert>
#include "emissions/include/emissions_summer.h"
#include "emissions/include/aghg.h"
#include "sectors/include/ag_sector.h"
#include "emissions/include/icarbon_calc.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "climate/include/magicc_model.h"

using namespace std;

/*! \brief Constructor
* \param aGHG GHG that is being summed.
*/
EmissionsSummer::EmissionsSummer( const string& aGHGName ):
mGHGName( aGHGName ){
}

/*! \brief Add emissions from a GHG to the stored emissions.
* \param aGHG GHG to update emissions from.
* \param aPeriod Period in which to update.
*/
void EmissionsSummer::startVisitGHG( const AGHG* aGHG, const int aPeriod ){
    if( aGHG->getName() == mGHGName ){
        mEmissionsByPeriod[ aPeriod ] += aGHG->getEmission( aPeriod );
    }
}

/*! \brief Add land use emissions from the land allocator leaves to the stored
*          emissions.
* \param aLandLeaf Land leaf from which to get emissions.
* \param aPeriod Period in which to update.
*/
void EmissionsSummer::startVisitAgSector( const AgSector* aAgSector,
                                          const int aPeriod )
{
    if( mGHGName == "CO2NetLandUse" ){
        mEmissionsByPeriod[ aPeriod ] += aAgSector->getLandUseEmissions( aPeriod );
    }
}

void EmissionsSummer::startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                            const int aPeriod )
{
    // Add land use change emissions.
    if( mGHGName == "CO2NetLandUse" ){
        int year = scenario->getModeltime()->getper_to_yr( aPeriod );
        year = max( static_cast<int>(CarbonModelUtils::getStartYear()), year );
        mEmissionsByPeriod[ aPeriod ] += aCarbonCalc->getNetLandUseChangeEmission( year );
    }
    else if( mGHGName == MagiccModel::getnetDefor80sName() ){
        double netDef80s = 0;
        for( int aYear = 1980; aYear < 1990; ++aYear){
            netDef80s += ( aCarbonCalc->getNetLandUseChangeEmission( aYear ) + 
                           aCarbonCalc->getNetLandUseChangeEmission( aYear + 1 ) ) / 2;
        }
        mEmissionsByPeriod[ aPeriod ] += netDef80s / 10; // Return decadal average
    }
}

/*! \brief Get the current emissions sum.
* \param aPeriod Model period for which to get emissions.
* \return The emissions sum.
*/
double EmissionsSummer::getEmissions( const int aPeriod ) const {
    // The value may not be initialized if there were no GHGs, or no AgLU for
    // net land use change emissions. The default zero will be correct though.

    // The emissions sum may be negative if uptake is occurring.
    return mEmissionsByPeriod[ aPeriod ];
}

/*! \brief Return whether any emissions were set for the period.
* \param aPeriod Model period.
* \return Whether any emissions were set.
*/
double EmissionsSummer::areEmissionsSet( const int aPeriod ) const {
    return mEmissionsByPeriod[ aPeriod ].isInited();
}
