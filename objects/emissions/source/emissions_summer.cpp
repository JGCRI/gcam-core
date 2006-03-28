/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Labratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responisbility for the use of this software.
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
#include "emissions/include/ghg.h"
#include "sectors/include/ag_sector.h"
#include "emissions/include/icarbon_calc.h"

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
void EmissionsSummer::startVisitGHG( const Ghg* aGHG, const int aPeriod ){
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
        mEmissionsByPeriod[ aPeriod ] += aCarbonCalc->getNetLandUseChangeEmission( year );
    }
}

/*! \brief Get the current emissions sum.
* \param aPeriod Model period for which to get emissions.
* \return The emissions sum.
*/
double EmissionsSummer::getEmissions( const int aPeriod ) const {
    assert( mEmissionsByPeriod[ aPeriod ].isInited() );
    // The emissions sum may be negative if uptake is occurring.
    return mEmissionsByPeriod[ aPeriod ];
}
