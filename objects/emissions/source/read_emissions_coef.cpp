/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*!
 * \file read_emissions_coef.cpp
 * \ingroup Objects
 * \brief ReadEmissionsCoef source file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "emissions/include/read_emissions_coef.h"

using namespace std;

//! Constructor that initializes the emissions coefficient.
ReadEmissionsCoef::ReadEmissionsCoef( const double aEmissionsCoef ):
AEmissionsCoef( aEmissionsCoef )
{
}

//! Clone operator.
ReadEmissionsCoef* ReadEmissionsCoef::clone() const {
    return new ReadEmissionsCoef( *this );
}

void ReadEmissionsCoef::initCalc( const IInfo* aSubsectorInfo, const string& aName ){
}

void ReadEmissionsCoef::overrideCoef( const double aEmissionsCoef ){
    mOverridesFuturePeriods = true;
    mEmissionsCoef = aEmissionsCoef;
}

const string& ReadEmissionsCoef::getXMLName() const{
    static const string XML_NAME = "emisscoef";
    return XML_NAME;
}
