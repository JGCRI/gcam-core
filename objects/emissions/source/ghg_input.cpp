/*! 
* \file ghg_output.cpp
* \ingroup CIAM
* \brief Ghg class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "emissions/include/ghg.h"
#include "emissions/include/ghg_input.h"

using namespace std;

//! Calculate Ghg emissions.
void GhgInput::calcEmission( const string& regionName, const string& fuelname, const double input, const string& prodname, const double output ) {
	
	if ( emissionsWereInput ) {
		emission = inputEmissions;
		emissFuel = inputEmissions;
		if ( input != 0 ) {
			emissCoef = inputEmissions / input;
		} else {
			emissCoef = 0;
		}
	} else {
		emission = input * emissCoef;
		emissFuel =  emission;
	}
	emissGwp = gwp * emission;
}

