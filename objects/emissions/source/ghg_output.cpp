/*! 
* \file ghg_output.cpp
* \ingroup CIAM
* \brief Ghg class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include "emissions/include/ghg.h"
#include "emissions/include/ghg_output.h"

using namespace std;

//! Calculate Ghg emissions based on the outptt value. 
void GhgOutput::calcEmission( const string& regionName, const string& fuelname, const double input, const string& prodname, const double output ) {
	
	if ( emissionsWereInput ) {
		emission = inputEmissions;
		emissFuel = inputEmissions;
		if ( output != 0 ) {
			emissCoef = inputEmissions / output;
		} else {
			emissCoef = 0;
		}
	} else {
		emission = output * emissCoef;
		emissFuel =  emission;
	}
	emissGwp = gwp * emission;
}

