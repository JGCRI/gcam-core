/*! 
* \file Emcoef_ind.cpp
* \ingroup CIAM
* \brief This file contains methods for the indirect Greenhouse gas emissions coefficients(EMcoef_ind) class.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include "Emcoef_ind.h"

using namespace std;

//! Default destructor.
Emcoef_ind::Emcoef_ind() {
}

//! Set the secondary good or sector name
void Emcoef_ind::setName( const string& secname ) {
	name = secname;
}

//! Set emissions coefficient for gas by providing emission and secondary output.
void Emcoef_ind::setemcoef( const map<string,double>& eminfo, const double toutput ) {
	
	typedef map<string,double>::const_iterator CI;
	for (CI fmap=eminfo.begin(); fmap!=eminfo.end(); ++fmap) {	
		if (toutput!=0) emcoef[fmap->first] = fmap->second/toutput;
	}
}

//! Get the name of the secondary good or sector.
const string& Emcoef_ind::getName() const {
	return name;
}

//! Return the emission coefficient for the gas.
double Emcoef_ind::getemcoef( const string& gasName ) const {
	
	map <string, double>::const_iterator findIter = emcoef.find( gasName );
	
	if( findIter != emcoef.end() ){
		return findIter->second;
	}
	else {
		return 0;
	}
}
