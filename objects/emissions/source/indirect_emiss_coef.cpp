/*! 
* \file indirect_emiss_coef.cpp
* \ingroup CIAM
* \brief This file contains methods for the indirect Greenhouse gas emissions coefficients(EMcoef_ind) class.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include "emissions/include/indirect_emiss_coef.h"

using namespace std;

/*! \brief Constructor
* \detailed This constructor is the sole available constructor for the Emcoef_ind object.
* It takes the name of the sector it is linked to as an argument.
* This is to prevent a Emcoef_ind from being created without a name.
* \param sectorName The name of the sector the object is linked to.
*/
Emcoef_ind::Emcoef_ind( const string sectorName ) : name( sectorName ){
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
