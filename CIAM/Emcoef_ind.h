#ifndef _EMCOEF_IND_H_
#define _EMCOEF_IND_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Emcoef_ind.h
* \ingroup CIAM
* \brief The Emcoef_ind class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <string>

/*! 
* \ingroup CIAM
* \brief This class contains the indirect Greenhouse gas emissions coefficients.
*
* Each object contains a map object of emissions
* coefficients, and each emisscoef_ind object is
* used to represent the secondary energy sector.
* \author Sonny Kim
*/

class Emcoef_ind
{
private:
    std::string name; //!< name of secondary good or sector
    std::map<std::string, double> emcoef; //!< contains all coefficients for all gases
public:
    Emcoef_ind( const std::string sectorName );
    void setemcoef( const std::map<std::string,double>& eminfo, const double toutput );
    const std::string& getName() const;
    double getemcoef( const std::string& gasName ) const;
};

#endif // _EMCOEF_IND_H_

