#ifndef _GHG_H_
#define _GHG_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg.h
* \ingroup CIAM
* \brief The Ghg class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOM.hpp>

// Forward declaration
class Emcoef_ind;

/*! 
* \ingroup CIAM
* \brief The Ghg class describes a single gas with
* attributes of gas name, unit, emissions coefficients,
* and the calculated emissions.
*
* Emissions emitted indirectly through use of technology are also calculated.
* \author Sonny Kim and Marshall Wise
*/

class Ghg
{
private:
    std::string name; //!< name of ghg gas
    std::string unit; //!< unit for ghg gas
    double rmfrac; //!< fraction of emissions removed
    double storageCost; //!< storage cost associated with the remove fraction
    double gwp; //!< global warming poential
    double emission; //!< emissions (calculated)
    double sequesteredAmount; //!< sequestered emissions (calculated)
    double emiss_gwp; //!< gwp emissions (calculated)
    double emiss_coef; //!< emissions coefficient
    double emiss_fuel; //!< implied emissions from total fuel consumption
    double emiss_ind; //!< indirect emissions
public:
    Ghg( const std::string& nameIn = "", const std::string& unitIn = "", const double rmfracIn = 0, const double gwpIn = 0, const double emissCoefIn = 0 );
    void clear();
    void setcoef( const double em_coef );
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    void setrmfrac( const double trmfrac );
    double taxcnvrt( const std::string& regionName, const std::string& fuelName ) const;
    double getGHGValue( const std::string& regionName, const std::string& fuelName, const int period) const;
    void calc_emiss( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output );
    void calc_emiss_ind( const double input, const std::string& fuelname, const std::vector<Emcoef_ind>& emcoef_ind  );
    std::string getname() const;
    std::string getunit() const;
    double getemission() const;
    double getSequesteredAmount() const;
    double getemiss_fuel() const;
    double getemiss_ind() const;
    double getemiss_coef() const;
};

#endif // _GHG_H_

