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
    std::string storageName; //!< name of ghg gas storage 
    double rmfrac; //!< fraction of carbon removed from fuel
	bool isGeologicSequestration; //!< is geologic sequestration, true or false
    double storageCost; //!< storage cost associated with the remove fraction
    double gwp; //!< global warming poential
    double emission; //!< emissions (calculated)
    double sequestAmountGeologic; //!< geologic sequestered emissions (calculated)
    double sequestAmountNonEngy; //!< sequestered in non-energy form (calculated)
    double emissGwp; //!< gwp emissions (calculated)
    double emissCoef; //!< emissions coefficient
    double emissFuel; //!< implied emissions from total fuel consumption
    double emissInd; //!< indirect emissions
public:
    Ghg( const std::string& nameIn = "", const std::string& unitIn = "", const double rmfracIn = 0, const double gwpIn = 0, const double emissCoefIn = 0 );
    void clear();
    void setCoef( const double emCoef );
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    void setRmfrac( const double trmfrac );
    double getGHGValue( const std::string& regionName, const std::string& fuelName, const std::string& prodName, const double efficiency, const int period) const;
    void calcEmission( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output );
    void calcIndirectEmission( const double input, const std::string& fuelname, const std::vector<Emcoef_ind>& emcoef_ind  );
    std::string getName() const;
    std::string getUnit() const;
    double getEmission() const;
    double getSequestAmountGeologic() const;
    double getSequestAmountNonEngy() const;
    double getEmissFuel() const;
    double getEmissInd() const;
    double getEmissCoef() const;
};

#endif // _GHG_H_

