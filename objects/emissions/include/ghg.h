#ifndef _GHG_H_
#define _GHG_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg.h
* \ingroup Objects
* \brief The Ghg class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <memory>
#include <string>

// Forward declaration
class Emcoef_ind;
class Tabs;
class GDP;
class GhgMAC;
/*! 
* \ingroup Objects
* \brief The Ghg class describes a single gas with
* attributes of gas name, unit, emissions coefficients,
* and the calculated emissions.
*
* Emissions emitted indirectly through use of technology are also calculated.
* \author Sonny Kim and Marshall Wise
*/

class Ghg
{ 
public:
    Ghg( const std::string& nameIn = "", const std::string& unitIn = "", const double rmfracIn = 0, const double gwpIn = 0, const double emissCoefIn = 0 );
    virtual ~Ghg();
    Ghg( const Ghg& other );
    virtual Ghg& operator=( const Ghg& other );
    void XMLParse( const xercesc::DOMNode* tempnode );
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual Ghg* clone() const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
    double getGHGValue( const std::string& regionName, const std::string& fuelName, const std::string& prodName, const double efficiency, const int period) const;
    virtual void calcEmission( const std::string& regionName, const std::string& fuelname, const double input, const std::string& prodname, const double output, const GDP* gdp, const int period );
    void calcIndirectEmission( const double input, const std::string& fuelname, const std::vector<Emcoef_ind>& emcoef_ind  );
    std::string getName() const;
    std::string getUnit() const;
    double getEmission() const;
    double getSequestAmountGeologic() const;
    double getSequestAmountNonEngy() const;
    double getEmissFuel() const;
    double getEmissInd() const;
    double getEmissCoef() const;
    void setEmissCoef( const double emissCoefIn );
    bool getEmissionsInputStatus() const;
    void setEmissionsInputStatus();
    bool getEmissionsCoefInputStatus() const;
    void setEmissionsCoefInputStatus();

protected:
    std::string name; //!< name of ghg gas
    std::string unit; //!< unit for ghg gas
    std::string storageName; //!< name of ghg gas storage 
    bool isGeologicSequestration; //!< is geologic sequestration, true or false
    bool emissionsWereInput;  //!< toggle to indicate that emissions were input for this object
    bool valueWasInput; //!< Flag to indicate if the emissions were input for the previous period 
    bool fMaxWasInput;//!< Flag indicating whether fMax was input for use in calculating control function
    bool finalEmissCoefWasInput; //!< Flag indicating whether finalEmissCoef was input
    double rmfrac; //!< fraction of carbon removed from fuel
    double storageCost; //!< storage cost associated with the remove fraction
    double gwp; //!< global warming poential
    double emission; //!< emissions (calculated)
    double sequestAmountGeologic; //!< geologic sequestered emissions (calculated)
    double sequestAmountNonEngy; //!< sequestered in non-energy form (calculated)
    double emissGwp; //!< gwp emissions (calculated)
    double emissCoef; //!< emissions coefficient
    double emissCoefPrev; //!< emissions coefficient passed forward from previous period
    double emissFuel; //!< implied emissions from total fuel consumption
    double emissInd; //!< indirect emissions
    double inputEmissions;  //!< input emissions for this object
    double emAdjust; //!< User inputed adjustment to emissions values(0 to 1)
    double fMax; //!<  final control fraction for ghg's
    double gdp0; //!< User inputed variable- represents midpoint of curve for control function
    double tau; //!< User inputed timescale parameter in control function
    double fControl; //!< value derived from control function
    double mac;
    double techCh; //!< technological change parameter- represents percent reduction in gdp0 per year;
    double finalEmissCoef; //!< user input final emissions factor that is approached asymptotically
    double emissDriver; //!< the amount of fuel that governs emissions levels for various GHGs
    std::auto_ptr<GhgMAC> ghgMac; //!< Marginal Abatement Cost Curve Object
    virtual void findControlFunction( const double gdpCap, const double emissDriver, const int period );
    virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
    double controlFunction( const double fMaxIn, const double tauIn, const double gdp0In, const double gdpCapIn );
    double calcTechChange( const int period );
private:
    void copy( const Ghg& other );
    const static std::string XML_NAME; //!< node name for toXML methods

};

#endif // _GHG_H_

