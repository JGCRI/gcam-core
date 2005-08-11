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
class Input;
/*! 
* \ingroup Objects
* \brief The Ghg class describes a single gas with
* attributes of gas name, unit, emissions coefficients,
* and the calculated emissions.
*
* Note that for non-CO2 GHGs, there are two methods of setting emissions. 
* Through an emissions coefficient or a read-in input emissions for a base year (or years).
* These are mutually exclusive. The last one of these read in determines the method used.
*
* Emissions emitted indirectly through use of technology are also calculated.
* \author Sonny Kim, Marshall Wise, Steve Smith, Nick Fernandez
*/

class Ghg
{ 
public:
    Ghg( const std::string& nameIn = "", const std::string& unitIn = "", const double rmfracIn = 0, const double gwpIn = 1, const double emissCoefIn = 0 );
    virtual ~Ghg();
    Ghg( const Ghg& other );
    virtual Ghg& operator=( const Ghg& other );
    virtual Ghg* clone() const;
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

	virtual const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
    void copyGHGParameters( const Ghg* prevGHG );
    double getGHGValue( const Input* aInput, const std::string& aRegionName, const std::string& aProdName,
                        const int aPeriod ) const;
    virtual void calcEmission( const std::vector<Input*> aInputs, const std::string& aRegionName,
                               const std::string& aGoodName, const double aOutput, const int aPeriod );
    double getGHGValue( const std::string& regionName, const std::string& fuelName, const std::string& prodName,
                        const double efficiency, const int period) const;
    virtual void calcEmission( const std::string& regionName, const std::string& fuelname, const double input,
                               const std::string& prodname, const double output, const GDP* aGDP, const int aPeriod );
    void calcIndirectEmission( const double input, const std::string& fuelname,
                               const std::vector<Emcoef_ind>& emcoef_ind  );
    const std::string& getName() const;
    const std::string& getUnit() const;
    double getEmission( const int aPeriod ) const;

    // These two should be one function!
    double getSequestAmountGeologic() const;
    double getSequestAmountNonEngy() const;
    double getEmissInd() const;
    double getEmissFuel( const int aPeriod ) const;
    double getEmissCoef() const;
    bool getEmissionsCoefInputStatus() const;
    void setEmissionsCoefInputStatus();
    double getCarbonTaxPaid( const std::string& aRegionName, int aPeriod ) const;
    void initCalc();

protected:
    double calcInputEmissions( const std::vector<Input*>& aInputs, const std::string& aRegionName, const int aPeriod ) const;
    std::string name; //!< name of ghg gas
    std::string unit; //!< unit for ghg gas
    std::string storageName; //!< name of ghg gas storage 
    bool isGeologicSequestration; //!< is geologic sequestration, true or false
	bool valueWasInputAtSomePoint; //!< Flag to indicate if the emissions were input in some previous period 

    double rmfrac; //!< fraction of carbon removed from fuel
    double storageCost; //!< storage cost associated with the remove fraction
    double gwp; //!< global warming poential
    std::vector<double> mEmissions; //!< emissions (calculated)
    std::vector<double> mEmissionsByFuel; //!< Emissions by primary fuel.
    double sequestAmountGeologic; //!< geologic sequestered emissions (calculated)
    double sequestAmountNonEngy; //!< sequestered in non-energy form (calculated)
    double emissCoef; //!< emissions coefficient
    double emissInd; //!< indirect emissions
	double maxCntrl; //!<  final control fraction for ghg's
    double gdpcap0; //!< User inputed variable- represents midpoint of curve for control function
    double tau; //!< User inputed timescale parameter in control function
	double gdpCap; //!< Saved value for GDP per capita. Needed to adjust control.
    double techDiff; //!< technological change parameter- represents percent reduction in gdp0 per year;
	double adjMaxCntrl; //!< multiplier to maxCntrl, keeping current emissions constant
	double multMaxCntrl; //!< multiplier to maxCntrl -- changes current emissions
    double inputEmissions;  //!< input emissions for this object
    double emAdjust; //!< User inputed adjustment to emissions values(0 to 1)
    double finalEmissCoef; //!< user input final emissions factor that is approached asymptotically

     std::auto_ptr<GhgMAC> ghgMac; //!< Marginal Abatement Cost Curve Object

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	double adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction, const int period );
	virtual void adjustMaxCntrl(const double GDPcap);

    virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
    double controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn );
    double calcTechChange( const int period );
	
private:
    void copy( const Ghg& other );
    const static std::string XML_NAME; //!< node name for toXML methods

};

#endif // _GHG_H_

