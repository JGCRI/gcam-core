#ifndef _GHG_H_
#define _GHG_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file ghg.h
* \ingroup Objects
* \brief The Ghg class header file.
* \author Sonny Kim
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <memory>
#include <string>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

// Forward declaration
class Emcoef_ind;
class GDP;
class GhgMAC;
class IInput;
class ICaptureComponent;
class IInfo;
class IOutput;

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

class Ghg: public IVisitable, public IRoundTrippable
{ 
    friend class XMLDBOutputter;
public:
    Ghg();
    virtual ~Ghg();
    Ghg( const Ghg& other );
    virtual Ghg& operator=( const Ghg& other );
    virtual Ghg* clone() const;
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	static const std::string& getXMLNameStatic();

    void copyGHGParameters( const Ghg* prevGHG );
	
	double getGHGValue( const std::string& aRegionName,
                        const std::vector<IInput*>& aInputs,
                        const std::vector<IOutput*>& aOutputs,
                        const ICaptureComponent* aSequestrationDevice,
						const int aPeriod ) const;

	double getGHGValue( const IInput* aInput, const std::string& aRegionName, const std::string& aGoodName,
                        const ICaptureComponent* aSequestrationDevice, const int aPeriod ) const;
    
	void calcEmission( const std::string& aRegionName, 
                       const std::vector<IInput*>& aInputs,
                       const std::vector<IOutput*>& aOutputs,
					   const GDP* aGDP,
					   ICaptureComponent* aSequestrationDevice,
                       const int aPeriod );

    const std::string& getName() const;
    double getEmission( const int aPeriod ) const;
    double getEmissFuel( const int aPeriod ) const;
    double getEmissCoef() const;
    bool getEmissionsCoefInputStatus() const;
    void setEmissionsCoefInputStatus();
    double getCarbonTaxPaid( const std::string& aRegionName, int aPeriod ) const;
    void setName( const std::string& aName );

    virtual void initCalc( const IInfo* aLocalInfo,
                           const int aPeriod );

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    std::string name; //!< name of ghg gas
	bool valueWasInputAtSomePoint; //!< Flag to indicate if the emissions were input in some previous period 
    double gwp; //!< global warming poential
    std::vector<double> mEmissions; //!< emissions (calculated)
    std::vector<double> mEmissionsByFuel; //!< Emissions by primary fuel.
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
	
	virtual const std::string& getXMLName() const;
	double calcInputCO2Emissions( const std::vector<IInput*>& aInputs, const std::string& aRegionName, const int aPeriod ) const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    double adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction, const int period );
    virtual void adjustMaxCntrl(const double GDPcap);

    virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
    double controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn );
    double calcTechChange( const int period );
    
    double calcOutputCoef( const std::vector<IOutput*>& aOutputs,
                           const int aPeriod ) const;
    
    double calcInputCoef( const std::vector<IInput*>& aInputs,
                          const int aPeriod ) const;

    double calcOutputEmissions( const std::vector<IOutput*>& aOutputs,
                                const int aPeriod ) const;
private:
    void copy( const Ghg& other );
    const static std::string XML_NAME; //!< node name for toXML methods
};

#endif // _GHG_H_

