#ifndef _SO2_EMISSIONS_H_
#define _SO2_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file so2_emissions.h
* \ingroup Objects
* \brief SO2Emissions class header file.
* \author
* \date $Date$
* \version $Revision$
*/

#include "emissions/include/ghg.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

class SO2Emissions: public Ghg { 
public:
	SO2Emissions();
	~SO2Emissions();
    SO2Emissions* clone() const;
	static const std::string& getXMLNameStatic();
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
	void setAdjMaxCntrl();
	virtual void adjustMaxCntrl(const double GDPcap);
	
    double ashRetention; //!< percentage of output that remains in ash form
	double percentSulfur; //!< sulfur content of input (percentage)  
	double gjPerTonne; //!< fuel energy(in GJ) per metric ton
	double finalSulfur; // Asymtotic final sulfur content (percentage) 
	virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
private:
    static const std::string XML_NAME; //!< XML name of this object.
};

#endif // _SO2_EMISSIONS_H_

