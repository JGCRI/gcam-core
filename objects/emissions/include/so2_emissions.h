#ifndef _SO2_EMISSIONS_H_
#define _SO2_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

#include "emissions/include/ghg.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

class SO2Emissions: public Ghg { 
public:
	SO2Emissions();
	~SO2Emissions();
    SO2Emissions* clone() const;
    const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
	bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
protected:
    static const std::string XML_NAME; //!< node name for toXML methods
    double ashRetention; //!< percentage of output that remains in ash form
	double percentSulfur; //!< sulfur content of input (percentage)  
	double gjPerTonne; //!< fuel energy(in GJ) per metric ton
	double emissionsDriver( const double inputIn, const double outputIn ) const;
};

#endif // _SO2_EMISSIONS_H_

