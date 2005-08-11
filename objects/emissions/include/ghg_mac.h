#ifndef _GHG_MAC_H_
#define _GHG_MAC_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_mac.h
* \ingroup objects
* \brief The ghg_mac class header file.
* \author Nick Fernandez
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <memory>
#include "emissions/include/ghg.h"

class Curve;
class Tabs;


/*! 
* \ingroup objects
* \brief This class represents a Marginal Abatement Curve for a GHG.
* \author Nick Fernandez
*/
class GhgMAC {
public:
	GhgMAC();
	~GhgMAC();
    GhgMAC( const GhgMAC& other );
    GhgMAC& operator=( const GhgMAC& other );
    GhgMAC* clone() const;
    const std::string& getXMLName() const;
	static const std::string& getXMLNameStatic();
	void XMLParse( const xercesc::DOMNode* node );
	
	double findReduction( const std::string& region, const int period );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

protected:
	std::string name; //!< name of mac

	double shiftNatGas( const int period, const std::string& regionName, const double carbonPrice);
	double adjustPhaseIn(const int period);
	double adjustTechCh( const int period, const int finalReductionPeriod, const double maxReduction);

	double natGasBasePrice; //!< natural gas price in base year
	double phaseIn; //!< number of periods over which phase in occurs. can be a non-integer
	double shiftRange; //!< the initial range over which carbon price changes due to the standard range of Nat. Gas price changes
	double macCurveOff;//!< turns off the Mac Curves if the value is 1.
	double finalReduction; //!< Increase maximum reduction to this value (due to tech change) 
	int finalReductionYear; //!< Year in which maximum reduction should be implimented
	bool noBelowZero;//!< turns off reductions if carbon Price is less than 0;
	std::string curveShiftFuelName; //!< Name of fuel who's price changes cause a shift in the curve
    
    std::auto_ptr<Curve> macCurve; //!< The underlying Curve

private:
    static const std::string XML_NAME; //!< node name for toXML methods
    void copy( const GhgMAC& other );
};

#endif // _GHG_MAC_H_
