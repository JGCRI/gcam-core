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
*/

#include <xercesc/dom/DOMNode.hpp>
#include <memory>
#include "emissions/include/aghg.h"

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

    static const std::string& getXMLNameStatic();
    void XMLParse( const xercesc::DOMNode* node );
    void initCalc( const std::string& ghgName );
    
    double findReduction( const std::string& region, const int period ) const;
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

protected:
    std::string name; //!< name of mac

    const std::string& getXMLName() const;
    double shiftNatGas( const int period, const std::string& regionName, const double carbonPrice) const;
    double adjustPhaseIn( const int period ) const;
    double adjustTechCh( const int period, const int finalReductionPeriod, const double maxReduction ) const;
    double shiftCostReduction( const int period, const double costReductionRate ) const;
    double getMACValue( const double carbonPrice ) const;

    //! number of periods over which phase in occurs. can be a non-integer
    double phaseIn;
    
    //! the initial range over which carbon price changes due to the standard range of Nat. Gas price changes
    double fuelShiftRange;
    
    //! the annual rate at which carbon price is shifted due to technological change (Direct number. i.e. 1% = 0.01)
    double costReductionRate; 
    
    //! Increase maximum reduction to this value (due to tech change)  (Direct number. i.e. 1% = 0.01)
    double finalReduction; 
    
    //! Base year from which to start decreasing costs
    int baseCostYear; 
    
    //! Year in which maximum reduction should be implemented
    int finalReductionYear; 
    
    //! turns off reductions if carbon Price is less than 0;
    bool noBelowZero;
    
    //! Name of fuel who's price changes cause a shift in the curve
    std::string curveShiftFuelName; 
    
    //! The underlying Curve
    std::auto_ptr<Curve> macCurve; 
    
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    void copy( const GhgMAC& other );
};

#endif // _GHG_MAC_H_
