#ifndef _GHG_POLICY_H_
#define _GHG_POLICY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_policy.h
* \ingroup Objects
* \brief The GHGPolicy class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <string>

class Tabs;

/*! 
* \ingroup Objects
* \brief Class which defines greenhouse has mitigation policy. 
* \author Sonny Kim
*/
class GHGPolicy {
public:
    GHGPolicy( const std::string nameIn = "", const std::string unitIn = "", const std::string marketIn = "", const bool isFixedTaxIn = false );
    std::string getName() const;
    void setMarket( const std::string& regionName );
    void changePolicyToFixedTax( const std::string& regionName );
    void setFixedTaxes( const std::string& regionName, const std::vector<double>& taxes );
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    std::string name; //!< GHG name
    std::string unit; //!< GHG unit
    std::string market; //!< Name of the market
    bool isFixedTax; //!< boolean to use fixed tax or constraint
    std::vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
    std::vector<double> fixedTaxes; //!< Fixed tax on Emissions by year($/TC)
};

#endif // _GHG_POLICY_H_

