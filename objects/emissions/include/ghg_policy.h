#ifndef _GHG_POLICY_H_
#define _GHG_POLICY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_policy.h
* \ingroup CIAM
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
* \ingroup CIAM
* \brief Class which defines greenhouse has mitigation policy. 
* \author Sonny Kim
*/

class GHGPolicy
{
private:
    std::string name; //!< GHG name
    std::string unit; //!< GHG unit
    std::string market; //!< Name of the market
    bool isFixedTax; //!< boolean to use fixed tax or constraint
    std::vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
    std::vector<double> fixedTaxes; //!< Fixed tax on Emissions by year($/TC)
    std::vector<double> emissions; //!< Emissions by year(tgC or MTC)

public:
    GHGPolicy( const std::string nameIn = "", const std::string unitIn = "", const std::string marketIn = "", const bool isFixedTaxIn = false );
    void clear();
    std::string getName() const;
    void setEmission( const double emission, const int period );
    void setMarket( const std::string& regname );
    void addGHGSupply( const std::string& regionName, const int period ) const;
    void changePolicyToFixedTax( const std::string& regionName );
    void setFixedTaxes( const std::string& regionName, const std::vector<double>& taxes );
    void XMLParse( const xercesc::DOMNode* node );
    void toXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
};

#endif // _GHG_POLICY_H_

