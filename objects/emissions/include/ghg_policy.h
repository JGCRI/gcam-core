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
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <string>
#include "util/base/include/iround_trippable.h"

/*! 
* \ingroup Objects
* \brief Class which defines greenhouse has mitigation policy. 
* \author Sonny Kim
*/
class GHGPolicy: public IRoundTrippable {
public:
    GHGPolicy();
    GHGPolicy( const std::string aName,
               const std::string aMarket );
    GHGPolicy( const std::string aName,
               const std::string aMarket,
               const std::vector<double>& aFixedTaxes );
    GHGPolicy* clone() const;
    const std::string& getName() const;
    void completeInit( const std::string& aRegionName );
    bool isApplicable( const std::string& aRegion ) const;
    void setConstraint( const std::vector<double>& aConstraint );
    void XMLParse( const xercesc::DOMNode* node );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    static const std::string& getXMLNameStatic();
private:
    static const std::string XML_NAME; //!< node name for toXML methods
    std::string mName; //!< GHG name
    std::string mMarket; //!< Name of the market
    bool isFixedTax; //!< Boolean to use fixed tax or constraint
    std::vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
    std::vector<double> fixedTaxes; //!< Fixed tax on Emissions by year($/TC)
};

#endif // _GHG_POLICY_H_

