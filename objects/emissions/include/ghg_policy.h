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
#include <xercesc/dom/DOM.hpp>

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
    std::vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
    std::vector<double> emission; //!< Emissions by year(tgC or MTC)

public:
    GHGPolicy();
    void clear();
    void XMLParse( const xercesc::DOMNode* node );
    void toXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    void setMarket( const std::string& regname );
    std::string getName() const;
    void setEmission( const double amount, const int per );
    double getConstraint( const int per ) const;
    double getEmission( const int per ) const;
};

#endif // _GHG_POLICY_H_

