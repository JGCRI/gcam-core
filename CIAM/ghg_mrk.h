#ifndef _GHG_MRK_H_
#define _GHG_MRK_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ghg_mrk.h
* \ingroup CIAM
* \brief The ghg_mrk class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>

/*! 
* \ingroup CIAM
* \brief Class which defines a market for a single greenhouse gas.
* \author Sonny Kim
*/

class ghg_mrk
{
private:
	std::string name; //!< GHG name
	std::string unit; //!< GHG unit
	std::string market; //!< Name of the market
	std::vector<double> constraint; //!< Emissions constraint by year(tgC or MTC)
	std::vector<double> emission; //!< Emissions by year(tgC or MTC)

public:
	ghg_mrk(); //default construtor
	void clear();
	void XMLParse( const xercesc::DOMNode* node );
   void toXML( std::ostream& out ) const; // write out xml.
   void toDebugXML( const int period, std::ostream& out ) const;
	void setMarket( const std::string& regname ); // creates markets
	std::string getName() const; // show emissions name
	void setEmission( const double amount, const int per ); // set emissions
	double getConstraint( const int per ) const; // return emissions constraint
	double getEmission( const int per ) const; // return emissions
};

#endif // _GHG_MRK_H_

