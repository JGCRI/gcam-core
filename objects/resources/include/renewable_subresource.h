#ifndef _RENEWABLE_SUBRESOURCE_H_
#define _RENEWABLE_SUBRESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file renewable_subresource.h
* \ingroup Objects
* \brief The SubRenewableResource class header file.
* \author Sonny Kim
*/
#include <xercesc/dom/DOMNode.hpp>
#include "resources/include/subresource.h"

// Forward declarations.
class Grade;
class SubResource;
class Tabs;
class IInfo;

/*! 
* \ingroup Objects
* \brief A class which defines a SubRenewableResource object, which is a container for multiple grade objects.
* \author Steve Smith
* \date $ Date $
* \version $ Revision $
*/
class SubRenewableResource: public SubResource {

protected:
    double maxSubResource;
	double gdpSupplyElasticity;
	//! subresource variance now read in rather than computed
	double subResourceVariance;
	//! read in average capacity factor for each subresource
	double subResourceCapacityFactor;  
 	virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
	virtual void toXMLforDerivedClass( std::ostream& out, Tabs* tabs ) const;
public: 
	SubRenewableResource();
	virtual void completeInit( const IInfo* aSectorInfo );
	void cumulsupply(double prc,int per);
    void annualsupply( int per, const GDP* gdp, double price1, double price2 );
	double getVariance() const;
	double getAverageCapacityFactor() const;
};
#endif // _RENEWABLE_SUBRESOURCE_H_
