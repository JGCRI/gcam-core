#ifndef _SUBRENEW_H_
#define _SUBRENEW_H_
#pragma once

/*! 
* \file subResource.h
* \ingroup CIAM
* \brief The SubResource class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

// Forward declaration.
class Grade;
class SubResource;

// xerces xml headers
#include <xercesc/dom/DOM.hpp>

using namespace xercesc;


/*! 
* \ingroup CIAM
* \brief A class which defines a SubRenewableResource object, which is a container for multiple grade objects.
* \author Steve Smith
* \date $ Date $
* \version $ Revision $
*/
class SubRenewableResource: public SubResource {

protected:
   double maxSubResource;
   double baseGDP;
   double gdpSupplyElasticity;
   
public: 
	virtual string getType() const; 
   virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* node );
   virtual void toXMLforDerivedClass( ostream& out ) const;
   virtual void initializeResource(); 
	virtual void cumulsupply(double prc,int per); // calculate cummulative production
	virtual void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
};
#endif // _SUBRENEW_H_
