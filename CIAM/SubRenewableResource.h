#ifndef _SUBRENEW_H_
#define _SUBRENEW_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file subResource.h
* \ingroup CIAM
* \brief The SubResource class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
#include <xercesc/dom/DOM.hpp>
#include "SubResource.h"

// Forward declarations.
class Grade;
class SubResource;

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
    virtual std::string getType() const; 
    virtual void XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* node );
    virtual void toXMLforDerivedClass( std::ostream& out ) const;
    virtual void toOutputXMLforDerivedClass( std::ostream& out ) const;
    virtual void initializeResource(); 
    virtual void cumulsupply(double prc,int per);
    virtual void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
};
#endif // _SUBRENEW_H_
