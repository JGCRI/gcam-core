#ifndef _RESOURCE_H_
#define _RESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Resource.h
* \ingroup CIAM
* \brief The Resource, DepletableResource, FixedResource, and RenewableResource classes header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
#include <xercesc/dom/DOM.hpp>
#include <vector>
#include <map>

// Forward declaration.
class SubResource;

/*! 
* \ingroup CIAM
* \brief An abstract class which defines a Resource object, which is a container for multiple Subresource objects.
* \author Sonny Kim
*/

class Resource {
    
protected:
    std::string name; //!< Resource name
    std::string market; //!< regional market
    int nosubrsrc; //!< number of subsectors for each Resource
    std::vector<SubResource*> subResource; //!< subsector objects for each Resource
    std::vector<double> rscprc; //!< Resource price
    std::vector<double> available; //!< total Resource available
    std::vector<double> annualprod; //!< annual production rate of Resource
    std::vector<double> cummprod; //!< cummulative production of Resource
    std::map<std::string,int> subResourceNameMap; //!< Map of subResource name to integer position in vector. 
    
public:
    Resource(); // default construtor
    virtual ~Resource();
    virtual const std::string getType() const = 0;
    virtual const std::string getXMLType() const = 0;
    void clear();
    virtual void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    virtual void XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* node ) = 0; // the = 0 makes this an abstract method
    void toXML( std::ostream& out ) const;
    void toOutputXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream &out ) const;
    std::string getName() const; 
    void setMarket( const std::string& regionName );
    double getPrice(int per); 
    void cumulsupply(double prc,int per);
    double getCummProd(int per);
    void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
    double getAnnualProd(int per);
    double getAvailable(int per);
    double getSubAvail( const std::string& subResourceName, const int per);
    int getNoSubrsrc();
    void show();
    void MCoutput( const std::string& regname ); 
    void outputfile( const std::string& regname ); 
};

/*! 
* \ingroup CIAM
* \brief A class which defines a DepletableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class DepletableResource: public Resource {
public: 
    virtual const std::string getType() const;
    virtual const std::string getXMLType() const;
    virtual void XMLDerivedClassParse( const std::string nodename, const xercesc::DOMNode* node );
};

/*! 
* \ingroup CIAM
* \brief A class which defines a FixedResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class FixedResource: public Resource {
public: 
    virtual const std::string getType() const;
    virtual const std::string getXMLType() const;
    virtual void XMLDerivedClassParse( const std::string nodename, const xercesc::DOMNode* node );
};

/*! 
* \ingroup CIAM
* \brief A class which defines a RenewableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz
* \date $ Date $
* \version $ Revision $
*/
class RenewableResource: public Resource {
public: 
    virtual const std::string getType() const;
    virtual const std::string getXMLType() const;
    virtual void XMLDerivedClassParse( const std::string nodename, const xercesc::DOMNode* node );
};

#endif // _RESOURCE_H_

