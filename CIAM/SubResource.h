#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
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

// xerces xml headers
#include <xercesc/dom/DOM.hpp>

using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief SubResource is a class that contains grades.
* \author Sonny Kim
*/

class SubResource
{
protected:
    
    string name; //!< SubResource name
    int nograde; //!< number of grades of each SubResource
    double priceElas; //!< price elasticity for short-term supply limit
    double minShortTermSLimit; //!< short-term supply limit.
    vector<double> rscprc; //!< subresource price
    vector<double> techChange; //!< technical change
    vector<double> environCost; //!< Environmental costs
    vector<double> severanceTax; //!< Severence Tax (exogenous)
    vector<double> available; //!< total available resource
    vector<double> annualprod; //!< annual production of SubResource
    vector<double> cumulprod; //!< cumulative production of SubResource
    vector<double> gdpExpans; //!< short-term supply limit expansion elasticity w/ gdp
	vector<double> scaleFactor; //!< Knob to control regional resource production. Default == 1.
    vector<double> cumulativeTechChange; //!< Cumulative Technical Change for this sub-sector
    // Cumulative technical change needs to be in sub-resource sector 
    vector<Grade*> grade; //!< amount of SubResource for each grade
    map<string,int> gradeNameMap; //!< Map of grade name to integer position in vector. 
    
public:
    SubResource(); //default constructor (automatically virtual)
    virtual ~SubResource();
    virtual string getType() const = 0; // The = 0 here makes this an abstract method (& class)
    void clear();
    string getName() const; // return SubResource name
    void XMLParse( const DOMNode* tempnode ); // initialize with xml data
    void completeInit();
    //abstract method, this class cannot be used to instantiate objects, can only be derived
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* node ) = 0; 
    virtual void initializeResource(); 
    void toXML( ostream& out ) const;
    virtual void toXMLforDerivedClass( ostream& out ) const;
    void toDebugXML( const int period, ostream& out ) const;
    virtual void cumulsupply(double prc,int per); // calculate cummulative production
    double getPrice(int per);
    double getCumulProd(int per); // return cummulative production
    // calculate annual production
    virtual void annualsupply(int per,double gnp1,double gnp2,double price1,double price2);
    double getAnnualProd(int per); // return annual production
    double getAvailable(int per); // return available resource
    int getMaxGrade(void); // returns total number of grades
    // MiniCAM style output to database table
    void MCoutput( const string& regname, const string& secname); 
    // output to file
    void outputfile(const string &regname, const string& sname); 
    void updateAvailable( const int period );
};


/*! 
* \ingroup CIAM
* \brief A class which defines a SubDepletableResource object, which is a container for multiple grade objects.
* \author Steve Smith
* \date $ Date $
* \version $ Revision $
*/
class SubDepletableResource: public SubResource {
    virtual string getType() const;
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* node );
};

/*! 
* \ingroup CIAM
* \brief A class which defines a SubFixedResource object, which is a container for multiple grade objects.
* \author Steve Smith
* \date $ Date $
* \version $ Revision $
*/
class SubFixedResource: public SubResource {
    virtual string getType() const;
    virtual void XMLDerivedClassParse( const string nodeName, const DOMNode* node );
};

#endif // _SUBRSRC_H_
