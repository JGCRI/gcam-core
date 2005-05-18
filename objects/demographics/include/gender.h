#ifndef _GENDER_H_
#define _GENDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file gender.h
* \ingroup Objects
* \brief The Gender class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <iosfwd>
#include <string>
// forward class declaration
class Tabs;

/*! 
* \ingroup Objects
* \brief The base class for Male and Female objects. Both have populations and survival rates.
*/

class Gender {
public:
	Gender();
	void XMLParse( const xercesc::DOMNode* node );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( std::ostream& out, Tabs* tabs ) const;
	static const std::string& getXMLNameStatic();
	double calcSurvivingPop();
	double getPopulation();
	void setPopulation( double aPopulation );

protected:
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual const std::string& getXMLName() const = 0;
	static const std::string XML_NAME; //!< node name for toXML methods
    double mPopulation; //!< population for this gender
    double mSurvivalRate; //!< survival rate
    double mSurvivingPop; //!< surviving Population, calculated
};

#endif // _GENDER_H_
