#ifndef _FEMALE_H_
#define _FEMALE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file female.h
* \ingroup Objects
* \brief The Female class header file.
* \author Sonny Kim
* \author Katherine Chung
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "demographics/include/gender.h"
class Tabs;
class Gender;

/*! 
* \ingroup Objects
* \brief Derived from Gender, a class which represents Females.
*/

class Female : public Gender {
public:
    Female();
	static const std::string& getXMLNameStatic();
    double calcMaleBirth();
    double calcFemaleBirth();

protected:
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
	virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
    
    double mFertilityRate; //!< fertility rate
    double mMaleBirth; //!< male birth
    double mFemaleBirth; //!< female birth 
    double mMaleBirthFrac; //!< fraction of births that are male

	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _FEMALE_H_

