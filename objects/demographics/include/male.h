#ifndef _MALE_H_
#define _MALE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file male.h
* \ingroup Objects
* \brief The Male class header file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include "demographics/include/gender.h"
class IVisitor;
/*! 
* \ingroup Objects
* \brief Derived from Gender, a class which represents Males.
*/

class Male : public Gender {
public:
	static const std::string& getXMLNameStatic();
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    static const std::string XML_NAME; //!< node name for toXML methods
    virtual bool XMLDerivedClassParse( const std::string &nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName() const;
};

#endif // _MALE_H_
