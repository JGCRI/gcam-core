#ifndef _GRADE_H_
#define _GRADE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file grade.h
* \ingroup Objects
* \brief The Grade class header file.
* \author Sonny Kim
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"
class Tabs;

/*! 
* \ingroup Objects
* \brief Technologies representing a Grade for each resource.
*
* grade is an object that contains technologies that characterize each grade.
*
* \author Sonny Kim
*/

class Grade: public IVisitable
{
	friend class XMLDBOutputter;
public:
    Grade();
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	static const std::string& getXMLNameStatic();
    void calcCost( const double tax, const double cumTechChange, const double environCost, const int per );
    double getAvail() const;
    double getCost( const int per ) const;
    double getExtCost() const;
    const std::string& getName() const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
private:
	const std::string& getXMLName() const;
    void initElementalMembers();
	static const std::string XML_NAME; //!< node name for toXML methods
    std::string name; //!< Grade name
    double available; //!< amount of Grade for each Grade
    double extractCost; //!< extraction cost of each Grade
    std::vector<double> totalCost; //!< total cost

};

#endif // _GRADE_H_
