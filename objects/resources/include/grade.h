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
class IInfo;

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
    virtual void completeInit( const IInfo* aSubresourceInfo );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();

    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );

    void calcCost( const double tax, const double cumTechChange, const double environCost, const int per );
    double getAvail() const;
    double getCost( const int per ) const;
    double getExtCost() const;
    const std::string& getName() const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    std::string name; //!< Grade name
    std::auto_ptr<IInfo> mGradeInfo; //!< The Grade's information store.
    double available; //!< amount of Grade for each Grade
    double extractCost; //!< extraction cost of each Grade
    std::vector<double> totalCost; //!< total cost
};

#endif // _GRADE_H_
