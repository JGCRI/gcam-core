#ifndef _GRADE_H_
#define _GRADE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file Grade.h
* \ingroup CIAM
* \brief The Grade class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <vector>
#include <xercesc/dom/DOM.hpp>

/*! 
* \ingroup CIAM
* \brief Technologies representing a Grade for each resource.
*
* grade is an object that contains technologies that characterize each grade.
*
* \author Sonny Kim
*/

class Grade
{
private:
    std::string name; //!< Grade name
    double available; //!< amount of Grade for each Grade
    double extractCost; //!< extraction cost of each Grade
    std::vector<double> totalCost; //!< total cost
public:
    Grade();
    Grade( const std::string nameIn, const int noIn );
    void clear();
    void initElementalMembers();
    void XMLParse( const xercesc::DOMNode* tempnode );
    void toXML( std::ostream& out ) const;
    void toOutputXML( std::ostream& out ) const;
    void toDebugXML( const int period, std::ostream& out ) const;
    void calcCost( const double tax, const double cumTechChange, const double environCost, const int per );
    double getAvail() const;
    double getCost( const int per ) const;
    double getExtCost() const;
    std::string getName() const;
};

#endif // _GRADE_H_
