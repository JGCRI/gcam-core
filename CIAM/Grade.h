#ifndef _GRADE_H_
#define _GRADE_H_
#pragma once

/*! 
* \file Grade.h
* \ingroup CIAM
* \brief The Grade class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>

// xerces xml headers
#include <xercesc/dom/DOM.hpp>
#include <xercesc/util/XMLString.hpp>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief Technologies representing a Grade for each resource.
*
=======
* grade is an object that contains technologies that characterize each grade.
*
* \author Sonny Kim
*/

class Grade
{
private:
	string name; //!< Grade name
	int year; //!< year
	double available; //!< amount of Grade for each Grade
	double extractCost; //!< extraction cost of each Grade
	vector<double> techChangeCumm; //!< cummulative technical change for all grades
	double totalCost; //!< total cost
public:
	Grade();
	Grade( const string nameIn, const int noIn );
	void clear();
	void initElementalMembers();
	void XMLParse( const DOMNode* tempnode );
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void printGrade() const;
	void calcCost( const double tax, const double cumTechChange, const double environCost, const int per );
	double getAvail() const;
   void setAvail( double availableVal );
	double getCost() const;
	double getExtCost() const;
	void setExtCost( const double ecost );
	string getName() const;
};

#endif // _GRADE_H_
