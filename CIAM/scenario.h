#ifndef _SCENARIO_H_
#define _SCENARIO_H_
#pragma once

/*! 
* \file scenario.h
* \ingroup CIAM
* \brief The Scenario class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOM.hpp>
#include <string>
#include <iostream>

using namespace std;
using namespace xercesc;

/*!
* \ingroup CIAM
* \brief A class which defines a model scenario.
* \author Sonny Kim
* \date $Date $
* \version $Revision $
*/

class Scenario
{
private:
	string name; //!< Scenario name.
	string scenarioSummary; //!< A summary of the purpose of the Scenario.

public:
	Scenario(); // default construtor
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXMLOpen( const int period, ostream& out ) const;
	void toDebugXMLClose( const int period, ostream& out ) const;
	string getName() const; // return scenario name
	string XMLCreateDate( const time_t& time ) const;
};

#endif // _SCENARIO_H_