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

#include "modeltime.h"
#include "world.h"
#include "Marketplace.h"

using namespace std;
using namespace xercesc;

/*!
* \ingroup CIAM
* \brief A class which defines a model scenario.
* \author Sonny Kim
*/

class Scenario
{
private:
	Modeltime modeltime; //!< The modeltime for the scenario
	World world; //!< The world object
	Marketplace marketplace; //!< The goods and services marketplace.
	string name; //!< Scenario name.
	string scenarioSummary; //!< A summary of the purpose of the Scenario.

public:
	Scenario();
	const Modeltime* getModeltime() const;
	const Marketplace* getMarketplace() const;
	Marketplace* getMarketplace();
	const World* getWorld() const;
	World* getWorld();
	void clear();
	void XMLParse( const DOMNode* node );
	void toXML( ostream& out ) const;
	void toDebugXMLOpen( const int period, ostream& out ) const;
	void toDebugXMLClose( const int period, ostream& out ) const;
	string getName() const;
	string static XMLCreateDate( const time_t& time );
	void run();
};

#endif // _SCENARIO_H_