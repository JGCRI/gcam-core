#ifndef _SCENARIO_H_
#define _SCENARIO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario.h
* \ingroup CIAM
* \brief The Scenario class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOM.hpp>
#include <iosfwd>

// Forward declarations
class Modeltime;
class Marketplace;
class World;

/*!
* \ingroup CIAM
* \brief A class which defines a model scenario.
* \author Sonny Kim
*/

class Scenario
{
private:
    Modeltime* modeltime; //!< The modeltime for the scenario
    World* world; //!< The world object
    Marketplace* marketplace; //!< The goods and services marketplace.
    std::string name; //!< Scenario name.
    std::string scenarioSummary; //!< A summary of the purpose of the Scenario.
    void printGraphs( const int period ) const;
    void printSectorDependencies() const;
    void clear();
public:
    Scenario();
    ~Scenario();
    const Modeltime* getModeltime() const;
    const Marketplace* getMarketplace() const;
    Marketplace* getMarketplace();
    const World* getWorld() const;
    World* getWorld();
    void XMLParse( const xercesc::DOMNode* node );
    void completeInit();
    void toXML( std::ostream& out ) const;
    void toDebugXMLOpen( const int period, std::ostream& out ) const;
    void toDebugXMLClose( const int period, std::ostream& out ) const;
    std::string getName() const;
    void run();
};

#endif // _SCENARIO_H_

