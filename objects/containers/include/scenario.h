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

#include <xercesc/dom/DOMNode.hpp>
#include <iosfwd>
#include <vector>
#include <string>
#include <map>

// Forward declarations
class Modeltime;
class Marketplace;
class World;
class Curve;
class Tabs;

/*!
* \ingroup CIAM
* \brief A class which defines a model scenario.
* \author Sonny Kim
*/

class Scenario
{
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
    void toXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLOpen( const int period, std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLClose( const int period, std::ostream& out, Tabs* tabs ) const;
    std::string getName() const;
    void run( std::string filenameEnding = "" );
    const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
    const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
private:
    Modeltime* modeltime; //!< The modeltime for the scenario
    World* world; //!< The world object
    Marketplace* marketplace; //!< The goods and services marketplace.
    std::string name; //!< Scenario name.
    std::string scenarioSummary; //!< A summary of the purpose of the Scenario.
    bool runCompleted; //!< A boolean which can be used internally to check if a run has been completed.
    void printGraphs( const int period ) const;
    void printSectorDependencies() const;
    void clear();
};

#endif // _SCENARIO_H_

