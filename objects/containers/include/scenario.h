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
#include <memory>

// Forward declarations
class Modeltime;
class Marketplace;
class World;
class Curve;
class Tabs;
class Solver;

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
	void setName(std::string newName);
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXMLOpen( std::ostream& out, Tabs* tabs ) const;
	void toDebugXMLClose( std::ostream& out, Tabs* tabs ) const;
	std::string getName() const;
	void run( std::string filenameEnding = "" );
	const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
	const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
    void csvOutputFile() const;
    void dbOutput() const;
private:
	const static std::string XML_NAME; //!< node name for toXML methods
    std::auto_ptr<Modeltime> modeltime; //!< The modeltime for the scenario
    std::auto_ptr<World> world; //!< The world object
    std::auto_ptr<Marketplace> marketplace; //!< The goods and services marketplace.
    std::auto_ptr<Solver> solver; //!< Pointer to a solution mechanism.
	std::string name; //!< Scenario name.
	std::string scenarioSummary; //!< A summary of the purpose of the Scenario.
	bool runCompleted; //!< A boolean which can be used internally to check if a run has been completed.
	std::vector<int> unsolvedPeriods; //!< Unsolved periods. 
	void printGraphs( const int period ) const;
	void printSectorDependencies() const;
	void clear();
	void solve( const int period );
    static void openDebugXMLFile( std::ofstream& xmlDebugStream, const std::string& fileNameEnding );
};

#endif // _SCENARIO_H_

