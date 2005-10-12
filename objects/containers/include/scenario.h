#ifndef _SCENARIO_H_
#define _SCENARIO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file scenario.h
* \ingroup Objects
* \brief The Scenario class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <iosfwd>
#include <vector>
#include <map>
#include <memory>
#include <string>
#include "util/base/include/iparsable.h"

// Forward declarations
class Modeltime;
class Marketplace;
class World;
class Curve;
class Tabs;
class Solver;

/*!
* \ingroup Objects
* \brief A class which defines a model scenario.
* \details The Scenario class object is the outermost container for all the
*          data, parameters, and results that defines a complete model run. A
*          scenario object contains the World object (which itself contains
*          regions, and so on) the Marketplace object, the Modeltime object, the
*          Solver object, and the FunctionManager object. The Scenario class
*          contains the highest levels methods for initializing data and running
*          the model, which trigger methods defined at more detailed levels
*          inside container relationships. As such, the scenario object has
*          special importance, and is defined globally (for now), as it is the
*          primary interface between key controlling parts of the model (like
*          the Main program and Solver) and the model details.
* \author Sonny Kim
*/

class Scenario: public IParsable 
{
public:
	Scenario();
	~Scenario();
	const Modeltime* getModeltime() const;
	const Marketplace* getMarketplace() const;
	Marketplace* getMarketplace();
	const World* getWorld() const;
	World* getWorld();
	bool XMLParse( const xercesc::DOMNode* node );
	void completeInit();
	void setName(std::string newName);
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXMLOpen( std::ostream& out, Tabs* tabs ) const;
	void toDebugXMLClose( std::ostream& out, Tabs* tabs ) const;
	std::string getName() const;
	bool run( std::string filenameEnding = "" );
	const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
	const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
    void csvOutputFile() const;
    void dbOutput() const;
	void csvSGMOutputFile( std::ostream& aFile, const int aPeriod );
	void csvSGMGenFile( std::ostream& aFile, const int aPeriod ) const;
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
	bool solve( const int period );
    static void openDebugXMLFile( std::ofstream& xmlDebugStream, const std::string& fileNameEnding );
};

#endif // _SCENARIO_H_

