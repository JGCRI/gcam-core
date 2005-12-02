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
class GHGPolicy;
class IClimateModel;

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

	const std::string& getName() const;
	bool run( const int aSinglePeriod, const bool aPrintDebugging, const std::string& aFilenameEnding = "" );
    void setTax( const GHGPolicy* aTax );
	const std::map<const std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
	const std::map<const std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
    void writeOutputFiles() const;
    void dbOutput() const;
    const IClimateModel* getClimateModel() const;
    static const std::string& getXMLNameStatic();

    //! Constant which when passed to the run method means to run all model periods.
    const static int RUN_ALL_PERIODS = -1;
private:
    //! A vector booleans, one per period, which denotes whether each period is valid.
    std::vector<bool> mIsValidPeriod;

    std::auto_ptr<Modeltime> modeltime; //!< The modeltime for the scenario
    std::auto_ptr<World> world; //!< The world object
    std::auto_ptr<Marketplace> marketplace; //!< The goods and services marketplace.
    std::auto_ptr<Solver> solver; //!< Pointer to a solution mechanism.
	std::string name; //!< Scenario name.
	std::string scenarioSummary; //!< A summary of the purpose of the Scenario.
	std::vector<int> unsolvedPeriods; //!< Unsolved periods. 

	bool solve( const int period );

	bool calculatePeriod( const int aPeriod,
                          std::ostream& aXMLDebugFile,
                          std::ostream& aSGMDebugFile,
                          Tabs* aTabs,
                          const bool aPrintDebugging );

    void printGraphs() const;
	void csvSGMGenFile( std::ostream& aFile ) const;
    void csvSGMOutputFile( std::ostream& aSGMDebugFile, const int aPeriod ) const;
    
    void openDebugXMLFile( std::ofstream& aXMLDebugFile,
                           Tabs* aTabs,
                           const std::string& aFileNameEnding ) const;

    void toDebugXMLOpen( std::ostream& aXMLDebugFile, Tabs* aTabs ) const;
	void toDebugXMLClose( std::ostream& aXMLDebugFile, Tabs* aTabs ) const;

    void logRunBeginning() const;
    void logPeriodBeginning( const int aPeriod ) const;
    void logPeriodEnding( const int aPeriod ) const;
    void logRunEnding() const;

    void openDebuggingFiles( std::ofstream& aXMLDebugFile,
                             std::ofstream& aSGMDebugFile,
                             Tabs* aTabs,
                             const std::string& aFileNameEnding ) const;

    void writeDebuggingFiles( std::ostream& aXMLDebugFile,
                              std::ostream& aSGMDebugFile,
                              Tabs* aTabs,
                              const int aPeriod ) const;

    void closeDebuggingFiles( std::ofstream& aXMLDebugFile,
                              std::ofstream& aSGMDebugFile,
                              Tabs* aTabs ) const;
};

#endif // _SCENARIO_H_

