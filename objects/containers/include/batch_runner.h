#ifndef _BATCH_RUNNER_H_
#define _BATCH_RUNNER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file batch_runner.h
* \ingroup Objects
* \brief The BatchRunner class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <map>
#include <vector>
#include <memory>
#include <xercesc/dom/DOMNode.hpp>
#include "containers/include/scenario_runner.h"

class Timer;

/*! 
* \ingroup Objects
* \brief This class defines a batch scenario runner. The batch runner reads in set of input files
* and runs scenarios based on permutations of these files. More documentation.
* \author Josh Lurz
*/
class BatchRunner: public ScenarioRunner {
public:
    BatchRunner( const std::string& aBatchFileName );
    virtual ~BatchRunner();
    virtual bool setupScenario( Timer& aTimer, const std::string aName = "", const std::list<std::string> aScenComponents = std::list<std::string>() );
    virtual void runScenario( Timer& aTimer );
    virtual void printOutput( Timer& aTimer, const bool aCloseDB ) const;
protected:
    typedef std::map<std::string, std::string> FileSet;
    typedef std::map<std::string, FileSet> ComponentSet;
    typedef std::map<std::string, ComponentSet> ComponentSetStructure;
    std::auto_ptr<ScenarioRunner> mInternalRunner;
    const std::string mBatchFileName; //!< Name of the XML file with batch information.
    ComponentSetStructure mComponentSets; //!< Big data structure.
    bool runSingleScenario( const std::map<std::string, FileSet> aCurrComponents, Timer& aTimer );
    void XMLParse( const xercesc::DOMNode* aRoot );
    void XMLParseComponentSet( const xercesc::DOMNode* aNode );
    void XMLParseFileSet( const xercesc::DOMNode* aNode, ComponentSet& aCurrComponentSet );
    };
#endif // _BATCH_RUNNER_H_
