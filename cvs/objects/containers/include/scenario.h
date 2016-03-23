#ifndef _SCENARIO_H_
#define _SCENARIO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* LEGAL NOTICE
* This computer software was prepared by Battelle Memorial Institute,
* hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
* with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
* CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
* LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
* sentence must appear on any copies of this computer software.
* 
* EXPORT CONTROL
* User agrees that the Software will not be shipped, transferred or
* exported into any country or used in any manner prohibited by the
* United States Export Administration Act or any other applicable
* export laws, restrictions or regulations (collectively the "Export Laws").
* Export of the Software may require some form of license or other
* authority from the U.S. Government, and failure to obtain such
* export control license may result in criminal liability under
* U.S. laws. In addition, if the Software is identified as export controlled
* items under the Export Laws, User represents and warrants that User
* is not a citizen, or otherwise located within, an embargoed nation
* (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
*     and that User is not otherwise prohibited
* under the Export Laws from receiving the Software.
* 
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/



/*! 
* \file scenario.h
* \ingroup Objects
* \brief The Scenario class header file.
* \author Sonny Kim
*/
#include <vector>
#include <map>
#include <memory>
#include <string>
#include <boost/shared_ptr.hpp>

#include "util/base/include/iparsable.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations
class Modeltime;
class Marketplace;
class World;
class Curve;
class Tabs;
class Solver;
class GHGPolicy;
class IClimateModel;
class OutputMetaData;
class SolutionInfoParamParser;

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

class Scenario: public IParsable, public IVisitable, public IRoundTrippable
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
    std::map<std::string, const Curve*> getEmissionsQuantityCurves( const std::string& ghgName ) const;
    std::map<std::string, const Curve*> getEmissionsPriceCurves( const std::string& ghgName ) const;
    void writeOutputFiles() const;
    void dbOutput() const;
    void accept( IVisitor* aVisitor, const int aPeriod ) const;
    const IClimateModel* getClimateModel() const;
    static const std::string& getXMLNameStatic();
    const std::vector<int>& getUnsolvedPeriods() const;
    void invalidatePeriod( const int aPeriod );

    //! Constant which when passed to the run method means to run all model periods.
    const static int RUN_ALL_PERIODS = -1;
protected:
    
    DEFINE_DATA(
        /*! \brief Scenario is the only member of this container hierarchy. */
        DEFINE_SUBCLASS_FAMILY( Scenario ),
                
        /*! \brief The Scenario name. */
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),

        /*! \brief The modeltime for the scenario. */
        CREATE_CONTAINER_VARIABLE( mModeltime, const Modeltime*, NoFilter, "modeltime" ),

        /*! \brief The goods and services marketplace. */
        CREATE_CONTAINER_VARIABLE( mMarketplace, Marketplace*, NoFilter, "marketplace" ),
                
        /*! \brief The goods and services marketplace. */
        CREATE_CONTAINER_VARIABLE( mWorld, World*, NoFilter, "world" ),
                
        /*! \brief A vector booleans, one per period, which denotes whether each period is valid. */
        CREATE_ARRAY_VARIABLE( mIsValidPeriod, std::vector<bool>, "is-valid-period" ),
                
        /*! \brief Unsolved periods. */
        CREATE_ARRAY_VARIABLE( mUnsolvedPeriods, std::vector<int>, "unsolved-periods" ),
                
        /*! \brief A pass through object used to parse SolutionInfo parameters
         *         until markets are created.
         */
        CREATE_CONTAINER_VARIABLE( mSolutionInfoParamParser, SolutionInfoParamParser*, NoFilter, "solution-info-param-parser" )
    )
    
    //! Pointer to solution mechanisms by period.  Note we can't use a period vector
    //! since that would rely on modeltime which is not avaiable at creation.  Also
    //! we are using shared pointers since we are avoiding copying solvers when not
    //! necessary
    std::vector<boost::shared_ptr<Solver> > mSolvers;

    //! A container of meta-data pertinent to outputting data.
    std::auto_ptr<OutputMetaData> mOutputMetaData;

    bool solve( const int period );

    bool calculatePeriod( const int aPeriod,
        std::ostream& aXMLDebugFile,
        std::ostream& aSGMDebugFile,
        Tabs* aTabs,
        const bool aPrintDebugging );

    void printGraphs( const int aPeriod ) const;
    void printLandAllocatorGraph( const int aPeriod, const bool aPrintValues ) const;
    void csvSGMGenFile( std::ostream& aFile ) const;
    void csvSGMOutputFile( std::ostream& aSGMDebugFile, const int aPeriod ) const;

    void logRunBeginning() const;
    void logPeriodBeginning( const int aPeriod ) const;
    void logPeriodEnding( const int aPeriod ) const;
    void logRunEnding() const;

    void writeDebuggingFiles( std::ostream& aXMLDebugFile,
        std::ostream& aSGMDebugFile,
        Tabs* aTabs,
        const int aPeriod ) const;

    void initSolvers();
};

#endif // _SCENARIO_H_

