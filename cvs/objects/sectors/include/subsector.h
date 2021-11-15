#ifndef _SUBSECTOR_H_
#define _SUBSECTOR_H_
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
* \file subsector.h
* \ingroup Objects
* \brief The Subsector class header file.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include <map>
#include <list>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations
class ITechnologyContainer;
class GDP;
class IInfo;
class NationalAccount;
class Demographic;
class Tabs;
class ILandAllocator;
class Demographics;
class InterpolationRule;
class IDiscreteChoice;

// Need to forward declare the subclasses as well.
class TranSubsector;
class AgSupplySubsector;
class SubsectorAddTechCosts;
class NestingSubsector;

/*! 
* \ingroup Objects
* \brief A class which defines a single Subsector of the model.
* \details The subsector contains a group of technology objects, which produce
*          and consume commodities in the marketplace. Each Subsector has
*          attributes such as share, share weight and a logit exponential.
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Subsector: public INamed,
                 public AParsable,
                 private boost::noncopyable
{
    friend class XMLDBOutputter;
     // needs to be friend so that it can set a new share weight directly into
    // shrwts, note that if there was a setShare weight method this would not
    // be necessary
    friend class CalibrateShareWeightVisitor;
    friend class NestingSubsector;
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Subsector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Subsector, TranSubsector, AgSupplySubsector, SubsectorAddTechCosts,
                                NestingSubsector ),

        //! subsector name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! region name
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "region-name", mRegionName, std::string ),

        //! sector name
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "sector-name", mSectorName, std::string ),

        //! Subsector logit share weights
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "real-share-weight", mShareWeights, objects::PeriodVector<Value> ),

        //! The original subsector logit share weights that were parsed
        DEFINE_VARIABLE( ARRAY, "share-weight", mParsedShareWeights, objects::PeriodVector<Value> ),
                    
        //! Fuel preference elasticity
        DEFINE_VARIABLE( ARRAY, "fuelprefElasticity", mFuelPrefElasticity, objects::PeriodVector<double> ),
        
        //! Vector of technology containers by name
        DEFINE_VARIABLE( CONTAINER, "technology", mTechContainers, std::vector<ITechnologyContainer*> ),

        //! Interpolation rules for subsector share weight values.
        DEFINE_VARIABLE( CONTAINER, "interpolation-rule", mShareWeightInterpRules, std::vector<InterpolationRule*> ),

        //! Discrete choice model used for allocating technology shares
        DEFINE_VARIABLE( CONTAINER, "discreate-choice-function", mDiscreteChoiceModel, IDiscreteChoice* )
    )
    
    // Some typedefs for technology interators
    typedef std::vector<ITechnologyContainer*>::iterator TechIterator;
    typedef std::vector<ITechnologyContainer*>::const_iterator CTechIterator;
    // Some typedefs to make using interpolation rules more readable.
    typedef std::vector<InterpolationRule*>::const_iterator CInterpRuleIterator;
    
    std::auto_ptr<IInfo> mSubsectorInfo; //!< The subsector's information store.

    virtual void interpolateShareWeights( const int aPeriod );

    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
    
    virtual const std::vector<double> calcTechShares ( const GDP* gdp, const int period ) const;
    
    void clear();
    void clearInterpolationRules();

public:
    Subsector();
    virtual ~Subsector();
    const std::string& getName() const;
    
    virtual void setNames( const std::string& aRegionName, const std::string& aSectorName );
    
    virtual const std::string& getXMLName() const;
   
    bool XMLParse( rapidxml::xml_node<char>* & aNode );

    virtual void completeInit( const IInfo* aSectorInfo,
                               ILandAllocator* aLandAllocator );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual double getPrice( const GDP* aGDP, const int aPeriod ) const;
    virtual bool allOutputFixed( const int period ) const;
    virtual bool containsOnlyFixedOutputTechnologies( const int period ) const;
    virtual double getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const;

    virtual void calcCost( const int aPeriod );

    virtual double calcShare( const IDiscreteChoice* aChoiceFn, const GDP* aGDP, const int aPeriod) const;
    virtual double getShareWeight( const int period ) const;

    virtual void setOutput( const double aVariableDemand,
                            const double aFixedOutputScaleFactor,
                            const GDP* aGDP,
                            const int aPeriod );

    virtual bool isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const;
    virtual double getFixedOutput( const int Period, const double aMarginalRevenue ) const;

    virtual double getTotalCalOutputs( const int period ) const;

    virtual double getOutput( const int period ) const;

    virtual double getEnergyInput( const int aPeriod ) const;

    virtual void postCalc( const int aPeriod );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
};
#endif // _SUBSECTOR_H_
