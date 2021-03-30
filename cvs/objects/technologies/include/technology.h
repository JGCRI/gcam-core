#ifndef _TECHNOLOGY_H_
#define _TECHNOLOGY_H_
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
* \file technology.h
* \ingroup Objects
* \brief The Technology class header file.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include <map>
#include <memory>
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/istandard_component.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/value.h"
#include "functions/include/ifunction.h" // For TechChange struct.
#include "technologies/include/itechnology.h"
#include "util/base/include/time_vector.h"

// Forward declaration
class AGHG;
class GDP;
class ICaptureComponent;
class IShutdownDecider;
class IInput;
class IFunction;
class IInfo;
class ICalData;
class ILandAllocator;
class Demographic;
class IProductionState;
class IOutput;
class ITechnicalChangeCalc;
class Tabs;

/*! 
 * \ingroup Objects
 * \brief This Technology class is based on the MiniCAM description of
 *        technology.
 * \details The technology class is where all fuels are either consumed or
 *          transformed. This class has options for calibration, fixed output,
 *          vintaging, emissions, secondary outputs, and carbon capture and
 *          storage.
 *
 *          The Technology class behavior is determined by a series of
 *          components which modify the behavior of the Technology. The
 *          components are as follows:
 *
 *         - Outputs. Output objects are stored in the mOutputs vector and
 *           defined by the IOutput interface. Output objects determine the
 *           level of output for a good based on the output determined by the
 *           Technology for the primary output. They may also assign a cost or
 *           value for the output. All technologies have at least one output,
 *           the primary output. This object is created automatically by the 
 *           Technology.
 *
 *         - Emissions. Emissions objects are stored in the ghg vector and
 *           defined by the AGHG interface. Emissions objects are similar to
 *           output objects, they determine a level of emissions from the
 *           primary output, or other factors such as the current income level,
 *           and determine a cost(such as a tax, or sequestration cost), or
 *           value(in the case of a negative emission). Emissions interact with
 *           the Technology's optional capture component to determine the cost
 *           of sequestration and the fraction of emissions captured.
 *
 *         - Capture component. A Technology may optionally have a single
 *           capture component that is stored in mCaptureComponent and
 *           represented by the ICaptureComponent interface. The Technology will
 *           call the capture component to adjust the Technology's non-energy
 *           cost and effective efficiency. The Technology also passes the
 *           capture component to the GHGs so that they may adjust their
 *           emissions, taxes and costs accordingly.
 *
 *         - Shutdown deciders. Shutdown deciders are stored in the
 *           mShutdownDeciders vector and are represented by the
 *           IShutdownDecider interface Shutdown deciders are only affect
 *           Technologies in the vintage output state; they do not affect new
 *           investment. Shutdown deciders determine the percentage of the
 *           initial output level to produce in a later period. The effects of
 *           the shutdown deciders are multiplicative.
 *
 *         - Calibration data. A Technology may optionally have a single
 *           calibration data object, stored in mCalValue and represented by the
 *           ICalData interface. The calibration object is responsible for
 *           determining the level of input and output in the new investment
 *           period, assuming that the period is defined to be a calibration
 *           period by the Modeltime.
 *
 *         - Production state. The production state for each period is stored in
 *           the mProductionState vector and represented by the IProductionState
 *           interface. The production state is responsible for determining the
 *           level of output of the Technology for each period. It contains
 *           information about whether the Technology is new investment, a
 *           vintage, fixed output, or retired. The production state is
 *           dynamically created by the Technology for each period. See
 *           ProductionStateFactory::create for an explanation of which
 *           production state is created for each point in the lifecycle of a
 *           Technology.
 *
 * \todo Document input.
 * \todo Better document the plug-in points for the derived classes.
 * \note Technology is the abstract base class for the various Technology
 *       implementations. It contains default implementations of the abstract
 *       functions which may be called by the derived classes to perform the
 *       default actions.
 * \author Sonny Kim, Josh Lurz
 */
class Technology: public ITechnology
{
    // TODO: Remove the need for this. These classes should use public
    // interfaces.
    friend class XMLDBOutputter;
    friend class MarginalProfitCalculator;
    friend class EnergyBalanceTable;
public:
    Technology( const std::string& aName, const int aYear );
    virtual Technology* clone() const = 0;
    virtual ~Technology();

    virtual void setYear( const int aNewYear );
    virtual int getYear() const;
    
    virtual bool isSameType( const std::string& aType ) const;

    bool XMLParse( const xercesc::DOMNode* tempnode );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

    static const std::string& getXMLVintageNameStatic();
    
    virtual const std::string& getXMLName() const = 0;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );
    
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

    virtual double calcShare( const IDiscreteChoice* aChoiceFn,
                              const GDP* aGDP,
                              int aPeriod ) const;
    
    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod );

    double getCost( const int aPeriod ) const;

    const std::string& getName() const;

    void setShareWeight( double shareWeightValue );
    
    virtual double getCalibrationOutput( const bool aHasRequiredInput,
                                         const std::string& aRequiredInput, 
                                         const int aPeriod ) const;

    virtual bool hasCalibratedValue( const int aPeriod ) const;

    // TODO: Make this non-virtual when transportation is fixed by units.
    virtual double getEnergyCost( const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const int aPeriod ) const;

    double getOutput( const int aPeriod ) const;

    virtual double getTotalGHGCost( const std::string& aRegionName, const std::string& aSectorName, 
                            const int aPeriod ) const;

    double getShareWeight() const;

    double getCapacityFactor() const;

    virtual Value getParsedShareWeight() const;

    virtual int getNumbGHGs() const;

    void copyGHGParameters( const AGHG* prevGHG );

    virtual const AGHG* getGHGPointer( const std::string& aGHGName ) const;

    double getEnergyInput( const int aPeriod ) const;

    const std::vector<std::string> getGHGNames() const;

    virtual double getFixedOutput( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const bool aHasRequiredInput,
                           const std::string& aRequiredInput,
                           const double aMarginalRevenue,
                           const int aPeriod ) const;
    
    bool isAllCalibrated( const int aPeriod,
                          double aCalAccuracy,
                          const std::string& aRegionName,
                          const std::string& aSectorName,
                          const std::string& aSubsectorName,
                          const bool aPrintWarnings ) const;

    // TODO: rename this method to isOutputFixedOrCalibrated
    //       or something else which better describes what it
    //       is intended for
    bool isOutputFixed( const bool aHasRequiredInput,
                        const std::string& aRequiredInput, 
                        const int aPeriod ) const;

    bool isFixedOutputTechnology( const int aPeriod ) const;

    virtual double calcFuelPrefElasticity( const int aPeriod ) const;

    virtual bool isAvailable( const int aPeriod ) const;
    
    virtual bool isOperating( const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
    virtual void doInterpolations( const Technology* aPrevTech, const Technology* aNextTech );
    
    virtual void initTechVintageVector();
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        ITechnology,

        // These member variables are ordered by decreasing size to optimize memory
        // usage. When adding a new variable add it to the section with the
        // variables type.

        //! Suite of greenhouse gases
        DEFINE_VARIABLE( CONTAINER, "ghg", mGHG, std::vector<AGHG*> ),

        //! Vector of output objects representing the outputs of the technology.
        DEFINE_VARIABLE( CONTAINER, "output", mOutputs, std::vector<IOutput*> ),

        //! Vector of inputs to the Technology.
        DEFINE_VARIABLE( CONTAINER, "input", mInputs, std::vector<IInput*> ),
                                
        //! The current production state for each period.
        DEFINE_VARIABLE( CONTAINER, "production-state", mProductionState, objects::PeriodVector<IProductionState*> ),

        //! The objects which combine to calculate the shutdown coefficient.
        DEFINE_VARIABLE( CONTAINER, "shutdown-decider", mShutdownDeciders, std::vector<IShutdownDecider*> ),
                                
        //! An add-on which sequesters emissions.
        DEFINE_VARIABLE( CONTAINER, "capture-component", mCaptureComponent, ICaptureComponent* ),
                                
        //! Calibration value
        DEFINE_VARIABLE( CONTAINER, "calibration-value", mCalValue, ICalData* ),

        //! An add-on which calculates technical change for the Technology.
        DEFINE_VARIABLE( CONTAINER, "tech-change-calc", mTechChangeCalc, ITechnicalChangeCalc* ),

        /*!
         * \brief The calculated cost of the Technology period.
         * \note calcCost must be called in an iteration before this value is valid.
         * \sa Technology::calcCost
         */
        DEFINE_VARIABLE( ARRAY | STATE, "cost", mCosts, objects::TechVintageVector<Value> ),

        //! A map of a keyword to its keyword group
        DEFINE_VARIABLE( SIMPLE, "keyword", mKeywordMap, std::map<std::string, std::string> ),

        //! Name of this technology.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Logit share weight
        DEFINE_VARIABLE( SIMPLE | STATE, "share-weight", mShareWeight, Value ),

        //! The Logit share weight that was parsed by the user
        DEFINE_VARIABLE( SIMPLE, "parsed-share-weight", mParsedShareWeight, Value ),

        //! The annual capacity factor
        DEFINE_VARIABLE( SIMPLE, "capacity-factor", mCapacityFactor, double ),
        
        //! Price multiplier (multiplies costs but not secondary revenue)
        DEFINE_VARIABLE( SIMPLE, "pMultiplier", mPMultiplier, double ),

        //! Amount of fixed supply for this tech, exclusive of constraints
        DEFINE_VARIABLE( SIMPLE, "fixedOutput", mFixedOutput, double ),

        //! Alpha-zero coefficient for the current period. This is calculated by the
        //! mTechChangeCalc if one exists, otherwise it is set to 1. It is constant
        //! throughout a period.
        DEFINE_VARIABLE( SIMPLE, "alpha-zero", mAlphaZero, double ),

        //! period year or vintage
        DEFINE_VARIABLE( SIMPLE, "year", mYear, int ),

        //! Number of years for which the vintage exists.
        DEFINE_VARIABLE( SIMPLE, "lifetime", mLifetimeYears, int ),
              
        //! The current marginal revenue.  TODO: cleaner solution for getting
        //! this information to the profit shutdown decider.
        DEFINE_VARIABLE( SIMPLE | STATE, "marginal-revenue", mMarginalRevenue, Value )
    )

    //! The technology's information store.
    std::auto_ptr<IInfo> mTechnologyInfo;
    
    //! Production function for the technology.
    const IFunction* mProductionFunction;

    static double getFixedOutputDefault();

    virtual void setProductionState( const int aPeriod );

    bool hasInput( const std::string& aInput ) const;
    
    virtual double getTotalInputCost( const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const int aPeriod ) const;

    virtual double getMarginalRevenue( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const int aPeriod ) const;

    virtual void calcEmissionsAndOutputs( const std::string& aRegionName,
                                  const double aPrimaryOutput,
                                  const GDP* aGDP,
                                  const int aPeriod );

    // TODO: Make this non-virtual when transportation is fixed by units.
    virtual double calcSecondaryValue( const std::string& aRegionName,
                                       const int aPeriod ) const;

    bool hasNoInputOrOutput( const int aPeriod ) const;

    virtual const IFunction* getProductionFunction() const;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const;

    virtual const IInfo* getTechInfo() const;
    int calcDefaultLifetime() const;
    void copy( const Technology& techIn );
    
private:
    void init();
    void clear();
};

#endif // _TECHNOLOGY_H_
