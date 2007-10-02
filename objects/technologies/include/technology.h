#ifndef _TECHNOLOGY_H_
#define _TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
#include <boost/shared_ptr.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"
#include "technologies/include/itechnology.h"
#include "technologies/include/itechnology_info.h"

// Forward declaration
class AGHG;
class GDP;
class DependencyFinder;
class ICaptureComponent;
class IShutdownDecider;
class IInfo;
class ICalData;
class ILandAllocator;
class Demographic;
class IProductionState;
class IOutput;
class GlobalTechnologyDatabase;
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
 *         - Technology data. Technology details are stored in mTechData and
 *           defined by the ITechnologyData interface. The technology data
 *           object is responsible for storing the initial read-in parameters of
 *           the Technology, such as efficiency and non-energy cost. This may be
 *           a global object, in which case the parameters are shared with other
 *           Technologies.
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
    friend class IndirectEmissionsCalculator;
public:
    Technology( const std::string& aName, const int aYear );
    Technology( const Technology& aTech );
    Technology& operator=( const Technology& techIn );
    virtual Technology* clone() const = 0;
    virtual ~Technology();

    virtual void setYear( const int aNewYear );

    void XMLParse( const xercesc::DOMNode* tempnode );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic2D();
    
    virtual const std::string& getXMLName1D() const = 0;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB ) = 0;
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod ) = 0;
    
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;

    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod ) = 0;

    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName, 
                              const GDP* aGDP,
                              const int aPeriod ) const = 0;
    
    virtual double getFuelCost( const std::string& aRegionName,
                                const std::string& aSectorName,
                                const int aPeriod ) const = 0;
    
    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const int aPeriod ) = 0;

    double getCost( const int aPeriod ) const;
    
    virtual double getNonEnergyCost( const int aPeriod ) const = 0;

    virtual double getEfficiency( const int aPeriod ) const = 0;

    // TODO: Make abstract
    virtual double getCalibrationOutput( const int aPeriod ) const;

    virtual void adjustForCalibration( double aTechnologyDemand,
                                       const std::string& aRegionName,
                                       const IInfo* aSubsectorInfo,
                                       const int aPeriod );

    // TODO: Remove
    virtual double getIntensity( const int aPeriod ) const;

    const std::map<std::string,double> getEmissions( const std::string& aGoodName, const int aPeriod ) const;
    const std::map<std::string,double> getEmissionsByFuel( const std::string& aGoodName, const int aPeriod ) const;

    const std::string& getName() const;
    const std::string& getFuelName() const;
    
    double getRequiredInputForOutput( double aRequiredOutput,
                                      const int aPeriod ) const;

    void scaleCalibrationInput( const double scaleFactor );
    void scaleShareWeight( double scaleValue );
    void setShareWeight( double shareWeightValue );
    
    double getCalibrationInput( const int aPeriod ) const;

    bool isOutputFixed( const int aPeriod ) const;
    double getInput( const int aPeriod ) const;
    double getOutput( const int aPeriod ) const;

    double getTotalGHGCost( const std::string& aRegionName, const std::string& aSectorName, 
                            const int aPeriod ) const;
    double getShareWeight() const;
    virtual int getNumbGHGs() const;
    void copyGHGParameters( const AGHG* prevGHG );

    virtual const AGHG* getGHGPointer( const std::string& aGHGName ) const;

    const std::vector<std::string> getGHGNames() const;
 
    double getEmissionsByGas( const std::string& aGasName, const int aPeriod ) const;

    double getFixedOutput( const std::string& aRegionName, const std::string& aSectorName, 
                           const int aPeriod ) const;

    void tabulateFixedDemands( const std::string& aRegionName, const std::string& aSectorName, const int aPeriod );
    
    bool isAllCalibrated( const int aPeriod,
                          double aCalAccuracy,
                          const std::string& aRegionName,
                          const std::string& aSectorName,
                          const std::string& aSubsectorName,
                          const bool aPrintWarnings ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    // These member variables are ordered by decreasing size to optimize memory
    // usage. When adding a new variable add it to the section with the
    // variables type.
    
    //! Technology output unit
    const std::string mOutputUnit;
    
    std::string mName; //!< Name of this technology.

    /*!
     * \brief Flag to know whether to get a GlobalTechnology.
     * \details Because GlobalTechnologies could be parsed after the technology
     *          we have to wait until complete init to fetch it, however if the
     *          user decided to parse values which would have gone in the global
     *          technology, a GenericTechnologyInfo will be created. The
     *          mTechData used will depend on which was parsed last.
     */
    bool mGetGlobalTech;

    /*!
     * \brief The calculated cost of the Technology period.
     * \note calcCost must be called in an iteration before this value is valid.
     * \sa Technology::calcCost
     */
    std::vector<double> mCosts;

    //! Total fuel input by period.
    std::vector<double> mInput;
    
    //! Vector of output objects representing the outputs of the technology.
    std::vector<IOutput*> mOutputs;

    //! Suite of greenhouse gases
    std::vector<AGHG*> ghg;
    
    //! The objects which combine to calculate the shutdown coefficient.
    std::vector<IShutdownDecider*> mShutdownDeciders;
    
    //! The current production state for each period.
    std::vector<IProductionState*> mProductionState;

    //! Map of ghg name to integer position in vector.
    std::map<std::string, int> ghgNameMap; 
    
    //! Map of name of the shutdown decider, which is the type, to shutdown decider.
    std::map<std::string, int> mShutdownDecidersMap;

    double shrwts; //!< logit share weight

    double pMultiplier; //!< multiplier on total cost or price
    double lexp; //!< logit exponential
    
    //! Amount of fixed supply for this technology, exclusive of constraints.
    double mFixedOutput; 
    
    //! An add-on which sequesters emissions.
    std::auto_ptr<ICaptureComponent> mCaptureComponent;
    
    //! Calibration value
    std::auto_ptr<ICalData> mCalValue;

    //! The ITechnologyInfo this class will delegate to for shared data.
    boost::shared_ptr<ITechnologyInfo> mTechData;

    int year; //!< period year or vintage

    //! Number of years for which the vintage exists.
    int mLifetimeYears;
    
    static double getLogitExpDefault();

    //! A map of a keyword to its keyword group
    std::map<std::string, std::string> mKeywordMap;
    
    void setProductionState( const int aPeriod );

    double getMarginalRevenue( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const int aPeriod ) const;

    bool isCalibrating( const int aPeriod ) const;

    virtual void calcEmissionsAndOutputs( const std::string& aRegionName,
                                  const double aInput,
                                  const double aPrimaryOutput,
                                  const GDP* aGDP,
                                  const int aPeriod );

    double calcSecondaryValue( const std::string& aRegionName,
                               const int aPeriod ) const;

    bool hasNoInputOrOutput( const int aPeriod ) const;

    double getFixedInput( const std::string& aRegionName, const std::string& aSectorName, 
                          const int aPeriod ) const;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ) = 0;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;

    void createTechData();
private:
    void init();
    void copy( const Technology& techIn );
    void clear();
};

#endif // _TECHNOLOGY_H_
