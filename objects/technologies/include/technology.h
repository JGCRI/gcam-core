#ifndef _TECHNOLOGY_H_
#define _TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file technology.h
* \ingroup Objects
* \brief The technology class header file.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include <map>
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
class IInfo;
class ICalData;
class ILandAllocator;
class Demographic;
class IOutput;
class GlobalTechnologyDatabase;

/*! 
* \ingroup Objects
* \brief This technology class is based on the MiniCAM description of technology.
*
* The technology class is where all fuels are either consumed or transformed. The default technology class is based on a MiniCAM-style logit representation. This class has options for capacity limits, calibration, and fixed output technologies (for supply sectors) -- although these capabilities depend on interaction with the sub-sector and sector classes. 
*
* \author Sonny Kim
*/
class technology: public ITechnology
{
    // TODO: Ideally these classes would use public interfaces.
    friend class XMLDBOutputter;
    friend class IndirectEmissionsCalculator;
private:
    void clear();
protected:
    std::string mName; //!< Name of this technolgy.

    /*!
     * \brief Flag to know whether to get a GlobalTechnology.
     * \details Because GlobalTechnologies could be parsed after the technology we have to
     *          wait until complete init to fetch it, however if the user decided to parse values
     *          which would have gone in the global technology, a GenericTechnologyInfo will be
     *          created.  The mTechData used will depend on which was parsed last.
     */
    bool mGetGlobalTech;

    int year; //!< period year or vintage
    double shrwts; //!< logit share weight

    double fuelcost; //!< fuel cost only
    double techcost; //!< total cost of technology

    double pMultiplier; //!< multiplier on total cost or price
    double lexp; //!< logit exponential
    double share; //!< technology shares
    double input; //!< total fuel input (fossil and uranium)
    double fixedOutput; //!< amount of fixed supply (>0) for this tech, exclusive of constraints
    double fixedOutputVal;
    //! Calibration value
    std::auto_ptr<ICalData> mCalValue;

    //! Vector of output objects representing the outputs of the technology.
    std::vector<IOutput*> mOutputs;

    std::vector<AGHG*> ghg; //!< suite of greenhouse gases
    std::map<std::string,double> emissmap; //!< map of ghg emissions
    std::map<std::string,double> emfuelmap; //!< map of ghg emissions implicit in fuel
    std::string note; //!< input data notation for this technology

    std::map<std::string,int> ghgNameMap; //!< Map of ghg name to integer position in vector.

    //! The ITechnologyInfo this class will delegate to for shared data.
    boost::shared_ptr<ITechnologyInfo> mTechData;

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    bool hasNoInputOrOutput() const; // return if tech is fixed for no output
    double calcSecondaryValue( const std::string& aRegionName, const int aPeriod ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {};
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {};
    virtual void copy( const technology& techIn );
    void initElementalMembers();
    static double getFixedOutputDefault();
    virtual const std::string& getXMLName2D() const;
    void calcEmissionsAndOutputs( const std::string& aRegionName,
                                  const double aInput,
                                  const double aPrimaryOutput,
                                  const GDP* aGDP,
                                  const int aPeriod );
    void createTechData();
public:
    technology( const std::string& aName, const int aYear );
    technology( const technology& techIn );
    technology& operator=( const technology& techIn );
    virtual ITechnology* clone() const;
    virtual ~technology();
    virtual void XMLParse( const xercesc::DOMNode* tempnode ); // initialize technology with xml data
    
    virtual void completeInit( const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    virtual const std::string& getXMLName1D() const;
    static const std::string& getXMLNameStatic1D();
    static const std::string& getXMLNameStatic2D();



    virtual void calcCost( const std::string& regionName, const std::string& sectorName, const int per ); 
    
    virtual void calcShare( const std::string& aRegionName,
                            const std::string& aSectorName,
                            const GDP* aGDP,
                            const int aPeriod ); 
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod );
    
    void normShare(double sum); // normalize technology share
    void resetFixedOutput(int per); // reset fixed supply to max value
    void adjShares(double subsecdmd, double subsecFixedOutput, double varShareTot, int per);
    void scaleFixedOutput(const double scaleRatio); // scale fixed supply
    // calculates fuel input and technology output

    void calcEmission( const std::string& aGoodName, const int aPeriod );

    // ****** return names and values ******
    const std::string& getName() const; // return technology name
    const std::string& getFuelName() const; // return fuel name

    double getInputRequiredForOutput( double aRequiredOutput,
                                      const int aPeriod ) const;

    double getEfficiency( const int aPeriod ) const;
    virtual double getIntensity(const int per) const;

    double getShare() const; // return normalized share
    virtual bool getCalibrationStatus( ) const; // return true if technology has calibration value
    void scaleCalibrationInput( const double scaleFactor ); // scale calibration value
    void scaleShareWeight( double scaleValue );
    void setShareWeight( double shareWeightValue );
    
    double getCalibrationInput( const int aPeriod ) const;
    
    virtual double getCalibrationOutput( const int aPeriod ) const;

    virtual void adjustForCalibration( double subSectorDemand,
                                       const std::string& regionName,
                                       const IInfo* aSubsectorIInfo,
                                       const int period );
    
    bool techAvailable( ) const; // Return available status (re: calibration)
    virtual bool outputFixed() const; // return calibration output value
    double getInput() const; // return fuel input amount
    double getOutput( const int aPeriod ) const;
    virtual double getFuelcost() const; // return fuel cost only
    double getTechcost() const; // return total technology cost
    
    double getNonEnergyCost( const int aPeriod ) const;

    double getTotalGHGCost( const std::string& aRegionName, const int aPeriod ) const;
    double getCarbonTaxPaid( const std::string& aRegionName, int aPeriod ) const;
    double getShareWeight() const;
    void copyGHGParameters( const AGHG* prevGHG );
    AGHG* getGHGPointer( const std::string& ghgName );
    const std::vector<std::string> getGHGNames() const;
    const std::map<std::string,double>& getemissmap() const; // return map of all ghg emissions
    const std::map<std::string,double>& getemfuelmap() const; // return map of all ghg emissions
    double get_emissmap_second( const std::string& str ) const; // return value for ghg
    double getlexp() const; // return logit exponential for the technology
    double getFixedOutput() const; // return fixed output
    double getFixedInput( const int aPeriod ) const;
    int getNumbGHGs()  const; // number of GHG objects in this technology
    void setYear( const int yearIn );
    virtual void tabulateFixedDemands( const std::string regionName, const int period, const IInfo* aSubsectorIInfo );
    void setTechShare(const double shareIn);
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
};
#endif // _TECHNOLOGY_H_
