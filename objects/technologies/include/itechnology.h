#ifndef _ITECHNOLOGY_H_
#define _ITECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
* \file itechnology.h
* \ingroup Objects
* \brief The technology interface header file.
* \author Pralit Patel
*/

#include <string>
#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iround_trippable.h"

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
* \brief This class defines the interface for what it means to be a MiniCAM technology.
*
* This inteface is abstract.  All MiniCAM technologies will implement these methods.
*
* \author Pralit Patel
*/
class ITechnology: public IVisitable, public IRoundTrippable
{
public:
    virtual ITechnology* clone() const = 0;
    inline virtual ~ITechnology();
    virtual void XMLParse( const xercesc::DOMNode* tempnode ) = 0; // initialize technology with xml data
    
    virtual void completeInit( const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB ) = 0;
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod ) = 0;

    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    virtual const std::string& getXMLName1D() const = 0;

    virtual void calcCost( const std::string& regionName, const std::string& sectorName, const int per ) = 0; 
    
    virtual void calcShare( const std::string& aRegionName,
                            const std::string& aSectorName,
                            const GDP* aGDP,
                            const int aPeriod ) = 0; 
    
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName,
                             const double aDemand,
                             const GDP* aGDP,
                             const int aPeriod ) = 0;
    
    virtual void normShare(double sum) = 0; // normalize technology share
    virtual void resetFixedOutput(int per) = 0; // reset fixed supply to max value
    virtual void adjShares(double subsecdmd, double subsecFixedOutput, double varShareTot, int per) = 0;
    virtual void scaleFixedOutput(const double scaleRatio) = 0; // scale fixed supply
    // calculates fuel input and technology output

    virtual void calcEmission( const std::string& aGoodName, const int aPeriod ) = 0;

    // ****** return names and values ******
    virtual const std::string& getName() const = 0; // return technology name
    virtual const std::string& getFuelName() const = 0; // return fuel name

    virtual double getInputRequiredForOutput( double aRequiredOutput,
                                      const int aPeriod ) const = 0;

    virtual double getEfficiency( const int aPeriod ) const = 0;
    virtual double getIntensity(const int per) const = 0;

    virtual double getShare() const = 0; // return normalized share
    virtual bool getCalibrationStatus( ) const = 0; // return true if technology has calibration value
    virtual void scaleCalibrationInput( const double scaleFactor ) = 0; // scale calibration value
    virtual void scaleShareWeight( double scaleValue ) = 0;
    virtual void setShareWeight( double shareWeightValue ) = 0;
    
    virtual double getCalibrationInput( const int aPeriod ) const = 0;
    
    virtual double getCalibrationOutput( const int aPeriod ) const = 0;

    virtual void adjustForCalibration( double subSectorDemand, const std::string& regionName, const IInfo* aSubsectorIInfo, const int period ) = 0; // Adjust share weights for calibration
    virtual bool techAvailable( ) const = 0; // Return available status (re: calibration)
    virtual bool outputFixed() const = 0; // return calibration output value
    virtual double getInput() const = 0; // return fuel input amount
    virtual double getOutput( const int aPeriod ) const = 0;
    virtual double getFuelcost() const = 0; // return fuel cost only
    virtual double getTechcost() const = 0; // return total technology cost
    
    virtual double getNonEnergyCost( const int aPeriod ) const = 0;

    virtual double getTotalGHGCost( const std::string& aRegionName, const int aPeriod ) const = 0;
    virtual double getCarbonTaxPaid( const std::string& aRegionName, int aPeriod ) const = 0;
    virtual double getShareWeight() const = 0;
    virtual void copyGHGParameters( const AGHG* prevGHG ) = 0;
    virtual const AGHG* getGHGPointer( const std::string& aGHGName ) = 0;
    virtual const std::vector<std::string> getGHGNames() const = 0;
    virtual const std::map<std::string,double>& getemissmap() const = 0; // return map of all ghg emissions
    virtual const std::map<std::string,double>& getemfuelmap() const = 0; // return map of all ghg emissions

    virtual double get_emissmap_second( const std::string& str ) const = 0; // return value for ghg

    virtual double getFixedOutput() const = 0; // return fixed output
    virtual double getFixedInput( const int aPeriod ) const = 0;
    virtual int getNumbGHGs()  const = 0; // number of GHG objects in this technology
    virtual void setYear( const int yearIn ) = 0;
    virtual void tabulateFixedDemands( const std::string regionName, const int period, const IInfo* aSubsectorIInfo ) = 0;
    virtual void setTechShare(const double shareIn) = 0;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
};

// Inline methods
ITechnology::~ITechnology(){
}

#endif // _ITECHNOLOGY_H_
