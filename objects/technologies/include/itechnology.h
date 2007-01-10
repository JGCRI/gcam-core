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

    inline virtual ~ITechnology() = 0;

    virtual void setYear( const int aNewYear ) = 0;

    virtual void XMLParse( const xercesc::DOMNode* tempnode ) = 0;
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    
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

    virtual double getCost( const int aPeriod ) const = 0;
    
    virtual double getNonEnergyCost( const int aPeriod ) const = 0;

    virtual double getEfficiency( const int aPeriod ) const = 0;

    // TODO: Make abstract
    virtual double getCalibrationOutput( const int aPeriod ) const = 0;

    virtual void adjustForCalibration( double aTechnologyDemand,
                                       const std::string& aRegionName,
                                       const IInfo* aSubsectorInfo,
                                       const int aPeriod ) = 0;

    // TODO: Remove
    virtual double getIntensity( const int aPeriod ) const = 0;

    virtual const std::map<std::string,double> getEmissions( const std::string& aGoodName,
                                                             const int aPeriod ) const = 0;

    virtual const std::map<std::string,double> getEmissionsByFuel( const std::string& aGoodName,
                                                                   const int aPeriod ) const = 0;

    virtual const std::string& getName() const = 0;
    virtual const std::string& getFuelName() const = 0;
    
    virtual double getRequiredInputForOutput( double aRequiredOutput,
                                              const int aPeriod ) const = 0;

    virtual void scaleCalibrationInput( const double scaleFactor ) = 0;
    virtual void scaleShareWeight( double scaleValue ) = 0;
    virtual void setShareWeight( double shareWeightValue ) = 0;
    
    virtual double getCalibrationInput( const int aPeriod ) const = 0;

    virtual bool isOutputFixed( const int aPeriod ) const = 0;
    virtual double getInput( const int aPeriod ) const = 0;
    virtual double getOutput( const int aPeriod ) const = 0;

    virtual double getTotalGHGCost( const std::string& aRegionName, const std::string& aSectorName, 
                            const int aPeriod ) const = 0;

    virtual double getShareWeight() const = 0;
    virtual void copyGHGParameters( const AGHG* prevGHG ) = 0;

    virtual const AGHG* getGHGPointer( const std::string& aGHGName ) const = 0;

    virtual const std::vector<std::string> getGHGNames() const = 0;
 
    virtual double getEmissionsByGas( const std::string& aGasName, const int aPeriod ) const = 0;

    virtual double getFixedOutput( const std::string& aRegionName, const std::string& aSectorName, 
                           const int aPeriod ) const = 0;

    virtual void tabulateFixedDemands( const std::string& aRegionName, const std::string& aSectorName, const int aPeriod ) = 0;
    
    virtual bool isAllCalibrated( const int aPeriod,
                          double aCalAccuracy,
                          const std::string& aRegionName,
                          const std::string& aSectorName,
                          const std::string& aSubsectorName,
                          const bool aPrintWarnings ) const = 0;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const = 0;
};

// Inline methods
ITechnology::~ITechnology(){
}

#endif // _ITECHNOLOGY_H_
