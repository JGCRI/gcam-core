/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

#ifndef _DEFAULT_TECHNOLOGY_H_
#define _DEFAULT_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file default_technology.h
 * \ingroup Objects
 * \brief The DefaultTechnology class header file.
 * \author Josh Lurz
 */

#include <xercesc/dom/DOMNode.hpp>
#include <iosfwd>
#include "technologies/include/technology.h"
class Tabs;

/*! 
 * \ingroup Objects
 * \brief Technology which represents a simple MiniCAM technology.
 * \details The Technology class is where all fuels are either consumed or
 *          transformed. The DefaultTechnology class is based on a MiniCAM-style
 *          logit representation. This class has options for calibration, fixed
 *          output, capture of emissions, and secondary outputs. The class is
 *          used for both supply and demand of goods.
 * \note DefaultTechnology uses implementations of functions shared with other
 *       Technology subclasses. This is done by including implementations of
 *       abstract functions in Technology. DefaultTechnology's implementations
 *       of those abstract functions call the Technology implementations.
 */
class DefaultTechnology: public Technology
{
public:
	DefaultTechnology( const std::string& aName,
                       const int aYear );

	virtual DefaultTechnology* clone() const;

	static const std::string& getXMLNameStatic1D();
	
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorIInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorIInfo,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );	
	
    virtual void production( const std::string& aRegionName,
                             const std::string& aSectorName, 
		                     double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod );

	virtual double getFuelCost( const std::string& aRegionName,
                                const std::string& aSectorName,
		                        const int aPeriod ) const;
    
    virtual void calcCost( const std::string& aRegionName,
                           const std::string& aSectorName,
		                   const int aPeriod );

    virtual double getNonEnergyCost( const int aPeriod ) const;
	
    virtual double calcShare( const std::string& aRegionName,
                              const std::string& aSectorName, 
		                      const GDP* aGDP,
                              const int aPeriod ) const;

    virtual double getEfficiency( const int aPeriod ) const;

protected:
	virtual bool XMLDerivedClassParse( const std::string& aNodeName,
                                       const xercesc::DOMNode* aCurr );

	virtual void toInputXMLDerived( std::ostream& aOut,
                                    Tabs* aTabs ) const;

	virtual void toDebugXMLDerived( const int aPeriod,
                                    std::ostream& aOut,
                                    Tabs* aTabs ) const;

	virtual const std::string& getXMLName1D() const;
};

#endif // _DEFAULT_TECHNOLOGY_H_
