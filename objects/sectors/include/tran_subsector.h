#ifndef _TRAN_SUBSECTOR_H_
#define _TRAN_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_subsector.h
* \ingroup Objects
* \brief The TranSubsector header file. 
* \author Marshall Wise, Sonny Kim, Josh Lurz
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

// Forward declarations
class GDP;
class MoreSectorInfo;
class Demographic;
class IInfo;

/*! 
* \ingroup Objects
* \brief A derived subsector representing a mode of transportation.
* \author Sonny Kim, Josh Lurz, Steve Smith, Marshall Wise
*/


class TranSubsector: public Subsector{
public:
    TranSubsector( const std::string& regionName, const std::string& sectorName );
    static const std::string& getXMLNameStatic();    
    double calcShare( const int period, const GDP* gdp ) const; 

    virtual void completeInit( const IInfo* aSectorInfo,
                               DependencyFinder* aDependencyFinder,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB );
    
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const MoreSectorInfo* aMoreSectorInfo,
                           const int aPeriod );
	double getPrice( const GDP* aGDP, const int aPeriod ) const;

protected:
    std::vector<double> speed; // Speed of Mode in Miles/hour
    std::vector<double> mPopulation; // copy of population from demographics
    std::vector<double> popDenseElasticity; // Population Density Elasticity of mode
    std::vector<double> mServiceOutputs; //!< Service output by period.
    double popDensity; // population density per land area

	virtual void MCoutputAllSectors( const GDP* aGDP,
                                     const IndirectEmissionsCalculator* aIndirectEmissionsCalc,
                                     const std::vector<double> aSectorOutput ) const;

    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    bool isNameOfChild  ( const std::string& nodename ) const;
    
    virtual ITechnology* createChild( const std::string& aTechType,
                                      const std::string& aTechName,
		                              const int aTechYear ) const;

	double getTimeValue( const GDP* aGDP, const int aPeriod ) const;
	double getTimeInTransit( const int aPeriod ) const;
	double getServicePerCapita( const int aPeriod ) const;
	double getGeneralizedPrice( const GDP* aGDP, const int aPeriod ) const;
private:
    static const std::string XML_NAME; //!< XML name of this object.
	bool mAddTimeValue;  // !< add value of time to price term
};


#endif // _TRAN_SUBSECTOR_H_
