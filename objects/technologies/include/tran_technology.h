#ifndef _TRAN_TECHNOLOGY_H_
#define _TRAN_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_technology.h
* \ingroup Objects
* \brief The transportation technology class header file.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

class GDP;
class IInfo;
/*! 
* \ingroup Objects
* \brief This transportation technology class is based on the MiniCAM description of technology.
* \author Sonny Kim, Josh Lurz, Steve Smith
*/

class TranTechnology : public Technology
{
public:
	TranTechnology( const std::string& aName, const int aYear );
	virtual TranTechnology* clone() const;
	virtual const std::string& getXMLName1D() const;
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

	virtual double calcShare( const std::string& aRegionName,
		const std::string& aSectorName, 
		const GDP* aGDP,
		const int aPeriod ) const; 

	virtual double getNonEnergyCost( const int aPeriod ) const;

	virtual double getEfficiency( const int aPeriod ) const;

	virtual double getCalibrationOutput( const int aPeriod ) const;

	virtual double getIntensity( const int aPeriod ) const;
protected:
	//!< Vehicle load factor.
	double mLoadFactor; 
	//!< Annual technical change rate.
	double mTechnicalChange;

	//!< Service output.
	double mServiceOutput; 
	//!< Intensity
	double mIntensity;

	double getCumulativeTechnicalChange( const int aPeriod ) const;
	bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
	void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
	void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

private:
	static const std::string XML_NAME; //!< The XML name of this object.
};

#endif // _TRAN_TECHNOLOGY_H_
