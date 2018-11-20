#ifndef _RESOURCE_RESERVE_TECHNOLOGY_H_
#define _RESOURCE_RESERVE_TECHNOLOGY_H_
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
* \file resource_reserve_technology.h
* \ingroup Objects
* \brief The ResourceReserveTechnology class header file.
* \details TODO:
*
* \author Pralit Patel
*/

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declaration
class Tabs;

class ResourceReserveTechnology : public Technology {
	friend class XMLDBOutputter;
public:
	ResourceReserveTechnology(const std::string& aName,
		const int aYear);
	~ResourceReserveTechnology();
	static const std::string& getXMLNameStatic();
	ResourceReserveTechnology* clone() const;

	virtual void completeInit(const std::string& aRegionName,
		const std::string& aSectorName,
		const std::string& aSubsectorName,
		const IInfo* aSubsectorIInfo,
		ILandAllocator* aLandAllocator);

	virtual void initCalc(const std::string& aRegionName,
		const std::string& aSectorName,
		const IInfo* aSubsectorInfo,
		const Demographic* aDemographics,
		PreviousPeriodInfo& aPrevPeriodInfo,
		const int aPeriod);

	virtual void production(const std::string& aRegionName,
		const std::string& aSectorName,
		double aVariableDemand,
		double aFixedOutputScaleFactor,
		const GDP* aGDP,
		const int aPeriod);
    
    virtual double getFixedOutput( const std::string& aRegionName,
                          const std::string& aSectorName,
                          const bool aHasRequiredInput,
                          const std::string& aRequiredInput,
                          const double aMarginalRevenue,
                          const int aPeriod ) const;
    
    virtual double getCalibrationOutput( const bool aHasRequiredInput,
                                         const std::string& aRequiredInput,
                                         const int aPeriod ) const;
    
    virtual double getMarginalRevenue( const std::string& aRegionName,
                                      const std::string& aSectorName,
                                      const int aPeriod ) const;
    
    virtual double getEnergyCost( const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const int aPeriod ) const;
    
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

	virtual void doInterpolations(const Technology* aPrevTech, const Technology* aNextTech);
    
    virtual void initTechVintageVector();
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! The capacity for this technology.
        DEFINE_VARIABLE( SIMPLE | STATE, "total-reserve", mTotalReserve, Value ),
                            
        DEFINE_VARIABLE( ARRAY, "cumulative-production", mCumulProd, objects::TechVintageVector<Value> ),
                            
        /*DEFINE_VARIABLE( SIMPLE, "eor-coef", mEORCoef, Value ),*/
                            
        // TODO: could make this a new vintage production state
        DEFINE_VARIABLE( SIMPLE, "production-phase-scaler", mProductionPhaseScaler, Value )
    )

	virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const;
	virtual bool XMLDerivedClassParse(const std::string& nodeName, const xercesc::DOMNode* curr);
	//virtual void acceptDerived(IVisitor* aVisitor, const int aPeriod) const;
	virtual const std::string& getXMLName() const;
    void copy( const ResourceReserveTechnology& aOther );
    virtual void setProductionState( const int aPeriod );
    virtual void acceptDerived( IVisitor* aVisitor, const int aPeriod ) const; 
};

#endif // _RESOURCE_RESERVE_TECHNOLOGY_H_

