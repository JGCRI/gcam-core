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
 * \author Pralit Patel
 */

#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/technology.h"

// Forward declaration
class Tabs;

/*!
 * \ingroup Objects
 * \brief A type of technology that has a cumulative resource base set when initially
 *        invested and produces from that resource base at some annualized rate until
 *        it has been fully depleted.
 * \details This class will work in conjunction with a ReserveSubResource for calculating
 *          new investment.  This technology is still subject to shutdown deciders, cost
 *          adders, emissions which may affect the level of vintaged production the same
 *          as any other technology.  However when calculating profit shutdown we tack on
 *          additional "investment" cost which is reflective of where on the supply curve
 *          the containing ReserveSubResource was.
 *
 *          <b>XML specification for ResourceReserveTechnology</b>
 *          - XML name: -c ResourceReserveTechnology::getXMLNameStatic()
 *          - Contained by: ReserveSubResource
 *          - Parsing inherited from class: Technology
 *          - Attributes: None
 *          - Elements:
 *              - \c buildup-years mBuildupYears
 *                   A parameter which can phase in annual production over the given
 *                   number of years.
 *              - \c decline-phase-percent mDeclinePhasePct
 *                   A parameter which can be used to linearly phase out annual production
 *                   after the remaining reserve has reached the configured percent.
 *
 * \sa ReserveSubResource
 * \author Pralit Patel
 */
class ResourceReserveTechnology : public Technology {
	friend class XMLDBOutputter;
public:
	ResourceReserveTechnology(const std::string& aName,
		const int aYear);
	~ResourceReserveTechnology();
	static const std::string& getXMLNameStatic();
	ResourceReserveTechnology* clone() const;
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           PreviousPeriodInfo& aPrevPeriodInfo,
                           const int aPeriod );

	virtual void completeInit(const std::string& aRegionName,
                              const std::string& aSectorName,
                              const std::string& aSubsectorName,
                              const IInfo* aSubsectorIInfo,
                              ILandAllocator* aLandAllocator);

	virtual void production(const std::string& aRegionName,
                            const std::string& aSectorName,
                            double aVariableDemand,
                            double aFixedOutputScaleFactor,
                            const GDP* aGDP,
                            const int aPeriod);
    
    virtual double getFixedOutput(const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const bool aHasRequiredInput,
                                  const std::string& aRequiredInput,
                                  const double aMarginalRevenue,
                                  const int aPeriod) const;
    
    virtual double getEnergyCost( const std::string& aRegionName,
                                  const std::string& aSectorName,
                                  const int aPeriod ) const;
    
    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

	virtual void doInterpolations(const Technology* aPrevTech, const Technology* aNextTech);
    
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Technology,

        //! The total reserve which was calculated during investment to produce from.
        DEFINE_VARIABLE( SIMPLE | STATE, "total-reserve", mTotalReserve, Value ),
        
        //! The "investment" cost which is the cost of where on the supply curve the
        //! containing resource was when it invested in this technology.  We use this
        //! as a way to reflect the costs captured in the supply curve when calculating
        //! the profit shutdown.
        DEFINE_VARIABLE( SIMPLE | STATE, "investment-cost", mInvestmentCost, Value ),
                            
        //! An expected average lifetime to fully produce the total reserve.  This
        //! value is calculate an annualized production.  Note this value is generally
        //! shorter than the actual technology lifetime as annual production may decline
        //! in some years due to profit shutdown or because we are in a "decline" phase.
        DEFINE_VARIABLE( SIMPLE, "average-production-lifetime", mAvgProdLifetime, Value ),
                  
        //! The cumulative production that has been depleted from the total reserve
        //! by each model period.
        DEFINE_VARIABLE( ARRAY, "cumulative-production", mCumulProd, objects::TechVintageVector<Value> ),
        
        //! A parameter which can phase in annual production over the given number of years.
        DEFINE_VARIABLE( SIMPLE, "buildup-years", mBuildupYears, int ),
    
        //! A parameter which can be used to linearly phase out annual production
        //! after the remaining reserve has reached the configured percent.
        DEFINE_VARIABLE( SIMPLE, "decline-phase-percent", mDeclinePhasePct, Value ),
        
        //! A flag that indicates if this resource in currently calibrating which we can use to disable
        //! certain dynamics such as decline phase or profit shutdown to ensure values match.
        DEFINE_VARIABLE( SIMPLE, "is-calibrating", mIsResourceCalibrating, bool )
    )
    
	virtual void toDebugXMLDerived(const int period, std::ostream& out, Tabs* tabs) const;
	virtual bool XMLDerivedClassParse(const std::string& nodeName, const xercesc::DOMNode* curr);
	virtual const std::string& getXMLName() const;
    void copy( const ResourceReserveTechnology& aOther );
    virtual void setProductionState( const int aPeriod );
};

#endif // _RESOURCE_RESERVE_TECHNOLOGY_H_

