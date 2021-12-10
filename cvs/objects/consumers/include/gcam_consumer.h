#ifndef _GCAM_CONSUMER_H_
#define _GCAM_CONSUMER_H_
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
 * \file gcam_consumer.h
 * \ingroup Objects
 * \brief GCAMConsumer class header file.
 * \author Pralit Patel
 * \author Sonny Kim
 */

#include <string>

#include "consumers/include/consumer.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class NationalAccount;
class Demographic;
class Tabs;
class IVisitor;

/*!
 * \ingroup Objects
 * \brief A SGM style consumer to drive final demands in GCAM.
 * \details Subregional population and income may be calculated within this consumer
 *          to allow for regional disaggregation of end use consumption.  This is
 *          done through exogenously specified shares of the regional population
 *          and GDP.
 *
 *          <b>XML specification for GCAMConsumer</b>
 *          - XML name: \c GCAMConsumer::getXMLNameStatic()
 *          - Contained by: RegionMiniCAM
 *          - Parsing inherited from class: BaseTechnology
 *          - Attributes: \c name BaseTechnology::name
 *          - Elements:
 *              - \c subregional-population-share GCAMConsumer::mSubregionalPopulationShare
 *                   The fraction of the total regional population which is represented in
 *                   this consumer.  Specified by year.
 *              - \c subregional-income-share GCAMConsumer::mSubregionalIncomeShare
 *                   The fraction of the total regional GDP that is represented in
 *                   this consumer.  This will then be converted to GDP per capita
 *                   or in other terms income.  Specified by year.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class GCAMConsumer : public Consumer
{
    friend class XMLDBOutputter;
public:
    GCAMConsumer();
    virtual GCAMConsumer* clone() const;
    virtual ~GCAMConsumer();
    
    virtual void copyParam( const BaseTechnology* baseTech,
                            const int aPeriod );
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName );
    
    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aSectorName,
                           NationalAccount& nationalAccount,
                           const Demographic* aDemographics,
                           const GDP* aGDP,
                           const double aCapitalStock,
                           const int aPeriod );
    
    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographics, 
                          const std::string& aRegionName, 
                          const std::string& aSectorName, const bool aIsNewVintageMode, const int aPeriod );

    virtual void postCalc( const std::string& aRegionName, const std::string& aSectorName, 
                           const int aPeriod );
    
    static const std::string& getXMLNameStatic();
    
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Consumer,

        //! Subregional population for reporting
        DEFINE_VARIABLE( ARRAY, "subregional-population", mSubregionalPopulation, objects::PeriodVector<Value> ),

        //! Subregional income for reporting
        DEFINE_VARIABLE( ARRAY, "subregional-income", mSubregionalIncome, objects::PeriodVector<Value> ),

        //! Subregional Population Share
        DEFINE_VARIABLE( ARRAY, "subregional-population-share", mSubregionalPopulationShare, objects::PeriodVector<Value> ),

        //! Subregional Income Share
        DEFINE_VARIABLE( ARRAY, "subregional-income-share", mSubregionalIncomeShare, objects::PeriodVector<Value> )
    )

    void copy( const GCAMConsumer& aOther );
    virtual const std::string& getXMLName() const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    // not sure what this is for
    bool isCoefBased() const { return true; }
    
    void initTechVintageVector();
};

#endif // _GCAM_CONSUMER_H_
