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


#ifndef _UNLIMITED_RESOURCE_H_
#define _UNLIMITED_RESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file unlimited_resource.h
 * \ingroup Objects
 * \brief UnlimitedResource header file.
 * \author Josh Lurz
 */
#include "resources/include/aresource.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
 * \ingroup Objects
 * \brief A class which defines an unlimited quantity fixed price resource.
 * \details The UnlimitedResource defines a resource which allows for an
 *          unlimited usage of a resource at a fixed read-in price. This class
 *          should be used instead of a renewable resource with a flat supply
 *          curve because this class does not create a solved market. The class
 *          creates an unsolved market and ensures that supply of the market is
 *          always equal to demand.
 *
 *          <b>XML specification for UnlimitedResource</b>
 *          - XML name: \c unlimited-resource
 *          - Contained by: Region
 *          - Parsing inherited from class: None
 *          - Attributes: name UnlimitedResource::mName
 *          - Elements:
 *              - \c market UnlimitedResource::mMarket
 *              - \c variance UnlimitedResource::mVariance
 *              - \c price UnlimitedResource::mFixedPrices
 *                  - Attributes: year Year
 *
 * \author Josh Lurz
 */
class UnlimitedResource: public AResource {
public:
    static const std::string& getXMLNameStatic();

    UnlimitedResource();

    virtual ~UnlimitedResource();

    virtual const std::string& getXMLName() const;

    virtual void toDebugXML( const int period,
                             std::ostream &out,
                             Tabs* tabs ) const;

    virtual const std::string& getName() const;

    virtual void completeInit( const std::string& aRegionName,
                               const IInfo* aRegionInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod );

    virtual void calcSupply( const std::string& aRegionName,
                             const GDP* aGDP,
                             const int aPeriod );

    virtual double getAnnualProd( const std::string& aRegionName,
                                  const int aPeriod ) const;

    virtual double getPrice( const int aPeriod ) const;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AResource,
                            
        //! Read in prices.
        DEFINE_VARIABLE( ARRAY, "price", mFixedPrices, objects::PeriodVector<Value> ),
        
        //! The last supply value that was added to the marketplace so it is equal.
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "supply-wedge", mSupplyWedge, Value)
    )

    void setMarket( const std::string& aRegionName );
};

#endif
