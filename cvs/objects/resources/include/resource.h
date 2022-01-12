#ifndef _RESOURCE_H_
#define _RESOURCE_H_
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
* \file resource.h
* \ingroup Objects
* \brief The Resource and RenewableResource classes header file.
* \author Sonny Kim
*/
#include <vector>
#include <map>
#include "resources/include/aresource.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

// Forward declaration.
class SubResource;

/*! 
* \ingroup Objects
* \brief A class which defines a single resource containing multiple
*        subresources, which can be of either depletable or renewable type.
* \todo This class needs much more documentation.
* \author Sonny Kim
*/
class Resource: public AResource {
    friend class XMLDBOutputter;
public:
    Resource();
    virtual ~Resource();
    static const std::string& getXMLNameStatic();
    void toDebugXML( const int period, std::ostream& aOut, Tabs* aTabs ) const;
    const std::string& getName() const; 
    virtual void completeInit( const std::string& aRegionName, const IInfo* aRegionInfo );
    
    virtual void initCalc( const std::string& aRegionName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const int aPeriod );
    
    void calcSupply( const std::string& aRegionName, const GDP* aGdp, const int aPeriod );
    virtual double getAnnualProd( const std::string& aRegionName, const int aPeriod ) const;
    virtual double getPrice( const int aPeriod ) const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:

    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AResource,

        //! subresource objects for each Resource
        DEFINE_VARIABLE( CONTAINER, "subresource", mSubResource, std::vector<SubResource*> ),

        //! Resource price
        DEFINE_VARIABLE( ARRAY | STATE, "price", mResourcePrice, objects::PeriodVector<Value> ),

        //! total Resource available
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "available", mAvailable, objects::PeriodVector<Value> ),

        //! annual production rate of Resource
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "annualprod", mAnnualProd, objects::PeriodVector<Value> ),

        //! cumulative production of Resource
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "cummprod", mCumulProd, objects::PeriodVector<Value> )
    )
    
    //! Pointer to the resource's information store.
    std::auto_ptr<IInfo> mResourceInfo;

    virtual const std::string& getXMLName() const;
    void setMarket( const std::string& aRegionName );
    virtual void annualsupply( const std::string& aRegionName, int aPeriod, const GDP* aGdp, double aPrice );
    void cumulsupply( const std::string& aRegionName, double aPrice, int aPeriod );
};

/*! 
* \ingroup Objects
* \brief A class which defines a RenewableResource object, which is a container for multiple Subresource objects.
* \author Josh Lurz, Sonny Kim
*/
class RenewableResource: public Resource {
public: 
    RenewableResource();
    static const std::string& getXMLNameStatic();
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        Resource
    )

    virtual const std::string& getXMLName() const;
    virtual void annualsupply( const std::string& aRegionName, int aPeriod, const GDP* aGdp, double aPrice );
};

#endif // _RESOURCE_H_
