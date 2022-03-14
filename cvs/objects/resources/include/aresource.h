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


#ifndef _ARESOURCE_H_
#define _ARESOURCE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
 * \file aresource.h
 * \ingroup Objects
 * \brief AResource header file.
 * \author Josh Lurz, Sonny Kim
 */
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/data_definition_util.h"

class Tabs;
class GDP;
class IInfo;

// Need to forward declare the subclasses as well.
class Resource;
class RenewableResource;
class UnlimitedResource;

/*! 
* \ingroup Objects
* \brief An abstract class which defines a single resource.
* \todo This class needs much more documentation.
* \author Josh Lurz
*/
class AResource: public INamed, public IVisitable, private boost::noncopyable {
    friend class XMLDBOutputter;
public:
    virtual ~AResource();

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const = 0;

    virtual const std::string& getName() const = 0;

    virtual void completeInit( const std::string& aRegionName,
                               const IInfo* aRegionInfo ) = 0;
    
    virtual void initCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;

    virtual void postCalc( const std::string& aRegionName,
                           const int aPeriod ) = 0;
    
    virtual void calcSupply( const std::string& aRegionName,
                             const GDP* aGDP,
                             const int period ) = 0;

    virtual double getAnnualProd( const std::string& aRegionName,
                                  const int aPeriod ) const = 0;

    virtual double getPrice( const int aPeriod ) const = 0;

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const = 0;
protected:
    // For the database output
    virtual const std::string& getXMLName() const = 0;
    
    DEFINE_DATA(
        /* Declare all subclasses of AResource to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( AResource, Resource, RenewableResource, UnlimitedResource ),

        //! Resource name.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Unit of resource output
        DEFINE_VARIABLE( SIMPLE, "output-unit", mOutputUnit, std::string ),

        //! Unit of resource price
        DEFINE_VARIABLE( SIMPLE, "price-unit", mPriceUnit, std::string ),

        //! Market name.
        DEFINE_VARIABLE( SIMPLE, "market", mMarket, std::string ),

        //! A map of a keyword to its keyword group
        DEFINE_VARIABLE( SIMPLE, "keyword", mKeywordMap, std::map<std::string, std::string> )
    )
};

// Inline function definitions
inline AResource::~AResource(){
}

#endif // _ARESOURCE_H_
