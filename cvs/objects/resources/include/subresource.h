#ifndef _SUBRESOURCE_H_
#define _SUBRESOURCE_H_
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
* \file subresource.h
* \ingroup Objects
* \brief The SubResource class header file.
* \author Sonny Kim
*/
#include <memory>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations.
class Grade;
class GDP;
class IInfo;
class IVisitor;
class Tabs;
class ITechnologyContainer;

// Need to forward declare the subclasses as well.
class SubRenewableResource;
class SmoothRenewableSubresource;
class ReserveSubResource;

/*! 
* \ingroup Objects
* \brief SubResource is a class that contains grades.
* \author Sonny Kim
*/

class SubResource: public INamed, public IVisitable, private boost::noncopyable
{
	friend class XMLDBOutputter;
    friend class CalibrateResourceVisitor;
    friend class EnergyBalanceTable;
public:
    SubResource();
    virtual ~SubResource();
    const std::string& getName() const;
    virtual void completeInit( const std::string& aRegionName, const std::string& aResourceName,
                               const IInfo* aResourceInfo );
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void cumulsupply( const std::string& aRegionName, const std::string& aResourceName,
                              double aPrice, int aPeriod );
    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName,
                           const IInfo* aResourceInfo, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual double getCumulProd( const int aPeriod ) const;
    virtual void annualsupply( const std::string& aRegionName, const std::string& aResourceName,
                               int aPeriod, const GDP* aGdp, double aPrice );
    double getAnnualProd( int aPeriod ) const;
    double getAvailable( int aPeriod ) const;
    void updateAvailable( const int period );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual double getLowestPrice( const int aPeriod ) const;
    virtual double getHighestPrice( const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;

    DEFINE_DATA(
        /* Declare all subclasses of SubResource to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( SubResource, SubRenewableResource, SmoothRenewableSubresource,
                                ReserveSubResource ),
        
        //! SubResource name.
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        //! total available resource
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "available", mAvailable, objects::PeriodVector<Value> ),
        
        //! annual production of SubResource
        DEFINE_VARIABLE( ARRAY | STATE, "annualprod", mAnnualProd, objects::PeriodVector<Value> ),
        
        //! cumulative production of SubResource
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "cumulprod", mCumulProd, objects::PeriodVector<Value> ),
        
        //! Cumulative Technical Change for this subsector
        DEFINE_VARIABLE( ARRAY | NOT_PARSABLE, "cumulative-tech-change", mCumulativeTechChange, objects::PeriodVector<double> ),
        
        //! effective price (global price + price adder)
        DEFINE_VARIABLE( ARRAY | STATE | NOT_PARSABLE, "effective-price", mEffectivePrice, objects::PeriodVector<Value> ),
        
        //! calibrated production
        DEFINE_VARIABLE( ARRAY, "cal-production", mCalProduction, objects::PeriodVector<double> ),
        
        //! technical change
        DEFINE_VARIABLE( ARRAY, "techChange", mTechChange, objects::PeriodVector<Value> ),
        
        //! price adder used for calibration purposes
        DEFINE_VARIABLE( ARRAY | STATE, "price-adder", mPriceAdder, objects::PeriodVector<Value> ),
    
        //! amount of SubResource for each grade
        DEFINE_VARIABLE( CONTAINER, "grade", mGrade, std::vector<Grade*> ),
                
        //! Technology container for producing this sub-resource which will handle
        //! adding output to the marketplace, calculating emissions, and tacking on
        //! additional costs if configured.
        DEFINE_VARIABLE( CONTAINER, "technology", mTechnology, ITechnologyContainer* )
    )
    
    //!< The subsector's information store.
    std::auto_ptr<IInfo> mSubresourceInfo;
};

#endif // _SUBRESOURCE_H_
