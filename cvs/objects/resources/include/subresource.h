#ifndef _SUBRSRC_H_
#define _SUBRSRC_H_
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
#include <xercesc/dom/DOMNode.hpp>
#include <boost/core/noncopyable.hpp>

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

// Need to forward declare the subclasses as well.
class SubRenewableResource;
class SmoothRenewableSubresource;

/*! 
* \ingroup Objects
* \brief SubResource is a class that contains grades.
* \author Sonny Kim
*/

class SubResource: public IVisitable, private boost::noncopyable
{
	friend class XMLDBOutputter;
    friend class CalibrateResourceVisitor;
    friend class EnergyBalanceTable;
public:
    SubResource();
    virtual ~SubResource();
    std::string getName() const;
    void XMLParse( const xercesc::DOMNode* tempnode );
    virtual void completeInit( const IInfo* aResourceInfo );
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void cumulsupply( double aPrice, int aPeriod );
    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    double getCumulProd( const int aPeriod ) const;
    virtual void annualsupply( int aPeriod, const GDP* aGdp, double aPrice, double aPrevPrice );
    double getAnnualProd( int aPeriod ) const;
    double getAvailable( int aPeriod ) const;
    void dbOutput( const std::string& regname, const std::string& secname); 
    void csvOutputFile(const std::string &regname, const std::string& sname); 
    void updateAvailable( const int period );
    virtual double getVariance() const;
    virtual double getAverageCapacityFactor() const;
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual double getLowestPrice( const int aPeriod ) const;
    virtual double getHighestPrice( const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node ) = 0;
    virtual void toXMLforDerivedClass( std::ostream& out, Tabs* tabs ) const;

    DEFINE_DATA(
        /* Declare all subclasses of SubResource to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( SubResource, SubRenewableResource, SmoothRenewableSubresource ),
        
        //! SubResource name.
        CREATE_SIMPLE_VARIABLE( mName, std::string, "name" ),
        
        //! total available resource
        CREATE_ARRAY_VARIABLE( mAvailable, objects::PeriodVector<double>, "available" ),
        
        //! annual production of SubResource
        CREATE_ARRAY_VARIABLE( mAnnualProd, objects::PeriodVector<double>, "annualprod" ),
        
        //! cumulative production of SubResource
        CREATE_ARRAY_VARIABLE( mCumulProd, objects::PeriodVector<double>, "cumulprod" ),
        
        //! Cumulative Technical Change for this subsector
        CREATE_ARRAY_VARIABLE( mCumulativeTechChange, objects::PeriodVector<double>, "cumulative-tech-change" ),
        
        //! effective price (global price + price adder)
        CREATE_ARRAY_VARIABLE( mEffectivePrice, objects::PeriodVector<double>, "effective-price" ),
        
        //! calibrated production
        CREATE_ARRAY_VARIABLE( mCalProduction, objects::PeriodVector<double>, "cal-production" ),
        
        //! technical change
        CREATE_ARRAY_VARIABLE( mTechChange, objects::PeriodVector<Value>, "techChange" ),
        
        //! Environmental costs
        CREATE_ARRAY_VARIABLE( mEnvironCost, objects::PeriodVector<Value>, "environCost" ),
        
        //! Severance Tax (exogenous)
        CREATE_ARRAY_VARIABLE( mSeveranceTax, objects::PeriodVector<Value>, "severanceTax" ),
        
        //! price adder used for calibration purposes
        CREATE_ARRAY_VARIABLE( mPriceAdder, objects::PeriodVector<Value>, "price-adder" ),
    
        //! amount of SubResource for each grade
        CREATE_CONTAINER_VARIABLE( mGrade, std::vector<Grade*>, NamedFilter, "grade" )
    )
    
    //!< The subsector's information store.
    std::auto_ptr<IInfo> mSubresourceInfo;
};


/*! 
* \ingroup Objects
* \brief A class which defines a SubDepletableResource object, which is a container for multiple grade objects.
* \author Steve Smith
*/
class SubDepletableResource: public SubResource {
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
};

/*! 
* \ingroup Objects
* \brief A class which defines a SubFixedResource object, which is a container for multiple grade objects.
* \author Steve Smith
*/
class SubFixedResource: public SubResource {
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
};

#endif // _SUBRSRC_H_
