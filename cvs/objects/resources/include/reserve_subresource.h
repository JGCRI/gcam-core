#ifndef _RESERVE_SUBRESOURCE_H_
#define _RESERVE_SUBRESOURCE_H_
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
* \file reserve_subresource.h
* \ingroup Objects
* \brief The ReserveSubResource class header file.
* \author Sonny Kim
*/
#include <memory>
#include <xercesc/dom/DOMNode.hpp>
#include <boost/core/noncopyable.hpp>

#include "resources/include/subresource.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

class ITechnologyContainer;

/*! 
* \ingroup Objects
* \brief ReserveSubResource is a class that contains grades.
* \author Pralit Patel
*/

class ReserveSubResource: public SubResource
{
public:
    ReserveSubResource();
    virtual ~ReserveSubResource();
    virtual void completeInit( const std::string& aRegionName, const std::string& aResourceName,
                               const IInfo* aResourceInfo );
    //void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void cumulsupply( double aPrice, int aPeriod );
    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName,
                           const IInfo* aResourceInfo, const int aPeriod );
    virtual void postCalc( const std::string& aRegionName, const std::string& aResourceName, const int aPeriod );
    virtual void annualsupply( const std::string& aRegionName, const std::string& aResourceName,
                               int aPeriod, const GDP* aGdp, double aPrice, double aPrevPrice );
    double getAnnualProd( int aPeriod ) const;
    double getAvailable( int aPeriod ) const;
    void updateAvailable( const int period );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual double getLowestPrice( const int aPeriod ) const;
    virtual double getHighestPrice( const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* node );
    
    void calCumulsupply( double aPrice, int aPeriod );

    DEFINE_DATA_WITH_PARENT(
        SubResource,
                         
        DEFINE_VARIABLE( ARRAY, "cal-reserve", mCalReserve, objects::PeriodVector<Value> ),
                            
        //! amount of ReserveSubResource for each grade
        DEFINE_VARIABLE( CONTAINER, "technology", mTechnology, ITechnologyContainer* )
    )
};


#endif // _RESERVE_SUBRESOURCE_H_
