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
#include <boost/core/noncopyable.hpp>

#include "resources/include/subresource.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

/*! 
* \ingroup Objects
* \brief ReserveSubResource is a class that contains grades.
* \author Pralit Patel
*/

/*!
 * \ingroup Objects
 * \brief A depleting sub-resource which producing using a resource - reserve paradigm.
 * \details This class treats the graded supply curve as a definition of "resource"
 *          potential and vintaged ResourceReserveTechnology that represents "reserves"
 *          from which we can produce the annual production that gets added as the
 *          supply to the marketplace.
 *          The procedure is not too different than the base case, we still use the
 *          market price (adjust by tech cost) to look up the supply curve.  However
 *          instead of assuming that quantity is produced entirely in the current timestep
 *          we treat it as a reserve that is produced over some assumed lifetime.  Thus
 *          we use that quantity as the "new investment" in a ResourceReserveTechnology
 *          the actual annual production is calculated by vintages with production
 *          coming not only from the new investment but also prior vintages which
 *          are still operating and have remaining reserve to produce from.
 *
 *          <b>XML specification for ReserveSubResource</b>
 *          - XML name: -c ReserveSubResource::getXMLNameStatic()
 *          - Contained by: Resource
 *          - Parsing inherited from class: SubResource
 *          - Attributes: None
 *          - Elements:
 *              - \c cal-reserve mCalReserve
 *                   A historical reserve amount to calibrate to in the historical years.
 *              - \c average-production-lifetime mAvgProdLifetime
 *                   An expected average lifetime to fully produce the reserve -- or
 *                   cumulsupply from a model period.  This value is calculate an annualized
 *                   production.
 *
 * \sa ResourceReserveTechnology
 * \author Pralit Patel
 */
class ReserveSubResource: public SubResource
{
    friend class CalibrateResourceVisitor;
public:
    ReserveSubResource();
    virtual ~ReserveSubResource();
    virtual void completeInit( const std::string& aRegionName, const std::string& aResourceName,
                               const IInfo* aResourceInfo );
    static const std::string& getXMLNameStatic();
    virtual void annualsupply( const std::string& aRegionName, const std::string& aResourceName,
                               int aPeriod, const GDP* aGdp, double aPrice );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    virtual double getLowestPrice( const int aPeriod ) const;
protected:
    virtual const std::string& getXMLName() const;

    DEFINE_DATA_WITH_PARENT(
        SubResource,
        
        //! A historical reserve amount to calibrate to in the historical years.
        DEFINE_VARIABLE( ARRAY, "cal-reserve", mCalReserve, objects::PeriodVector<Value> ),
        
        //! An expected average lifetime to fully produce the reserve -- or cumulsupply from
        //! a model period.  This value is calculate an annualized production.
        DEFINE_VARIABLE( SIMPLE, "average-production-lifetime", mAvgProdLifetime, Value )
    )
};

#endif // _RESERVE_SUBRESOURCE_H_
