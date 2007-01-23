/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
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
#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include "resources/include/aresource.h"
#include "util/base/include/value.h"

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
 *              - \c capacity-factor UnlimitedResource::mCapacityFactor
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
    
    virtual void XMLParse( const xercesc::DOMNode* aNode );
    
    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

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

    virtual void dbOutput( const std::string& aRegionName );

    virtual void csvOutputFile( const std::string& aRegionName ); 

    virtual void setCalibratedSupplyInfo( const int aPeriod,
                                          const std::string& aRegionName );

    virtual void accept( IVisitor* aVisitor,
                         const int aPeriod ) const;

protected:
    //! Read in prices.
    std::vector<double> mFixedPrices;

    //! Capacity factor.
    Value mCapacityFactor;

    //! Variance.
    Value mVariance;

    void setMarket( const std::string& aRegionName );
};

#endif
