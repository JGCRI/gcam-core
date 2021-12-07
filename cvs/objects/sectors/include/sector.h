#ifndef _SECTOR_H_
#define _SECTOR_H_
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
* \file sector.h
* \ingroup Objects
* \brief The Sector class header file.
* \author Sonny Kim
*/

#include <vector>
#include <map>
#include <memory>
#include <list>
#include <boost/core/noncopyable.hpp>

#include "util/base/include/ivisitable.h"
#include "util/base/include/inamed.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations
class Subsector;
class ILogger;
class GDP;
class Tabs;
class IInfo;
class Demographic;
class NationalAccount;
class ILandAllocator;
class AGHG;
class IDiscreteChoice;
namespace ObjECTS {
class TObjectMetaInfo;
}

// Need to forward declare the subclasses as well.
class SupplySector;
class AgSupplySector;
class PassThroughSector;

/*! 
* \ingroup Objects
* \brief This class represents a single good that is produced, transformed, or consumed.
*
* All production, consumption, and transformation (other than resource extraction)
* is contained within the Sector class. Each Sector represents a distinct good that
* can either be supplied or demanded. The demand Sector derived from this class contains
* a few classes where changes are necessary, although most of the basic mechanisms are unchanged.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/

class Sector: public IVisitable,
              public INamed,
              private boost::noncopyable
{
    friend class XMLDBOutputter;
    friend class CalibrateShareWeightVisitor;
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Sector to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Sector, SupplySector, AgSupplySector, 
                                PassThroughSector ),

        //! Sector name
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! unit of good or service produced by sector
        DEFINE_VARIABLE( SIMPLE, "output-unit", mOutputUnit, std::string ),

        //! unit of input demanded by sector
        DEFINE_VARIABLE( SIMPLE, "input-unit", mInputUnit, std::string ),

        //! price unit of good or service produced by sector
        DEFINE_VARIABLE( SIMPLE, "price-unit", mPriceUnit, std::string ),

        //! region name
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "region-name", mRegionName, std::string ),

        //! subsector objects
        DEFINE_VARIABLE( CONTAINER, "subsector", mSubsectors, std::vector<Subsector*> ),
        
        //! Sector price by period updated with solution prices.
        DEFINE_VARIABLE( ARRAY | STATE, "price", mPrice, objects::PeriodVector<Value> ),

        //! A map of a keyword to its keyword group
        DEFINE_VARIABLE( SIMPLE, "keyword", mKeywordMap, std::map<std::string, std::string> ),
        
        //! The discrete choice model used to calculate sector shares.
        DEFINE_VARIABLE( CONTAINER, "discrete-choice-function", mDiscreteChoiceModel, IDiscreteChoice* ),
        
        //! A flag that will force the market dependency finder to create trial price/demand
        //! markets for this sector.
        DEFINE_VARIABLE( SIMPLE, "use-trial-market", mUseTrialMarkets, bool ),
                
        DEFINE_VARIABLE( CONTAINER, "object-meta-info", mObjectMetaInfo, std::vector<ObjECTS::TObjectMetaInfo*> )
    )
    
    typedef std::vector<Subsector*>::iterator SubsectorIterator;
    typedef std::vector<Subsector*>::const_iterator CSubsectorIterator;

    //! Pointer to the sector's information store.
    std::auto_ptr<IInfo> mSectorInfo;

    typedef ObjECTS::TObjectMetaInfo object_meta_info_type;
    typedef std::vector<object_meta_info_type> object_meta_info_vector_type;

    virtual void toDebugXMLDerived( const int period, std::ostream& aOut, Tabs* aTabs ) const = 0;

    virtual double getFixedOutput( const int aPeriod ) const;
    const std::vector<double> calcSubsectorShares( const GDP* aGDP, const int aPeriod ) const;

    bool outputsAllFixed( const int period ) const;
    
    double getCalOutput( const int period ) const;

    virtual double getPrice( const GDP* aGDP, const int aPeriod ) const;

public:
    explicit Sector();
    virtual ~Sector();
    virtual const std::string& getName() const;
    
    void setNames( const std::string& aRegionName );
    
    virtual const std::string& getXMLName() const = 0;

    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    virtual void completeInit( const IInfo* aRegionInfo,
                               ILandAllocator* aLandAllocator ) = 0;

    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod ) = 0;

    bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const;

    virtual void supply( const GDP* aGDP,
                         const int aPeriod ) = 0;

    void calcCosts( const int aPeriod );

    virtual double getOutput( const int period ) const = 0;

    virtual void calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ) = 0;

    virtual void postCalc( const int aPeriod );

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
private:
    void clear();
};

#endif // _SECTOR_H_
