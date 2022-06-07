#ifndef _REGION_H_
#define _REGION_H_
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
* \file region.h
* \ingroup Objects
* \brief The Region class header file.
* \author Sonny Kim
*/

#include <map>
#include <vector>
#include <memory>
#include <string>
#include <list>
#include <boost/noncopyable.hpp>

#include "util/base/include/inamed.h"
#include "util/base/include/aparsable.h"
#include "util/base/include/ivisitable.h"
#include "util/base/include/data_definition_util.h"

// Forward declarations.
class Demographic;
class Sector;
class GHGPolicy;
class Curve;
class AResource;
class IInfo;
class Tabs;

// Need to forward declare the subclasses as well.
class RegionMiniCAM;

/*! 
* \ingroup Objects
* \brief This is an abstract base class for Regions.
*
* \author Sonny Kim
*/

class Region: public INamed, public IVisitable, public AParsable, protected boost::noncopyable
{
    friend class XMLDBOutputter;
public:
    Region();
    virtual ~Region();
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
    static const std::string& getXMLNameStatic();
    virtual void completeInit();
    const std::string& getName() const;
    
    virtual void initCalc( const int period );
    
    virtual void postCalc( const int aPeriod );

    void setTax( const GHGPolicy* aTax );
    const Curve* getEmissionsQuantityCurve( const std::string& ghgName ) const;
    const Curve* getEmissionsPriceCurve( const std::string& ghgName ) const;

    virtual bool isAllCalibrated( const int period, double calAccuracy, const bool printWarnings ) const { return true; };

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    
    DEFINE_DATA(
        /* Declare all subclasses of Region to allow automatic traversal of the
         * hierarchy under introspection.
         */
        DEFINE_SUBCLASS_FAMILY( Region, RegionMiniCAM ),
                
        /*! \brief Region name */
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),
        
        /*! \brief Population object */
        DEFINE_VARIABLE( CONTAINER, "demographic", mDemographic, Demographic* ),
        
        /*! \brief  vector of pointers to supply sector objects */
        DEFINE_VARIABLE( CONTAINER, "sector", mSupplySector, std::vector<Sector*> ),
        
        /*! \brief vector of pointers to ghg market objects, container for constraints and emissions */
        DEFINE_VARIABLE( CONTAINER, "policies", mGhgPolicies, std::vector<GHGPolicy*> ),
        
        /*! \brief vector of pointers to resource objects */
        DEFINE_VARIABLE( CONTAINER, "resource", mResources, std::vector<AResource*> ),
        
        /*! \brief The region's information store. */
        DEFINE_VARIABLE( SIMPLE | NOT_PARSABLE, "info", mRegionInfo, IInfo* )
    )

    virtual const std::string& getXMLName() const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
private:
    void clear();
};

#endif // _REGION_H_
