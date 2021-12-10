#ifndef _THERMAL_BUILDING_SERVICE_INPUT_H_
#define _THERMAL_BUILDING_SERVICE_INPUT_H_
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
 * \file thermal_building_service_input.h
 * \ingroup Objects
 * \brief ThermalBuildingServiceInput class header file.
 * \author Pralit Patel
 * \author Jiyong Eom
 */

#include "util/base/include/definitions.h"

#include "functions/include/building_service_input.h"

/*! 
 * \ingroup Objects
 * \brief An input class which demands heating or cooling building services.
 * \details Building services will operate with a building service function which
 *          will gradually satiate demands as affordability of the service increases
 *          and is adjusted for changing climate and building characteristics.
 *
 *          <b>XML specification for ThermalBuildingServiceInput</b>
 *          - XML name: \c ThermalBuildingServiceInput::getXMLNameStatic()
 *          - Contained by: BuildingNodeInput
 *          - Parsing inherited from class: None
 *          - Attributes: \c name BuildingServiceInput::mName
 *          - Elements:
 *              - \c base-service BuildingServiceInput::mServiceDemand
 *                   The base year service which can be utilized to back out
 *                   coefficients.
 *              - \c internal-gains-scalar ThermalBuildingServiceInput::mInternalGainsScalar
 *                   The extent which internal gains effects the thermal load requirement.
 *              - \c degree-days ThermalBuildingServiceInput::mDegreeDays
 *                   Total number of degree * days in a year that the heating or cooling service
 *                   is required.
 *              - \c SatiationDemandFunction::getXMLNameStatic() BuildingServiceInput::mSatiationDemandFunction
 *                   The self contained satiation demand function which will parse it's own
 *                   parameters.
 *
 * \author Pralit Patel
 * \author Jiyong Eom
 */
class ThermalBuildingServiceInput : public BuildingServiceInput
{
    friend class XMLDBOutputter;
public:
    ThermalBuildingServiceInput();
    virtual ~ThermalBuildingServiceInput();
    
    static const std::string& getXMLNameStatic();
    
    // Building service specific methods
    virtual double calcThermalLoad( const BuildingNodeInput* aBuildingInput,
                                    const double aFloorspace,
                                    const int aPeriod ) const;
    
    // IInput methods
    virtual IInput* clone() const;

    virtual const std::string& getXMLReportingName() const;
    
    virtual const std::string& getXMLName() const;
    
    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName,
                               const std::string& aSubsectorName,
                               const std::string& aTechName,
                               const IInfo* aTechInfo );
    
    virtual double getCoefficient( const int aPeriod ) const;
    
    virtual void setCoefficient( const double aCoefficient,
                                 const int aPeriod );
    
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        BuildingServiceInput,

        //! Internal gains scaling parameter.
        DEFINE_VARIABLE( SIMPLE, "internal-gains-scalar", mInternalGainsScalar, Value ),
        
        //! Degree days by period.
        DEFINE_VARIABLE( ARRAY, "degree-days", mDegreeDays, objects::PeriodVector<Value> ),
        
        //! Demand function coefficients to capture base year thermal characteristics.
        DEFINE_VARIABLE( SIMPLE | STATE | NOT_PARSABLE, "coefficient", mCoefficient, Value )
    )
    
    void copy( const ThermalBuildingServiceInput& aInput );
};

#endif // _THERMAL_BUILDING_SERVICE_INPUT_H_
