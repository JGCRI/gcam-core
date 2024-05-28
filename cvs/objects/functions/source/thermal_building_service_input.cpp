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
 * \file thermal_building_service_input.cpp
 * \ingroup Objects
 * \brief The ThermalBuildingServiceInput class source file.
 * \author Pralit Patel
 * \author Jiyong Eom
 */

#include "util/base/include/definitions.h"
#include <iostream>
#include <cmath>

#include "functions/include/thermal_building_service_input.h"
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector_utils.h"
#include "functions/include/satiation_demand_function.h"
#include "functions/include/building_node_input.h"

using namespace std;

extern Scenario* scenario;

//! Default Constructor
ThermalBuildingServiceInput::ThermalBuildingServiceInput()
{
}

//! Destructor
ThermalBuildingServiceInput::~ThermalBuildingServiceInput() {}

/*! \brief Get the XML name for reporting to XML file.
 *
 * This public function accesses the private constant string, XML_NAME. This way
 * the tag is always consistent for reporting outputs and can be easily
 * changed.
 * \author Sonny Kim
 * \return The constant XML_NAME.
 */
const string& ThermalBuildingServiceInput::getXMLNameStatic() {
    static const string XML_REPORTING_NAME = "thermal-building-service-input";
    return XML_REPORTING_NAME;
}

const string& ThermalBuildingServiceInput::getXMLName() const {
    return getXMLNameStatic();
}

const string& ThermalBuildingServiceInput::getXMLReportingName() const {
    return getXMLNameStatic();
}

void ThermalBuildingServiceInput::completeInit( const string& aRegionName,
                                        const string& aSectorName,
                                        const string& aSubsectorName,
                                        const string& aTechName,
                                        const IInfo* aTechInfo)
{
    BuildingServiceInput::completeInit( aRegionName, aSectorName, aSubsectorName,
                                        aTechName, aTechInfo );
    
    SectorUtils::fillMissingPeriodVectorInterpolated( mDegreeDays );
}

IInput* ThermalBuildingServiceInput::clone() const {
    ThermalBuildingServiceInput* retNodeInput = new ThermalBuildingServiceInput;
    retNodeInput->copy( *this );
    return retNodeInput;
}

void ThermalBuildingServiceInput::copy( const ThermalBuildingServiceInput& aInput ) {
    BuildingServiceInput::copy( aInput );
    mCoef = aInput.mCoef;
    mInternalGainsScalar = aInput.mInternalGainsScalar;
    mDegreeDays = aInput.mDegreeDays;
	mBiasAdderEn = aInput.mBiasAdderEn;
    mCoalA = aInput.mCoalA;
    mCoalK = aInput.mCoalK;
    mCoalBase = aInput.mCoalBase;
    mTradBioX = aInput.mTradBioX;
    mTradBioY = aInput.mTradBioY;
    mTradBioBase = aInput.mTradBioBase;
    mServPriceBase = aInput.mServPriceBase;
    mServBaseDens = aInput.mServBaseDens;
	mCoef = aInput.mCoef;
}

//! Output debug info to XML
void ThermalBuildingServiceInput::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLNameStatic(), aOut, aTabs, mName );
    
    XMLWriteElement( mServiceDemand[ aPeriod ], "service", aOut, aTabs );
    XMLWriteElement( mServiceDensity[ aPeriod ], "service-density", aOut, aTabs );
    XMLWriteElement( mCoef, "coef", aOut, aTabs );
	XMLWriteElement( mBiasAdderEn[ aPeriod ], "bias-adder", aOut, aTabs);
    XMLWriteElement(mCoalA, "a-coal", aOut, aTabs);
    XMLWriteElement(mCoalK, "k-coal", aOut, aTabs);
    XMLWriteElement(mCoalBase, "base-coal", aOut, aTabs);
    XMLWriteElement(mTradBioX, "x-TradBio", aOut, aTabs);
    XMLWriteElement(mTradBioY, "y-TradBio", aOut, aTabs);
    XMLWriteElement(mTradBioBase, "base-TradBio", aOut, aTabs);
    XMLWriteElement(mServPriceBase, "price", aOut, aTabs);
    XMLWriteElement(mServBaseDens, "base-density", aOut, aTabs);
    XMLWriteElement( mInternalGainsScalar, "internal-gains-scalar", aOut, aTabs );
    XMLWriteElement( mDegreeDays[ aPeriod ], "degree-days", aOut, aTabs );
    
    // write the closing tag.
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*!
 * \brief Calculate the thermal load for energy service.
 * \param aBuildingInput The parent building input from which to get building characteristics.
 * \param aInternalGainsPerSqMeter The level of internal gains normalized per square meter
 *                                 of building floorspace.
 * \param aPeriod The model period.
 * \return The thermal load for heating and cooling.
 */
double ThermalBuildingServiceInput::calcThermalLoad( const BuildingNodeInput* aBuildingInput,
                                                     const double aInternalGainsPerSqMeter,
                                                     const int aPeriod ) const
{
    /*!
     * \pre Degree days have been set for this period.
     */
    assert( mDegreeDays[ aPeriod ].isInited() );
    
    /*!
     * \pre The internal gains scalar has been set.
     */
    assert( mInternalGainsScalar.isInited() );
    
    return ( mDegreeDays[ aPeriod ] * aBuildingInput->getShellConductance( aPeriod )
             * aBuildingInput->getFloorToSurfaceRatio( aPeriod )
             + mInternalGainsScalar * aInternalGainsPerSqMeter );
}


double ThermalBuildingServiceInput::getCoefficient(const int aPeriod) const {

    return 1;
}


/*! \brief Set the building service coefficient.
 * \param aCoefficient new coefficient value
 * \param aPeriod Model period.
 */
void ThermalBuildingServiceInput::setCoefficient( const double aCoefficient, const int aPeriod ) {
    assert( aCoefficient != 0 ); // Can't set coefficients to zero.
    mCoef.set( aCoefficient );
}
