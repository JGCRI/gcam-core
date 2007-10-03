/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
 * \file carbon_model_utils.cpp
 * \ingroup Objects
 * \brief CarbonModelUtils class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include <cfloat>

#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/util.h"
#include "land_allocator/include/land_use_history.h"
#include "climate/include/iclimate_model.h"

using namespace std;

/*!
 * \brief Constructor.
 */
CarbonModelUtils::CarbonModelUtils(){
}

//! Default destructor
CarbonModelUtils::~CarbonModelUtils() {
}

/*!
 * \brief Get the land usage for a year.
 * \details Returns the land usage for a year. An appropriate historical or
 *          calculated value will be returned depending on the year.
 * \param aYear the year.
 * \param aLandUseHistory the land use history object.
 * \param aHistoricalShare the historical share.
 * \param aLandUse the land use object.
 * \return Land usage for the year.
 * \warning This function does not check if the model projection data is valid.
 *          For example, its potentially possible that land-use data for 2020
 *          is requested before that period is calculated.
 * \todo Address the above warning.
 */
double CarbonModelUtils::getLandUse( const unsigned int aYear,
                                     const LandUseHistory* aLandUseHistory,
                                     const double aHistoricalShare,
                                     const objects::PeriodVector<double> aLandUse ){
    // If the year is within the range of the history use the historical
    // allocation. The land use history may be null if none was read-in.
    unsigned int maxHistoryYear = 0;
    if( aLandUseHistory ){
        maxHistoryYear = aLandUseHistory->getMaxYear();
    }

    const unsigned int basePeriod =
        max( scenario->getModeltime()->getyr_to_per( max( static_cast<unsigned int>( 1975 ), maxHistoryYear ) ) + 1, 1 );

    // Store the first calculated year to save time.
    static const unsigned int baseYear
        = static_cast<unsigned int>( scenario->getModeltime()->getper_to_yr( basePeriod ) );

    double landUse;

    if( aYear < maxHistoryYear ){
        landUse = aHistoricalShare * aLandUseHistory->getAllocation( aYear );
    }
    // If the year is between the last historical year and the first
    // calculated year interpolate between the two.
    else if( aYear < baseYear && maxHistoryYear != 0 ){
        landUse = util::linearInterpolateY( aYear, maxHistoryYear, baseYear,
                                            aHistoricalShare * aLandUseHistory->getAllocation( maxHistoryYear ),
                                            aLandUse[ basePeriod ] );
    }
    // Otherwise use data interpolated from the current model projection.
    else {
        landUse = interpYearHelper( aLandUse, aYear );
    }
    return landUse;
}

/*!
 * \brief A static function to return the starting year to index the arrays.
 * \todo Use a read-in value for the start year.
 * \return The start year.
 */
const int CarbonModelUtils::getStartYear(){
    const IClimateModel* climateModel = scenario->getClimateModel();
    if( climateModel != 0 ){
        return scenario->getClimateModel()->getCarbonModelStartYear();
    }
    else {
        //TODO: try and get the land use history start year
        return 1975;
    }
}

/*!
 * \brief Return the last year of the climate calculation.
 * \todo Make this value dynamic.
 * \author Jim Naslund
 * \return The last year of the climate calculation.
 */
const int CarbonModelUtils::getEndYear(){
    return 2095;
}

/*
 * \brief Returns a parameter which defines the time scale for the soil
 *        emissions decay function.
 * \return Soil decay function time scale parameter.
 * \todo This should be dynamic by land type.
 */
double CarbonModelUtils::getSoilTimeScale(){
    return 40;
}

/*!
 * \brief Helper function to interpolate a value for a year from a PeriodVector.
 * \details Calculates a linearly interpolated value for the year. If the year
 *          is before the first period of the vector, the first value is
 *          returned. If the year is after the last period of the vector, the
 *          last value is used. Otherwise a value is linearly interpolated
 *          between the nearest two periods.
 * \param aPeriodVector Vector from which to interpolate the value.
 * \param aYear Year for which to interpolate a value.
 * \return Interpolated value for the year.
 */
double CarbonModelUtils::interpYearHelper( const objects::PeriodVector<double>& aPeriodVector,
                                           const unsigned int aYear ){
    // If the year is before the first period of the model use the value
    // in the base period.
    const Modeltime* modeltime = scenario->getModeltime();
    if( aYear <= static_cast<unsigned int>( modeltime->getStartYear() ) ){
        return *aPeriodVector.begin();
    }

    // If the year is after the end period use the value in the last period.
    if( aYear > static_cast<unsigned int>( modeltime->getEndYear() ) ){
        return *aPeriodVector.last();
    }
    
    // Find the period containing aYear. This cannot be zero because the year
    // was already checked against the start year.
    int currPeriod = modeltime->getyr_to_per( aYear );

    // Find the last year of the current period.
    int lastYear = modeltime->getper_to_yr( currPeriod );

    // Find the first year of the current period.
    int firstYear = modeltime->getper_to_yr( currPeriod - 1 );

    // Interpolate the result.
    return util::linearInterpolateY( aYear, firstYear, lastYear,
                                     aPeriodVector[ currPeriod - 1 ],
                                     aPeriodVector[ currPeriod ] );
}

/*!
 * \brief Helper function to get a value for a year from a YearVector.
 * \details Determines a value for a given year from a YearVector. If the year
 *          is within the range of the year vector the value for the year will
 *          be returned, otherwise a value will be extrapolated. The
 *          extrapolation considers all values before the first year equal to
 *          the first year, and all values after the end year to be equal to the
 *          end year.
 * \param aYearVector Vector from which to interpolate the value.
 * \param aStartYear First year of the climate model.
 * \param aEndYear Last year of the climate model.
 * \param aYear Year for which to interpolate a value.
 * \return Interpolated value for the year.
 */
double CarbonModelUtils::interpYearHelper( const objects::YearVector<double>& aYearVector,
                                           const unsigned int aStartYear,
                                           const unsigned int aEndYear,
                                           const unsigned int aYear ){
    // If the year is before the first year of the carbon cycle use the value in the
    // first year.
    if( aYear < aStartYear ){
        return *aYearVector.begin();
    }

    // If the year is after the last year of the carbon cycle use the value in the last year.
    if( aYear > aEndYear ){
        return *aYearVector.last();
    }

    // Return the value from inside the range of the vector.
    return aYearVector[ aYear ];
}

string CarbonModelUtils::flowTypeToString( FlowType aFlow ) {
    switch( aFlow ) {
        case 0:
            return "BoxFlow";
            break;
        case 1:
            return "LUCFlow";
            break;
        case 2:
            return "LUCFlowOut";
            break;
        case 3:
            return "LUCFlowIn";
            break;
        case 4:
            return "AnyFlow";
            break;
    }
}

std::string CarbonModelUtils::boxTypeToString( BoxType aBoxType ) {
	switch( aBoxType ) {
		case 0:
			return "Vegetation";
			break;
		case 1:
			return "Soil";
			break;
		case 2:
			return "Litter";
			break;
		case 3:
			return "NPP";
			break;
		case 4:
			return "Atmosphere";
			break;
		case 5:
			return "AnyBox";
			break;
	}
}

BoxType CarbonModelUtils::stringBoxNameToType( const std::string aBoxName ) {
	std::string tempBoxName = aBoxName;
	std::transform( tempBoxName.begin(),
					tempBoxName.end(),
					tempBoxName.begin(),
					::tolower );
	if ( aBoxName == "vegetation" ) {
		return eVegetation;
	}
	else if ( aBoxName == "soil") {
		return eSoil;
	}
	else if ( aBoxName == "litter" ) {
		return eLitter;
	}
	else if ( aBoxName == "npp" ) {
		return eNPP;
	}
	else if ( aBoxName == "atmosphere" ) {
		return eAtmosphere;
	}
	else if ( aBoxName == "anybox" ) {
		return eAnyBox;
	}
}