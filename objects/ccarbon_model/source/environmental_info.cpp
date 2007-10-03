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
 * \file environmental_info.cpp
 * \ingroup objects
 * \brief EnvironmentalInfo class source file.
 * \author Jim Naslund and Ming Chang
 */
#include "util/base/include/definitions.h"
#include "ccarbon_model/include/environmental_info.h"
#include "climate/include/iclimate_model.h"
#include <typeinfo>

using namespace std;

/*!
 * \brief Default constructor.
 * \details Constructs the object and initializes the members.
 * \param aClimateModel the climate model.
 * \param aLandUse the land use.
 * \param aHistoricalShare the historical share.
 * \param aLandUseHistory the land use history.
 */
EnvironmentalInfo::EnvironmentalInfo( const IClimateModel* aClimateModel )
: mClimateModel( aClimateModel )
{
}
EnvironmentalInfo::EnvironmentalInfo(const EnvironmentalInfo &aEnvironmentalInfo)
: mClimateModel( aEnvironmentalInfo.mClimateModel ),
  mLandUseHistory( aEnvironmentalInfo.mLandUseHistory ){
	
	objects::PeriodVector<double>::const_iterator periodVecIterator;
	
	if ( typeid( aEnvironmentalInfo.mLandUse ) == typeid( this->mLandUse ) ){
		std::copy( aEnvironmentalInfo.mLandUse.begin(),
				   aEnvironmentalInfo.mLandUse.end(),
				   this->mLandUse.begin() );
	}

	this->mHistoricalShare = aEnvironmentalInfo.mHistoricalShare;
	this->mKey = aEnvironmentalInfo.mKey;
}

EnvironmentalInfo::~EnvironmentalInfo(){
}
/*!
 * \brief Returns the temperature for a year.
 * \details Returns the temperature for the passed in year.
 * \param aYear the year.
 * \return a double representing the temperature in the given year.
 */
const double EnvironmentalInfo::getTemperature( const int aYear ) const {
    return mClimateModel->getTemperature( aYear );
}

/*!
 * \brief Returns the land use object.
 * \details Returns the land use object.
 * \return the land use object.
 */
const objects::PeriodVector<double> EnvironmentalInfo::getLandUse() const {
    return mLandUse;
}

/*!
 * \brief Sets the land use for a given period.
 * \details Sets the land use to the passed in value for the passed in period.
 * \param aLandUse to the land use.
 * \param aPeriod the period.
 */
void EnvironmentalInfo::setLandUse( const double aLandUse, const int aPeriod ){
    mLandUse[ aPeriod ] = aLandUse;
}

/*!
 * \brief Returns the historical share object.
 * \details Returns the historical share object.
 * \return the historical share object.
 */
const double EnvironmentalInfo::getHistoricalShare() const {
    return mHistoricalShare;
}

/*!
 * \brief Sets the historical share.
 * \details Sets the historical share.
 * \param aShare the share.
 */
void EnvironmentalInfo::setHistoricalShare( double aShare ){
    mHistoricalShare = aShare;
}
    

/*!
 * \brief Returns the land use history object.
 * \details Returns the land use history  object.
 * \return the land use history  object.
 */
const LandUseHistory* EnvironmentalInfo::getLandUseHistory() const {
    return mLandUseHistory;
}

/*!
 * \brief Sets the land use history.
 * \details Sets the land use history.
 * \param aLandUseHistory the land use history.
 */
void EnvironmentalInfo::setLandUseHistory( const LandUseHistory* aLandUseHistory ){
    mLandUseHistory = aLandUseHistory;
}

/*!
 * \brief Sets the key.
 * \details Sets the key that is unique to the CarbonBoxModel that owns this
 *          EnvironmentalInfo object.  This key is stored here so it can be
 *          easily accessed by objects the CarbonBoxModel owns such as CarbonBoxes
 *          and CarbonFlows.
 * \param aLandUseHistory the unique key.
 */
void EnvironmentalInfo::setKey( const int aKey ){
    mKey = aKey;
}

/*!
 * \brief Returns the key..
 * \details Returns the key that is unique to the CarbonBoxModel that owns this
 *          EnvironmentalInfo object.
 * \return the unique key.
 */
int EnvironmentalInfo::getKey() const {
    return mKey;
}
