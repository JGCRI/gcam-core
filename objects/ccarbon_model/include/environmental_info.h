#ifndef _ENVIRONMENTAL_INFO_H_
#define _ENVIRONMENTAL_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
* \file carbon_box_model.h
* \ingroup Objects
* \brief CarbonBoxModel class header file.
* \author Jim Naslund and Ming Chang
*/

#include "emissions/include/icarbon_calc.h"
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/time_vector.h"

class IClimateModel;

/*
 * \brief Class that encapsulates environmental info for the carbon box model.
 * \details This object is passed around during many of the carbon box model
 *          operations.  Methods may use it to get useful information. Each
 *          CarbonBoxModel owns an EvironmentalInfo object.
 */
class EnvironmentalInfo {

public:
    EnvironmentalInfo( const IClimateModel* aClimateModel );
	EnvironmentalInfo( const EnvironmentalInfo& aEnvironmentalInfo );
	~EnvironmentalInfo();
    const double getTemperature( const int aYear ) const;
    const objects::PeriodVector<double> getLandUse() const;
    void setLandUse( const double aLandUse, const int aPeriod );
    const double getHistoricalShare() const;
    void setHistoricalShare( double aShare );
    const LandUseHistory* getLandUseHistory() const;
    void setLandUseHistory( const LandUseHistory* aLandUseHistory );
    void setKey( const int aKey );
    int getKey() const;

private:
    //! Pointer to the world's climate model.
    const IClimateModel* mClimateModel;
    //! Total land used by period.
    objects::PeriodVector<double> mLandUse;
    //! Constant share of historical land.
    double mHistoricalShare;
    /*! 
     * \brief The land use history for the land leaf or it's parent land node.
     * \details Weak pointer to the land use history either for this leaf
     *          or the parent land type. The historical land share will be set to
     *          1 if this is the land use history for the leaf, and the historical share
     *          if it is for the parent land type.
     */
    const LandUseHistory* mLandUseHistory;
    //! Key to this box model's summer
    int mKey;

};

#endif // _ENVIRONMENTAL_INFO_H_

