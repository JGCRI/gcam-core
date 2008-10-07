#ifndef _ENVIRONMENTAL_INFO_H_
#define _ENVIRONMENTAL_INFO_H_
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
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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
 *          CarbonBoxModel owns an EvironmentalInfo object,which can contain
 *          either shared information (such as pointer to the global climate 
 *          model) or information that is specific to one carbon box model.
 */
class EnvironmentalInfo {

public:
    EnvironmentalInfo( const IClimateModel* aClimateModel );
    EnvironmentalInfo( const EnvironmentalInfo& aEnvironmentalInfo );
    ~EnvironmentalInfo();
    const double getTemperature( const int aYear ) const;
    const objects::PeriodVector<double>& getModeledLandUse() const;
    void setModeledLandUse( const double aLandUse, const int aPeriod );
    double getLandUse( const int aYear ) const;
    void setLandUse( const double aLandUse, const int aYear );
    const double getHistoricalShare() const;
    void setHistoricalShare( double aShare );
    const LandUseHistory* getLandUseHistory() const;
    void setLandUseHistory( const LandUseHistory* aLandUseHistory );
    void setKey( const int aKey );
    int getKey() const;

private:
    //! Pointer to the world's climate model.
    const IClimateModel* mClimateModel;
    //! Total land used by period over modeled periods.
    objects::PeriodVector<double> mModeledLandUse;
    //! Total land used by over all years.
    objects::YearVector<double> mLandUse;
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
    //! Key to this box model's summer box (one summer box per conceptual root)
    int mKey;

};

#endif // _ENVIRONMENTAL_INFO_H_

