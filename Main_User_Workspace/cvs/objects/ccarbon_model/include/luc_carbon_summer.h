#ifndef _LUC_CARBON_SUMMER_H_
#define _LUC_CARBON_SUMMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file luc_carbon_summer.h
 * \ingroup Objects
 * \brief CarbonSummer class header file.
 * \author Jim Naslund and Ming Chang
 */

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

#include "util/base/include/hash_map.h"
#include <boost/ptr_container/ptr_list.hpp>
#include "ccarbon_model/include/carbon_model_utils.h"

class ICarbonContainer;
class SummerSubContainer;

/*!
 * \brief Singleton class that sums land use change out of carbon box models.
 * \details This class keeps track of land use change out of carbon box
 *          models.  After land use change out is complete it sends the carbon
 *          it summed to all carbon box models that gained land. Each
 *          conceptual root in the land allocator needs to keep track of
 *          its own sum.  Thus, the summer contains multiple carbon containers.
 *          Carbon box model's may use a unique key to access their container.
 */
class CarbonSummer {

public:
    static CarbonSummer* getInstance();
    void addContainer( const int aKey );
    ICarbonContainer* getContainer( const int aKey );
    void calcLandUseChange( const int aYear, FlowType aFlowType );
    void sumLandGain( const double aLandLost, const int aKey );
    double getTotalLandGain( const int aKey ) const;
    void resetState( const double aYear );

protected:

private:
    //!< The static instance of the Summer class.
    static std::auto_ptr<CarbonSummer> gInstance; 

    //!< A map that maps keys to containers.
    HashMap<int,SummerSubContainer*> mContainers;

    //!< A map that maps keys to doubles that sum land lost for each sub-container.
    HashMap<int,double> mLandLostSums;

    //!< A list of carbon containers.
    boost::ptr_list<SummerSubContainer> mContainerPointers;

    //! Private constructor to prevent a programmer from creating a second object.
    CarbonSummer();

    //! Private undefined copy constructor to prevent a programmer from creating a second object.
    CarbonSummer( const CarbonSummer& );

    //! Private undefined assignment operator to prevent a programmer from creating a second object.
    CarbonSummer& operator=( const CarbonSummer& );
};

#endif // _LUC_CARBON_SUMMER_H_

