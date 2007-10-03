#ifndef _SUMMER_BOX_H_
#define _SUMMER_BOX_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file summer_box.h
 * \ingroup Objects
 * \brief SummerBox class header file.
 * \author Jim Naslund and Ming Chang
 */

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
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
class SummerBox {

public:
    static SummerBox* getInstance();
    void addContainer( const int aKey );
    ICarbonContainer* getContainer( const int aKey );
    void calcLandUseChange( const int aYear, FlowType aFlowType );
    void sumLandLoss( const double aLandLost, const int aKey );
    double getLandLoss( const int aKey ) const;
    void resetState();

protected:

private:
    //!< The static instance of the Summer class.
    static std::auto_ptr<SummerBox> gInstance; 

    //!< A map that maps keys to containers.
    HashMap<int,SummerSubContainer*> mContainers;

    //!< A map that maps keys to doubles that sum land lost for each sub-container.
    HashMap<int,double> mLandLostSums;

    //!< A list of carbon containers.
    boost::ptr_list<SummerSubContainer> mContainerPointers;

    //! Private constructor to prevent a programmer from creating a second object.
    SummerBox();

    //! Private undefined copy constructor to prevent a programmer from creating a second object.
    SummerBox( const SummerBox& );

    //! Private undefined assignment operator to prevent a programmer from creating a second object.
    SummerBox& operator=( const SummerBox& );
};

#endif // _SUMMER_BOX_H_

