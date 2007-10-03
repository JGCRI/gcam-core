#ifndef _ICARBON_CONTAINER_H_
#define _ICARBON_CONTAINER_H_
#if defined(_MSC_VER)
#pragma once
#endif

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
 * \file icarbon_container.h
 * \ingroup Objects
 * \brief ICarbonContainer class header file.
 * \author Jim Naslund
 */

#include <boost/ptr_container/ptr_list.hpp>
#include "util/base/include/ivisitable.h"
#include "util/base/include/iparsable.h"
#include "util/base/include/iround_trippable.h"
#include "util/base/include/inamed.h"
#include "ccarbon_model/include/carbon_model_utils.h"

class EnvironmentalInfo;
class DependencyFinder;
class ACarbonFlow;

/*!
 * \brief An interface that represents a carbon container.
 * \details A carbon container stores an amount of carbon.  It can conduct
            transfers to other carbon containers.  It must also accept transfers
            from other containers, and may itself contain containers.
 * \author Jim Naslund
 */
class ICarbonContainer {

public:
    //! \brief Inline default constructor.
    ICarbonContainer(){}
    virtual ~ICarbonContainer(){}
	
	ICarbonContainer( const ICarbonContainer& aICarbonContainer );
    /*!
     * \brief Performs all the transfers out of this carbon container.
     * \details Performs all the transfers for the given year of the given
     *          flow type.
     * \param aEnvInfo environmental info object.
     * \param aFlowType desired flow type.
     * \param aYear the year.
     */
    virtual void doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                              const int aYear ) = 0;
    /*!
     * \brief Adds carbon to this box.
     * \details Adds aCarbonValue to this box for the given year.  A carbon
     *          container may contain other boxes.  If so, this function
     *          will only add carbon to the boxes inside that are of type
     *          aBoxType.
     * \param aCarbonValue the amount of carbon to add.
     * \param the year.
     * \param aBoxType the type of box to add the carbon to.
     */
    virtual void acceptTransfer( double aCarbonValue, const int aYear,
                                 const BoxType aBoxType ) = 0;
    /*!
     * \brief Adds a flow object to this carbon container.
     * \details Adds the given flow to this container.  IF this container contains
     *          other containers it will only add the flow to the containers
     *          of type aBoxType.
     * \param aCarbonFlow the flow to add.
     * \param aBoxType the type of box to add the flow to.
     */
    virtual void addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow,
                          const BoxType aBoxType ) = 0;

    /*!
     * \brief Adds dependencies on this object.
     * \details Each flow out of this box to a target box means the target box
     *          is dependant on this box.  This function adds those dependencies
     *          to the passed in dependency finder.
     * \param aDepFinder the dependency finder object to add the dependency to.
     */
    virtual void addDependencies( DependencyFinder& aDepFinder ) const = 0;

};

#endif // _ICARBON_CONTAINER_H_

