#ifndef _ICARBON_CONTAINER_H_
#define _ICARBON_CONTAINER_H_
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
 * \details A carbon container represents the amount, state, and behavior of the
 *          carbon contained in the parent object (which may be another carbon 
 *          container). It must be able to conduct transfers to other carbon 
 *          containers, accept transfers from other containers, and may itself 
 *          contain containers.
 * \author Jim Naslund
 */
class ICarbonContainer {
public:
    //! \brief Inline default constructor.
    ICarbonContainer(){}
    virtual ~ICarbonContainer(){}
    
    ICarbonContainer( const ICarbonContainer& aICarbonContainer );
    /*!
     * \brief Performs transfers out of this carbon container.
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
     * \details Adds the given flow to this container.  If this container contains
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

