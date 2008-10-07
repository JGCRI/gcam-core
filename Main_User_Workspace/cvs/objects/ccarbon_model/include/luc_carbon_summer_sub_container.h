#ifndef _SUMMER_SUB_CONTAINER_H_
#define _SUMMER_SUB_CONTAINER_H_
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
 * \file luc_carbon_summer_sub_container.h
 * \ingroup Objects
 * \brief SummerSubContainer class header file.
 * \author Jim Naslund and Ming Chang
 */


#include "util/base/include/time_vector.h"
#include <vector>
#include "ccarbon_model/include/icarbon_container.h"
#include "ccarbon_model/include/carbon_model_utils.h"

class EnvironmentalInfo;
class DependencyFinder;
class CarbonBox;

/*!
 * \brief An implementation of the ICarbonContainer interface that is 
 *        used by luc summer objects.
 * \details This is a basic implementation of the ICarbonContainer interface.
 *          The summer object creates these containers for each conceptual root.
 */
class SummerSubContainer : public ICarbonContainer {

friend class ComplexCarbonDotPrinter;
friend class ComplexCarbonPrinter;
friend class XMLDBOutputter;

public:
    //! \brief Inline default constructor.
    SummerSubContainer();
    virtual ~SummerSubContainer();
    void resetTransferedIn( const double aYear );
    virtual void doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                              const int aYear );
    virtual void acceptTransfer( double aCarbonValue, const int aYear,
                                 const BoxType aBoxType );
    virtual void addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType );
    virtual void addDependencies( DependencyFinder& aDepFinder ) const;

private:
    std::vector<CarbonBox*> mCarbonBoxes;
    objects::YearVector<bool> mTransferedIn;
};

#endif // _SUMMER_SUB_CONTAINER_H_

