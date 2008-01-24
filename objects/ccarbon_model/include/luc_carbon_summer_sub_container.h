#ifndef _SUMMER_SUB_CONTAINER_H_
#define _SUMMER_SUB_CONTAINER_H_
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

