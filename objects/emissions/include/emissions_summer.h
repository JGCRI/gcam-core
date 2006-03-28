#ifndef _EMISSIONS_SUMMER_H_
#define _EMISSIONS_SUMMER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Labratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responisbility for the use of this software.
*/

/*! 
* \file emissions_summer.h
* \ingroup Objects
* \brief EmissionsSummer class header file.
* \author Josh Lurz
*/

#include "util/base/include/time_vector.h"
#include "util/base/include/default_visitor.h"
#include "util/base/include/value.h"

class AgSector;

/*! 
* \ingroup Objects
* \brief A class which sums emissions for a particular gas.
* \details 
* \author Josh Lurz
*/

class EmissionsSummer : public DefaultVisitor {
public:
    explicit EmissionsSummer( const std::string& aGHGName );
    
    virtual void startVisitGHG( const Ghg* aGHG,
                                const int aPeriod );
    
    // TODO: Remove this when the Fortran land allocator is removed.
    virtual void startVisitAgSector( const AgSector* aAgSector,
                                     const int aPeriod );

    virtual void startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                                       const int aPeriod );

    // Non-IVisitor interface method.
    double getEmissions( const int aPeriod ) const;
private:
    //! The name of the GHG being summed.
    const std::string mGHGName;

    //! The current sum.
    objects::PeriodVector<Value> mEmissionsByPeriod;
};

#endif // _EMISSIONS_SUMMER_H_
