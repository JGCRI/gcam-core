#ifndef _INDIRECT_EMISSIONS_CALCULATOR_H_
#define _INDIRECT_EMISSIONS_CALCULATOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
 * \file indirect_emissions_calculator.h
 * \ingroup Objects
 * \brief IndirectEmissionsCalculator class header file.
 * \author Josh Lurz
 */
#include "util/base/include/default_visitor.h"
#include "util/base/include/time_vector.h"
#include "util/base/include/value.h"
#include <map>
#include <string>

/*! 
 * \ingroup Objects
 * \brief Object which calculates total indirect emissions for all sectors and
 *        upstream emissions coefficients.
 * \details Calculates the indirect emissions and upstream coefficients for all
 *          sectors within a region. Indirect emissions are all upstream
 *          emissions for the fuels used by a sector. They do not include direct
 *          emissions from the sector. The upstream emissions coefficient for a
 *          sector includes upstream and direct emissions since it is used by
 *          other sectors to calculate their indirect emissions. Indirect
 *          emissions are equal to the sum of all inputs multiplied by their
 *          upstream coefficients.
 * \warning This class should only be used to generate indirect emissions and
 *          upstream coefficients for one region at a time.
 * \todo Once the Access database is removed this can be simplified to only work
 *       for one period at a time and not keep track of total sector level
 *       indirect emissions.
 * \author Josh Lurz
 */
class IndirectEmissionsCalculator : public DefaultVisitor {
public:
    IndirectEmissionsCalculator();

    double getUpstreamEmissionsCoefficient( const std::string& aSector,
                                            const int aPeriod ) const;

    double getIndirectEmissions( const std::string& aSector,
                                 const int aPeriod ) const;

    // IVisitor interface methods.
    void startVisitSector( const Sector* aSector, const int aPeriod );
    
    void endVisitSector( const Sector* aSector, const int aPeriod );
    
    void startVisitTechnology( const technology* aTechnology,
                               const int aPeriod );
private:
    /*!
     * \brief Storage type for the coefficients that contains one value per
     *        sector and period.
     * \todo Convert to a one period storage structure once the Access database
     *       is removed.
     */
    typedef std::map<std::string, objects::PeriodVector<Value> > DoubleMap;
    
    //! Map of sector name to upstream emissions coefficient.
    DoubleMap mUpstreamEmissionsCoefficients;

    //! Map of sector name to indirect emissions.
    DoubleMap mIndirectEmissions;

    //! Current sector name.
    std::string mCurrSectorName;

    //! Total emissions from the current sector.
    double mCurrTotalEmissions;

    //! Total indirect emissions from the current sector.
    double mCurrIndirectEmissions;
    
    //! Total output for the current sector.
    double mCurrOutput;
};

#endif // _INDIRECT_EMISSIONS_CALCULATOR_H_
