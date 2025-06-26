#if DEBUG_STATE

#ifndef _ANALYZE_CONTAINER_H_
#define _ANALYZE_CONTAINER_H_
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
 * Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
 * Distributed as open-source under the terms of the Educational Community
 * License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
 *
 * For further details, see: http://www.globalchange.umd.edu/models/gcam/
 *
 */

/*!
 * \file analyze_container.hpp
 * \ingroup util
 * \brief AnalyzeContainer class header file.
 * \author Pralit Patel
 */

#include <cassert>
#include <string>
#include <map>

#include "util/base/include/definitions.h"
#include "containers/include/imodel_feedback_calc.h"

/*!
 * \brief Use GCAMFusion to analyze memory usage of GCAM `CONTAINER`.
 * \details A GCAM containers are almost any class which are "nodes" in the GCAM heirachy.  They
 *          may contain additional containers or just "simple" data.  This diagnostics class will perform
 *          introspection to generate reports, per model period, of which classes are active (by actual instance
 *          and base class so users can aggregate as useful) the number of instances created and their direct
 *          actual class size as well as the "minimum size" (which could be different due to "padding", virtual
 *          method pointers, or member variables declared outside of `DEFINE_DATA`).
 *
 * \author Pralit Patel
 */
class AnalyzeContainer : public IModelFeedbackCalc {
public:

    // INamed methods
    virtual const gcamstr& getName() const { static const gcamstr XML_NAME("analyze_container"); return XML_NAME; }
    
    // IModelFeedbackCalc methods
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );

    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );
    
    // Templated callbacks for GCAMFusion so that we can perform introspection
    template<typename DataType>
    void processData( DataType& aData );

private:
    
    // A data structure to collect results in.  We use a map where the key is the instantiated
    // class type name and the value is a tuple including: base class name, actual class size,
    // minimum class size, and number of instances.  Such a data structure allows us to quickly
    // accumulate results per instantiated class type.
    std::map<std::string, std::tuple<std::string, size_t, size_t, size_t> > mContainerStats;
};

#endif // _ANALYZE_CONTAINER_H_

#endif // DEBUG_STATE
