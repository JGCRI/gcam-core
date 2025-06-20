#if DEBUG_STATE
#ifndef _ANALYZE_TECH_MEM_H_
#define _ANALYZE_TECH_MEM_H_
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
 * \file analyze_tech_mem.hpp
 * \ingroup util
 * \brief AnalyzeTechMem class header file.
 * \author Pralit Patel
 */

#include <cassert>
#include "util/base/include/definitions.h"
#include "containers/include/imodel_feedback_calc.h"

/*!
 * \brief Use GCAMFusion to analyze memory usage within the various types of `ARRAY` Data.
 * \details Produce diagnostic reports on which types of vectors are in use (std::vector, PeriodVector,
 *          YearVector, or TechVintageVector) including how many instances and how much memory is
 *          consumed by them both overhead and direct contiguous memory for the contained data.  With
 *          a focus on those within Technology which can create "dead space" when not using TechVintageVector.
 *
 * \author Pralit Patel
 */
class AnalyzeTechMem : public IModelFeedbackCalc {
public:

    // INamed methods
    virtual const gcamstr& getName() const { static const gcamstr XML_NAME("analyze_tech_mem"); return XML_NAME; }
    
    // IModelFeedbackCalc methods
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );

    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );
    
    // Templated callbacks for GCAMFusion
    template<typename DataType>
    void processData( DataType& aData );
    template<typename DataType>
    void pushFilterStep( const DataType& aData );
    template<typename DataType>
    void popFilterStep( const DataType& aData );

private:

    //! A flag to indicate if the processData call back is currently within a Technology
    bool mInTech;

    //! The lifetime in terms of model periods of the current technology (if mInTech)
    int mCurrTechLifetime;

    //! A count of the number of Technology instances
    size_t mNumTech;

    //! A count of the number of ARRAY held within and below the Technology level of nesting
    size_t mNumInTech;

    //! The total sizeof the number of ARRAY held within and below the Technology level of nesting
    size_t mTotalSizeInTech;

    //! The wasted space of arrays allocated for indices which would never be utilized
    size_t mDeadSizeInTech;

    //! A count of the number of ARRAY not held within and below the Technology level of nesting
    size_t mNumOutTech;

    //! The total sizeof the number of ARRAY not held within and below the Technology level of nesting
    size_t mTotalSizeOutTech;
    
    //! The number of TechVintageVector instances
    size_t mNumTechVec;
    
    //! The total sizeof TechVintageVector instances
    size_t mTotalSizeInTechVec;
    
    //! A count of YearVectors, which are primarily used for land use change results
    size_t mNumLUCArr;
    
    //! The total sizeof YearVectors, which are primarily used for land use change results
    size_t mTotalSizeLUCArr;
    
    //! A count of the number of Value classes held within ARRAYs (as apposed to double)
    size_t mNumValueClasses;
};

#endif // _ANALYZE_TECH_MEM_H_

#endif // DEBUG_STATE
