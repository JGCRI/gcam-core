#ifndef _IMODEL_FEEDBACK_CALC_H_
#define _IMODEL_FEEDBACK_CALC_H_
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
 * \file imodel_feedback_calc.h
 * \ingroup Objects
 * \brief The IModelFeedbackCalc interface header file.
 * \author Pralit Patel
 */

#include <boost/core/noncopyable.hpp>

#include "util/base/include/inamed.h"

class Scenario;
class IClimateModel;

/*!
 * \ingroup Objects
 * \brief This provides the interface for classes which will provide feedback to
 *        model parameters as calculated based upon model results as the simulation
 *        moves forward through the model years.
 * \details This interface will provide two hooks to notify when to calculate feedbacks:
 *          Before a new model period is about to start and after a model period has
 *          has finished solving and climate model results for the period are available.
 *          A references to the Scenario and IClimateModel will be provided however
 *          it is implied that the subclasses of this interface will utilize the 
 *          GCAM fusion capabilities to gain access to the internal model state
 *          necessary to compute and/or push feedbacks into GCAM.
 * \warning MAGICC does not currently run between periods so these feedbacks may
 *          not work correctly if climate results are needed and MAGICC is configured
 *          as the climate model.
 *
 * \author Pralit Patel
 */
class IModelFeedbackCalc : public INamed,
                           private boost::noncopyable
{
public:
    virtual ~IModelFeedbackCalc() { }
    
    /*!
     * \brief A call back to indicate that a new simulation period is about to begin.
     * \details Note that climate model results will not yet be available for the current.
     *          period however results up to where the year where the model left off
     *          from the prior model year should still be available.
     * \param aScenario The Scenario object that contains the full state of the currently
     *                  running model.
     * \param IClimateModel The climate model instance which can be queried for climate
     *                      results.
     * \param aPeriod The model period that is about to begin calculation.
     */
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) = 0;
    
    /*!
     * \brief A call back to indicate that a simulation period has ended and the climate
     *        model has been run updated through this period.
     * \param aScenario The Scenario object that contains the full state of the currently
     *                  running model.
     * \param IClimateModel The climate model instance which can be queried for climate
     *                      results.
     * \param aPeriod The model period that just finished it's calculation.
     */
    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) = 0;
};

#endif // _IMODEL_FEEDBACK_CALC_H_
