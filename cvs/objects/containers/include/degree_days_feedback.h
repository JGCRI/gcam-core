#ifndef _DEGREE_DAYS_FEEDBACK_H_
#define _DEGREE_DAYS_FEEDBACK_H_
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
 * \file degree_days_feedback.h
 * \ingroup Objects
 * \brief The DegreeDaysFeedback class header file.
 * \author Pralit Patel
 */

#include "containers/include/imodel_feedback_calc.h"

/*!
 * \ingroup Objects
 * \brief Calc feed back to heating and cooling degree days.
 * \details Test implementation.
 *
 * \author Pralit Patel
 */
class DegreeDaysFeedback : public IModelFeedbackCalc
{
public:
    DegreeDaysFeedback();
    virtual ~DegreeDaysFeedback();
    
    static const std::string& getXMLNameStatic();
    
    // INamed methods
    virtual const std::string& getName() const;
    
    // IParsable methods
    virtual bool XMLParse( const xercesc::DOMNode* aNode );
    
    // IRoundTrippable methods
    virtual void toInputXML( std::ostream& aOut, Tabs* aTabs ) const;
    
    // IModelFeedbackCalc methods
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );
    
    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );
    
    // call back methods for GCAMFusion
    template<typename T>
    void processData( T& aData );
    
    void pushFilterStep( INamed* const& aContainer );
    
    template<typename T>
    typename boost::disable_if<
        boost::is_base_of<INamed, typename boost::remove_pointer<T>::type>,
    void>::type pushFilterStep( const T& aContainer );
    
protected:
    //! The name of this feedback
    std::string mName;
    
    //! A HDD feedback coefficient of sorts
    double mHDDCoef;
    
    //! A CDD feedback coefficient of sorts
    double mCDDCoef;
    
    //! The base year uhh.. value to calculate feedback from
    double mBaseYearValue;
    
    //! Store the degree day scaler for use in the call back from GCAMFusion to
    //! adjust the actual HDD/CDD values.
    double mCurrDDScaler;
    
    struct GatherEmiss {
        double mEmiss = 0;
        // call back method for GCAMFusion
        template<typename T>
        void processData( T& aData );
    };
};

#endif // _IMODEL_FEEDBACK_CALC_H_
