#ifndef _SUPPLY_DEMAND_CURVE_SAVER_H_
#define _SUPPLY_DEMAND_CURVE_SAVER_H_
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
* \file supply_demand_curve_saver.h
* \ingroup Objects
* \brief The SupplyDemandCurveSaver class header file.
* \author Rich Plevin
*/

#include <cassert>
#include <vector>
#include <iostream>
#include "containers/include/imodel_feedback_calc.h"

class SolutionInfo;

/*!
 * \brief Writes out supply & demand curves for user-designated markets after each
 *        model period has solved.
 * \details Users can configure via XML parse
 *   - mName The market name to create the Supply/Demand curves for
 *   - mPrices A vector of prices at which to calculate Supply / Demand points.
 *   - mIsPricesRelative A flag to indicate if mPrices are relative to the solved price in each model period.
 *   All results will be written to the output file specified in the Configuration
 *   parameter "supplyDemandCurves".  Note that the file will get reset the first
 *   time any instanve of this class needs to write to the file and will append to
 *   it thereafter.
 *
 * \sa SupplyDemandCurve
 * \author Rich Plevin
 */
class SupplyDemandCurveSaver : public IModelFeedbackCalc
{
public:
    SupplyDemandCurveSaver();
    virtual ~SupplyDemandCurveSaver();
    
    static const std::string& getXMLNameStatic();
    
    // INamed methods
    virtual const std::string& getName() const;
     
    // IModelFeedbackCalc methods
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario,
                                            const IClimateModel* aClimateModel,
                                            const int aPeriod );
    
    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario,
    					                   const IClimateModel* aClimateModel,
                                           const int aPeriod );

protected:
    //! The name of this feedback
    std::string mName;
    
    //! A flag indicating if a user wanted the mPrices to be relative to the 
    //! the solved price for the configured market in each model period.
    bool mIsPricesRelative;
    
    //! The price points for which we should calculate supply and demands
    std::vector<double> mPrices;

    //! A flag to help us determine if we need to reset the output file (the
    //! first time around) or simply append to it.
    static std::ios_base::openmode mOpenMode;

    void printCSV( std::ostream& aOut, Scenario* aScenario, const int aPeriod, bool aPrintHeader );

    int getMarketIndex(const std::string& aMarketName, std::vector<SolutionInfo> &aSolvable );

};

#endif // _SUPPLY_DEMAND_CURVE_SAVER_H_

