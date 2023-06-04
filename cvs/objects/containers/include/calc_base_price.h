#ifndef _CALC_BASE_PRICE_H_
#define _CALC_BASE_PRICE_H_
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
 * \file calc_base_price.h
 * \ingroup Objects
 * \brief The CalcBasePrice class header file.
 * \author Pralit Patel
 */

#include <boost/core/noncopyable.hpp>
#include <string>
#include <map>

#include "containers/include/imodel_feedback_calc.h"
#include "util/base/include/aparsable.h"

class Scenario;
class IClimateModel;
class ITechnology;

/*!
 * \ingroup Objects
 * \brief A helper object to be able to calculate average aggregate sector prices, particularly
 *        useful to work around "pass through" sectors.
 * \details This object will parse from XML a mapping of sectors to aggregate (or just calculate
 *          a "base price" for).  To do so it will aggregate currency output and physical output by
 *          aggregate sector then simply multiply currency by physical output.  It will then save the
 *          calculated "base-price" into the market info object for each individual sector in the
 *          mapping.  Note: the only reason for this to be a model feedback is that it can get
 *          called at an appropriate time (postCalc).
 *
 * \author Pralit Patel
 */
class CalcBasePrice : public IModelFeedbackCalc, public AParsable
{
public:
    CalcBasePrice();
    virtual ~CalcBasePrice();

    static const std::string& getXMLNameStatic();
    
    const std::string& getXMLName() const;

    // INamed methods
    virtual const std::string& getName() const;

    // AParsable methods
    bool XMLParse( rapidxml::xml_node<char>* & aNode );
    
    // IModelFeedbackCalc methods
    // Only interested in "after period"
    virtual void calcFeedbacksBeforePeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod ) { }
    
    virtual void calcFeedbacksAfterPeriod( Scenario* aScenario, const IClimateModel* aClimateModel, const int aPeriod );

    // GCAMFusion callback methods
    template<typename ContainerType>
    void pushFilterStep( const ContainerType& aData );

    template<typename ContainerType>
    void popFilterStep( const ContainerType& aData );
    
    template<typename DataType>
    void processData( DataType& aData );
private:

    //! The name of the object
    std::string mName;
    
    //! The mapping of GCAM sectors names to an aggregate sector by which to calculate a base price
    std::map<std::string, std::string> mSectorNameMap;
    
    //! A mapping of aggregate sector name to a pair of currency and physical outputs which are being
    //! aggregated.
    std::map<std::string, std::pair<double, double> > mMappedSectorOutputs;

    //! The current model period
    int mPeriod;

    //! The region name which will get set as GCAMFusion searches for the data to aggregate
    std::string mCurrRegionName;

    //! The GCAM sector name which will get set as GCAMFusion searches for the data to aggregate
    std::string mCurrSectorName;

    //! The current technology which will get set as GCAMFusion searches for the data to aggregate
    const ITechnology* mCurrTech;

    //! A flag to let us know if we did indeed find data in which case we can go ahead and set the
    //! base-price value into the current sector's market info
    bool mDidSetValue;

    //! Save currency output by sector to back calculate base sector price which gets set as
    //! GCAMFusion searches for the data to aggregate
    double mCurrencyOutput;

    //! Save physical output by sector to back calculate base sector price which gets set as
    //! GCAMFusion searches for the data to aggregate
    double mPhysicalOutput;
};

#endif // _CALC_BASE_PRICE_H_
