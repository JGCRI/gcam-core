#ifndef _NODE_CARBON_CALC_H_
#define _NODE_CARBON_CALC_H_
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
 * \file node_carbon_calc.h
 * \ingroup Objects
 * \brief The NodeCarbonCalc class header file.
 * \author Pralit Patel
 */
#include <boost/core/noncopyable.hpp>

#include "ccarbon_model/include/icarbon_calc.h"
#include "util/base/include/default_visitor.h"
#include "util/base/include/data_definition_util.h"

class LandUseHistory;
class Tabs;

/*!
 * \brief A carbon calculator which simply drive the carbon calculation from leaves
 *        contained below the node that are "swappable"
 * \details which would include such items
 *          as managed and unmanaged Forests.  If the model decided to swap one for the
 *          other all of the carbon should not be emitted and then regrown.  Rather the
 *          marginal difference in carbon should be emitted/uptaken.  The calculation
 *          the follows the following steps:
 *              - Compute the change in total land in this node.
 *              - Allocate changes in total land area preferentially adding land to the
 *                most carbon dense land first when the total increase and conversely removing
 *                land from the least carbon dense lands first when it decreases.
 *              - The differences in land area left are all between the land options with in 
 *                this node carbon calc and so we just allocate the net change in carbon between
 *                these options.
 * \warning This class works in conjunction with the NoEmissCarbonCalc which must be
 *          the type of carbon calculation for all leaves in this Node that are "swappable"
 *          so that the carbon calculations are not incorrectly calculated twice.
 */
class NodeCarbonCalc: public DefaultVisitor,
                      private boost::noncopyable
{
public:
    NodeCarbonCalc();
    virtual ~NodeCarbonCalc();

    static const std::string& getXMLNameStatic();

    // IStandardComponent methods
    virtual void toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const;

    // DefaultVisitor methods
    virtual void startVisitNoEmissCarbonCalc( const NoEmissCarbonCalc* aNoEmissCarbonCalc, const int aPeriod );
    
    void completeInit();
    
    void initCalc( const int aPeriod );
    
    void calc( const int aPeriod, const int aEndYear, const ICarbonCalc::CarbonCalcMode aCalcMode );
    
protected:
    
    DEFINE_DATA(
        /*! \brief NodeCarbonCalc is the only member of this container hierarchy. */
        DEFINE_SUBCLASS_FAMILY( NodeCarbonCalc )
                
        // TODO: should any of these member variables be accessible through introspection?
    )
    
    //! The carbon leaves that will drive the carbon calculations at this node.
    std::vector<NoEmissCarbonCalc*> mCarbonCalcs;

    //! If the historical emissions have already been calculated.
    bool mHasCalculatedHistoricEmiss;
    
    //! An index to the carbon calculations sorted from lowest to highest above
    //! ground carbon densities.
    std::vector<size_t> mIndLowToHigh;
    
    //! An index to the carbon calculations sorted from highest to lowest above
    //! ground carbon densities.
    std::vector<size_t> mIndHighToLow;
    
    void calcLandUseHistory();
    
    bool hasSameSign( const double aValue1, const double aValue2 ) const;
};

#endif // _NODE_CARBON_CALC_H_
