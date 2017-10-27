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


#ifndef _NEGATIVE_EMISSIONS_FINAL_DEMAND_H_
#define _NEGATIVE_EMISSIONS_FINAL_DEMAND_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file negative_emissions_final_demand.h
 * \ingroup Objects
 * \brief The NegativeEmissionsFinalDemand class header file.
 * \author Pralit Patel
 */

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/afinal_demand.h"
#include "util/base/include/value.h"
#include "util/base/include/time_vector.h"

// Forward declarations
class GDP;
class Demographic;

/*! 
 * \ingroup Objects
 * \brief A class which calculates the total negative emissions for a gas and
 *        adds the value to the demand of a configured policy market.
 * \details TODO
 */

class NegativeEmissionsFinalDemand: public AFinalDemand
{

public:
    static const std::string& getXMLNameStatic();

    NegativeEmissionsFinalDemand();

    virtual ~NegativeEmissionsFinalDemand();

    virtual bool XMLParse( const xercesc::DOMNode* aNode );

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    virtual const std::string& getName() const;
    
    virtual void completeInit( const std::string& aRegionName,
                               const IInfo* aRegionInfo );

    virtual void initCalc( const std::string& aRegionName,
                           const GDP* aGDP,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void setFinalDemand( const std::string& aRegionName,
                                 const Demographic* aDemographics,
                                 const GDP* aGDP,
                                 const int aPeriod );

    virtual double getWeightedEnergyPrice( const std::string& aRegionName,
                                           const int aPeriod ) const;

    virtual void csvOutputFile( const std::string& aRegionName ) const;

    virtual void dbOutput( const std::string& aRegionName ) const;

    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
protected:
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        AFinalDemand,
    
        //! Name of the gas to track
        DEFINE_VARIABLE( SIMPLE, "name", mName, std::string ),

        //! Name of the policy market to add negative emissions value to.
        DEFINE_VARIABLE( SIMPLE, "policy-name", mPolicyName, std::string ),

        //! State value necessary to use Marketplace::addToDemand
        DEFINE_VARIABLE( SIMPLE | STATE, "curr-negative-emiss-value", mCurrNegEmissValue, Value )
    )
    
    virtual const std::string& getXMLName() const;
};

#endif // _NEGATIVE_EMISSIONS_FINAL_DEMAND_H_

