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
 * smooth_renewable_subresource.h
 * Created: 02/02/2007
 * Version: 03/02/2007
 *
 * This software, which is provided in confidence, was prepared by employees
 * of Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software
 * which should not be copied or otherwise disseminated outside your
 * organization without the express written authorization from Battelle.
 * All rights to the software are reserved by Battelle.   Battelle makes no
 * warranty, express or implied, and assumes no liability or responsibility
 * for the use of this software.
*/

#if !defined( __SMOOTH_RENEWABLE_SUBRESOURCE_H )
#define __SMOOTH_RENEWABLE_SUBRESOURCE_H     // prevent multiple includes

// include files ***********************************************************

#include "util/base/include/xml_helper.h"
#include "util/curves/include/cost_curve.h"
#include "containers/include/gdp.h"
#include "resources/include/renewable_subresource.h"

// class: SmoothRenewableSubresource ***************************************

/*!
 * \ingroup Objects
 * \brief Subclass of SubRenewableResource that has a continuous price
 * function
 *
 *   <b>XML specification for SmoothRenewableSubresource</b>
 *   - XML name: \c smooth-renewable-subresource
 *   - Contained by: Technology
 *   - Parsing inherited from class: SubRenewableResource
 *   - Attributes: none
 *   - Elements:
 *   - \c curve-exponent SmoothRenewableSubresource::mCostCurve.get/setCurveExponent()
 *   - \c mid-price SmoothRenewableSubresource::mCostCurve.get/setMidprice()
 *   - \c price-exponent SmoothRenewableSubresource::mPriceExponent
 *
 * \author Kevin Walker, Sonny Kim
 * \date $ Date $
 * \version $ Revision $
 */
class SmoothRenewableSubresource: public SubRenewableResource
{
    friend class XMLDBOutputter;
public:
    SmoothRenewableSubresource();
    virtual ~SmoothRenewableSubresource();
    //! Return the XML tag name
    static const std::string& getXMLNameStatic();
    virtual void completeInit( const std::string& aRegionName, const std::string& aResourceName,
                               const IInfo* aSectorInfo );
    virtual void initCalc( const std::string& aRegionName, const std::string& aResourceName,
                           const IInfo* aSectorInfo, const int aPeriod );
    virtual void annualsupply( const std::string& aRegionName, const std::string& aResourceName,
                               int aPeriod, const GDP* aGDP, double aPrice );
    virtual double getLowestPrice( const int aPeriod ) const;
    virtual double getHighestPrice( const int aPeriod ) const;

protected :
    
    // Define data such that introspection utilities can process the data from this
    // subclass together with the data members of the parent classes.
    DEFINE_DATA_WITH_PARENT(
        SubRenewableResource,
        
        //! Multiplier price increase
        DEFINE_VARIABLE( SIMPLE, "price-exponent", mPriceExponent, double ),
        
        //! Mid-price for cost curve, assuming no technical change
        DEFINE_VARIABLE( SIMPLE, "mid-price", mMidPrice, double ),
                            
        //! curve exponent from parsing
        DEFINE_VARIABLE( SIMPLE, "curve-exponent", mCurveExponent, double )
    )

   //! The cost curve calculator
   // TODO: is this really necessary?
   ObjECTS::TCostCurve<> mCostCurve;

   // Documentation is inherited.
   virtual const std::string& getXMLName() const;
};
#endif   // __SMOOTH_RENEWABLE_SUBRESOURCE_H

// end of smooth_renewable_subresource.h ***********************************
