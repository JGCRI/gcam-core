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
 * \author Kevin Walker
 * \date $ Date $
 * \version $ Revision $
 */
class SmoothRenewableSubresource : public SubRenewableResource
{
public :

   typedef SubRenewableResource  parent;

   // Constructor
   SmoothRenewableSubresource(void);

   // Destructor
   virtual ~SmoothRenewableSubresource(void);

   // Documentation is inherited.
   virtual void annualsupply(
      int        aPeriod,
      const GDP* aGDP,
      double     aPrice,
      double     aPrevPrice );

   // Documentation is inherited.
	virtual void completeInit( const IInfo* aSectorInfo );

   //! Return the XML tag name
   static const std::string& getXMLNameStatic( void );

protected :

   //! SmoothRenewableSubresource
   static const std::string   sXMLName;

   //! The cost curve calculator
   ObjECTS::TCostCurve<> mCostCurve;

   //! Multiplier price increase
   double mPriceExponent;

   // Documentation is inherited.
   virtual const std::string& getXMLName() const;

   // Documentation is inherited.
   virtual void toXMLforDerivedClass(
      std::ostream& out,
      Tabs*         tabs ) const;

   // Documentation is inherited.
 	virtual bool XMLDerivedClassParse(
      const std::string&      nodeName,
      const xercesc::DOMNode* node );
};

#endif   // __SMOOTH_RENEWABLE_SUBRESOURCE_H

// end of smooth_renewable_subresource.h ***********************************

