/*!
 * smooth_renewable_subresource.cpp
 * Created: 02/02/2007
 * Version: 02/21/2007
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

// include files ***********************************************************

#include "util/base/include/definitions.h"

#include "resources/include/smooth_renewable_subresource.h"

#include <cassert>
#include <cmath>

// SmoothRenewableSubresource::sXMLName ************************************

const std::string SmoothRenewableSubresource::sXMLName
   = "smooth-renewable-subresource";

// Constructor: SmoothRenewableSubresource: ********************************

SmoothRenewableSubresource::SmoothRenewableSubresource(void)
   : parent(),
     mCostCurve(),
     mPriceExponent( 0.01 )
{
}

// Destructor: SmoothRenewableSubresource **********************************

SmoothRenewableSubresource::~SmoothRenewableSubresource(void)
{
}

// SmoothRenewableSubresource::annualsupply ********************************

void SmoothRenewableSubresource::annualsupply(
   int        aPeriod,
   const GDP* aGDP,
   double     aPrice,
   double     aPrevPrice )
{
   // Compute the fraction of the total possible supply that is
   // available at a given price
   double fractionAvailable = mCostCurve( aPrice );

   // Make supply increase continuously with price to improve convergence.
   fractionAvailable *=
      std::pow( ( 1 + ( aPrice / 2.0 ) ), mPriceExponent );

   // Calculate expansion in supply due to GDP increase
   double gpdSupplyExpansion = std::pow(
      aGDP->getApproxGDP( aPeriod ) / aGDP->getApproxGDP( 0 ),
      gdpSupplyElasticity );

   // now convert to absolute value of production
   annualprod[ aPeriod ] =
      fractionAvailable * maxSubResource * gpdSupplyExpansion;
}

// SmoothRenewableSubresource::completeInit ********************************

void SmoothRenewableSubresource::completeInit( const IInfo* aSectorInfo )
{
#if !defined( _MSC_VER )
   using std::exit;
#endif

   parent::completeInit( aSectorInfo );

   if ( !( mCostCurve.getMidprice() > 0 && mCostCurve.getCurveExponent() > 0 ) )
   // Invalid input parameter
   {
      ILogger& mainLog = ILogger::getLogger( "main_log" );
      mainLog.setLevel( ILogger::ERROR );
      mainLog << "Invalid input parameter(s) to "
         << getXMLNameStatic() << std::endl;
      exit( -1 );
   }
}

// SmoothRenewableSubresource::getXMLName **********************************

const std::string& SmoothRenewableSubresource::getXMLName( void ) const
// Pre:
// Modifies:
// Post: Return the XML tag name
{
   return getXMLNameStatic();
}

// SmoothRenewableSubresource::getXMLNameStatic ****************************

const std::string& SmoothRenewableSubresource::getXMLNameStatic( void )
// Pre:
// Modifies:
// Post: Return the XML tag name
{
   return sXMLName;
}

// SmoothRenewableSubresource::toXMLforDerivedClass ************************

void SmoothRenewableSubresource::toXMLforDerivedClass(
   std::ostream& out,
   Tabs*         tabs ) const
{
   parent::toXMLforDerivedClass( out, tabs );

   XMLWriteElementCheckDefault(
      mCostCurve.getMidprice(), "mid-price", out, tabs, 0.0 );
   XMLWriteElementCheckDefault(
      mCostCurve.getCurveExponent(), "curve-exponent", out, tabs, 0.0 );
   XMLWriteElementCheckDefault(
      mPriceExponent, "price-exponent", out, tabs, 0.01 );
}

// SmoothRenewableSubresource::XMLDerivedClassParse ************************

bool SmoothRenewableSubresource::XMLDerivedClassParse(
   const std::string&      nodeName,
   const xercesc::DOMNode* node )
{
   bool didParse = parent::XMLDerivedClassParse( nodeName, node );
   if ( !didParse )
   {
      if( nodeName == "mid-price" )
      {
		   mCostCurve.setMidprice( XMLHelper<double>::getValue( node ) );
		   didParse  = true;
	   }
	   else if( nodeName == "curve-exponent" )
      {
		   mCostCurve.setCurveExponent( XMLHelper<double>::getValue( node ) );
		   didParse       = true;
	   }
      else if ( nodeName == "price-exponent" )
      {
         mPriceExponent = XMLHelper<double>::getValue( node );
         didParse       = true;
      }
   }

   return didParse;
}

// end of smooth_renewable_subresource.cpp *********************************

