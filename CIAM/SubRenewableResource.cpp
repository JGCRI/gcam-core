/*! 
* \file SubRenewableResource.cpp
* \ingroup CIAM
* \brief SubRenewableResource class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <vector>
#include <string>
#include <iostream>
#include <cassert>
#include "scenario.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "Grade.h"
#include "SubRenewableResource.h"
#include "SubResource.h"

using namespace std;

extern Scenario* scenario;

//! NOTE SURE ABOUT what to do with THIS
//void SubRenewableResource::updateAvailable( const int period ){

//! Performs XML read-in that is specific to this derived class
void SubRenewableResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {
   if( nodeName == "maxSubResource" ){
      maxSubResource = XMLHelper<double>::getValue( node );
   }
   else if( nodeName == "gdpSupplyElast" ){
      gdpSupplyElasticity = XMLHelper<double>::getValue( node );
   }
}

//! Do any initializations needed for this resource
/*! Renewable resources should have only grades with well defined cost curves. 
Also remove any grades with zero available by resetting the parameter nograde. */
void SubRenewableResource::initializeResource( ) {   
   int tempNumGrades = 1; 
   // Start with at least one grade. Then see if others are in order above it.
   bool badGradeFound = false;
   
   // Note that available only exists in period 0 at present data structure.
   for ( int i = 1; i < nograde && !badGradeFound; i++ ) {
      if ( ( grade[ i ]->getAvail() > 0 ) && ( grade[ i ]->getAvail() > grade[ i - 1 ]->getAvail() ) ) {
         tempNumGrades++;
      }
      else {
         badGradeFound = true;
      }
   }
   nograde = tempNumGrades;
}

//! Write out to XML variables specific to this derived class
void SubRenewableResource::toXMLforDerivedClass( ostream& out ) const {
   
   XMLWriteElement( maxSubResource, "maxSubResource", out );
   XMLWriteElement( gdpSupplyElasticity, "gdpSupplyElast", out );
   
}   

//! Write out to XML variables specific to this derived class
void SubRenewableResource::toOutputXMLforDerivedClass( ostream& out ) const {
   
   XMLWriteElement( maxSubResource, "maxSubResource", out );
   XMLWriteElement( gdpSupplyElasticity, "gdpSupplyElast", out );
   
}   

//! Returns the type of the Resource.
string SubRenewableResource::getType() const {
   return "Renewable";
}

//! Cumulative Production
/*! Cumulative production  Is not needed for renewable resources. But still do any preliminary calculations that need to be done before calculating production */
void SubRenewableResource::cumulsupply( double prc, int per ) {   
   const Modeltime* modeltime = scenario->getModeltime();
      
   // calculate total extraction cost for each grade
   // This is a waste of time, should only do this once!
   for ( int gr=0; gr<nograde; gr++ ) {
      if ( per > 0 ) {
         cumulativeTechChange[ per ] = cumulativeTechChange[ per - 1 ] * 
            pow( ( 1.0 + techChange[ per ] ), modeltime->gettimestep( per ) );
      }
      // Determine cost
      grade[ gr ]->calcCost( severanceTax[ per ],cumulativeTechChange[ per ], environCost[ per ], per );
   }   
}

//! calculate annual supply 
/*! Annual production (supply) is placed into variable (into variable annualprod[]).
* For renewable resources interprets parameters as a cost curve.
* Technological change is applied if present. 
* Note that the cost curve needs to be in the form of price, and cumulative fraction available.
*/
void SubRenewableResource::annualsupply( int period, double gnp, double prev_gnp, double price, double prev_price ) {
   
   const Modeltime* modeltime = scenario->getModeltime();
   double gradeAvail;
   double gradeCost;
   double prevGradeCost = grade[ 0 ]->getCost(period - 1); // Minimum cost for any production
   double prevGradeAvail = grade[ 0 ]->getAvail(); // Lowest fraction available
   double gradeFraction;
   
   // default value
   annualprod[ period ] = 0;
   
   // Make sure base GDP has been stored
   if ( period == 0 ) {
      baseGDP = gnp;
   }
   
   // if below minimum cost
   if ( price < prevGradeCost ) {
      annualprod[ period ] = 0;
      return;
   }
   
   //! \todo perhaps turn production into a function -- arguments period, gnp, price;
   
   // To save time without doing the loop, check first to see if price is above max price point
   if ( price > grade[ nograde - 1 ]->getCost(period) ) {
      gradeFraction = grade[ nograde - 1 ]->getAvail();
      annualprod[ period ] = grade[ nograde - 1 ]->getAvail() 
         * maxSubResource  * pow( gnp / baseGDP, gdpSupplyElasticity );
   }
   else {
      bool pricePointFound = false;
      // Move up cost curve until reach price
      for ( int increment = 1; increment < nograde && !pricePointFound; increment++ ) {
         gradeAvail = grade[ increment ]->getAvail();
         gradeCost = grade[ increment ]->getCost(period);
         
         // if have reached the appropriate point in cost curve, calculate production
         if ( price <= gradeCost && price > prevGradeCost ) {
            // how far up this segement
            gradeFraction = ( price - prevGradeCost ) /  ( gradeCost - prevGradeCost ); 
            
            // compute production as fraction of total possible
            annualprod[ period ] = prevGradeAvail + 
               gradeFraction * ( gradeAvail - prevGradeAvail ); 
            
            // now convert to absolute value of production
            annualprod[ period ] *= maxSubResource * pow( gnp/baseGDP, gdpSupplyElasticity ); 
            
            pricePointFound = true; // can exit loop now
         } 
         else {
            prevGradeCost = gradeCost;
            prevGradeAvail = gradeAvail;
         } // end price if-else block
      } // end loop
   } // end else block
   
   return;
}

