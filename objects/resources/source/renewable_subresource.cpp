/*! 
* \file renewable_subresource.cpp
* \ingroup Objects
* \brief SubRenewableResource class source file.
* \author Steve Smith
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <vector>
#include <string>
#include <cassert>
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "resources/include/grade.h"
#include "resources/include/renewable_subresource.h"
#include "resources/include/subresource.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

const double GDP_SUPPLY_ELASTICITY_DEFAULT = 0;
//! Constructor

SubRenewableResource::SubRenewableResource(){
	maxSubResource = 0;
	gdpSupplyElasticity = GDP_SUPPLY_ELASTICITY_DEFAULT;
	subResourceVariance = 0;
	subResourceCapacityFactor = 1;
}

//! Performs XML read-in that is specific to this derived class
bool SubRenewableResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node ) {
	bool didParse = false;
	if( nodeName == "maxSubResource" ){
		maxSubResource = XMLHelper<double>::getValue( node );
		didParse = true;
	}
	else if( nodeName == "subResourceVariance" ){
		subResourceVariance = XMLHelper<double>::getValue( node );
		didParse = true;
	}
	else if( nodeName == "subResourceCapacityFactor" ){
		subResourceCapacityFactor = XMLHelper<double>::getValue( node );
		didParse = true;
	}
	else if( nodeName == "gdpSupplyElast" ){
		gdpSupplyElasticity = XMLHelper<double>::getValue( node );
		didParse = true;
	}
	return didParse;
}

//! Do any initializations needed for this resource
/*! Renewable resources should have only grades with well defined cost curves. 
\todo The extra elements in the vector should be removed. 
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
void SubRenewableResource::toXMLforDerivedClass( ostream& out, Tabs* tabs ) const {
	XMLWriteElementCheckDefault( maxSubResource, "maxSubResource", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( gdpSupplyElasticity, "gdpSupplyElast", out, tabs, GDP_SUPPLY_ELASTICITY_DEFAULT );
	XMLWriteElementCheckDefault( subResourceVariance, "subResourceVariance", out, tabs, 0.0 );
	XMLWriteElementCheckDefault( subResourceCapacityFactor, "subResourceCapacityFactor", out, tabs, 1.0 );
}

//! Cumulative Production
/*! Cumulative production Is not needed for renewable resources. But still do
*   any preliminary calculations that need to be done before calculating
*   production 
*/
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
* Calls calcVariance() method
*/
void SubRenewableResource::annualsupply( int period, const GDP* gdp, double price, double prev_price ) {

	double gradeAvail;
	double gradeCost;

	double prevGradeAvail = grade[ 0 ]->getAvail(); // Lowest fraction available
	double gradeFraction;

	// default value
	annualprod[ period ] = 0;

	// if below minimum cost
	double prevGradeCost = 0; // Minimum cost for any production
	if( period > 0 ){
		prevGradeCost = grade[ 0 ]->getCost(period - 1);
	}

	if ( price < prevGradeCost ) {
		annualprod[ period ] = 0;
		return;
	}

	//! \todo perhaps turn production into a function -- arguments period, gdp, price;

	double currentApproxGDP = gdp->getApproxGDP( period );

	// To save time without doing the loop, check first to see if price is above max price point
	if ( price > grade[ nograde - 1 ]->getCost(period) ) {
		gradeFraction = grade[ nograde - 1 ]->getAvail();
		annualprod[ period ] = grade[ nograde - 1 ]->getAvail() 
			* maxSubResource  * pow( currentApproxGDP / gdp->getApproxGDP( 0 ), gdpSupplyElasticity );
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
				annualprod[ period ] *= maxSubResource * pow( currentApproxGDP/ gdp->getApproxGDP( 0 ), gdpSupplyElasticity ); 

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

/*! \brief Get the variance.
* \details Return the variance for this subresource.
* \return The variance.
*/
double SubRenewableResource::getVariance() const {
	return subResourceVariance;
}

/*! \brief Get the average capacity factor.
* \details Return the capacity factor for this subresource.
* \return The average capacity factor.
*/
double SubRenewableResource::getAverageCapacityFactor() const {
	return subResourceCapacityFactor;
}

