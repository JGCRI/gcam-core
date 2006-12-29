/*! 
* \file renewable_subresource.cpp
* \ingroup Objects
* \brief SubRenewableResource class source file.
* \author Steve Smith
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
#include "containers/include/iinfo.h"

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
void SubRenewableResource::completeInit( const IInfo* aSectorInfo ) {   
    double lastAvailable = 0;
    for( vector<Grade*>::iterator i = grade.begin(); i != grade.end(); ++i ){
        if( i != grade.begin() && (*i)->getAvail() <= lastAvailable ){
            // Remove the bad grade.
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Removing invalid grade in subresource " << name << "." << endl;
            delete *i;
            grade.erase( i-- ); 
        }
        else {
            lastAvailable = (*i)->getAvail();
        }
    }
    SubResource::completeInit( aSectorInfo );
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
}

//! calculate annual supply 
/*! Annual production (supply) is placed into variable (into variable annualprod[]).
* For renewable resources interprets parameters as a cost curve.
* Technological change is applied if present. 
* Note that the cost curve needs to be in the form of price, and cumulative fraction available.
* Calls calcVariance() method
*/
void SubRenewableResource::annualsupply( int period, const GDP* gdp, double price, double prev_price ) {

    double fractionAvailable = -1;

    // Move up the cost curve until a point is found above the current price.
    for ( unsigned int i = 0; i < grade.size(); ++i ) {
        if( grade[ i ]->getCost( period ) > price ){
            // Determine the cost and available for the previous point. If
            // this is the first point in the cost curve the previous grade
            // is the bottom of the curve.
            double prevGradeCost;
            double prevGradeAvailable;
            if( i == 0 ){
                prevGradeCost = 0;
                prevGradeAvailable = 0;
            }
            else {
                prevGradeCost = grade[ i - 1 ]->getCost( period );
                prevGradeAvailable = grade[ i - 1 ]->getAvail();
            }

            // This should not be able to happen because the above if
            // statement would fail.
            assert( grade[ i ]->getCost( period ) > prevGradeCost );
            double gradeFraction = ( price - prevGradeCost )
                / ( grade[ i ]->getCost( period ) - prevGradeCost ); 
            // compute production as fraction of total possible
            fractionAvailable = prevGradeAvailable + gradeFraction
                                * ( grade[ i ]->getAvail() - prevGradeAvailable ); 

            break;
        }
    }

    // If the fraction available has not been set there is not a point with a
    // cost greater than the price. This means the price is above the curve.
    if( fractionAvailable == -1 ){
        double maxCost = grade[ grade.size() - 1 ]->getCost( period );

        // Use the function 1-1/(p/pMax) to determine the amount of the
        // additional percent to use.
        double additionalFraction = 1 - 1 / ( price / maxCost );

        // Add up to an additional percent to the total resource to create a
        // smooth derivative above the max price.
        const double ADDITIONAL_PERCENT = 0.05;

        // Calculate the total fraction of the max subresource to use. Note that
        // the max fraction available can be more than 100 percent.
        double maxFraction = grade[ grade.size() - 1 ]->getAvail();

        // Determine the maximum available fraction including the additional
        // increment.
        fractionAvailable = maxFraction * ( 1 + additionalFraction * ADDITIONAL_PERCENT );
    }

    // Calculate the GDP expansion constraint.
    double expansionConstraint = pow( gdp->getApproxGDP( period ) / gdp->getApproxGDP( 0 ),
                                      gdpSupplyElasticity );

    // now convert to absolute value of production
    annualprod[ period ] = fractionAvailable * maxSubResource * expansionConstraint;
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

