/*! 
* \file summary.cpp
* \ingroup Objects
* \brief summary class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include "util/base/include/summary.h"
#include "util/base/include/util.h"
#include "containers/include/world.h"
#include "containers/include/scenario.h"
#include <vector>

using namespace std;

extern Scenario* scenario;


//! Default constructor
/*! \todo This class needs to be documented.
*   \todo Redesign using a map of maps or other improved datastructure.
*/
Summary::Summary() {
}

void Summary::initfuelcons( const string& fname, const double value ){

	fuelcons[ fname ] += value;
	fuelcons[ "zTotal" ] += value;
}

void Summary::initpeprod( const string& fname, const double value ){
	peprod[ fname ] += value;
	peprod[ "zTotal" ] += value;
}

const Summary::SummaryItem& Summary::getfuelcons() const {
	return fuelcons;
}

const Summary::SummaryItem& Summary::getpecons() const {	
	return pecons;
}

const Summary::SummaryItem& Summary::getpetrade() const {
	return petrade;
}

const Summary::SummaryItem& Summary::getemission() const {
	return emission;
}

const Summary::SummaryItem& Summary::getemfuelmap() const {
	return emissfuel;
}

//! return map of sequestered amount of emissions
const Summary::SummaryItem& Summary::getSequesteredAmountMap() const {
	return sequesteredAmount;
}

//! Add the passed fuelmap to the summary fuelinfo map 
/* The consumption values in the fuelinfo map that is passed are added 
to the summary object maps fuelcons and pecons.

The iterator fmap is used to traverse the fuelinfo map.
*/ 
void Summary::updatefuelcons( const list<string>& aPrimaryFuelList, const SummaryItem& fuelinfo ) {
	// map all primary and secondary fuel consumption
	for (CSummaryIterator fmap=fuelinfo.begin(); fmap!=fuelinfo.end(); ++fmap) {	// iterate to one less than the end
		fuelcons[fmap->first] += fmap->second; // Add values from the passed map to fuelcons
        // Don't need a zTotal b/c the fuels are uncomparable. 
	}

	// map primary energy consumption only.
   for( list<string>::const_iterator fuelIter = aPrimaryFuelList.begin();
	   fuelIter != aPrimaryFuelList.end(); ++fuelIter )
   {
	   CSummaryIterator fmap=fuelinfo.find( *fuelIter );
	   if( fmap!=fuelinfo.end() ) {
		   pecons[fmap->first] += fmap->second;
		   pecons["zTotal"] += fmap->second;
	   }
   }
}

void Summary::updatepetrade() {
	// map all primary and secondary fuel consumption
	for ( CSummaryIterator fmap = peprod.begin(); fmap != peprod.end(); ++fmap ) {
		petrade[ fmap->first ] = peprod[ fmap->first ] - pecons[ fmap->first ];
	}
}

void Summary::updateemiss( const SummaryItem& ghginfo ) {
	// map all primary and secondary fuel consumption
	for ( CSummaryIterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap){
		emission[ fmap->first ] += fmap->second;
	}
}

void Summary::updateemfuelmap( const SummaryItem& ghginfo ) {
	// map all primary and secondary fuel consumption
	for ( CSummaryIterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap ) {
		emissfuel[ fmap->first ] += fmap->second;
	}
}

//! update the map of sequestered amount of emissions
void Summary::updateSequesteredAmountMap( const SummaryItem& ghginfo ) {
	// map sequestered amount of CO2 for secondary fuels and zTotal
	for ( CSummaryIterator fmap = ghginfo.begin(); fmap != ghginfo.end(); ++fmap ) {
		sequesteredAmount[ fmap->first ] += fmap->second;
	}
}

void Summary::clearfuelcons() {
	fuelcons.clear();
	pecons.clear();
}

void Summary::clearpeprod() {
	peprod.clear();
	petrade.clear();
}

void Summary::clearemiss() {
	emission.clear();
}

void Summary::clearemfuelmap() {
	emissfuel.clear();
}

//! clear out map of sequestered amount
void Summary::clearSequesteredAmountMap() {
	sequesteredAmount.clear();
}
/*! \todo Fix all these names. Should not include 'second'.*/
double Summary::get_fmap_second( const string& name ) const {
    return util::searchForValue( fuelcons, name );
}

double Summary::get_pemap_second( const string& name ) const {
	return util::searchForValue( pecons, name );
}

double Summary::get_petrmap_second( const string& name ) const {
	return util::searchForValue( petrade, name );
}

double Summary::get_peprodmap_second( const string& name ) const {
	return util::searchForValue( peprod, name );
}

double Summary::get_emissmap_second( const string& name ) const {
	return util::searchForValue( emission, name );
}

//! return the sequestered amount which is second part of the map
double Summary::getSequesteredAmount( const string& name ) const {
	return util::searchForValue( sequesteredAmount, name );
}

double Summary::get_emissfuelmap_second( const string& name ) const {
	return util::searchForValue( emissfuel, name );
}
