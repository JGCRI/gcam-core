/*! 
* \file population_sgm_fixed.cpp
* \ingroup Objects
* \brief PopulationSGMFixed class source file.
* \author Sonny Kim
* \author Katherine Chung
*/

#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "demographics/include/population_sgm_fixed.h"
#include "demographics/include/age_cohort.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/ivisitor.h"
#include "util/logger/include/ilogger.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string PopulationSGMFixed::XML_NAME = "populationSGMFixed";

//! Default constructor.
PopulationSGMFixed::PopulationSGMFixed() : Population() {
}

//! PopulationSGMFixed destructor. 
PopulationSGMFixed::~PopulationSGMFixed(){
    clear();
}

//! Helper member function for the destructor. Performs memory deallocation. 
void PopulationSGMFixed::clear(){
    for(AgeCohortIterator acIter = ageCohort.begin(); acIter != ageCohort.end(); ++acIter){
        delete *acIter;
    }
}

//! parses rest of PopulationSGMFixed xml object
bool PopulationSGMFixed::XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ){

    if( nodeName == AgeCohort::getXMLNameStatic() ) {
        ageCohort.push_back( new AgeCohort() );
        ageCohort.back()->XMLParse( curr );
    }
    else {
        return false;
    }
    return true;
}

//! Write out rest of datamembers to XML output stream.
void PopulationSGMFixed::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const{
    for( vector<AgeCohort *>::const_iterator i = ageCohort.begin(); i != ageCohort.end(); i++ ){
        ( *i )->toInputXML( out, tabs );
    }
}

//! Write out XML for debugging purposes.
void PopulationSGMFixed::toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const{
    for( vector<AgeCohort *>::const_iterator i = ageCohort.begin(); i != ageCohort.end(); i++ ){
        ( *i )->toDebugXML( out, tabs );
    }
}

//! Complete the initialization.
void PopulationSGMFixed::completeInit( const vector<double>& femalePopFromPrev, const vector<double>& malePopFromPrev ){
    calcPop();
}

//! initialize anything that won't change during the calcuation
void PopulationSGMFixed::initCalc(){
}

//! returns total working age population (ages 15-65)
double PopulationSGMFixed::getWorkingAgePop() const { // ages 15-65
    return getWorkingAgePopMale() + getWorkingAgePopFemale();
}

//! returns male working age population (ages 15-65)
/* \todo Handle age bounds not exactly equal to min and max working age.
*/
double PopulationSGMFixed::getWorkingAgePopMale() const{ // ages 15-65
    double workAgePop = 0;
    // Iterate over the cohorts and add populations between ages 15 and 65.
    for( CAgeCohortIterator cohort = ageCohort.begin(); cohort != ageCohort.end(); ++cohort ){
        int lowerAgeBound = (*cohort)->getLowerAgeBound();
        int upperAgeBound = (*cohort)->getUpperAgeBound();
        if( ( lowerAgeBound != -1 && lowerAgeBound >= mWorkingAgeMin ) &&
            ( upperAgeBound != -1 && upperAgeBound <= mWorkingAgeMax ) ){
                workAgePop += (*cohort)->getMalePop();
            }
    }
    return workAgePop;
}

//! returns female working age population (ages 15-65)
/*! \todo Make min and max working age read in.
* \todo Handle age bounds not exactly equal to min and max working age.
*/
double PopulationSGMFixed::getWorkingAgePopFemale() const{ // ages 15-65
    double workAgePop = 0;
    // Iterate over the cohorts and add populations between ages 15 and 65.
    for( CAgeCohortIterator cohort = ageCohort.begin(); cohort != ageCohort.end(); ++cohort ){
        int lowerAgeBound = (*cohort)->getLowerAgeBound();
        int upperAgeBound = (*cohort)->getUpperAgeBound();
        if( ( lowerAgeBound != -1 && lowerAgeBound >= mWorkingAgeMin ) &&
            ( upperAgeBound != -1 && upperAgeBound <= mWorkingAgeMax ) ){
                workAgePop += (*cohort)->getFemalePop();
            }
    }
    return workAgePop;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& PopulationSGMFixed::getXMLName() const{
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& PopulationSGMFixed::getXMLNameStatic() {
    return XML_NAME;
}

//! Adds up the male and female populations for each cohort and gets a total
void PopulationSGMFixed::calcPop(){
    double totMalePop = 0;
    double totFemalePop = 0;
    for ( CAgeCohortIterator cohort = ageCohort.begin(); cohort != ageCohort.end(); ++cohort ) {
        totMalePop += (*cohort)->getMalePop();
        totFemalePop += (*cohort)->getFemalePop();
    }    
    mTotalPop = totMalePop + totFemalePop;  
}

void PopulationSGMFixed::csvSGMOutputFile( ostream& aFile, const int period ) const {
     aFile << "Population Data by Age Cohort Total" << endl << endl;
     aFile << "Males- " << mYear << endl;
     for( CAgeCohortIterator it = ageCohort.begin(); it != ageCohort.end(); it++ ) {
         aFile << ( *it )->getAgeGroup() << ',' << ( *it )->getMalePop() << endl;
     }
     aFile << endl << "Females- " << mYear << endl;
     for( CAgeCohortIterator it = ageCohort.begin(); it != ageCohort.end(); it++ ) {
         aFile << ( *it )->getAgeGroup() << ',' << ( *it )->getFemalePop() << endl;
     }
}

/*! \brief Update a visitor with information about a SGM fixed cohort population.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void PopulationSGMFixed::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitPopulationSGMFixed( this, aPeriod );
	// Call the parent class visit.
	Population::accept( aVisitor, aPeriod );
	// Update the cohorts.
	for( unsigned int i = 0; i < ageCohort.size(); ++i ){
		ageCohort[ i ]->accept( aVisitor, aPeriod );
	}
	aVisitor->endVisitPopulationSGMFixed( this, aPeriod );
}
