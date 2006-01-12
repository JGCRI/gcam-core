/*! 
* \file population_sgm_rate.cpp
* \ingroup Objects
* \brief PopulationSGMRate class source file.
* \author Sonny Kim
* \author Katherine Chung
*/
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include "demographics/include/population_sgm_rate.h"
#include "demographics/include/age_cohort.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize
const string PopulationSGMRate::XML_NAME = "populationSGMRate";

//! Default constructor.
PopulationSGMRate::PopulationSGMRate() {
}

//! PopulationSGMRate destructor. 
PopulationSGMRate::~PopulationSGMRate() {
    clear();
}

//! Helper member function for the destructor. Performs memory deallocation. 
void PopulationSGMRate::clear(){
    for( AgeCohortIterator acIter = ageCohort.begin(); acIter != ageCohort.end(); ++acIter ){
        delete *acIter;
    }
}

//! parses rest of PopulationSGMFixed xml object
bool PopulationSGMRate::XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ){

    if( nodeName == AgeCohort::getXMLNameStatic() ) {
        ageCohort.push_back( new AgeCohort() );
        ageCohort.back()->XMLParse( curr );
    }
    else {
        return false;
    }
    return true;
}

//! returns total working age population (ages 15-65)
/*! \todo Make min and max working age read in.
* \todo Handle age bounds not exactly equal to min and max working age.
*/
double PopulationSGMRate::getWorkingAgePop() const{ // ages 15-65
    return getWorkingAgePopMale() + getWorkingAgePopFemale();
}

//! returns male working age population (ages 15-65)
/*! \todo Make min and max working age read in.
* \todo Handle age bounds not exactly equal to min and max working age.
*/
double PopulationSGMRate::getWorkingAgePopMale() const{ // ages 15-65
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
double PopulationSGMRate::getWorkingAgePopFemale() const{ // ages 15-65
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

//! Write out rest of datamembers to XML output stream.
void PopulationSGMRate::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const{
    for( CAgeCohortIterator i = ageCohort.begin(); i != ageCohort.end(); ++i ){
        ( *i )->toInputXML( out, tabs );
    }
}

//! Write out XML for debugging purposes.
void PopulationSGMRate::toDebugXMLDerived( std::ostream& out, Tabs* tabs ) const{
    for( CAgeCohortIterator i = ageCohort.begin(); i != ageCohort.end(); ++i ){
        ( *i )->toDebugXML( out, tabs );
    }
}

//! Complete the initialization.
void PopulationSGMRate::completeInit( const vector<double>& femalePopFromPrev, const vector<double>& malePopFromPrev ){
    if( !femalePopFromPrev.empty() && !malePopFromPrev.empty() ){
        // Check to make sure that male and female have the same number of sizes.
        assert( femalePopFromPrev.size() == malePopFromPrev.size() );
        fillFemalePop( femalePopFromPrev );
        fillMalePop( malePopFromPrev );
    }
    calcPop();
}

//! initialize anything that won't change during the calcuation
void PopulationSGMRate::initCalc(){
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& PopulationSGMRate::getXMLName() const{
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
const std::string& PopulationSGMRate::getXMLNameStatic() {
    return XML_NAME;
}

// Note: These blocks of code are repeated, we could do something different.
// calculate male population for all age groups
const vector<double> PopulationSGMRate::getSurvMalePop() const {    
    // Vector of values per cohort for the next periods population.
    vector<double> survMalePop( ageCohort.size() );
    
    // loop for all age groups 
    for ( unsigned int currCohort = 0; currCohort < ageCohort.size(); currCohort++ ){
        // calc male births from all female age groups. This calculates the first new cohort.
        survMalePop[ 0 ] += ageCohort[ currCohort ]->calcMaleBirth();
        
        // Special case for last new age cohort.
        if( currCohort == ageCohort.size() - 1 ){
            // Add remaining population to new last age cohort.
            survMalePop[ currCohort ] += ageCohort[ currCohort ]->calcSurvMalePop();
        }
        else {
            survMalePop[ currCohort + 1 ] += ageCohort[ currCohort ]->calcSurvMalePop();
        }
    }
    return survMalePop;
}

// calculate female population for all age groups
const vector<double> PopulationSGMRate::getSurvFemalePop() const {
    // Vector of values per cohort for the next periods population.
    vector<double> survFemalePop( ageCohort.size() );
    
    // loop for all age groups 
    for ( unsigned int currCohort = 0; currCohort < ageCohort.size(); currCohort++ ){
        // calc male births from all female age groups. This calculates the first new cohort.
        survFemalePop[ 0 ] += ageCohort[ currCohort ]->calcFemaleBirth();
        
        // Special case for last new age cohort.
        if( currCohort == ageCohort.size() - 1 ){
            // Add remaining population to new last age cohort.
            survFemalePop[ currCohort ] += ageCohort[ currCohort ]->calcSurvFemalePop();
        }
        else {
            survFemalePop[ currCohort + 1 ] += ageCohort[ currCohort ]->calcSurvFemalePop();
        }
    }
    return survFemalePop;
}
    
void PopulationSGMRate::fillMalePop( const vector<double>& tempMalePop ){ 
    // Check that the new population vector has the same number of elements as we have cohorts.
    assert( tempMalePop.size() == ageCohort.size() );

    // Fill out the cohorts. 
    for( unsigned int i = 0; i < tempMalePop.size(); i++ ) {
        ageCohort[ i ]->setMalePop( tempMalePop[i] );
    }    
}

void PopulationSGMRate::fillFemalePop( const vector<double>& tempFemalePop ){
    // Check that the new population vector has the same number of elements as we have cohorts.
    assert( tempFemalePop.size() == ageCohort.size() );

    // Fill out the cohorts. 
    for( unsigned int i = 0; i < tempFemalePop.size(); i++ ) {
        ageCohort[ i ]->setFemalePop( tempFemalePop[i] );
    }    
}

//! Adds up the male and female populations for each cohort and gets a total
void PopulationSGMRate::calcPop(){
    double totMalePop = 0;
    double totFemalePop = 0;
    for ( CAgeCohortIterator cohort = ageCohort.begin(); cohort != ageCohort.end(); ++cohort ) {
        totMalePop += (*cohort)->getMalePop();
        totFemalePop += (*cohort)->getFemalePop();
    }    
    mTotalPop = totMalePop + totFemalePop;  
}

void PopulationSGMRate::csvSGMOutputFile( ostream& aFile, const int period ) const {
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

/*! \brief Update a visitor with information about a SGM rate based cohort
*          population.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void PopulationSGMRate::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitPopulationSGMRate( this, aPeriod );
	// Call the parent class visit.
	Population::accept( aVisitor, aPeriod );
	// Update the cohorts.
	for( unsigned int i = 0; i < ageCohort.size(); ++i ){
		ageCohort[ i ]->accept( aVisitor, aPeriod );
	}
	aVisitor->endVisitPopulationSGMRate( this, aPeriod );
}
