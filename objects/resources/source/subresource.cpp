/*! 
* \file subresource.cpp
* \ingroup CIAM
* \brief SubResource class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <vector>
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "util/base/include/model_time.h"
#include "resources/include/subresource.h"
#include "resources/include/grade.h"
#include "util/base/include/xml_helper.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string SubResource::XML_NAME = "subresource";
const double SCALE_FACTOR_DEFAULT = 1;
const double GDP_EXPANS_DEFAULT = 1;

//! Default constructor.
SubResource::SubResource() {
    nograde = 0;
    minShortTermSLimit = 0;
    priceElas = 1;	// default value if not read in
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    cumulativeTechChange.resize( maxper ); // cumulative technical change
    cumulativeTechChange[0] = 1.0;
    annualprod.resize( maxper ); // annual production of subresource
    rscprc.resize( maxper ); // subresource price
    techChange.resize( maxper ); // subresource tech change
    environCost.resize( maxper ); // environmental extraction costs change
    severanceTax.resize( maxper ); // subresource severance tax
    available.resize( maxper ); // total available resource
    cumulprod.resize( maxper ); // cumulative production of subrsrc
    gdpExpans.resize( maxper, GDP_EXPANS_DEFAULT ); // cumulative production of subrsrc
    scaleFactor.resize( maxper, SCALE_FACTOR_DEFAULT );
    
}

//! Destructor.
SubResource::~SubResource() {
    for ( vector<Grade*>::iterator outerIter = grade.begin(); outerIter != grade.end(); outerIter++ ) {
        delete *outerIter;
    }
}

//! Initialize member variables from xml data
void SubResource::XMLParse( const DOMNode* node ){	
    // make sure we were passed a valid node.
    assert( node );
    
    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );
    
    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == "#text" ) {
            continue;
        }
		else if( nodeName == Grade::getXMLNameStatic() ){
            parseContainerNode( curr, grade, gradeNameMap, new Grade() );
        }
        else if( nodeName == "annualprod" ){
            XMLHelper<double>::insertValueIntoVector( curr, annualprod, modeltime );
        }
        else if( nodeName == "minShortTermSLimit" ){
            minShortTermSLimit = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "priceElas" ){
            priceElas = XMLHelper<double>::getValue( curr );
            if ( priceElas == 0 ) {
                cerr << "WARNING: priceElas = 0 in sub-resource: " << name << endl;
            }
        }
        else if( nodeName == "techChange" ){
            XMLHelper<double>::insertValueIntoVector( curr, techChange, modeltime );
        }
        else if( nodeName == "environCost" ){
            XMLHelper<double>::insertValueIntoVector( curr, environCost, modeltime );
        }
        else if( nodeName == "severanceTax" ){
            XMLHelper<double>::insertValueIntoVector( curr, severanceTax, modeltime );
        }
        else if( nodeName == "gdpExpans" ){
            XMLHelper<double>::insertValueIntoVector( curr, gdpExpans, modeltime );
        }
        else if( nodeName == "scaleFactor" ){
            XMLHelper<double>::insertValueIntoVector( curr, scaleFactor, modeltime );
        }
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }  
         else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing SubResource." << endl;
        }
    }
    
}

//! Complete initialization.
void SubResource::completeInit() {
    nograde = static_cast<int>( grade.size() ); // number of grades for each subresource   
    initializeResource(); // Do any initializations needed for this resource
    // update the available resource for period 0
    // this function must be called after all the grades have been parsed and nograde set
    updateAvailable( 0 );
}

//! Blank definition so that don't have to define in derived classes if there is nothing to write out
void SubResource::toXMLforDerivedClass( ostream& out, Tabs* tabs ) const {   
}	

//! Do any initializations needed for this resource
void SubResource::initializeResource( ) {   
}	

//! Write datamembers to datastream in XML format for replicating input file.
void SubResource::toInputXML( ostream& out, Tabs* tabs ) const {
    
    const Modeltime* modeltime = scenario->getModeltime();
    int m = 0;

	XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.

    for(m = 0; m < static_cast<int>(environCost.size() ); m++ ) {
        XMLWriteElementCheckDefault(environCost[m],"environCost",out, tabs, 0.0 ,modeltime->getper_to_yr(m));
    }

    for(m = 0; m < static_cast<int>(gdpExpans.size() ); m++ ) {
        XMLWriteElementCheckDefault(gdpExpans[m],"gdpExpans",out, tabs, GDP_EXPANS_DEFAULT ,modeltime->getper_to_yr(m));
    }

    for(m = 0; m < static_cast<int>(severanceTax.size() ); m++ ) {
        XMLWriteElementCheckDefault(severanceTax[m],"severanceTax",out, tabs, 0.0 ,modeltime->getper_to_yr(m));
    }
    
    // for base year only
    m = 0;
    XMLWriteElementCheckDefault(annualprod[m],"annualprod",out, tabs, 0.0 , modeltime->getper_to_yr(m));
    
    for(m = 0; m < static_cast<int>(techChange.size() ); m++ ) {
        XMLWriteElementCheckDefault(techChange[m],"techChange",out, tabs, 0.0 ,modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(scaleFactor.size() ); m++ ) {
        XMLWriteElementCheckDefault(scaleFactor[m],"scaleFactor",out, tabs, SCALE_FACTOR_DEFAULT,modeltime->getper_to_yr(m));
    }
    
    XMLWriteElementCheckDefault(minShortTermSLimit,"minShortTermSLimit",out, tabs, 0.0  );
    XMLWriteElementCheckDefault(priceElas,"priceElas",out, tabs, 1.0 );
    // finished writing xml for the class members.
    
    // write out anything specific to the derived classes
    toXMLforDerivedClass( out, tabs );
    
    // write out the grade objects.
    for( vector<Grade*>::const_iterator i = grade.begin(); i != grade.end(); i++ ){	
        ( *i )->toInputXML( out, tabs );
    }
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Write datamembers to datastream in XML format for outputting results
void SubResource::toOutputXML( ostream& out, Tabs* tabs ) const {
    
    const Modeltime* modeltime = scenario->getModeltime();
    int m = 0;

	XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.
    
    // write out the grade objects.
    for( vector<Grade*>::const_iterator i = grade.begin(); i != grade.end(); i++ ){	
        ( *i )->toInputXML( out, tabs );
    }
    
    for(m = 0; m < static_cast<int>(annualprod.size() ); m++ ) {
        XMLWriteElement(annualprod[m],"annualprod",out, tabs, modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(gdpExpans.size() ); m++ ) {
        XMLWriteElement(gdpExpans[m],"gdpExpans",out, tabs, modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(techChange.size() ); m++ ) {
        XMLWriteElement(techChange[m],"techChange",out, tabs, modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(environCost.size() ); m++ ) {
        XMLWriteElement(environCost[m],"environCost",out, tabs, modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(severanceTax.size() ); m++ ) {
        XMLWriteElement(severanceTax[m],"severanceTax",out, tabs, modeltime->getper_to_yr(m));
    }
    
    for(m = 0; m < static_cast<int>(scaleFactor.size() ); m++ ) {
        if ( scaleFactor[m] != SCALE_FACTOR_DEFAULT ) {  
            XMLWriteElement(scaleFactor[m],"scaleFactor",out, tabs, modeltime->getper_to_yr(m));
        }
    }
    
    XMLWriteElement(minShortTermSLimit,"minShortTermSLimit",out, tabs );
    XMLWriteElement(priceElas,"priceElas",out, tabs );
    
    // finished writing xml for the class members.
    
    // write out anything specific to the derived classes
    toXMLforDerivedClass( out, tabs );
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}


void SubResource::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.
    XMLWriteElement( nograde, "nograde", out, tabs );
    XMLWriteElement( minShortTermSLimit, "minShortTermSLimit", out, tabs );
    XMLWriteElement( priceElas, "priceElas", out, tabs );
    
    // Write out data for the period we are in from the vectors.
    XMLWriteElement( rscprc[ period ], "rscprc", out, tabs );
    XMLWriteElement( available[ period ], "available", out, tabs );
    XMLWriteElement( annualprod[ period ], "annualprod", out, tabs );
    XMLWriteElement( cumulprod[ period ], "cumulprod", out, tabs );
    XMLWriteElement( gdpExpans[ period ], "gdpExpans", out, tabs );
    XMLWriteElement( techChange[ period ], "techChange", out, tabs );
    XMLWriteElement( environCost[ period ], "environCost", out, tabs );
    XMLWriteElement( severanceTax[ period ], "severanceTax", out, tabs );
    if ( scaleFactor[ period ] != SCALE_FACTOR_DEFAULT ) {  
        XMLWriteElement( scaleFactor[ period ], "scaleFactor", out, tabs );
    }
    
    // write out the grade objects.
    for( int i = 0; i < static_cast<int>( grade.size() ); i++ ){	
        grade[ i ]->toDebugXML( period, out, tabs );
    }
    
    // finished writing xml for the class members.
    
    // write out anything specific to the derived classes
    toXMLforDerivedClass( out, tabs );
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& SubResource::getXMLName() const {
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
const std::string& SubResource::getXMLNameStatic() {
	return XML_NAME;
}

//! return SubResource name
string SubResource::getName() const {
    return name;
}

double SubResource::getPrice(int per){
    return rscprc[per] ;
}

int SubResource::getMaxGrade() { // returns total number of grades
    return nograde;
}

void SubResource::cumulsupply(double prc,int per){	
    const Modeltime* modeltime = scenario->getModeltime();
    int i=0,maxgrd;
    double slope=0;
    cumulprod[per]=0.0;
    
    rscprc[per] = prc;
    // the index of the last grade is number of grades minus one
    // don't forget 0 as first
    maxgrd = nograde-1;
    
    // calculate total extraction cost for each grade
    // This is a waste of time, should only do this once!
    for (int gr=0; gr<nograde; gr++) {
        if (per > 0) {
            cumulativeTechChange[per] = cumulativeTechChange[ per-1 ] * 
                pow( ( 1.0 + techChange[per] ), modeltime->gettimestep(per) );
        }
        // Determine cost
        grade[gr]->calcCost( severanceTax[per],cumulativeTechChange[per], environCost[ per ], per );
    }
    
    if (per == 0) {
        cumulprod[per] = 0.0;
    }
    else {
        // Case 1
        // if market price is less than cost of first grade, then zero cumulative 
        // production
        if (prc <= grade[0]->getCost(per)) {
            cumulprod[per] = cumulprod[per-1];
        }
        
        // Case 2
        // if market price is in between cost of first and last grade, then calculate 
        // cumulative production in between those grades
        if (prc > grade[0]->getCost(per) && prc <= grade[maxgrd]->getCost(per)) {
            int iL=0,iU=0;
            while (grade[i]->getCost(per) < prc) {
                iL=i; i++; iU=i;
            }
            // add subrsrcs up to the lower grade
            for (i=0;i<=iL;i++) {
                cumulprod[per] += grade[i]->getAvail();
            }
            // price must reach upper grade cost to produce all of lower grade
            slope = grade[iL]->getAvail()
                / (grade[iU]->getCost(per) - grade[iL]->getCost(per));
            cumulprod[per] -= slope * (grade[iU]->getCost(per) - prc);
        }
        
        // Case 3
        // if market price greater than the cost of the last grade, then
        // cumulative production is the amount in all grades
        if (prc > grade[maxgrd]->getCost(per)) {
            for (i=0;i<nograde;i++) {
                cumulprod[per] += grade[i]->getAvail();
            }
        }
    }
    
    // This doesn't seem to work for dep resource. sjs
    if ( scaleFactor[ per ] != 1 ) {
        cout << "scale factor: " << scaleFactor[ per ] << " cumulprod[ per ] : " << cumulprod[ per ]  
            << " cumulprod[ per-1 ]: " << cumulprod[ per-1 ] << endl;
    }
    // multiply change in cumulative produciton by the adjustement scale factor
    if( per == 0 ){
        cumulprod[ per ] = cumulprod[ per ]  * scaleFactor[ per ];
    }
    else {
        cumulprod[ per ] = cumulprod[ per-1 ] + (cumulprod[ per ] - cumulprod[ per-1 ]) * scaleFactor[ per ];
    }

    if ( scaleFactor[ per ] != 1 ) {
        cout << " new cumul prod: " << cumulprod[ per ] << endl;
    }
    
    //available is adjusted in annualsupply
    //available[per]=available[0]-cumulprod[per];
}

double SubResource::getCumulProd(int per){
    return cumulprod[per];
}

/*! Update the sub-resource availability for a period
* Resource depletion by grade is not calculated.
* This function only returns the maximum amount of resource
* available by grade.  
*
*/
void SubResource::updateAvailable( const int period ){
    available[ period ] = 0;
    for ( int i = 0; i < nograde; i++ ) {
        available[ period ] += grade[ i ]->getAvail();
    }
}

//! calculate annual supply
/*! Takes into account short-term capacity limits.
Note that cumulsupply() must be called before calling this function. */
void SubResource::annualsupply( int per, const GDP* gdp, double price, double prev_price ) {
    const Modeltime* modeltime = scenario->getModeltime();
    // for per = 0 use initial annual supply
    // cumulative production is 0 for per = 0
    if (per >= 1) {
        // 2 is for the average of the annual productions
        annualprod[per] = 2.0 * (cumulprod[per] - cumulprod[per-1])/modeltime->gettimestep(per)
            - annualprod[per-1];
        if(annualprod[per] <= 0) {
            cumulprod[per] = cumulprod[per-1];
            annualprod[per] = 0.0;
        } 
        
        // incorporate short-term capacity limits after 1990
        // This represents a limit to how fast production capacity can expand
        if (per >= 2) {
            // minShortTermSLimit is defined in object and read in from xml
            // minShortTermSLimit is the minimun short-term capacity limit
            double cur_annualprod = 0;
            
				// Change in GDP. 
				// since per >=2 in this branch have not checked for invalid periods
				double gdpRatio = gdp->getApproxGDP( per ) / gdp->getApproxGDP( per - 1 ); 

            // check to see if base short-term capacity (supply) limit is smaller than the minimum
            double max_annualprod = annualprod[per-1]
                *pow(gdpRatio,gdpExpans[per])
                *pow((1+techChange[per]),modeltime->gettimestep(per));
            
            // Allow the resource to produce up to the greater of the minimum short 
            // term supply and the previous period's production.
            if( max_annualprod > max( minShortTermSLimit, annualprod[ per - 1 ] )  ) {
                cur_annualprod = max_annualprod; 
            }
            else { 
                cur_annualprod = max( minShortTermSLimit, annualprod[ per - 1 ] );
            }
            
            // adjust short-term capacity limit for price effects
            cur_annualprod *= pow((price/prev_price),priceElas);
            
            // Adjust current production and cumulative production to date
            // if greater than the short-term capacity limit
            if(cur_annualprod < annualprod[per]) {
                cumulprod[per] = cumulprod[per-1]+(cur_annualprod+annualprod[per-1])
                    *modeltime->gettimestep(per)/2.0;
                annualprod[per] = cur_annualprod;
            }
        }
        // available is the total resource (stock)
        //available[per]=available[per-1]-(annualprod[per]*modeltime->gettimestep(per)/2);
        available[per]=available[per-1]-((annualprod[per]+annualprod[per-1])
           /2*modeltime->gettimestep(per));
        if (available[per]<=0) available[per] = 0;
    }
}

//! return annual production for period
double SubResource::getAnnualProd(int per){
    return annualprod[per];
}

//! return available resource for period
double SubResource::getAvailable(int per){
    return available[per];
}

//! write SubResource output to database
void SubResource::dbOutput( const string &regname, const string& secname ){
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    int i=0, m=0;
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    string tssname = name; // tempory subsector name
    string str; // tempory string
    str = name + "Total";
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total subsector output
    dboutput4(regname,"Pri Energy Production",secname,name,"EJ",annualprod);
//    dboutput4(regname,"Resource",secname,str,"EJ",available);
    dboutput4(regname,"Resource","Available "+secname,name,"EJ",available);
    dboutput4(regname,"Resource","CummProd "+secname,name,"EJ",cumulprod);
    dboutput4(regname,"Price",secname,name,"$/GJ",rscprc);
    
    // do for all grades in the sector
    for (i=0;i<nograde;i++) {
        str = tssname + "_" + grade[i]->getName();
        // grade cost
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getCost(m);
        }
        dboutput4(regname,"Price",secname,str,"$/GJ",temp);
        // grade extraction cost
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getExtCost();
        }
        dboutput4(regname,"Price ExtCost",secname,str,"$/GJ",temp);
        // available resource for each grade
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getAvail();
        }
        dboutput4(regname,"Resource",secname,str,"EJ",temp);
    }
}

//! write SubResource output to file
void SubResource::csvOutputFile( const string &regname, const string& sname) {
    const Modeltime* modeltime = scenario->getModeltime();
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total subsector output
    fileoutput3( regname,sname,name," ","production","EJ",annualprod);
    fileoutput3( regname,sname,name," ","resource","EJ",available);
    
}


// ************************************************************
// Definitions for two of the derived classes below.
// Since these are very small changes, keep in same file for simplicity
// ************************************************************

//! Parses any input variables specific to this derived class
bool SubDepletableResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {
    return false;
}

//! Parses any input variables specific to this derived class
bool SubFixedResource::XMLDerivedClassParse( const string nodeName, const DOMNode* node ) {
    return false;
}
