/*! 
* \file subresource.cpp
* \ingroup Objects
* \brief SubResource class source file.
* \author Sonny Kim
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
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string SubResource::XML_NAME = "subresource";
const double GDP_EXPANS_DEFAULT = 1;

//! Default constructor.
SubResource::SubResource() {
    minShortTermSLimit = 0;
    priceElas = 1;  // default value if not read in

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    cumulativeTechChange.resize( maxper ); // cumulative technical change
    cumulativeTechChange[0] = 1.0;
    annualprod.resize( maxper ); // annual production of subresource
    techChange.resize( maxper ); // subresource tech change
    environCost.resize( maxper ); // environmental extraction costs change
    severanceTax.resize( maxper ); // subresource severance tax
    available.resize( maxper ); // total available resource
    cumulprod.resize( maxper ); // cumulative production of subrsrc
    gdpExpans.resize( maxper, GDP_EXPANS_DEFAULT ); // cumulative production of subrsrc
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
    name = XMLHelper<string>::getAttr( node, "name" );
    
    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); ++i ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == Grade::getXMLNameStatic() ){
            parseContainerNode( curr, grade, gradeNameMap, new Grade );
        }
        else if( nodeName == "annualprod" ){
            XMLHelper<double>::insertValueIntoVector( curr, annualprod, modeltime );
        }
        else if( nodeName == "minShortTermSLimit" ){
            minShortTermSLimit = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "priceElas" ){
            priceElas = XMLHelper<double>::getValue( curr );
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
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName 
                << " found while parsing " << getXMLName() << "." << endl;
        }
    }

}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
*
* \author Josh Lurz, Sonny Kim
* \warning markets are not necesarilly set when completeInit is called
*/
void SubResource::completeInit( const IInfo* aResourceInfo ) {
    mSubresourceInfo.reset( InfoFactory::constructInfo( aResourceInfo ) ); 
    // update the available resource for period 0
    // this function must be called after all the grades have been parsed and nograde set
    updateAvailable( 0 );
    if ( priceElas == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Short-term capacity price elasticity is zero in sub-resource: "
                << name << endl;
    }
    // call completeInit for grades
    for( vector<Grade*>::iterator gradeIter = grade.begin(); gradeIter != grade.end(); gradeIter++ ) {
        ( *gradeIter )->completeInit( mSubresourceInfo.get() );
    }

}

/*! \brief Perform any initializations needed for each period.
* \details Any initializations or calcuations that only need to be done once per
*          period(instead of every iteration) should be placed in this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param aPeriod Model aPeriod
*/
void SubResource::initCalc( const string& aRegionName, const string& aResourceName,
                           const int aPeriod )
{
    // call grade initializations
    for (unsigned int i = 0; i < grade.size(); i++) {
        grade[i]->initCalc( aRegionName, aResourceName, aPeriod );
    }
    // calculate total extraction cost for each grade
    for ( unsigned int gr=0; gr< grade.size(); gr++) {
        if ( aPeriod > 0) {
            const Modeltime* modeltime = scenario->getModeltime();
            cumulativeTechChange[ aPeriod ] = cumulativeTechChange[ aPeriod - 1 ] * 
                pow( ( 1.0 + techChange[ aPeriod ] ), modeltime->gettimestep( aPeriod ) );
        }
        // Determine cost
        grade[gr]->calcCost( severanceTax[ aPeriod ], cumulativeTechChange[ aPeriod ],
            environCost[ aPeriod ], aPeriod );
    }
}

/*! \brief Perform any initializations needed after each period.
* \details Any initializations or calcuations that only need to be done once
*          after each period(instead of every iteration) should be placed in
*          this function.
* \author Sonny Kim
* \param aRegionName Region name.
* \param aResourceName Resource name.
* \param period Model aPeriod
*/
void SubResource::postCalc( const string& aRegionName, const string& aResourceName, const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    // Available is the total resource (stock) initialized in initCalc and
    // is the initial amount at the beginning of the period.
    // It does not subtract the amount used in that period.
    updateAvailable( aPeriod ); // reinitialize available amount
    if( aPeriod > 0 ) {
        available[ aPeriod ] -= cumulprod[ aPeriod - 1 ];
        available[ aPeriod ] = max( available[ aPeriod ], 0.0 );
    }

    // call grade post calculations.
    for( unsigned int i = 0; i < grade.size(); i++ ) {
        grade[i]->postCalc( aRegionName, aResourceName, aPeriod );
    }
}

//! Blank definition so that don't have to define in derived classes if there is nothing to write out
void SubResource::toXMLforDerivedClass( ostream& out, Tabs* tabs ) const {   
}   

//! Write datamembers to datastream in XML format for replicating input file.
void SubResource::toInputXML( ostream& out, Tabs* tabs ) const {

    const Modeltime* modeltime = scenario->getModeltime();

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write the xml for the class members.

    XMLWriteVector( environCost, "environCost", out, tabs, modeltime, 0.0 );

    XMLWriteVector( gdpExpans, "gdpExpans", out, tabs, modeltime, GDP_EXPANS_DEFAULT );

    XMLWriteVector( severanceTax, "severanceTax", out, tabs, modeltime, 0.0 );
    
    // for base year only
    XMLWriteElementCheckDefault(annualprod[0],"annualprod",out, tabs, 0.0 , modeltime->getper_to_yr(0));
    
    XMLWriteVector( techChange, "techChange", out, tabs, modeltime, 0.0 );

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

void SubResource::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {

    XMLWriteOpeningTag( getXMLName(), out, tabs, name );

    // write the xml for the class members.
    XMLWriteElement( minShortTermSLimit, "minShortTermSLimit", out, tabs );
    XMLWriteElement( priceElas, "priceElas", out, tabs );

    // Write out data for the period we are in from the vectors.
    XMLWriteElement( available[ period ], "available", out, tabs );
    XMLWriteElement( annualprod[ period ], "annualprod", out, tabs );
    XMLWriteElement( cumulprod[ period ], "cumulprod", out, tabs );
    XMLWriteElement( gdpExpans[ period ], "gdpExpans", out, tabs );
    XMLWriteElement( techChange[ period ], "techChange", out, tabs );
    XMLWriteElement( environCost[ period ], "environCost", out, tabs );
    XMLWriteElement( severanceTax[ period ], "severanceTax", out, tabs );

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
* This function may be virtual to be overridden by derived class pointers.
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

void SubResource::cumulsupply(double prc,int per){  
    const Modeltime* modeltime = scenario->getModeltime();


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
        if (prc > grade[0]->getCost(per) && prc <= grade[ grade.size() - 1 ]->getCost(per)) {
            cumulprod[per]=0;
            int i = 0;
            int iL=0;
            int iU=0;
            while (grade[i]->getCost(per) < prc) {
                iL=i; i++; iU=i;
            }
            // add subrsrcs up to the lower grade
            for (i=0;i<=iL;i++) {
                cumulprod[per] += grade[i]->getAvail();
            }
            // price must reach upper grade cost to produce all of lower grade
            double slope = grade[iL]->getAvail()
                / (grade[iU]->getCost(per) - grade[iL]->getCost(per));
            cumulprod[per] -= slope * (grade[iU]->getCost(per) - prc);
        }

        // Case 3
        // if market price greater than the cost of the last grade, then
        // cumulative production is the amount in all grades
        if (prc > grade[ grade.size() - 1 ]->getCost(per)) {
            cumulprod[per]=0;
            for ( unsigned int i=0;i< grade.size();i++) {
                cumulprod[per] += grade[i]->getAvail();
            }
        }
    }
}

double SubResource::getCumulProd( const int aPeriod ) const {
    return cumulprod[ aPeriod ];
}

/*! Update the sub-resource availability for a period
* Resource depletion by grade is not calculated.
* This function only returns the maximum amount of resource
* available by grade.  
*
*/
void SubResource::updateAvailable( const int period ){
    available[ period ] = 0;
    for ( unsigned int i = 0; i < grade.size(); ++i ) {
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
        // Calculate the annual production given that the cumulative production
        // for the period is known. Cumulative production for the current period
        // is equal to the cumulative production of the previous period plus the
        // trapezoidal area formed by the previous annual production and the
        // current annual production.
        // Cumulative(t) = Cumulative(t-1) + 1/2 * (Annual(t) - Annual(t-1))* timestep.
        // Solving this for Annual(t) gives us the following equation.
        annualprod[ per ] = 2.0 * ( cumulprod[ per ] - cumulprod[ per - 1 ] ) 
            / modeltime->gettimestep( per ) - annualprod[ per - 1 ];

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
        available[ per ] = max( available[ per ], 0.0 );
    }
}

//! return annual production for period
double SubResource::getAnnualProd(int per) const {
    return annualprod[per];
}

/*! \brief Update an output container for a SubResource.
* \param aOutputContainer Output container to update.
* \param aPeriod Period to update.
*/
void SubResource::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitSubResource( this, aPeriod );

    // Update the output container for the subresources.
    for( unsigned int i = 0; i < grade.size(); ++i ){
        grade[ i ]->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitSubResource( this, aPeriod );
}

//! return available resource for period
double SubResource::getAvailable(int per) const {
    return available[per];
}

//! write SubResource output to database
void SubResource::dbOutput( const string &regname, const string& secname ){
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    int m=0;
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const string outputUnit = mSubresourceInfo->getString( "outputUnit", true );
    const string priceUnit = mSubresourceInfo->getString( "priceUnit", true );
    vector<double> temp(maxper);
    string tssname = name; // tempory subsector name

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total subsector output
    dboutput4(regname,"Pri Energy Production",secname,name,outputUnit,annualprod);
    //    dboutput4(regname,"Resource",secname,str,"EJ",available);
    dboutput4(regname,"Resource","Available "+secname,name,outputUnit,available);
    dboutput4(regname,"Resource","CummProd "+secname,name,outputUnit,cumulprod);

    // do for all grades in the sector
    for ( unsigned int i=0;i< grade.size();i++) {
        string str = tssname + "_" + grade[i]->getName();
        // grade cost
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getCost(m);
        }
        dboutput4(regname,"Price",secname,str,priceUnit,temp);
        // grade extraction cost
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getExtCost();
        }
        dboutput4(regname,"Price ExtCost",secname,str,priceUnit,temp);
        // available resource for each grade
        for (m=0;m<maxper;m++) {
            temp[m] = grade[i]->getAvail();
        }
        dboutput4(regname,"Resource",secname,str,outputUnit,temp);
    }
}

//! write SubResource output to file
void SubResource::csvOutputFile( const string &regname, const string& sname) {
    const Modeltime* modeltime = scenario->getModeltime();
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    const int maxper = modeltime->getmaxper();
    const string outputUnit = mSubresourceInfo->getString( "outputUnit", true );
    vector<double> temp(maxper);

    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total subsector output
    fileoutput3( regname,sname,name," ","production",outputUnit,annualprod);
    fileoutput3( regname,sname,name," ","resource",outputUnit,available);

}


// ************************************************************
// Definitions for two of the derived classes below.
// Since these are very small changes, keep in same file for simplicity
// ************************************************************

//! Parses any input variables specific to this derived class
bool SubDepletableResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node ) {
    return false;
}

//! Parses any input variables specific to this derived class
bool SubFixedResource::XMLDerivedClassParse( const string& nodeName, const DOMNode* node ) {
    return false;
}
//! get variance
/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
double SubResource::getVariance() const {
    return 0.0;
}

//! get resource capacity factor
/*! do nothing here.  Applies to derived subrenewableresource
* \author Marshall Wise
*/
double SubResource::getAverageCapacityFactor() const {
    return 0.0;
}