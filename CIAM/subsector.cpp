/*! 
* \file subsector.cpp
* \ingroup CIAM
* \brief subsector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>

#include "Configuration.h"
#include "subsector.h"
#include "technology.h"
#include "scenario.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "marketplace.h"
#include "summary.h"
#include "Emcoef_ind.h"
#include "World.h"

using namespace std;
using namespace xercesc;

extern ofstream outfile;	
extern Scenario* scenario;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
subsector::subsector(){
    notech = 0;
    tax = 0;
    basesharewt = 0;
    Configuration* conf = Configuration::getInstance();
    debugChecking = conf->getBool( "debugChecking" );
    
    // resize vectors.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    capLimit.resize( maxper, 1.0 );
    shrwts.resize( maxper, 1.0 ); // default 1.0, for sectors with one tech.
    lexp.resize( maxper );
    share.resize(maxper); // subsector shares
    input.resize(maxper); // subsector energy input
    pe_cons.resize(maxper); // subsector primary energy consumption
    subsectorprice.resize(maxper); // subsector price for all periods
    fuelprice.resize(maxper); // subsector fuel price for all periods
    output.resize(maxper); // total amount of final output from subsector
    carbontaxpaid.resize(maxper); // total subsector carbon taxes paid
    summary.resize(maxper); // object containing summaries
    fuelPrefElasticity.resize( maxper );
    summary.resize( maxper );
    calOutputValue.resize( maxper );
    doCalibration.resize( maxper, false );
    calibrationStatus.resize( maxper, false );
    fixedShare.resize( maxper );
    capLimited.resize( maxper, false );
}

/*! \brief Default destructor.
*
* deletes all technology objects associated  with this sector.
*
* \author ?????
*/
subsector::~subsector() {
    
    for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            delete *innerIter;
        }
    }
}

//! Clear the subsector member variables.
void subsector::clear(){
    notech = 0;
    tax = 0;
    basesharewt = 0;
    name = "";
    unit = "";
    fueltype = "";
    
    
    // clear the vectors.
    techs.clear();
    hydro.clear();
    shrwts.clear();
    lexp.clear();
    fuelPrefElasticity.clear();
    share.clear();
    input.clear();
    pe_cons.clear();
    subsectorprice.clear();
    fuelprice.clear();
    output.clear();
    carbontaxpaid.clear();
    summary.clear();
    
}

/*! \brief Returns sector name
*
* \author Sonny Kim
* \return sector name as a string
*/
const string subsector::getName() const {
    return name;
}

//! Initialize subsector with xml data
void subsector::XMLParse( const DOMNode* node ) {	
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxperiod = modeltime->getmaxper();
    DOMNodeList* nodeList = 0;
    DOMNodeList* childNodeList = 0;
    DOMNode* curr = 0;
    DOMNode* currChild = 0;
    string nodeName;
    string childNodeName;
    vector<technology*> techVec( modeltime->getmaxper() );
    technology* tempTech = 0;
    
    /*! \pre Make sure we were passed a valid node. */
    assert( node );
    
    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );
    
    // get all child nodes.
    nodeList = node->getChildNodes();
    
    // loop through the child nodes.
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "capacitylimit" ){
            XMLHelper<double>::insertValueIntoVector( curr, capLimit, modeltime );
        }
        else if( nodeName == "sharewt" ){
            XMLHelper<double>::insertValueIntoVector( curr, shrwts, modeltime );
        }
        else if( nodeName == "calOutputValue" ){
            XMLHelper<double>::insertValueIntoVector( curr, calOutputValue, modeltime );
            int thisPeriod = XMLHelper<double>::getNodePeriod( curr, modeltime );
            doCalibration[ thisPeriod ] = true;
        }
        else if( nodeName == "logitexp" ){
            XMLHelper<double>::insertValueIntoVector( curr, lexp, modeltime );
        }
        
        else if( nodeName == "fuelprefElasticity" ){
            XMLHelper<double>::insertValueIntoVector( curr, fuelPrefElasticity, modeltime );  
        }
        
        // basesharewt is not a vector but a single value
        else if( nodeName == "basesharewt" ){
            basesharewt = XMLHelper<double>::getValue( curr );
            share[0] = basesharewt;
        }
        
        else if( nodeName == "technology" ){
            map<string,int>::const_iterator techMapIter = techNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
            if( techMapIter != techNameMap.end() ) {
                // technology already exists.
                childNodeList = curr->getChildNodes();
                
                // loop through technologies children.
                for( int j = 0; j < static_cast<int>( childNodeList->getLength() ); j++ ){
                    
                    currChild = childNodeList->item( j );
                    childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                    
                    if( childNodeName == "#text" ){
                        continue;
                    }
                    else if( childNodeName == "period" ){
                        int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                        techs[ techMapIter->second ][ thisPeriod ]->XMLParse( currChild );
                    }
                }
            }
            
            else {
                // create a new vector of techs.
                childNodeList = curr->getChildNodes();
                
                // loop through technologies children.
                for( int j = 0; j < static_cast<int>( childNodeList->getLength() ); j++ ){
                    
                    currChild = childNodeList->item( j );
                    childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                    
                    if( childNodeName == "period" ){
                        tempTech = new technology();
                        tempTech->XMLParse( currChild );
                        int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                        techVec[ thisPeriod ] = tempTech;
                        
                        // boolean to fill out the readin value to all the periods
                        const bool fillout = XMLHelper<bool>::getAttr( currChild, "fillout" );
                        
                        // copy technology object for one period to all the periods
                        if (fillout) {
                            // will not do if period is already last period or maxperiod
                            for (int i = thisPeriod+1; i < maxperiod; i++) {
                                techVec[ i ] = new technology( *techVec[ thisPeriod ] );
                                techVec[ i ]->setYear( modeltime->getper_to_yr( i ) );
                            }
                        }
                        
                    }
                }
                techs.push_back( techVec );
                techNameMap[ techVec[ 0 ]->getName() ] = static_cast<int>( techs.size() ) - 1;
                techVec.clear();
                techVec.resize( modeltime->getmaxper(), 0 );
            }
        }
        // parsed derived classes
        else {
            XMLDerivedClassParse( nodeName, curr );
        }
    }
}

//! Parses any input variables specific to derived classes
void subsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
    cout << "Unrecognized text string: " << nodeName << " found while parsing Subsector." << endl;
}

//! Complete the initialization.
void subsector::completeInit() {
    // Initialize any arrays that have non-zero default value
    notech = static_cast<int>( techs.size() );
    
    for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            assert( *innerIter ); // Make sure the technology has been defined.
            ( *innerIter )->completeInit();
        }
    }
}

//! Output the subsector member variables in XML format.
void subsector::toXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i;
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<subsector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // write the xml for the class members.
    for( i = 0; i < static_cast<int>( capLimit.size() ); i++ ){
        XMLWriteElementCheckDefault( capLimit[ i ], "capacitylimit", out, 1, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( calOutputValue.size() ); i++ ){
        if ( doCalibration[ i ] ) {
            XMLWriteElementCheckDefault( calOutputValue[ i ], "calOutputValue", out, 0, modeltime->getper_to_yr( i ) );
        }
    }
    
    for( i = 0; i < static_cast<int>( shrwts.size() ); i++ ){
        XMLWriteElementCheckDefault( shrwts[ i ], "sharewt", out, 1, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( lexp.size() ); i++ ){
        XMLWriteElementCheckDefault( lexp[ i ], "logitexp", out, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( fuelPrefElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( fuelPrefElasticity[ i ], "fuelprefElasticity", out, 0, modeltime->getper_to_yr( i ) );
    }
    
    XMLWriteElementCheckDefault( basesharewt, "basesharewt", out, 0, modeltime->getstartyr( ) );
    
    // write out the technology objects.
    for( vector< vector< technology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
        Tabs::writeTabs( out );
        
        // If we have an empty vector this won't work, but that should never happen.
        assert( j->begin() != j->end() );
        
        out << "<technology name=\"" << ( * ( j->begin() ) )->getName() << "\">" << endl;
        
        Tabs::increaseIndent();
        
        for( vector<technology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
            ( *k )->toXML( out );
        }
        
        Tabs::decreaseIndent();
        
        Tabs::writeTabs( out );
        out << "</technology>" << endl;
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</subsector>" << endl;
}

//! XML output for viewing.
void subsector::toOutputXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i;
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<subsector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // write the xml for the class members.
    for( i = 0; i < static_cast<int>( capLimit.size() ); i++ ){
        XMLWriteElementCheckDefault( capLimit[ i ], "capacitylimit", out, 1, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( calOutputValue.size() ); i++ ){
        if ( doCalibration[ i ] ) {
            XMLWriteElementCheckDefault( calOutputValue[ i ], "calOutputValue", out, 0, modeltime->getper_to_yr( i ) );
        }
    }
    
    for( i = 0; i < static_cast<int>( shrwts.size() ); i++ ){
        XMLWriteElementCheckDefault( shrwts[ i ], "sharewt", out, 1, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( lexp.size() ); i++ ){
        XMLWriteElementCheckDefault( lexp[ i ], "logitexp", out, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( fuelPrefElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( fuelPrefElasticity[ i ], "fuelPrefElasticity", out, 0, modeltime->getper_to_yr( i ) );
    }
    
    XMLWriteElementCheckDefault( basesharewt, "basesharewt", out, 0, modeltime->getstartyr( ) );
    
    // write out the technology objects.
    for( vector< vector< technology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
        Tabs::writeTabs( out );
        
        // If we have an empty vector this won't work, but that should never happen.
        assert( j->begin() != j->end() );
        
        out << "<technology name=\"" << ( * ( j->begin() ) )->getName() << "\">" << endl;
        
        Tabs::increaseIndent();
        
        for( vector<technology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
            ( *k )->toXML( out );
        }
        
        Tabs::decreaseIndent();
        
        Tabs::writeTabs( out );
        out << "</technology>" << endl;
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</subsector>" << endl;
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
*/
void subsector::toDebugXML( const int period, ostream& out ) const {
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<subsector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // Write the xml for the class members.
    XMLWriteElement( unit, "unit", out );
    XMLWriteElement( fueltype, "fueltype", out );
    XMLWriteElement( notech, "notech", out );
    XMLWriteElement( tax, "tax", out );
    
    // Write the data for the current period within the vector.
    XMLWriteElement( capLimit[ period ], "capLimit", out );
    XMLWriteElement( shrwts[ period ], "sharewt", out );
    XMLWriteElement( lexp[ period ], "lexp", out );
    XMLWriteElement( fuelPrefElasticity[ period ], "fuelprefElasticity", out );
    XMLWriteElement( share[ period ], "share", out );
    XMLWriteElement( basesharewt, "basesharewt", out );
    XMLWriteElement( input[ period ], "input", out );
    XMLWriteElement( pe_cons[ period ], "pe_cons", out );
    XMLWriteElement( subsectorprice[ period ], "subsectorprice", out );
    XMLWriteElement( output[ period ], "output", out );
    XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );
    
    // Write out the summary object.
    // summary[ period ].toDebugXML( period, out );
    // write out the technology objects.
    
    for( int j = 0; j < static_cast<int>( techs.size() ); j++ ){
        techs[ j ][ period ]->toDebugXML( period, out );
    }
    
    // write out the hydrotech. Not yet implemented
    // hydro[ period ].toDebugXML( period, out );
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</subsector>" << endl;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \author Steve Smith
* \param regionName region name
* \param per Model period
*/
void subsector::initCalc( const int period ) {
    
    // Set any fixed demands
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][ period ]->calcFixedSupply( period );
        techs[i][ period ]->initCalc( );
    }
    
    setCalibrationStatus( period );
    shareWeightScale( period ); 
    fixedShare[ period ] = 0;
    
    // Prevent pathological situation were share is zero where a fixed capacity is present.\
    // This can happen at begining of an initialization. Share will be set properly within secotr::calcShare 
    if ( ( getFixedSupply( period ) > 0 ) && ( fixedShare[ period ] == 0 ) ) {
       fixedShare[ period ] = 0.1;
    }
}

/*! \brief Computes weighted cost of all technologies in subsector.
*
* Called from calcShare after technology shares are determined. Calculates share-weighted total price (subsectorprice) and cost of fuel (fuelprice). 
*
* Price function separated to allow different weighting for subsector price
* changed to void return maw
*
* \author Sonny Kim, Marshall Wise
* \param regionName region name
* \param per Model period
*/
void subsector::calcPrice( const string regionName, const int per ) {
    int i=0;
    subsectorprice[per] = 0; // initialize to 0 for summing
    fuelprice[per] = 0; // initialize to 0 for summing
    
    for (i=0;i<notech;i++) {
        // calculate weighted average price for subsector
        subsectorprice[per] += techs[i][per]->getShare()*
            techs[i][per]->getTechcost();
        // calculate weighted average price of fuel only
        // technology shares are based on total cost
        fuelprice[per] += techs[i][per]->getShare()*
            techs[i][per]->getFuelcost();
    }
}

/*! \brief returns the sector price.
*
* Returns the weighted price from sectorprice variable. See also price method.
*
* \author Sonny Kim
* \param per Model period
*/
double subsector::getPrice( const int period ) const {
    return subsectorprice[ period ];
}

/*! \brief Returns calibration status.
*
* Since this information in needed often, this is stored in a variable. 
* Can be set just once, since this never chagnes during an interation.
* See setCalibrationStatus
*
* \author Steve Smith
* \param period Model period
* \pre must be set with setCalibrationStatus
* \return Boolean that is true if sub-sector is calibrated
*/
bool subsector::getCalibrationStatus( const int period ) const {
    return calibrationStatus[ period ];
}

/*! \brief Returns true if this subsector, or underlying technologies, are calibrated.
*
* If either the subsector output, or the output of all the technologies under this subsector (not including those with zero output) are calibrated, then the calibrationStatus for the sector is set to true.
*
* \author Steve Smith
* \param period Model period
*/
void subsector::setCalibrationStatus( const int period ) {
    if ( doCalibration[ period ] ) {
        calibrationStatus[ period ] = true;
        return;
    } else {
        for (int i=0; i<notech; i++ ) {
            if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
                calibrationStatus[ period ] = true;
                return;
            }
        }
    }
}

/*! \brief returns subsector capacity limit.
*
* The capacity limit is in terms of the sector share.
*
* \author Steve Smith
* \param period Model period
* \return Capacity limit for this sub-sector
*/
double subsector::getCapacityLimit( const int period ) const {
    return capLimit[ period ];
}

/*! \brief sets flag for subsector capacity limit status.
*
* capLimited is true when the sector has pegged at its capacity limit
*
* \author Steve Smith
* \param period Model period
*/
void subsector::setCapLimitStatus( const bool value, const int period ) {
   capLimited[ period ] = value;
}

/*! \brief returns subsector capacity limit status.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return Boolean capacity limit status
*/
bool subsector::getCapLimitStatus( const int period ) const {
    return capLimited[ period ];
}

/*! \brief returns subsector fuel price.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return Boolean capacity limit status
*/
double subsector::getfuelprice(int per) const
{
    return fuelprice[per];
}

/*! \brief returns subsector fuel price times share
*
* Returns the share-weighted fuel price, which is later summed to get the sector-weighted fuel price (or cost)
*
* \author Sonny Kim
* \param period Model period
* \return share-weighted fuel price
*/
double subsector::getwtfuelprice(int per) const
{
    if (per == 0) {
        //return basesharewt*fuelprice[per];
        return share[per]*fuelprice[per]; // base year share initialized to basesharewt
    }
    else {
        return share[per-1]*fuelprice[per];
    }
}

/*! \brief Pass along a fixed carbon price to technologies
*
* Routine passes a fixed carbon price to each technology as given through data read-in
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param tax the carbon prie for this region
* \param period model period
* \todo combine this with ghgtax by using a "fixedTax" tag in data input (see e-mail of 10/21/03)
*/
void subsector::applycarbontax( const string& regionName, const double tax, const int period ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][ period ]->applycarbontax( regionName, tax );
    }
}

/*! \brief Passes ghg price (tax) to technologies
*
* The GHG price is passed along to each technology. This is the price as determined through the solution routine.
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param ghgname name of the ghg to apply tax to
* \param period model period
*/
void subsector::addghgtax( const string& ghgname, const string& regionName, const int period ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][ period ]->addghgtax( ghgname, regionName, period );
    }
}


/*! \brief calculate technology shares within subsector
*
* Calls technology objects to first calculate cost, then their share. Follos this by normalizing shares. 
*
* \author Marshall Weise, Josh Lurz
* \param regionName region name
* \param period model period
* \warning technologies can not independently have fixed outputs at this point
*/
void subsector::calcTechShares( const string& regionName, const int per ) {
    int i=0;
    double sum = 0;
    
    for (i=0;i<notech;i++) {
        // calculate technology cost
        techs[i][per]->calcCost( regionName, per );
        // determine shares based on technology costs
        techs[i][per]->calcShare( regionName,per );
        sum += techs[i][per]->getShare();
    }
    // normalize technology shares to total 100 %
    for (i=0;i<notech;i++) {
        techs[i][per]->normShare(sum);
        // Logit exponential should not be zero or positive when more than one technology
        if(notech>1 && techs[i][per]->getlexp()>=0) 
          cerr << "Tech for sector " << name << " Logit Exponential is invalid (>= 0)" << endl;
    }
}	


/*! \brief calculate subsector unnormalized shares
*
* Calculates the un-normalized share for this sector. 
* Also claculates the sector aggregate price (or cost)
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param per model period
* \param gnp_cap GDP per capita (absolute or relative?)
* \warning technologies can not independently have fixed outputs
*/
void subsector::calcShare( const string& regionName, const int per, const double gnp_cap )
{
    double prevShare = share[per];
    // call function to compute technology shares
    subsector::calcTechShares(regionName, per);
    
    // calculate and return subsector share; uses above price function
    // calc_price() uses normalized technology shares calculated above
    // Logit exponential should not be zero
    
    // compute subsector weighted average price of technologies
    subsector::calcPrice( regionName,per);

    // subsector logit exponential check
    if(lexp[per]==0) cerr << "SubSec Logit Exponential is 0." << endl;
    
    if( subsectorprice[per]==0) {
        share[per] = 0;
    }
    else {
        // this logic doesn't work now, but does no harm
        if (fuelPrefElasticity.empty()) { // supply subsector
            share[per] = shrwts[per]*pow(subsectorprice[per],lexp[per]);
        }
        else { // demand subsector
            share[per] = shrwts[per]*pow(subsectorprice[per],lexp[per])*pow(gnp_cap,fuelPrefElasticity[per]);
        }

	}
   
   if (shrwts[per]  > 1e4) {
    cout << "WARNING: Huge shareweight for sub-sector " << name << " : " << shrwts[per] 
         << " in region " << regionName <<endl;
   }
      
   if (share[per] < 0) {
     cerr << "Share is < 0 for " << name << " in " << regionName << endl;
     cerr << "    subsectorprice[per]: " << subsectorprice[per] << endl;
     cerr << "    shrwts[per]: " << shrwts[per] << endl;
   }   
}

/*! \brief normalizes subsector shares to 100%
*
* \author Sonny Kim, Josh Lurz
* \param sum sum of sector shares
* \param per model period
* \warning sum must be correct sum of shares
* \pre calc shares must be called first
*/
void subsector::normShare( const double sum, const int per) {
    if ( sum==0 ) {
        share[per]=0;
    }
    else {
        setShare( share[per] / sum, per );
    }
}

/*!
* \brief normalizes shares to 100% subject to capacity limit.
*
* Used by sector::calcShare() to re-normalize shares, adjusting for capacity limits.
*
* Note that a multiplier is passed, not a divisor. The appropriate multiplier must be calculated by the calling routine.
*
* Sub-sectors that are not subject to a capacity limit get multiplied by mult.
* Capacity limited subsectors are set to their capacity limit.
*
* \author Steve Smith
* \warning The routine assumes that shares are already normalized.
* \param multiplier Multiplier by which to scale shares of all non-capacity limited sub-sectors
* \param per Model period
*/
void subsector::limitShares( const double multiplier, const int per ) {
    if ( multiplier == 0 ) {
        share[per] = 0;
    }
    else {		
        if ( share[per] >= capLimit[per]) {
            setShare( capLimit[per], per );
            setCapLimitStatus( true, per ); // set status to true
        } else {
           if ( fixedShare[ per ] == 0 ) { // don't change if fixed
               setShare( share[per] * multiplier, per );
           }
        }
    }
}

/*! \brief Return the total exogenously fixed technology output for this sector.
*
* \author Steve Smith
* \param per model period
* \pre calc shares must be called first
*/
double subsector::getFixedSupply( const int per ) const {
    double fixedOutput = 0;
    for ( int i=0 ;i<notech; i++ ) {
        fixedOutput += techs[i][per]->getFixedSupply();
    }
    return fixedOutput;
}

/*!\brief Return the share from this sub-sector that is fixed supply
* Enables communication of fixed share to other classes. 
*This is necessary since, while the amount of fixed supply is available (via getFixedSupply), the total output of a sector is not always known. So this function enables the amount of fixed supply in terms of the sector share to be communicated. 
*
* \author Steve Smith
* \param per Model period
*/
double subsector::getFixedShare( const int period ) const {
    return fixedShare[ period ];
}

/*! \brief Save the share from this sub-sector that is fixed supply
* Enables communication of fixed share to other classes. See documentation for getFixedShare.
*
* \author Steve Smith
\param per Model period
\param share sector share that is fixed supply
*/
void subsector::setFixedShare( const int period, const double share ) {
World* world = scenario->getWorld();

    // option to turn this off during calibration -- but doesn't seem to help
   if ( world->getCalibrationSetting() || 1==1) {
   fixedShare[ period ] = share;
   if ( share > 1 ) {
     cerr << "Share set to value > 1. Value = " << share << endl;
   }
   }
}

/*! \brief Set the share from this sub-sector to that saved for fixed supply
* This function changes the share to the share previously saved for the fixed supply.
* This is done instead of using a function to directly set the share in general. 
* Doing this allows the price and calibration routines to operate with an appropriate share.
*
*\author Steve Smith
*\param period Model period
*/
void subsector::setShareToFixedValue( const int period ) {
   setShare( fixedShare[ period ], period );
}

/*! \brief Reset fixed supply for each technology
* Reset fixed supply to read-in value. This is needed in case the fixed supply had been downscaled to match demand.
* This is done instead of using a function to directly set the share in general. 
* Doing this allows the price and calibration routines to operate with an appropriate share.
*
*\author Steve Smith
*\param period Model period
*/
void subsector::resetFixedSupply( const int per ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][per]->resetFixedSupply(per); // eliminate any previous down-scaleing
    }
}

/*! \brief Scale down fixed supply
* This is use dif the total fixed production is greater than the actual demand. See scaleFixedSupply.
*
* \author Steve Smith
* \param per Model period
* \param scaleRatio multiplicative scale factor by which to scale fixed supply
*/
void subsector::scaleFixedSupply( const double scaleRatio, const int per ) {
    // scale fixed technology output down
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][ per ]->scaleFixedSupply( scaleRatio );
    }
    setFixedShare( per, fixedShare[ per ] * scaleRatio ); 
}

/*! \brief Consistantly adjust share weights after calibration 
* If the sector share weight in the previous period was changed due to calibration, 
* then adjust next few shares so that there is not a big jump in share weights.
*
* \author Steve Smith
* \param period Model period
* \todo Make end period year more general from data read-in.
*/
void subsector::shareWeightScale( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // ***** SHK comments: I don't like the idea of having hardcoded numbers in the model
    // I don't think we can arbitrarily set a year here without knowing what
    // the input data is.  You use 2050 because shareWeights are the same after that.
    // Also this does not allow flexibility in the time step.
    
    // if previous period was calibrated, then adjust future shares
    if ( period > modeltime->getyr_to_per( 1990 ) ) {
        if ( calibrationStatus[ period - 1 ] ) {
            int endPeriod = modeltime->getyr_to_per( 2050 );
            shareWeightInterp( period - 1, endPeriod );
        }
    }
}

/*! \brief Linearly interpolate share weights between specified endpoints 
* Utility function to linearly scale share weights between two specified points.
*
* \author Steve Smith
* \param period Model period
*/
void subsector::shareWeightInterp( const int beginPeriod,  const int endPeriod ) {
    
    if ( endPeriod > beginPeriod ) {
        double shareIncrement = ( shrwts[ endPeriod ] - shrwts[ beginPeriod ] ) / 
            ( endPeriod - beginPeriod );
        for ( int per = beginPeriod + 1 ;per<endPeriod ; per++ ) {
            shrwts[ per ] = shrwts[ per - 1 ] + shareIncrement;
        }
    }
}

//! Adjusts shares to be consistant with any fixed production 
/*! This routine does two things. 

If this sub-sector has a fixed supply, it sets the share to be consistant with the fixed supply
If this sub-sector does not have a fixed supply, it adjusts the share to be consistant with all the fixed supplies of all other sub-sectors (totalFixedSupply)

\param dmd total demand for all sectors
\param shareRatio amount variable shares need to be adjusted to be consistant with fixed supply
\param totalFixedSupply total fixed supply from all sub-sectors
\param per model period
*/
void subsector::adjShares( const double dmd, double shareRatio, 
                          const double totalFixedSupply, const int per) {
    double sumSubsectFixedSupply = 0; // total subsector fixed supply
    double fixedSupply = 0; // fixed supply for each technology
    double varShareTot = 0; // sum of shares without fixed supply
    double subsecdmd; // subsector demand adjusted with new shares

    // add up the fixed supply and share of non-fixed supply
    for ( int i=0 ;i<notech; i++ ) {
        fixedSupply = techs[i][per]->getFixedSupply();
        sumSubsectFixedSupply += fixedSupply;
        if (fixedSupply == 0) { 
           varShareTot += techs[i][per]->getShare();
        }
    }
    
    // Adjust the share for this subsector
    // This makes the implicit assumption that the subsector is either all
    // fixed production or all variable. Would need to amend the logic below
    // to take care of other cases.
    
    // totalFixedSupply is the sector total
    if(totalFixedSupply > 0) {
        if (sumSubsectFixedSupply > 0) {	// This subsector has a fixed supply
            if ( dmd > 0 ) {
                setShare( sumSubsectFixedSupply/dmd, per ); 
            }
            else { // no fixed share if no demand
                share[per] = 0; 
            }
        }
        else {	// This subsector does not have fixed supply 
            if ( dmd > 0 ) {
                setShare( share[per] * shareRatio, per ); 
            }
            else {
                share[per] = 0; 
            }  
        } 
    }
    
    // then adjust technology shares to be consistent
    subsecdmd = share[per]*dmd; // share is subsector level
    for (int j=0;j<notech;j++) {
        // adjust tech shares 
        techs[j][per]->adjShares(subsecdmd, sumSubsectFixedSupply, varShareTot, per);
    }
    
}

/*! \brief The demand passed to this function is shared out at the technology level.
* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See also sector::setoutput. 
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param prodName name of product for this sector
* \param dmd Total demand for this product
* \param per Model period
* \pre dmd must be the total demand for this project, so must be called after this has been determined
*/
void subsector::setoutput( const string& regionName, const string& prodName, const double dmd, const int per) {
    int i=0;
    input[per] = 0; // initialize subsector total fuel input 
    carbontaxpaid[per] = 0; // initialize subsector total carbon taxes paid 
    
    // note that output is in service unit when called from demand sectors
    // multiply dmd by subsector share go get the total demand to be supplied by this subsector
    double subsecdmd = share[per]*dmd; 
    
    for ( i=0; i<notech; i++ ) {
        // calculate technology output and fuel input from subsector output
        techs[i][per]->production( regionName, prodName, subsecdmd, per );
        
        // total energy input into subsector, must call after tech production
        input[per] += techs[i][per]->getInput();
        // sum total carbon tax paid for subsector
        carbontaxpaid[per] += techs[i][per]->getCarbontaxpaid();
    }
    
}

/*! \brief Adjusts share weights and subsector demand to be consistant with calibration value.
* Calibration is performed by scaling share weights to be consistant with the calibration value. Calibration is, therefore, performed as part of the iteration process. Since this can change derivitives, best to turn calibration off when using N-R solver.
* 
* Subector demand is also set equal to calibration value in order to pass down to technologies.
* This routine takes total demand into account so that total calibrated outputs cannot exceed demand.
* Also takes into account fixed supply, which is assumed to take presidence over calibration values
* Note that this routine doesn't notice if the calibration is at the technology or sub-sector level, this is taken care of by routine getTotalCalOutputs.
*
* \author Steve Smith
* \param sectorDemand total demand for this sector
* \param totalFixedSupply total amount of fixed supply for this sector
* \param totalCalOutputs total amount of calibrated outputs for this sector
* \param period Model period
* \warning If calvalue is larger than sector demand nothing is done
* \warning The value of subsecdmd is changed (for sub-sector output calibration)
*/
void subsector::adjustForCalibration( double sectorDemand, double totalFixedSupply, double totalCalOutputs, const int period ) {
   double shareScaleValue = 0;
   double availableDemand;
   double subSectorDemand;

   // total calibrated outputs for this sub-sector
   double calOutputSubsect = getTotalCalOutputs( period );

   // Determine available demand that can be shared out (subtract sub-sectors with fixed supply)
   availableDemand = sectorDemand - totalFixedSupply;
   if ( availableDemand < 0 ) {
      availableDemand = 0;
   }
   
   // If total sector caloutputs are larger than available demand, then adjust all sub-sector cal values
   if ( totalCalOutputs > availableDemand ) {
     // adjust cal value, but leave a slight bit of headroom, taking into account other cal outputs
     // In this case all demand will be supplied by cal outputs, so divide proportionately.
      calOutputSubsect = (calOutputSubsect/totalCalOutputs) * availableDemand;
   }
   
    // make sure share weights aren't zero or else cann't calibrate
    if ( shrwts[ period ]  == 0 && ( calOutputSubsect > 0 ) ) {
        shrwts[ period ]  = 1;
    }
    
   subSectorDemand = share[ period ] * sectorDemand;
   if ( subSectorDemand > 0 ) {
      shareScaleValue = calOutputSubsect / subSectorDemand;
      shrwts[ period ]  = shrwts[ period ]  * shareScaleValue;
    }
    
   if ( shrwts[ period ] < 0 ) {
     cerr << "Share Weight is < 0 in subsector " << name << endl;
     cerr << "    shrwts[per]: " << shrwts[ period ] << " (reset to 1)" << endl;
     shrwts[ period ] = 1;
   }

   bool watchSector = (name=="biomass" && sectorName == "building" && regionName == "USAxx");
   if ( debugChecking && shrwts[ period ] > 1e4 || watchSector ) {
      if ( !watchSector ) {
         cout << "In calibration for sub-sector: " << name;
         cout << " in sector: "<< sectorName << " in region: " << regionName << endl;
      } else { cout << " ||" ; }
      cout << "  shrwts = " << shrwts[ period ] << ", sub-sec share = " << share[ period ]  << endl;
      cout << "  AvailD, totCal, calOutSect, subSectD, scaleVal: " << availableDemand << ", "; 
      cout <<  totalCalOutputs << ", " <<  calOutputSubsect << ", " ; 
      cout << subSectorDemand << ", " << shareScaleValue << endl;
   }
}
  

/*! \brief returns the total calibrated output from this sector.
*
* Routine adds up calibrated values from both the sub-sector and (if not calibrated at subsector), technology levels.
*
* \author Steve Smith
* \param per Model period
* \return Total calibrated output for this subsector
*/
double subsector::getTotalCalOutputs( const int per ) const {
    double sumCalValues = 0;

   if ( doCalibration[ per ] ) {
      sumCalValues += calOutputValue[ per ];
   } 
   else {
   	for ( int i=0; i<notech; i++ ) {
         if ( techs[ i ][ per ]->getCalibrationStatus( ) ) {
            
            if ( debugChecking ) {
              if ( techs[ i ][ per ]->getCalibrationOutput( ) < 0 ) {
                 cerr << "calibration < 0 for tech " << techs[ i ][ per ]->getName() 
                      << " in subsector " << name << endl;
              }
            }

            sumCalValues += techs[ i ][ per ]->getCalibrationOutput( );
         }
      }
   }
   
   return sumCalValues;
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this subsector or all technologies in this sub-sector returns true.
*
* \author Steve Smith
* \param per Model period
* \return Total calibrated output for this subsector
*/
bool subsector::allOuputFixed( const int per ) const {
   bool oneNotFixed = false;
   bool outputFixed = false;

   if ( doCalibration[ per ] ) {
      outputFixed = true;  // this sector has fixed output
   } 
   else  if ( shrwts[ per ] == 0 ) {
      outputFixed = true; // this sector has no output, so is also fixed
   }
   // if not fixed at sub-sector level, then check at the technology level
   else {
      for ( int i=0; i<notech; i++ ) {
         if ( !( techs[ i ][ per ]->ouputFixed( ) ) ) {
            oneNotFixed = true;
        }
      }
   }
   
   if ( outputFixed ) {
      return true;
   } else {
      return !oneNotFixed;
   }
   
}

/*! \brief scale calibration values.
*
* Scale calibration values in each technology by specified amount. 
* Calibration values are not permanantly changed, only for this iteration
*
* \author Steve Smith
* \param per Model period
* \param scaleFactor Multiplicitive scale factor for each calibration value
*/
void subsector::scaleCalibrationInput( const int per, const double scaleFactor ) {
    for ( int i=0; i<notech; i++ ) {
        techs[ i ][ per ]->scaleCalibrationInput( scaleFactor );
    }
}

/*! \brief returns share for this subsector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \pre calcShare
* \return share value
*/
double subsector::getShare( const int per ) const {
    return share[per];
}

/*! \brief set share for this subsector with normalization check
*
* Use this function to set the share at any time where shares are supposed to be normalized
*
* \author Steve Smith
* \param per Model period
*/
void subsector::setShare( const double shareVal, const int per ) {
const double tinyNumber = util::getVerySmallNumber();

   share[ per ] = shareVal;
   if ( shareVal > (1 + tinyNumber ) ) {
      cerr << "ERROR - share value set > 1. Val: " << shareVal << endl;
   }
}

/*! \brief Set name of region that contains this sub-sector
*
* Extremely useful for debugging!
*
* \author Steve Smith
* \param nameStr Name of region
*/
void subsector::setRegionName( const string& nameStr ) {
    regionName = nameStr;
}

/*! \brief Set name of sector that contains this sub-sector
*
* Extremely useful for debugging!
*
* \author Steve Smith
* \param nameStr Name of region
*/
void subsector::setSectorName( const string& nameStr ) {
    sectorName = nameStr;
}


//! write subsector output to database
void subsector::outputfile( const string& regname, const string& secname ) const {
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    int i=0, m=0;
    int mm=0; // temp period
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total subsector output
    fileoutput3( regname,secname,name," ","production","EJ",output);
    // subsector price
    fileoutput3( regname,secname,name," ","price","$/GJ(ser)",subsectorprice);
    for (m=0;m<maxper;m++)
        temp[m] = summary[m].get_emissmap_second("CO2");
    fileoutput3( regname,secname,name," ","CO2 emiss","MTC",temp);
    // subsector carbon taxes paid
    fileoutput3( regname,secname,name," ","C tax paid","Mil90$",carbontaxpaid);
    
    // do for all technologies in the subsector
    for (i=0;i<notech;i++) {
        // output or demand for each technology
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"production","EJ",temp);
        // technology share
        if(notech>1) {
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getShare();
            }
            fileoutput3( regname,secname,name,techs[i][mm]->getName(),"tech share","%",temp);
        }
        // technology cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"price","$/GJ",temp);
        // ghg tax applied to technology
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbontax();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"C tax","$/TC",temp);
        // ghg tax paid
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbontaxpaid();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"C tax paid","90Mil$",temp);
        // technology fuel input
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getInput();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"fuel consump","EJ",temp);
        // technology efficiency
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEff();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"efficiency","%",temp);
        // technology non-energy cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNecost();
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"non-energy cost","$/GJ",temp);
        // technology CO2 emission
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2");
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"CO2 emiss","MTC",temp);
        // technology indirect CO2 emission
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2ind");
        }
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"CO2 emiss(ind)","MTC",temp);
    }
}

//! write MiniCAM style subsector output to database
/*! Part A for supply sector, titles and units are different for Part B */
void subsector::MCoutputA( const string& regname, const string& secname ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    int i=0, m=0;
    int mm=0; // temp period
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double cvrt90 = 2.212; //  convert '75 price to '90 price
    vector<double> temp(maxper);
    
    // total subsector output
    dboutput4(regname,"Secondary Energy Prod",secname,name,"EJ",output);
    // subsector price
    dboutput4(regname,"Price",secname,name,"75$/GJ",subsectorprice);
    // for electricity sector only
    if (secname == "electricity") {
        for (m=0;m<maxper;m++) {
            temp[m] = subsectorprice[m] * cvrt90 * 0.36;
        }
        dboutput4(regname,"Price",secname+" C/kWh",name,"90C/kWh",temp);
    }
    
    string tssname = "tech_"; // tempory subsector name
    string str1, str2; // tempory string
    // do for all technologies in the subsector
    for (i=0;i<notech;i++) {
        str1 = secname;
        str1 += "_tech";
        str2 = techs[i][mm]->getName();
        // technology non-energy cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNecost();
        }
        dboutput4(regname,"Price NE Cost",secname,str2,"75$/GJ",temp);
        // secondary energy and price output by tech
        // output or demand for each technology
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput();
        }
        dboutput4(regname,"Secondary Energy Prod",str1,str2,"EJ",temp);
        // technology cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost()*cvrt90;
        }
        dboutput4(regname,"Price",str1,str2,"90$/GJ",temp);
    }
}

//! write MiniCAM style subsector output to database
/*! Part B for demand sector, titles and units are different from Part A */
void subsector::MCoutputB( const string& regname, const string& secname ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    int i=0, m=0;
    int mm=0; // temp period
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // total subsector output
    dboutput4(regname,"End-Use Service",secname+" by Subsec",name,"Ser Unit",output);
    dboutput4(regname,"End-Use Service",secname+" "+name,"zTotal","Ser Unit",output);
    // subsector price
    dboutput4(regname,"Price",secname,name+" Tot Cost","75$/Ser",subsectorprice);
    
 
    string tssname = "tech "; // tempory subsector name
    string str; // tempory string
    // do for all technologies in the subsector
    for (i=0;i<notech;i++) {
        //str = tssname + techs[i][mm].showname();
        str = techs[i][mm]->getName();
        if(notech>1) {  // write out if more than one technology
            // output or demand for each technology
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getOutput();
            }
            dboutput4(regname,"End-Use Service",secname+" "+name,str,"Ser Unit",temp);
            // total technology cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getTechcost();
            }
            dboutput4(regname,"Price",secname+" "+name,str,"75$/Ser",temp);
           // technology fuel cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getFuelcost();
            }
            dboutput4(regname,"Price",secname+" "+name+" Fuel Cost",str,"75$/Ser",temp);
            // technology non-energy cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getNecost();
            }
            dboutput4(regname,"Price",secname+" "+name+" NE Cost",str,"75$/Ser",temp);
        }
    }
}


//! write MiniCAM style subsector output to database
void subsector::MCoutputC( const string& regname, const string& secname ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    int i=0, m=0;
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    string str; // tempory string
    
    //for (m=0;m<maxper;m++)
    //	temp[m] = summary[m].get_emissmap_second("CO2");
    //dboutput4(regname,"CO2 Emiss",secname,name,"MTC",temp);
    // subsector carbon taxes paid
    dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
    // subsector share 
    dboutput4(regname,"Subsec Share",secname,name,"100%",share);
    // subsector emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        str = "Subsec: "; // subsector heading
        str+= secname; // sector name
        str+= "_";
        str+= name; // subsector name
        dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
    }
    
    //string tssname = name; // tempory subsector name
    string tssname = "tech_"; // tempory subsector name
    int mm=0; // temp period to get base period
    // do for all technologies in the subsector
    for (i=0;i<notech;i++) {
        //str = tssname + techs[i][mm].showname();
        str = techs[i][mm]->getName();
        //		if(notech>1) {  // write out if more than one technology
        if(notech>0) {  // write out if more than one technology
            // technology CO2 emission
            for (m=0;m<maxper;m++) {
                // this gives subsector total CO2 emissions
                //temp[m] = summary[m].get_emissmap_second("CO2");
                // get CO2 emissions for each technology
                temp[m] = techs[i][m]->get_emissmap_second("CO2");
            }
            dboutput4(regname,"CO2 Emiss",secname,str,"MTC",temp);
            // technology indirect CO2 emission
            for (m=0;m<maxper;m++) {
                temp[m] = summary[m].get_emindmap_second("CO2");
            }
            dboutput4(regname,"CO2 Emiss(ind)",secname,str,"MTC",temp);
            // technology ghg emissions, get gases for per 
            // all gases not just CO2
            map<string,double> temissmap = techs[i][0]->getemissmap();
            /*            for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
            for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second(gmap->first);
            }
            str = "Tech: "; // subsector heading
            str += secname; // sector name
            str += "_";
            str += name; // subsector name
            str += "_";
            str += techs[i][mm]->getName(); // technology name
            dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
            }
            */           // technology share
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getShare();
            }
            dboutput4(regname,"Tech Share",secname,str,"%",temp);
            // ghg tax applied to technology
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCarbontax();
            }
            dboutput4(regname,"C Tax",secname,str,"$/TC",temp);
            // ghg tax paid
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCarbontaxpaid();
            }
            dboutput4(regname,"C Tax Paid",secname,str,"90Mil$",temp);
            // technology fuel input
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getInput();
            }
            dboutput4(regname,"Fuel Consumption",secname,techs[i][0]->getFName(),"EJ",temp);
        }
        
        
        /*	CI fmap; // define fmap
        map<string,double> tfuelmap = summary[0].getfuelcons();
        for (fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        dboutput4(regname,"Fuel Consumption",secname,fmap->first,"EJ",temp);
        }
        */		
        // fuel consumption by subsector
        dboutput4(regname,"Fuel Consumption",secname+" by Subsec",name,"EJ",input);
        
        // for 1 or more technologies
        // technology efficiency
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEff();
        }
        dboutput4(regname,"Tech Efficiency",secname+" "+name,str,"%",temp);
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getIntensity(m);
        }
        dboutput4(regname,"Tech Intensity",secname+" "+name,str,"In/Out",temp);
    }
}

/*! \brief returns the number of technologies in this subsector
*
* \author Sonny Kim
* \return number of technologies
*/
int subsector::getNumberTech() const {
    return notech;
}

//! calculate GHG emissions from annual production of subresource
void subsector::emission( const int per, const string& prodname ){
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    summary[per].clearemiss(); // clear emissions map
    summary[per].clearemfuelmap(); // clear emissions map
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][per]->emission(prodname);
        summary[per].updateemiss(techs[i][per]->getemissmap());
        summary[per].updateemfuelmap(techs[i][per]->getemfuelmap());
    }
}

//! calculate indirect GHG emissions from annual production of subresource
void subsector::indemission(const int per, const vector<Emcoef_ind>& emcoef_ind ) {
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    summary[per].clearemindmap(); // clear emissions map
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][per]->indemission( emcoef_ind );
        summary[per].updateemindmap(techs[i][per]->getemindmap());
    }
}


/*! \brief returns (energy) input to sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return sector input
*/
double subsector::getInput( const int per ) const {
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return input[per];
}

/*! \brief calculates fuel input and subsector output.
*
* Sums technology output to get total sector output
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void subsector::sumOutput( const int per ) {
    output[per] = 0;
    bool watchSector = (name=="coal" && sectorName == "electricity" && regionName == "USAxx");
   if (watchSector) {cout << "(";}
    for ( int i=0 ;i<notech; i++ ) {
        output[per] += techs[i][per]->getOutput();
         if (watchSector) {
            cout << " S:"<< techs[i][per]->getShare();
            cout << " O:"<< techs[i][per]->getOutput();
            cout << " I:"<< techs[i][per]->getInput();         
         }
    }
   if (watchSector) {cout << ")";}
}

/*! \brief returns subsector output
*
* output summed every time to ensure consistency
* this is never called for demand sectors!
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return sector output
*/
double subsector::getOutput( const int per ) {
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    sumOutput( per );
    
    bool watchSector = (name=="biomass" && sectorName == "electricity" && regionName == "USAxx");
     watchSector = (name=="coal" && sectorName == "electricity" && regionName == "USAxx");
    // cout << "N: " << name  << " Sect: " << sectorName  << " reg: " << regionName << endl;
   if (watchSector) { 
   cout << " Out: "<< output[per], ", ";}
    return output[per];
}

/*! \brief returns total subsector carbon taxes paid
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return total carbon taxes paid by this sub-sector
*/
double subsector::getTotalCarbonTaxPaid( const int per ) const {
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return carbontaxpaid[per];
}

/*! \brief returns gets fuel consumption map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \pre updateSummary
* \todo Sonny or Josh -- is this precondition correct? Please edit (I'm not sure I understand when these functions are valid) 
* \return fuel consumption map
*/
map<string, double> subsector::getfuelcons(const int per) const {
    /*! \pre per is less than or equal to max period. */
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return summary[per].getfuelcons();
}

/*! \brief clears fuel consumption map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void subsector::clearfuelcons(const int per) {
    summary[per].clearfuelcons();
}

/*! \brief returns GHG emissions map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return GHG emissions map
*/
map<string, double> subsector::getemission(const int per) const {
    return summary[per].getemission();
}

/*! \brief returns map of GHG emissions by fuel for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return map of GHG emissions by fuel
*/
map<string, double> subsector::getemfuelmap( const int per ) const {
    return summary[per].getemfuelmap();
}

/*! \brief returns map of indirect GHG emissions for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
* \return map of indirect GHG emissions
*/
map<string, double> subsector::getemindmap(const int per) const {
    return summary[per].getemindmap();
}

/*! \brief update summaries for reporting
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void subsector::updateSummary(const int per) {
    int i = 0;
    string goodName;
    
    // clears subsector fuel consumption map
    summary[per].clearfuelcons();
    
    for (i=0;i<notech;i++) {
        goodName = techs[i][0]->getFName();
        summary[per].initfuelcons(goodName,techs[i][per]->getInput());
    }
}


