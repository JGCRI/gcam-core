/*! 
* \file subsector.cpp
* \ingroup CIAM
* \brief Subsector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/configuration.h"
#include "sectors/include/subsector.h"
#include "technologies/include/technology.h"
#include "containers/include/scenario.h"
#include "sectors/include/sector.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "containers/include/gdp.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Subsector::XML_NAME = "subsector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
const double LOGIT_EXP_DEFAULT = -3;

Subsector::Subsector( const string regionName, const string sectorName ){
    this->regionName = regionName;
    this->sectorName = sectorName;

    notech = 0;
    tax = 0;
    basesharewt = 0;
    Configuration* conf = Configuration::getInstance();
    debugChecking = conf->getBool( "debugChecking" );
	CO2EmFactor = 0;

    // resize vectors.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    capLimit.resize( maxper, 1.0 );
    shrwts.resize( maxper, 1.0 ); // default 1.0, for sectors with one tech.
    lexp.resize( maxper, LOGIT_EXP_DEFAULT );
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
* \author Josh Lurz
*/
Subsector::~Subsector() {
    
    for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            delete *innerIter;
        }
    }
}

//! Clear the Subsector member variables.
void Subsector::clear(){
    notech = 0;
    tax = 0;
    basesharewt = 0;
	CO2EmFactor = 0;
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
const string Subsector::getName() const {
    return name;
}

//! Initialize Subsector with xml data
void Subsector::XMLParse( const DOMNode* node ) {	
    
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
        
		else if( nodeName == technology::getXMLNameStatic1D() ){
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
					else if( childNodeName == technology::getXMLNameStatic2D() ){
                        int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                        techs[ techMapIter->second ][ thisPeriod ]->XMLParse( currChild );
                    }
                }
            }
            
            else {
                // create a new vector of techs.
                /*! \todo Clean this up and make it work with the deletion of objects. */
                childNodeList = curr->getChildNodes();
                
                // loop through technologies children.
                for( int j = 0; j < static_cast<int>( childNodeList->getLength() ); j++ ){
                    
                    currChild = childNodeList->item( j );
                    childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                    
                    if( childNodeName == technology::getXMLNameStatic2D() ){
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
void Subsector::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
    cout << "Unrecognized text string: " << nodeName << " found while parsing Subsector." << endl;
}

//! Complete the initialization.
void Subsector::completeInit() {
    // Initialize any arrays that have non-zero default value
    notech = static_cast<int>( techs.size() );
    
    for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            assert( *innerIter ); // Make sure the technology has been defined.
            ( *innerIter )->completeInit();
        }
    }
}

//! Output the Subsector member variables in XML format.
void Subsector::toInputXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i;
    
	XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.
    for( i = 0; i < static_cast<int>( capLimit.size() ); i++ ){
        XMLWriteElementCheckDefault( capLimit[ i ], "capacitylimit", out, tabs, 1.0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( calOutputValue.size() ); i++ ){
        if ( doCalibration[ i ] ) {
            XMLWriteElementCheckDefault( calOutputValue[ i ], "calOutputValue", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
        }
    }
    
    for( i = 0; i < static_cast<int>( shrwts.size() ); i++ ){
        XMLWriteElementCheckDefault( shrwts[ i ], "sharewt", out, tabs, 1.0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( lexp.size() ); i++ ){
        XMLWriteElementCheckDefault( lexp[ i ], "logitexp", out, tabs, LOGIT_EXP_DEFAULT, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( fuelPrefElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( fuelPrefElasticity[ i ], "fuelprefElasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    
    XMLWriteElementCheckDefault( basesharewt, "basesharewt", out, tabs, 0.0, modeltime->getstartyr( ) );
    
    // write out the technology objects.
    for( vector< vector< technology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
        
        // If we have an empty vector this won't work, but that should never happen.
        assert( j->begin() != j->end() );
        const technology* firstTech = *( j->begin() ); // Get pointer to first element in row. 
		XMLWriteOpeningTag( firstTech->getXMLName1D(), out, tabs, firstTech->getName() );
        
        for( vector<technology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
            ( *k )->toInputXML( out, tabs );
        }
        
		XMLWriteClosingTag( firstTech->getXMLName1D(), out, tabs );
    }
    
    // finished writing xml for the class members.
    
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! XML output for viewing.
void Subsector::toOutputXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    int i;
    
	XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.
    for( i = 0; i < static_cast<int>( capLimit.size() ); i++ ){
        XMLWriteElementCheckDefault( capLimit[ i ], "capacitylimit", out, tabs, 1.0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( calOutputValue.size() ); i++ ){
        if ( doCalibration[ i ] ) {
            XMLWriteElementCheckDefault( calOutputValue[ i ], "calOutputValue", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
        }
    }
    
    for( i = 0; i < static_cast<int>( shrwts.size() ); i++ ){
        XMLWriteElementCheckDefault( shrwts[ i ], "sharewt", out, tabs, 1.0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( lexp.size() ); i++ ){
        XMLWriteElementCheckDefault( lexp[ i ], "logitexp", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    
    for( i = 0; i < static_cast<int>( fuelPrefElasticity.size() ); i++ ){
        XMLWriteElementCheckDefault( fuelPrefElasticity[ i ], "fuelPrefElasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    
    XMLWriteElementCheckDefault( basesharewt, "basesharewt", out, tabs, 0.0, modeltime->getstartyr( ) );
    
    // write out the technology objects.
    for( vector< vector< technology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
        
        // If we have an empty vector this won't work, but that should never happen.
        assert( j->begin() != j->end() );
        const technology* firstTech = *( j->begin() ); // Get pointer to first element in row. 
		XMLWriteOpeningTag( firstTech->getXMLName1D(), out, tabs, firstTech->getName() );
        
        for( vector<technology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
            ( *k )->toInputXML( out, tabs );
        }
      
		XMLWriteClosingTag( firstTech->getXMLName1D(), out, tabs );
    }
    
    // finished writing xml for the class members.
    
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
*/
void Subsector::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
	XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // Write the xml for the class members.
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( fueltype, "fueltype", out, tabs );
    XMLWriteElement( notech, "notech", out, tabs );
    XMLWriteElement( tax, "tax", out, tabs );
    
    // Write the data for the current period within the vector.
    XMLWriteElement( capLimit[ period ], "capLimit", out, tabs );
    XMLWriteElement( shrwts[ period ], "sharewt", out, tabs );
    XMLWriteElement( lexp[ period ], "lexp", out, tabs );
    XMLWriteElement( fuelPrefElasticity[ period ], "fuelprefElasticity", out, tabs );
    XMLWriteElement( share[ period ], "share", out, tabs );
    XMLWriteElement( basesharewt, "basesharewt", out, tabs );
    XMLWriteElement( input[ period ], "input", out, tabs );
    XMLWriteElement( pe_cons[ period ], "pe_cons", out, tabs );
    XMLWriteElement( subsectorprice[ period ], "subsectorprice", out, tabs );
    XMLWriteElement( output[ period ], "output", out, tabs );
    XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out, tabs );
    
    // Write out the summary object.
    // summary[ period ].toDebugXML( period, out );
    // write out the technology objects.
    
    for( int j = 0; j < static_cast<int>( techs.size() ); j++ ){
        techs[ j ][ period ]->toDebugXML( period, out, tabs );
    }
    
    // write out the hydrotech. Not yet implemented
    // hydro[ period ].toDebugXML( period, out );
    
    // finished writing xml for the class members.
    
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
const std::string& Subsector::getXMLName() const {
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
const std::string& Subsector::getXMLNameStatic() {
	return XML_NAME;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \warning the ghg part of this routine assumes the existance of technologies in the previous and future periods
* \author Steve Smith
* \param period Model period
*/
void Subsector::initCalc( const int period ) {
   const Modeltime* modeltime = scenario->getModeltime();
    
    int i = 0;
    // Set any fixed demands
    for ( i=0 ;i<notech; i++ ) {        
        techs[i][ period ]->initCalc( );
        techs[i][ period ]->calcfixedOutput( period );
    }
    
    setCalibrationStatus( period );
    shareWeightScale( period ); 
    fixedShare[ period ] = 0;
    
    // Prevent pathological situation were share is zero where a fixed capacity is present.\
    // This can happen at begining of an initialization. Share will be set properly within secotr::calcShare 
    if ( ( getFixedOutput( period ) > 0 ) && ( fixedShare[ period ] == 0 ) ) {
       fixedShare[ period ] = 0.1;
    }
   
   // check to see if input fuel has changed
    for ( i=0 ;i<notech && period > 0; i++ ) {
      string prevFuel = techs[i][ period-1 ]->getFuelName();
      if ( prevFuel != techs[i][ period ]->getFuelName() ) {
         cerr << "WARNING: Type of fuel "<< prevFuel << " changed in period " << period << ", tech: ";
         cerr << techs[i][ period ]->getName();
         cerr << ", sub-s: "<< name << ", sect: " << sectorName << ", region: " << regionName << endl;
      }
    }

   // Pass forward any emissions information
    for ( i=0 ;i<notech && period > 0 && period < modeltime->getmaxper() ; i++ ) {
		std::vector<std::string> ghgNames;
		ghgNames = techs[i][period]->getGHGNames();
		
		int numberOfGHGs =  techs[ i ][ period ]->getNumbGHGs();

		if ( numberOfGHGs != techs[i][ period - 1 ]->getNumbGHGs() ) {
			cerr << "WARNING: Number of GHG objects changed in period " << period << ", tech: ";
			cerr << techs[i][ period ]->getName();
			cerr << ", sub-s: "<< name << ", sect: " << sectorName << ", region: " << regionName << endl;
		}
		// If number of GHG's decreased, then copy GHG objects
		if ( numberOfGHGs < techs[i][ period - 1 ]->getNumbGHGs() ) {
			// Not sure if to impliment this or not
		}
		
		// New method
		if ( period > 1 ) { // Note the hard coded base period
         for ( int j=0 ; j<numberOfGHGs; j++ ) {
            techs[i][ period ]->copyGHGParameters( techs[i][ period - 1]->getGHGPointer( ghgNames[j]  ) );
         } // End For
		}
      
	} // End For
}

/*! \brief Computes weighted cost of all technologies in Subsector.
*
* Called from calcShare after technology shares are determined. Calculates share-weighted total price (subsectorprice) and cost of fuel (fuelprice). 
*
* Price function separated to allow different weighting for Subsector price
* changed to void return maw
*
* \author Sonny Kim, Marshall Wise
* \param regionName region name
* \param period Model period
*/
void Subsector::calcPrice( const int period ) {
    const World* world = scenario->getWorld();
	int i=0;
    subsectorprice[period] = 0; // initialize to 0 for summing
    fuelprice[period] = 0; // initialize to 0 for summing
	CO2EmFactor = 0; // initialize to 0 for summing

    for (i=0;i<notech;i++) {
        // calculate weighted average price for Subsector
        subsectorprice[period] += techs[i][period]->getShare()*
            techs[i][period]->getTechcost();
        // calculate weighted average price of fuel only
        // technology shares are based on total cost
        fuelprice[period] += techs[i][period]->getShare()*
            techs[i][period]->getFuelcost();
        // calculate share weighted average CO2 emissions factor
        CO2EmFactor += techs[i][period]->getShare()*
			world->getPrimaryFuelCO2Coef(regionName, techs[i][period]->getFuelName());
    }
}

/*! \brief returns the sector price.
*
* Returns the weighted price from sectorprice variable. See also price method.
*
* \author Sonny Kim
* \param period Model period
*/
double Subsector::getPrice( const int period ) const {
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
bool Subsector::getCalibrationStatus( const int period ) const {
    return calibrationStatus[ period ];
}

/*! \brief Returns true if this Subsector, or underlying technologies, are calibrated.
*
* If either the Subsector output, or the output of all the technologies under this Subsector (not including those with zero output) are calibrated, then the calibrationStatus for the sector is set to true.
*
* \author Steve Smith
* \param period Model period
*/
void Subsector::setCalibrationStatus( const int period ) {
    if ( doCalibration[ period ] ) {
        calibrationStatus[ period ] = true;
    } 
	else {
        for (int i=0; i<notech; i++ ) {
            if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
                calibrationStatus[ period ] = true;
                return;
            }
        }
    }
}

/*! \brief returns Subsector capacity limit.
*
* The capacity limit is in terms of the sector share.
*
* \author Steve Smith
* \param period Model period
* \return Capacity limit for this sub-sector
*/
double Subsector::getCapacityLimit( const int period ) const {
    return capLimit[ period ];
}

/*! \brief sets flag for Subsector capacity limit status.
*
* capLimited is true when the sector has pegged at its capacity limit
*
* \author Steve Smith
* \param value This variable should be renamed and documented.
* \param period Model period
*/
void Subsector::setCapLimitStatus( const bool value, const int period ) {
   capLimited[ period ] = value;
}

/*! \brief returns Subsector capacity limit status.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return Boolean capacity limit status
*/
bool Subsector::getCapLimitStatus( const int period ) const {
    return capLimited[ period ];
}

/*! \brief returns Subsector fuel price.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return fuel price
*/
double Subsector::getfuelprice(int period) const
{
    return fuelprice[period];
}

/*! \brief returns Subsector CO2 emissions factor.
*
* \author Sonny Kim
* \param period Model period
* \return CO2EmFactor
*/
double Subsector::getCO2EmFactor(int period) const
{
    return CO2EmFactor;
}
/*! \brief returns Subsector fuel price times share
*
* Returns the share-weighted fuel price, which is later summed to get the sector-weighted fuel price (or cost)
*
* \author Sonny Kim
* \param period Model period
* \return share-weighted fuel price
*/
double Subsector::getwtfuelprice(int period) const
{
	double tempShare;
	// base year share
    if (period == 0) {
        tempShare = share[period]; 
    }
	// lagged one period
    else {
        tempShare = share[period-1];
    }
    return tempShare*fuelprice[period];
}

/*! \brief Sets ghg tax from the market to individual technologies.
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param ghgname name of the ghg to apply tax to
* \param period model period
*/
void Subsector::addGhgTax( const string& ghgname, const int period ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][ period ]->addGhgTax( ghgname, regionName, sectorName, period );
    }
}


/*! \brief calculate technology shares within Subsector
*
* Calls technology objects to first calculate cost, then their share. Follos this by normalizing shares. 
*
* \author Marshall Weise, Josh Lurz
* \param regionName region name
* \param period model period
* \warning technologies can not independently have fixed outputs at this point
*/
void Subsector::calcTechShares( const int period ) {
    int i=0;
    double sum = 0;
    
    for (i=0;i<notech;i++) {
        // calculate technology cost
        techs[i][period]->calcCost( regionName, period );
        // determine shares based on technology costs
        techs[i][period]->calcShare( regionName,period );
        sum += techs[i][period]->getShare();
    }
    // normalize technology shares to total 100 %
    for (i=0;i<notech;i++) {
        techs[i][period]->normShare(sum);
        // Logit exponential should not be zero or positive when more than one technology
        if(notech>1 && techs[i][period]->getlexp()>=0) {
          cerr << "Tech for sector " << name << " Logit Exponential is invalid (>= 0)" << endl;
        }
    }
}	


/*! \brief calculate Subsector unnormalized shares
*
* Calculates the un-normalized share for this sector. 
* Also claculates the sector aggregate price (or cost)
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param period model period
* \param gdp_cap GDP per capita, relative to base year
* \warning technologies can not independently have fixed outputs
* \warning there is no difference between demand and supply technologies. Control behavior with value of parameter fuelPrefElasticity
*/
void Subsector::calcShare(const int period, const GDP* gdp ) {
    // call function to compute technology shares
    calcTechShares( period );
    double gdp_cap;
    // calculate and return Subsector share; uses above price function
    // calc_price() uses normalized technology shares calculated above
    // Logit exponential should not be zero
    
    // compute Subsector weighted average price of technologies
    calcPrice( period);

    // Subsector logit exponential check
    if(lexp[period]==0) cerr << "SubSec Logit Exponential is 0." << endl;
    
    if( subsectorprice[period]==0) {
        share[period] = 0;
    }
    else {
		gdp_cap = gdp->getBestScaledGDPperCap( period );
		share[period] = shrwts[period]*pow(subsectorprice[period],lexp[period])*pow(gdp_cap,fuelPrefElasticity[period]);
	}
	
   if (shrwts[period]  > 1e4) {
    cout << "WARNING: Huge shareweight for sub-sector " << name << " : " << shrwts[period] 
         << " in region " << regionName <<endl;
   }
      
   if (share[period] < 0) {
     cerr << "Share is < 0 for " << name << " in " << regionName << endl;
     cerr << "    subsectorprice[period]: " << subsectorprice[period] << endl;
     cerr << "    shrwts[period]: " << shrwts[period] << endl;
   }   
	
}

/*! \brief normalizes Subsector shares
*
* \author Sonny Kim, Josh Lurz
* \param sum sum of sector shares
* \param period model period
* \warning sum must be correct sum of shares
* \pre calc shares must be called first
*/
void Subsector::normShare( const double sum, const int period) {
    if ( sum==0 ) {
        share[period]=0;
    }
    else {
        setShare( share[period] / sum, period );
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
* \param period Model period
*/
void Subsector::limitShares( const double multiplier, const int period ) {
   if ( multiplier == 0 ) {
      share[period] = 0;
   }
   else {	
      double capLimitValue = capLimitTransform( capLimit[period], share[period] );
      if ( share[period] >= capLimitValue ) {
         // Only adjust if not already capacity limited
         // need this because can't transform more than once, see capLimitTransform
         if ( !capLimited[ period ] ) {
            setShare( capLimitValue, period );
            setCapLimitStatus( true, period ); // set status to true
         }
      } 
	  else {
         if ( fixedShare[ period ] == 0 ) { // don't change if fixed
            setShare( share[period] * multiplier, period );
         }
      }
   }
}

/*! \brief Transform share to smoothly implement capacity limit.
*
* Function transforms the original share value into one that smoothly approaches the capacity limit.
* Returns the original orgShare when share << capLimit and returns capLimit when orgShare is large by using a logistic transformation.
* 
*
* \author Steve Smith
* \param capLimit capacity limit (share)
* \param orgShare original share for sector
* \return transfomred share value
*/
 double Subsector::capLimitTransform( double capLimit, double orgShare ) {
   const double SMALL_NUM = util::getSmallNumber();
   const double exponentValue =  2;
   const double mult =  1.4;
   double newShare = capLimit ;

   if ( capLimit < ( 1 - SMALL_NUM ) ) {
      double factor = exp( pow( mult * orgShare/capLimit , exponentValue ) );
      newShare = orgShare * factor/( 1 + ( orgShare/capLimit ) * factor);
   }
   return newShare;
}

/*! \brief Return the total exogenously fixed technology output for this sector.
*
* \author Steve Smith
* \param period model period
* \pre calc shares must be called first
*/
double Subsector::getFixedOutput( const int period ) const {
    double fixedOutput = 0;
    for ( int i=0 ;i<notech; i++ ) {
        fixedOutput += techs[i][period]->getFixedOutput();
    }
    return fixedOutput;
}

/*!\brief Return the share from this sub-sector that is fixed supply
* Enables communication of fixed share to other classes. 
*This is necessary since, while the amount of fixed supply is available (via getFixedOutput), the total output of a sector is not always known. So this function enables the amount of fixed supply in terms of the sector share to be communicated. 
*
* \author Steve Smith
* \param period Model period
*/
double Subsector::getFixedShare( const int period ) const {
    return fixedShare[ period ];
}

/*! \brief Save the share from this sub-sector that is fixed supply
* Enables communication of fixed share to other classes. See documentation for getFixedShare.
*
* \author Steve Smith
\param period Model period
\param share sector share that is fixed supply
*/
void Subsector::setFixedShare( const int period, const double share ) {

    // option to turn this off during calibration
    // This does not work correctly, shares will not sum to one. -JPL
    // if ( world->getCalibrationSetting() ) {
        fixedShare[ period ] = share;
        if ( share > 1 ) {
            cerr << "Share set to value > 1. Value = " << share << endl;
        }
    // }
}

/*! \brief Set the share from this sub-sector to that saved for fixed supply
* This function changes the share to the share previously saved for the fixed supply.
* This is done instead of using a function to directly set the share in general. 
* Doing this allows the price and calibration routines to operate with an appropriate share.
*
*\author Steve Smith
*\param period Model period
*/
void Subsector::setShareToFixedValue( const int period ) {
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
void Subsector::resetfixedOutput( const int period ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][period]->resetfixedOutput(period); // eliminate any previous down-scaleing
    }
}

/*! \brief Scale down fixed supply
* This is use dif the total fixed production is greater than the actual demand. See scalefixedOutput.
*
* \author Steve Smith
* \param period Model period
* \param scaleRatio multiplicative scale factor by which to scale fixed supply
*/
void Subsector::scalefixedOutput( const double scaleRatio, const int period ) {
    // scale fixed technology output down
    for ( int i=0 ;i<notech; i++ ) {
        techs[ i ][ period ]->scalefixedOutput( scaleRatio );
    }
    setFixedShare( period, fixedShare[ period ] * scaleRatio ); 
}

/*! \brief Consistantly adjust share weights after calibration 
* If the sector share weight in the previous period was changed due to calibration, 
* then adjust next few shares so that there is not a big jump in share weights.
*
* \author Steve Smith
* \param period Model period
* \todo Make end period year more general from data read-in.
*/
void Subsector::shareWeightScale( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // ***** SHK comments: I don't like the idea of having hardcoded numbers in the model
    // I don't think we can arbitrarily set a year here without knowing what
    // the input data is.  You use 2050 because shareWeights are the same after that.
    // Also this does not allow flexibility in the time step.
    
    // if previous period was calibrated, then adjust future shares
    if ( period > modeltime->getyr_to_per( 1990 ) ) {
        if ( calibrationStatus[ period - 1 ] ) {
            //int endPeriod = modeltime->getyr_to_per( 2050 );
            int endPeriod = modeltime->getyr_to_per( 2095 );
            shareWeightInterp( period - 1, endPeriod );
        }
    }
}

/*! \brief Linearly interpolate share weights between specified endpoints 
* Utility function to linearly scale share weights between two specified points.
*
* \author Steve Smith
* \param beginPeriod Period in which to begin the interpolation.
* \param endPeriod Period in which to end the interpolation.
*/
void Subsector::shareWeightInterp( const int beginPeriod,  const int endPeriod ) {
    
    if ( endPeriod > beginPeriod ) {
        double shareIncrement = ( shrwts[ endPeriod ] - shrwts[ beginPeriod ] ) / 
            ( endPeriod - beginPeriod );
        for ( int period = beginPeriod + 1 ;period<endPeriod ; period++ ) {
            shrwts[ period ] = shrwts[ period - 1 ] + shareIncrement;
        }
    }
}

//! Adjusts shares to be consistant with any fixed production 
/*! This routine does two things. 

If this sub-sector has a fixed supply, it sets the share to be consistant with the fixed supply
If this sub-sector does not have a fixed supply, it adjusts the share to be consistant with all the fixed supplies of all other sub-sectors (totalfixedOutput)

\param dmd total demand for all sectors
\param shareRatio amount variable shares need to be adjusted to be consistant with fixed supply
\param totalfixedOutput total fixed supply from all sub-sectors
\param period model period
*/
void Subsector::adjShares( const double demand, double shareRatio, 
                          const double totalfixedOutput, const int period ) {
    double sumSubsectfixedOutput = 0; // total Subsector fixed supply
    double fixedOutput = 0; // fixed supply for each technology
    double varShareTot = 0; // sum of shares without fixed supply
    double subsecdmd; // Subsector demand adjusted with new shares

    // add up the fixed supply and share of non-fixed supply
    for ( int i=0 ;i<notech; i++ ) {
        fixedOutput = techs[i][period]->getFixedOutput();
        sumSubsectfixedOutput += fixedOutput;
        if (fixedOutput == 0) { 
           varShareTot += techs[i][period]->getShare();
        }
    }
    
    // Adjust the share for this Subsector
    // This makes the implicit assumption that the Subsector is either all
    // fixed production or all variable. Would need to amend the logic below
    // to take care of other cases.
    
    // totalfixedOutput is the sector total
    if(totalfixedOutput > 0) {
        if (sumSubsectfixedOutput > 0) {	// This Subsector has a fixed supply
            if ( demand > 0 ) {
                setShare( sumSubsectfixedOutput/demand, period ); 
            }
            else { // no fixed share if no demand
                share[period] = 0; 
            }
        }
        else {	// This Subsector does not have fixed supply 
            if ( demand > 0 ) {
                setShare( share[period] * shareRatio, period ); 
            }
            else {
                share[period] = 0; 
            }  
        } 
    }
    
    // then adjust technology shares to be consistent
    subsecdmd = share[period]*demand; // share is Subsector level
    for (int j=0;j<notech;j++) {
        // adjust tech shares 
        techs[j][period]->adjShares(subsecdmd, sumSubsectfixedOutput, varShareTot, period);
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
* \param demand Total demand for this product
* \param period Model period
* \pre dmd must be the total demand for this project, so must be called after this has been determined
*/
void Subsector::setoutput( const double demand, const int period, const GDP* gdp ) {
    int i=0;
    input[period] = 0; // initialize Subsector total fuel input 
    carbontaxpaid[period] = 0; // initialize Subsector total carbon taxes paid 
    
    // note that output is in service unit when called from demand sectors
    // multiply dmd by Subsector share go get the total demand to be supplied by this Subsector
    double subsecdmd = share[period]* demand; 
    
    for ( i=0; i<notech; i++ ) {
        // calculate technology output and fuel input from Subsector output
        techs[i][period]->production( regionName, sectorName, subsecdmd, gdp, period );
        
        // total energy input into Subsector, must call after tech production
        input[period] += techs[i][period]->getInput();
        // sum total carbon tax paid for Subsector
        carbontaxpaid[period] += techs[i][period]->getCarbontaxpaid();
    }
}

/*! \brief Adjusts share weights and Subsector demand to be consistant with calibration value.
* Calibration is performed by scaling share weights to be consistant with the calibration value. Calibration is, therefore, performed as part of the iteration process. Since this can change derivitives, best to turn calibration off when using N-R solver.
* 
* Subector demand is also set equal to calibration value in order to pass down to technologies.
* This routine takes total demand into account so that total calibrated outputs cannot exceed demand.
* Also takes into account fixed supply, which is assumed to take presidence over calibration values
* Note that this routine doesn't notice if the calibration is at the technology or sub-sector level, this is taken care of by routine getTotalCalOutputs.
*
* \author Steve Smith
* \param sectorDemand total demand for this sector
* \param totalfixedOutput total amount of fixed supply for this sector
* \param totalCalOutputs total amount of calibrated outputs for this sector
* \param period Model period
* \warning If calvalue is larger than sector demand nothing is done
* \warning The value of subsecdmd is changed (for sub-sector output calibration)
*/
void Subsector::adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const int period ) {
   double shareScaleValue = 0;
   double availableDemand;
   double subSectorDemand;

   // total calibrated outputs for this sub-sector
   double calOutputSubsect = getTotalCalOutputs( period );

   // Determine available demand that can be shared out (subtract sub-sectors with fixed supply)
   availableDemand = sectorDemand - totalfixedOutput;
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
     cerr << "Share Weight is < 0 in Subsector " << name << endl;
     cerr << "    shrwts[period]: " << shrwts[ period ] << " (reset to 1)" << endl;
     shrwts[ period ] = 1;
   }


   // Debugging code useful for when something is amiss with base-year calibrations
   bool watchSector = ( name == "oil" && sectorName == "building" && regionName == "USAxx");
   if ( debugChecking && (shrwts[ period ] > 1e4 || watchSector) ) {
      if ( !watchSector ) {
         cout << "In calibration for sub-sector: " << name;
         cout << " in sector: "<< sectorName << " in region: " << regionName << endl;
      } else { cout << " ||" ; }
		cout << "Sector: "<< sectorName << " subsector: "<< name << "  subSector CalOut: " << getTotalCalOutputs( period );
		cout << "  CalIn: " << getFixedInputs( period, name, true ) << endl;
      cout << "  shrwts = " << shrwts[ period ] << ", sub-sec share = " << share[ period ]  << endl;
      cout << "  AvailD, totFix, totCal, calOutSect, subSectD, scaleVal: " << availableDemand << ", "; 
      cout <<  totalfixedOutput << ", " << totalCalOutputs << ", " <<  calOutputSubsect << ", " ; 
      cout << subSectorDemand << ", " << shareScaleValue << endl;
   }
}
  

/*! \brief returns the total calibrated output from this sector.
*
* Routine adds up calibrated values from both the sub-sector and (if not calibrated at Subsector), technology levels.
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
double Subsector::getTotalCalOutputs( const int period ) const {
    double sumCalValues = 0;

   if ( doCalibration[ period ] ) {
      sumCalValues += calOutputValue[ period ];
   } 
   else {
   	for ( int i=0; i<notech; i++ ) {
         if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
            
            if ( debugChecking ) {
              if ( techs[ i ][ period ]->getCalibrationOutput( ) < 0 ) {
                 cerr << "calibration < 0 for tech " << techs[ i ][ period ]->getName() 
                      << " in Subsector " << name << endl;
              }
            }

            sumCalValues += techs[ i ][ period ]->getCalibrationOutput( );
         }
      }
   }
   
   return sumCalValues;
}

/*! \brief returns the total calibrated or fixed input from this sector for the specified good.
*
* Routine adds up calibrated or fixed input values from all technologies.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
* \param bothVals optional parameter that specifies if both calibration and fixed values are returned (default is both)
* \return Total calibrated input for this Subsector
*/
double Subsector::getFixedInputs( const int period, const std::string& goodName, const bool bothVals ) const {
	double sumCalInputValues = 0;

	for ( int i=0; i<notech; i++ ) {
		if ( techHasInput( techs[ i ][ period ], goodName ) ) {
			if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
				sumCalInputValues += techs[ i ][ period ]->getCalibrationInput( );
			} 
			else if ( techs[ i ][ period ]->ouputFixed( ) && bothVals ) {
				sumCalInputValues += techs[ i ][ period ]->getFixedInput( );
			}
		}
   }
   return sumCalInputValues;
}

/*! \brief returns the total calibrated or fixed input from this sector for the specified good.
*
* Routine adds up calibrated or fixed input values from all technologies.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
* \param bothVals optional parameter that specifies if both calibration and fixed values are returned (default is both)
* \return Total calibrated input for this Subsector
*/
bool Subsector::inputsAllFixed( const int period, const std::string& goodName ) const {
	bool allInputsFixed = false;
	
	// test for each method of fixing output, if none of these are true then demand is not all fixed
	for ( int i=0; i<notech; i++ ) {
		if ( techHasInput( techs[ i ][ period ], goodName ) ) {
			if ( ( techs[ i ][ period ]->getCalibrationStatus( ) ) ) {
				allInputsFixed = true;
			} 
			else if ( techs[ i ][ period ]->ouputFixed( ) != 0 ) {
				allInputsFixed =  true;
			} else if ( shrwts[ period ] == 0) {
				allInputsFixed = true;
			} else {
				return false;
			}
		}
   }
   
   return true;
}

/*! \brief checks to see if technology demands the specified good
*
* Routine adds up calibrated or fixed input values from all technologies.
*
* \author Steve Smith
* \warning This routine depends on technologies being named for their fuel type. This works currently for electricity, but will not for other techs. Need to impliment a more robust method of checking calibrations.
* \param goodName market good to check for
* \param pointer to technology to consider
* \return True if the specified technology has goodname as input
* \todo Need a more robust way of doing this check (requires a more fundamental change to the way calibrated inputs and outputs are found)
*/
bool Subsector::techHasInput( const technology* thisTech, const std::string& goodName ) const {
	
	return ( thisTech->getName() == goodName );
	
}

/*! \brief Scales calibrated values for the specified good.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
* \param bothVals optional parameter that specifies if both calibration and fixed values are returned (default is both)
* \return Total calibrated input for this Subsector
*/
void Subsector::scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue ) {

	for ( int i=0; i<notech; i++ ) {
		if ( techHasInput( techs[ i ][ period ], goodName ) ) {
			if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
				techs[ i ][ period ]->scaleCalibrationInput( scaleValue );
			} 
		}
   }
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this Subsector or all technologies in this sub-sector returns true.
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
bool Subsector::allOuputFixed( const int period ) const {
   bool oneNotFixed = false;
   bool outputFixed = false;

   if ( doCalibration[ period ] ) {
      outputFixed = true;  // this sector has fixed output
   } 
   else  if ( shrwts[ period ] == 0 ) {
      outputFixed = true; // this sector has no output, so is also fixed
   }
   // if not fixed at sub-sector level, then check at the technology level
   else {
      for ( int i=0; i<notech; i++ ) {
         if ( !( techs[ i ][ period ]->ouputFixed( ) ) ) {
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
*
* \author Steve Smith
* \param period Model period
* \param scaleFactor Multiplicitive scale factor for each calibration value
*/
void Subsector::scaleCalibrationInput( const int period, const double scaleFactor ) {
    for ( int i=0; i<notech; i++ ) {
        techs[ i ][ period ]->scaleCalibrationInput( scaleFactor );
    }
}

/*! \brief returns share for this Subsector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \pre calcShare
* \return share value
*/
double Subsector::getShare( const int period ) const {
    return share[period];
}

/*! \brief set share for this Subsector with normalization check
*
* Use this function to set the share at any time where shares are supposed to be normalized
*
* \author Steve Smith
* \param shareVal Value to which to set the share.
* \param period Model period
*/
void Subsector::setShare( const double shareVal, const int period ) {
const double tinyNumber = util::getVerySmallNumber();

   share[ period ] = shareVal;
   if ( shareVal > (1 + tinyNumber ) ) {
      cerr << "ERROR - share value set > 1. Val: " << shareVal << endl;
   }
}

//! write Subsector output to database
void Subsector::csvOutputFile() const {
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
    // total Subsector output
    fileoutput3( regionName,sectorName,name," ","production","EJ",output);
    // Subsector price
    fileoutput3( regionName,sectorName,name," ","price","$/GJ(ser)",subsectorprice);
    // Subsector carbon taxes paid
    for (m=0;m<maxper;m++)
        temp[m] = summary[m].get_emissmap_second("CO2");
    fileoutput3( regionName,sectorName,name," ","C tax paid","Mil90$",carbontaxpaid);
    fileoutput3( regionName,sectorName,name," ","CO2 emiss","MTC",temp);

// sjs -- bad coding here, hard-wired period. But is only for csvOutputFile.
		int numberOfGHGs =  techs[ i ][ 2 ]->getNumbGHGs();
		if (numberOfGHGs > 1 ) {
		   std::vector<std::string> ghgNames;
			ghgNames = techs[i][ 2 ]->getGHGNames();
			for ( int ghgN = 2; ghgN <= numberOfGHGs; ghgN++ ) {
				for (m=0;m<maxper;m++) {
					temp[m] = summary[m].get_emissmap_second( ghgNames[ ghgN - 1 ] );
				}
				string ghgLabel = ghgNames[ ghgN - 1 ] + " emiss";
				fileoutput3( regionName,sectorName,name," ",ghgLabel,"Tg",temp);
			}
		}
    
    // do for all technologies in the Subsector
    for (i=0;i<notech;i++) {
        // output or demand for each technology
		for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"production","EJ",temp);
        // technology share
        if(notech>1) {
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getShare();
            }
            fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"tech share","%",temp);
        }
        // technology cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"price","$/GJ",temp);
        // ghg tax applied to technology
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbontax();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"C tax","$/TC",temp);
        // ghg tax paid
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbontaxpaid();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"C tax paid","90Mil$",temp);
        // technology fuel input
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getInput();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"fuel consump","EJ",temp);
        // technology efficiency
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEff();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"efficiency","%",temp);
        // technology non-energy cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNecost();
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"non-energy cost","$/GJ",temp);
        // technology CO2 emission
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2");
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"CO2 emiss","MTC",temp);
        // technology indirect CO2 emission
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2ind");
        }
        fileoutput3( regionName,sectorName,name,techs[i][mm]->getName(),"CO2 emiss(ind)","MTC",temp);
    }
	
}

//! write MiniCAM style Subsector output to database
/*! Part A for supply sector, titles and units are different for Part B */
void Subsector::MCoutputA() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    int i=0, m=0;
    int mm=0; // temp period
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double cvrt90 = 2.212; //  convert '75 price to '90 price
    vector<double> temp(maxper);
    
    // total Subsector output
    dboutput4(regionName,"Secondary Energy Prod",sectorName,name,"EJ",output);
    // Subsector price
    dboutput4(regionName,"Price",sectorName,name,"75$/GJ",subsectorprice);
    // for electricity sector only
    if (sectorName == "electricity") {
        for (m=0;m<maxper;m++) {
            temp[m] = subsectorprice[m] * cvrt90 * 0.36;
        }
        dboutput4(regionName,"Price",sectorName+" C/kWh",name,"90C/kWh",temp);
    }
    
    string tssname = "tech_"; // tempory Subsector name
    string str1, str2; // tempory string
    // do for all technologies in the Subsector
    for (i=0;i<notech;i++) {
        str1 = sectorName;
        str1 += "_tech";
        str2 = techs[i][mm]->getName();
        // technology non-energy cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNecost();
        }
        dboutput4(regionName,"Price NE Cost",sectorName,str2,"75$/GJ",temp);
        // secondary energy and price output by tech
        // output or demand for each technology
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput();
        }
        dboutput4(regionName,"Secondary Energy Prod",str1,str2,"EJ",temp);
        // technology cost
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost()*cvrt90;
        }
        dboutput4(regionName,"Price",str1,str2,"90$/GJ",temp);
    }
}

//! write MiniCAM style Subsector output to database
/*! Part B for demand sector, titles and units are different from Part A */
void Subsector::MCoutputB() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    int i=0, m=0;
    int mm=0; // temp period
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // total Subsector output
    dboutput4(regionName,"End-Use Service",sectorName+" by Subsec",name,"Ser Unit",output);
    dboutput4(regionName,"End-Use Service",sectorName+" "+name,"zTotal","Ser Unit",output);
    // Subsector price
    dboutput4(regionName,"Price",sectorName,name+" Tot Cost","75$/Ser",subsectorprice);
    
 
    string tssname = "tech "; // tempory Subsector name
    string str; // tempory string
    // do for all technologies in the Subsector
    for (i=0;i<notech;i++) {
        //str = tssname + techs[i][mm].showname();
        str = techs[i][mm]->getName();
        if(notech>1) {  // write out if more than one technology
            // output or demand for each technology
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getOutput();
            }
            dboutput4(regionName,"End-Use Service",sectorName+" "+name,str,"Ser Unit",temp);
            // total technology cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getTechcost();
            }
            dboutput4(regionName,"Price",sectorName+" "+name,str,"75$/Ser",temp);
           // technology fuel cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getFuelcost();
            }
            dboutput4(regionName,"Price",sectorName+" "+name+" Fuel Cost",str,"75$/Ser",temp);
            // technology non-energy cost
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getNecost();
            }
            dboutput4(regionName,"Price",sectorName+" "+name+" NE Cost",str,"75$/Ser",temp);
        }
    }
}


//! write MiniCAM style Subsector output to database
void Subsector::MCoutputC() const {
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
    //dboutput4(regionName,"CO2 Emiss",sectorName,name,"MTC",temp);
    // Subsector carbon taxes paid
    dboutput4(regionName,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
    // Subsector share 
    dboutput4(regionName,"Subsec Share",sectorName,name,"100%",share);
    // Subsector emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        str = "Subsec: "; // Subsector heading
        str+= sectorName; // sector name
        str+= "_";
        str+= name; // Subsector name
        dboutput4(regionName,"Emissions",str,gmap->first,"MTC",temp);
    }
    
    string tssname = name; // tempory Subsector name
    int mm=0; // temp period to get base period
    // do for all technologies in the Subsector
    for (i=0;i<notech;i++) {
        str = tssname + techs[i][mm]->getName();
        //str = techs[i][mm]->getName();
        if(notech>0) {  // write out for all technology
            // technology CO2 emission
            for (m=0;m<maxper;m++) {
                // this gives Subsector total CO2 emissions
                //temp[m] = summary[m].get_emissmap_second("CO2");
                // get CO2 emissions for each technology
                temp[m] = techs[i][m]->get_emissmap_second("CO2");
            }
            //dboutput4(regionName,"CO2 Emiss",sectorName,str,"MTC",temp);
            dboutput4(regionName,"CO2 Emiss",sectorName,name,"MTC",temp);
            // technology indirect CO2 emission
            for (m=0;m<maxper;m++) {
                temp[m] = summary[m].get_emindmap_second("CO2");
            }
            dboutput4(regionName,"CO2 Emiss(ind)",sectorName,str,"MTC",temp);
            // technology ghg emissions, get gases for period 
            // all gases not just CO2
            map<string,double> temissmap = techs[i][0]->getemissmap();
            /*            for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
            for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second(gmap->first);
            }
            str = "Tech: "; // Subsector heading
            str += sectorName; // sector name
            str += "_";
            str += name; // Subsector name
            str += "_";
            str += techs[i][mm]->getName(); // technology name
            dboutput4(regionName,"Emissions",str,gmap->first,"MTC",temp);
            }
            */           // technology share
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getShare();
            }
            dboutput4(regionName,"Tech Share",sectorName,str,"%",temp);
            
			// ghg tax applied to technology
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCarbontax();
            }
            dboutput4(regionName,"C Tax",sectorName,str,"$/TC",temp);

			// ghg tax and storage cost applied to technology if any
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCarbonValue();
            }
            dboutput4(regionName,"C Value",sectorName,str,"$/gj",temp);

			// ghg tax paid
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCarbontaxpaid();
            }
            dboutput4(regionName,"C Tax Paid",sectorName,str,"90Mil$",temp);
            
			// technology fuel input
            for (m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getInput();
            }
            dboutput4(regionName,"Fuel Consumption",sectorName,techs[i][0]->getFuelName(),"EJ",temp);
        }
        
        
        /*	CI fmap; // define fmap
        map<string,double> tfuelmap = summary[0].getfuelcons();
        for (fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        dboutput4(regionName,"Fuel Consumption",sectorName,fmap->first,"EJ",temp);
        }
        */		
        // fuel consumption by Subsector
        dboutput4(regionName,"Fuel Consumption",sectorName+" by Subsec",name,"EJ",input);
        
        // for 1 or more technologies
        // technology efficiency
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEff();
        }
        dboutput4(regionName,"Tech Efficiency",sectorName+" "+name,str,"%",temp);
        for (m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getIntensity(m);
        }
        dboutput4(regionName,"Tech Intensity",sectorName+" "+name,str,"In/Out",temp);
    }
}

//! calculate GHG emissions from annual production of each technology
void Subsector::emission( const int period ){
    /*! \pre period is less than or equal to max period. */
    assert( period <= scenario->getModeltime()->getmaxper() );
    summary[period].clearemiss(); // clear emissions map
    summary[period].clearemfuelmap(); // clear emissions map

    for ( int i = 0; i < notech; i++ ) {
        techs[i][period]->calcEmission( sectorName );
        summary[period].updateemiss( techs[i][period]->getemissmap() );
        summary[period].updateemfuelmap( techs[i][period]->getemfuelmap() );
    }
}

//! calculate indirect GHG emissions from annual production of each technology
void Subsector::indemission( const int period, const vector<Emcoef_ind>& emcoef_ind ) {
    /*! \pre period is less than or equal to max period. */
    assert( period <= scenario->getModeltime()->getmaxper() );
    summary[period].clearemindmap(); // clear emissions map
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][period]->indemission( emcoef_ind );
        summary[period].updateemindmap(techs[i][period]->getemindmap());
    }
}


/*! \brief returns (energy) input to sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector input
*/
double Subsector::getInput( const int period ) const {
    /*! \pre period is less than or equal to max period. */
    assert( period <= scenario->getModeltime()->getmaxper() );
    
    return input[period];
}

/*! \brief calculates fuel input and Subsector output.
*
* Sums technology output to get total sector output
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
*/
void Subsector::sumOutput( const int period ) {
    output[period] = 0;
    for ( int i=0 ;i<notech; i++ ) {
        output[period] += techs[i][period]->getOutput();
    }
}

/*! \brief returns Subsector output
*
* output summed every time to ensure consistency
* this is never called for demand sectors!
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector output
*/
double Subsector::getOutput( const int period ) {
    /*! \pre period is less than or equal to max period. */
   assert( period <= scenario->getModeltime()->getmaxper() );
   sumOutput( period );
   return output[period];
}

/*! \brief returns total Subsector carbon taxes paid
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return total carbon taxes paid by this sub-sector
*/
double Subsector::getTotalCarbonTaxPaid( const int period ) const {
    /*! \pre period is less than or equal to max period. */
    assert( period <= scenario->getModeltime()->getmaxper() );
    
    return carbontaxpaid[ period ];
}

/*! \brief returns gets fuel consumption map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \pre updateSummary
* \todo Sonny or Josh -- is this precondition correct? Please edit (I'm not sure I understand when these functions are valid) 
* \return fuel consumption map
*/
map<string, double> Subsector::getfuelcons( const int period ) const {
    /*! \pre period is less than or equal to max period. */
    assert( period <= scenario->getModeltime()->getmaxper() );
    
    return summary[period].getfuelcons();
}

/*! \brief clears fuel consumption map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
*/
void Subsector::clearfuelcons( const int period ) {
    summary[ period ].clearfuelcons();
}

/*! \brief returns GHG emissions map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return GHG emissions map
*/
map<string, double> Subsector::getemission( const int period ) const {
    return summary[ period ].getemission();
}

/*! \brief returns map of GHG emissions by fuel for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return map of GHG emissions by fuel
*/
map<string, double> Subsector::getemfuelmap( const int period ) const {
    return summary[ period ].getemfuelmap();
}

/*! \brief returns map of indirect GHG emissions for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return map of indirect GHG emissions
*/
map<string, double> Subsector::getemindmap( const int period ) const {
    return summary[ period ].getemindmap();
}

/*! \brief update summaries for reporting
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
*/
void Subsector::updateSummary( const int period ) {
    int i = 0;
    string goodName;
    
    // clears Subsector fuel consumption map
    summary[period].clearfuelcons();
    
    for (i=0;i<notech;i++) {
        goodName = techs[i][0]->getFuelName();
        summary[period].initfuelcons(goodName,techs[i][period]->getInput());
    }
}

