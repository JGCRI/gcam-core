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
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cmath>
#include <cassert>
#include <vector>

#include "subsector.h"
#include "technology.h"
#include "scenario.h"
#include "modeltime.h"
#include "xmlHelper.h"
#include "marketplace.h"
#include "summary.h"
#include "Emcoef_ind.h"

using namespace std;

extern ofstream outfile;	
extern Scenario* scenario;

//! Default constructor
subsector::subsector(){
    notech = 0;
    tax = 0;
    basesharewt = 0;
    
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
}

//! Destructor.
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

//! Return the sector name.
const string subsector::getName() const {
    return name;
}


//! Initialize subsector with xml data
void subsector::XMLParse( const DOMNode* node ) {	
    
    const Modeltime* modeltime = scenario->getModeltime();
    DOMNodeList* nodeList = 0;
    DOMNodeList* childNodeList = 0;
    DOMNode* curr = 0;
    DOMNode* currChild = 0;
    string nodeName;
    string childNodeName;
    vector<technology*> techVec( modeltime->getmaxper(), 0 );
    technology* tempTech = 0;
    
    //! \pre Make sure we were passed a valid node.
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
               for( int j = 0; j < childNodeList->getLength(); j++ ){
                
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
               for( int j = 0; j < childNodeList->getLength(); j++ ){
                
                  currChild = childNodeList->item( j );
                  childNodeName = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                
                  if( childNodeName == "period" ){
                     tempTech = new technology();
                     tempTech->XMLParse( currChild );
                     int thisPeriod = XMLHelper<int>::getNodePeriod( currChild, modeltime );
                     techVec[ thisPeriod ] = tempTech;
                  }
               }
               techs.push_back( techVec );
               techNameMap[ techVec[ 0 ]->getName() ] = techs.size() - 1;
               techVec.clear();
               techVec.resize( modeltime->getmaxper(), 0 );
            }
        }
    }
}

//! Complete the initialization.
void subsector::completeInit() {
   // Initialize any arrays that have non-zero default value
    notech = techs.size();
   
    for ( vector< vector< technology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< technology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
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

//! Write out the state of all member variables to the debug xml output stream.
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

//! Set the share weight and logit exponentional for the current period to the values from the last period.
void subsector::copytolast( const int period ) {
    shrwts[ period ] = shrwts[ period - 1 ];
    lexp[ period ] = lexp[ period - 1 ];
}


//! Perform initializations that only need to be done once per period
void subsector::initCalc( const int period ) {
    
    // Set any fixed demands
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][ period ]->calcFixedSupply( period );
    }
    
    setCalibrationStatus( period );
    
    shareWeightScale( period ); 
    
}


//! Computes weighted cost of all technologies in subsector
/*! price function called below in calc_share after technology shares are determined.
price function separated to allow different weighting for subsector price
changed to void return maw */
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

//! returns subsector price 
double subsector::getPrice( const int period ) const {
    return subsectorprice[ period ];
}

//! returns true if this subsector, or underlying technologies, are calibrated 
bool subsector::getCalibrationStatus( const int period ) const {
    return calibrationStatus[ period ];
}

//! returns true if this subsector, or underlying technologies, are calibrated 
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

//! returns subsector capacity limit 
double subsector::getCapacityLimit( const int period ) const {
    return capLimit[ period ];
}

//! returns subsector fuel price 
double subsector::getfuelprice(int per) const
{
    return fuelprice[per];
}

//! returns subsector base share weighted fuel price 
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

//! passes carbon tax to technology
void subsector::applycarbontax( const string& regionName, const double tax, const int period ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][ period ]->applycarbontax( regionName, tax );
    }
}

//! sets ghg tax to technologies
void subsector::addghgtax( const string& ghgname, const string& regionName, const int per ) {
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][per]->addghgtax( ghgname, regionName, per );
    }
}


// maw  calculate technology shares within subsector
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
        // Logit exponential should not be zero or positive when more than
        // one technology
        if(notech>1 && techs[i][per]->getlexp()>=0) cerr << "Tech Logit Exponential is invalid." << endl;
    }
}	


//! calculate subsector share numerator 
void subsector::calcShare( const string& regionName, const int per, const double gnp_cap )
{
	
     // call function to compute technology shares
	subsector::calcTechShares(regionName, per);

	// calculate and return subsector share; uses above price function
	// calc_price() uses normalized technology shares calculated above
	// Logit exponential should not be zero

	// compute subsector weighted average price of technologies
	subsector::calcPrice( regionName,per);
	
	// subsector logit exponential
	if(lexp[per]==0) cerr << "SubSec Logit Exponential is 0." << endl;

	if(subsectorprice[per]==0) {
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
    cout << "WARNING: Huge shareweight for " << name << " : " << shrwts[per] 
         << " in region " << regionName <<endl;
   }
      
   if (share[per] < 0) {
     cerr << "Share is < 0 for " << name << " in " << regionName << endl;
     cout << "subsectorprice[per]: " << subsectorprice[per] << endl;
     cout << "shrwts[per]: " << shrwts[per] << endl;
   }
}

//! normalizes shares to 100%
void subsector::normShare( const double sum, const int per) {
    if ( sum==0 ) {
        share[per]=0;
    }
    else {
        share[per] /= sum;
    }
}

/*!
* \brief normalizes shares to 100% subject to capacity limit.
*
* Used by sector::calc_share() to re-normalize shares, adjusting for capacity limits.
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
void subsector::limitShares( const double multiplier, const int per) {
    if ( multiplier == 0 ) {
        share[per] = 0;
    }
    else {		
        if ( share[per] >= capLimit[per]) {
            share[per] = capLimit[per];
        } else {
            share[per] *= multiplier;
        }
    }
}

//! Return the total exogenously fixed technology output
/*! Since the calls below set output, this call must be done before
    \todo elimiate calcFixedSupply calls below
    */
double subsector::getFixedSupply( const int per ) const {
	double fixedOutput = 0;
	for ( int i=0 ;i<notech; i++ ) {
		fixedOutput += techs[i][per]->getFixedSupply();
	}
	return fixedOutput;
}

//! Reset fixed supply for each technology
void subsector::resetFixedSupply( const int per ) {
	for ( int i=0 ;i<notech; i++ ) {
		techs[ i ][per]->resetFixedSupply(per); // eliminate any previous down-scaleing
 	}
}

//! Scale down fixed supply if the total fixed production is greater than the actual demand 
void subsector::scaleFixedSupply( const double scaleRatio, const int per ) {
    // scale fixed technology output down
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][per]->scaleFixedSupply(scaleRatio);
    }
}

//! Consistantly adjust share weights after calibration 
/*! If the previous sector share was changed due to calibration, 
* then adjust next few shares so that there is not a big jump in share weights.
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

//! Linearly interpolate share weights between specified endpoints
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
// total demand for all sectors
// sum of sector shares that have no fixed production
// total fixed supply from all sectors
// model period
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
        if (fixedSupply == 0) varShareTot += techs[i][per]->getShare();
    }
    
    // Adjust the share for this subsector
    // This makes the implicit assumption that the subsector is either all
    // fixed production or all variable. Would need to amend the logic below
    // to take care of other cases.
    
    // totalFixedSupply is the sector total
    if(totalFixedSupply > 0) {
        if (sumSubsectFixedSupply > 0) {	// This subsector has a fixed supply
            if (dmd > 0) {
                share[per] = sumSubsectFixedSupply/dmd; 
            }
            else { // no fixed share if no demand
				share[per] = 0; 
			}
		}
		else {	// This subsector does not have fixed supply
			if (dmd > 0) {
				share[per] = share[per] * shareRatio; 
			}
			else {
				share[per] = 0; // Maybe not correct
			}  
		} 
	}
	
	// then adjust technology shares to be consistent
	subsecdmd = share[per]*dmd; // share is subsector level
	for (int j=0;j<notech;j++) {
		// adjust tech shares 
      // TEMP sjs -- see what happens if remove this
		techs[j][per]->adjShares(subsecdmd, sumSubsectFixedSupply, varShareTot, per);
	}
}

//! sets demand to output and output
/* Demand from the "dmd" parameter (could be energy or energy service) is passed to technologies.
*  This is then shared out at the technology level.
*  See explanation for sector::setoutput. 
*/
void subsector::setoutput( const string& regionName, const string& prodName, const double dmd, const int per) {
	int i=0;
	input[per] = 0; // initialize subsector total fuel input 
	carbontaxpaid[per] = 0; // initialize subsector total carbon taxes paid 
   
   // output is in service unit when called from demand sectors
   double subsecdmd = share[per]*dmd; // share is subsector level

   for ( i=0; i<notech; i++ ) {
		// calculate technology output and fuel input from subsector output
		techs[i][per]->production( regionName, prodName, subsecdmd, per );

		// total energy input into subsector, must call after tech production
		input[per] += techs[i][per]->getInput();
		// sum total carbon tax paid for subsector
		carbontaxpaid[per] += techs[i][per]->getCarbontaxpaid();
	}
   
}

//! Adjusts share weights and subsector demand to be consistant with calibration value
/* Share weights are scaled to be consistant with the calibration value.
 * subector demand is also set equal to calibration value in order to pass down to technologies.
 * \warning If calvalue is larger than sector demand nothing is done
 * \warning The value of subsecdmd is changed (for sub-sector output calibration)
 * \todo add in pre-period run a check to see if everything was calibrated
 */
void subsector::adjustForCalibration( double sectorDemand, double totalFixedSupply, double totalCalOutputs, const int period ) {
   double shareScaleValue = 0;
   double availableDemand;
   double subSectorDemand;

   // total calibration outputs for this sector
   double calOutputSubsect = getTotalCalOutputs( period );

   // Adjust calibration value if too large
   availableDemand = sectorDemand - totalFixedSupply;
   if ( availableDemand < 0 ) {
      availableDemand = 0;
   }
   
   if ( calOutputSubsect > availableDemand ) {
     // adjust cal value, but leave a slight bit of headroom, taking into account other cal outputs
      calOutputSubsect = (calOutputSubsect/totalCalOutputs) * availableDemand * 0.95;
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
      
 }
  
//! returns total calibrated output values
double subsector::getTotalCalOutputs( const int per ) const {
	double sumCalValues = 0;

   if ( doCalibration[ per ] ) {
      sumCalValues += calOutputValue[ per ];
   } 
   else {
   	for ( int i=0; i<notech; i++ ) {
         if ( techs[ i ][ per ]->getCalibrationStatus( ) ) {
            sumCalValues += techs[ i ][ per ]->getCalibrationOutput( );
         }
      }
   }
   
   return sumCalValues;
}

//! scale calibration values
void subsector::scaleCalibrationInput( const int per, const double scaleFactor ) {
   for ( int i=0; i<notech; i++ ) {
	   techs[ i ][ per ]->scaleCalibrationInput( scaleFactor );
	}
}

//! calculates fuel input and subsector output
void subsector::sumoutput( const int per ) {
    output[per] = 0;
    for ( int i=0 ;i<notech; i++ ) {
        output[per] += techs[i][per]->getOutput();
    }
}


//! write subsector info to screen
void subsector::show_subsec() const {
    int i=0;
    int m=0; // temp period
    //write to file or database later
    cout << name << endl;
    cout << "Number of Technologies: " << notech << endl;
    for (i=0;i<notech;i++)
        cout<<"Share["<<i<<"] "<<techs[i][m]->getShare()<< endl;
    cout <<"Total subsector Output: " << output[m] << endl;
}
//! returns share for each subsector
double subsector::getShare( const int per ) const {
    return share[per];
}

//! prints to outputfile all technologies in subsector by period
void subsector::showtechs( const int per, const string ofile ) const {
    int i=0;
    
    for (i=0;i<notech;i++) {
        // use technology class method show_tech
        techs[i][per]->printTech(ofile);
    }
}

//! prints to outputfile all subsector labels (name and index)
void subsector::showlabel( const string& ofile ) const {
    ofstream outfile;
    
    outfile.open(ofile.c_str(), ios::app);
    
    if (!outfile) {
        //open failed
        cerr<<"Cannot open file for output\n";
        exit(-1);
    }
    
    outfile << "Subsector: " << name <<  endl;
    outfile << "Number of Technologies: " << notech <<  endl;
    outfile.close();
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
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getOutput();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"production","EJ",temp);
        // technology share
        if(notech>1) {
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getShare();
            fileoutput3( regname,secname,name,techs[i][mm]->getName(),"tech share","%",temp);
        }
        // technology cost
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getTechcost();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"price","$/GJ",temp);
        // ghg tax applied to technology
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getCarbontax();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"C tax","$/TC",temp);
        // ghg tax paid
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getCarbontaxpaid();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"C tax paid","90Mil$",temp);
        // technology fuel input
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getInput();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"fuel consump","EJ",temp);
        // technology efficiency
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getEff();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"efficiency","%",temp);
        // technology non-energy cost
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getNecost();
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"non-energy cost","$/GJ",temp);
        // technology CO2 emission
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->get_emissmap_second("CO2");
        fileoutput3( regname,secname,name,techs[i][mm]->getName(),"CO2 emiss","MTC",temp);
        // technology indirect CO2 emission
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->get_emissmap_second("CO2ind");
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
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getNecost();
        dboutput4(regname,"Price NE Cost",secname,str2,"75$/GJ",temp);
        // secondary energy and price output by tech
        // output or demand for each technology
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getOutput();
        dboutput4(regname,"Secondary Energy Prod",str1,str2,"EJ",temp);
        // technology cost
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getTechcost()*cvrt90;
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
    dboutput4(regname,"End-Use Service",secname,name,"Ser Unit",output);
    // subsector price
    dboutput4(regname,"Price",secname,name,"75$/Ser",subsectorprice);
    
    string tssname = "tech_"; // tempory subsector name
    string str; // tempory string
    // do for all technologies in the subsector
    for (i=0;i<notech;i++) {
        //str = tssname + techs[i][mm].showname();
        str = techs[i][mm]->getName();
        if(notech>1) {  // write out if more than one technology
            // output or demand for each technology
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getOutput();
            dboutput4(regname,"End-Use Service",secname,str,"Ser Unit",temp);
            // technology cost
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getTechcost();
            dboutput4(regname,"Price",secname,str,"75$/Ser",temp);
        }
        // technology fuel cost
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getFuelcost();
        dboutput4(regname,"Price",secname+" Fuel Cost",str,"75$/Ser",temp);
        // technology non-energy cost
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getNecost();
        dboutput4(regname,"Price",secname+" NE Cost",str,"75$/Ser",temp);
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
            for (m=0;m<maxper;m++)
                temp[m] = summary[m].get_emindmap_second("CO2");
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
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getShare();
            dboutput4(regname,"Tech Share",secname,str,"%",temp);
            // ghg tax applied to technology
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getCarbontax();
            dboutput4(regname,"C Tax",secname,str,"$/TC",temp);
            // ghg tax paid
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getCarbontaxpaid();
            dboutput4(regname,"C Tax Paid",secname,str,"90Mil$",temp);
            // technology fuel input
            for (m=0;m<maxper;m++)
                temp[m] = techs[i][m]->getInput();
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
        
        
        // for 1 or more technologies
        // technology efficiency
        for (m=0;m<maxper;m++)
            temp[m] = techs[i][m]->getEff();
        dboutput4(regname,"Tech Efficiency",secname,str,"%",temp);
    }
}

int subsector::shownotech() const {
    return notech;
}

//! calculate GHG emissions from annual production of subresource
void subsector::emission( const int per, const string& prodname ){
    //! \pre per is less than or equal to max period.
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
    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    summary[per].clearemindmap(); // clear emissions map
    for ( int i=0 ;i<notech; i++ ) {
        techs[i][per]->indemission( emcoef_ind );
        summary[per].updateemindmap(techs[i][per]->getemindmap());
    }
}

//! returns subsector primary energy consumption
double subsector::showpe_cons( const int per ) {

    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    pe_cons[per] = 0;
    for ( int i=0 ;i<notech; i++ ) {
        // depleatable resource indeces are less than 5
        // how should this condition be made generic?
        // shk 7/23/01
        if (techs[i][per]->getFuelNo() < 5) {
            // int num = techs[i][per].showfuelno() ;
            pe_cons[per] += techs[i][per]->getInput();
            //double temp = techs[i][per]->getInput();
            int stop = 1;
        }
    }
    return pe_cons[per];
}

//! returns primary or final energy input
double subsector::showinput( const int per ) const {
    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return input[per];
}

//! returns subsector output
double subsector::getoutput( const int per ) const {
    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return output[per];
}

//! returns total subsector carbon taxes paid
double subsector::showcarbontaxpaid( const int per ) const {
    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return carbontaxpaid[per];
}

//!  gets fuel consumption map in summary object.
map<string, double> subsector::getfuelcons(const int per) const {
    //! \pre per is less than or equal to max period.
    assert( per <= scenario->getModeltime()->getmaxper() );
    
    return summary[per].getfuelcons();
}

//! clears fuel consumption map in summary object
void subsector::clearfuelcons(const int per) {
    summary[per].clearfuelcons();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemission(const int per) const {
    return summary[per].getemission();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemfuelmap( const int per ) const {
    return summary[per].getemfuelmap();
}

//!  get ghg emissions map in summary object
map<string, double> subsector::getemindmap(const int per) const {
    return summary[per].getemindmap();
}

//! update summaries for reporting
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


