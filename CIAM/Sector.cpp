/*! 
* \file Sector.cpp
* \ingroup CIAM
* \brief Sector class source file.
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

#include "market.h"
#include "modeltime.h"
#include "sector.h"
#include "Marketplace.h"
#include "Configuration.h"

// xml headers
#include "xmlHelper.h"
#include "scenario.h"
#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

using namespace std;

extern Scenario scenario;
extern ofstream outfile, bugoutfile;

//! Default constructor
sector::sector() {
	initElementalMembers();
        // Note that this can be in the constructor of most objects -- except globals such as marketplace, modeltime, and scenario
        Configuration* conf = Configuration::getInstance();
        debugChecking = conf->getBool( "debugChecking" ); 
}

//! Default destructor.
sector::~sector() {
	for( vector<subsector*>::iterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
		delete *subSecIter;
	}
}

//! Clear member variables
void sector::clear(){
	initElementalMembers();
	name = "";
	unit = "";
	market = "";
	subsec.clear();
	sectorprice.clear();
	price_norm.clear();
	pe_cons.clear();
	input.clear();
	output.clear();
	carbontaxpaid.clear();
	summary.clear();
}

//! Initialize elemental data members.
void sector::initElementalMembers(){
	nosubsec = 0;
	tax = 0;
}

//! Return sector name.
string sector::getName()
{
	return name;
}

//! Set data members from XML input.
void sector::XMLParse( const DOMNode* node ){
	
	DOMNode* curr = 0;
	DOMNodeList* nodeList = 0;
	string nodeName;
	subsector* tempSubSector = 0;

	//! \pre make sure we were passed a valid node.
	assert( node );
	
	// get the name attribute.
	name = XMLHelper<string>::getAttrString( node, "name" );
	
	#if( _DEBUG )
		cout << "\tSector name set as " << name << endl;
	#endif
	
	// get all child nodes.
	nodeList = node->getChildNodes();

	// loop through the child nodes.
	for( int i = 0; i < nodeList->getLength(); i++ ){
		curr = nodeList->item( i );
		nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

		if( nodeName == "market" ){
			market = XMLHelper<string>::getValueString( curr ); // only one market element.
		}
		else if( nodeName == "price" ){
			sectorprice.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "output" ) {
			output.push_back( XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "subsector" ){
			tempSubSector = new subsector(); // memory leak
			tempSubSector->XMLParse( curr );
			subsec.push_back( tempSubSector );
		}	
	}
	nosubsec = subsec.size();
	// resize vectors not read in
	const Modeltime* modeltime = scenario.getModeltime();
	const int maxper = modeltime->getmaxper();
	price_norm.resize( maxper ); // sector price normalized to base year
	pe_cons.resize( maxper ); // sectoral primary energy consumption
	input.resize( maxper ); // sector total energy consumption
	// output.resize( maxper ); // total amount of final output from sector
	carbontaxpaid.resize( maxper ); // total sector carbon taxes paid
	summary.resize( maxper ); // object containing summaries // memory leak
}

//! Write object to xml output stream.
void sector::toXML( ostream& out ) const {
	const Modeltime* modeltime = scenario.getModeltime();

	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<supplysector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	
	for( int i = 0; i < static_cast<int>( sectorprice.size() ); i++ ){
		XMLWriteElement( sectorprice[ i ], "price", out, modeltime->getper_to_yr( i ) );
	}
	
	for( int j = 0; j < static_cast<int>( output.size() ); j++ ){
		XMLWriteElement( output[ j ], "output", out, modeltime->getper_to_yr( j ) );
	}

	// write out the subsector objects.
	for( vector<subsector*>::const_iterator k = subsec.begin(); k != subsec.end(); k++ ){
		( *k )->toXML( out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</supplysector>" << endl;
}

//! Write object to xml output stream.
void sector::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<supplysector name=\"" << name << "\">"<< endl;
	
	// increase the indent.
	Tabs::increaseIndent();

	// write the xml for the class members.
	// write out the market string.
	XMLWriteElement( market, "market", out );
	XMLWriteElement( unit, "unit", out );
	
	// Write out the data in the vectors for the current period.
	XMLWriteElement( sectorprice[ period ], "sectorprice", out );
	XMLWriteElement( pe_cons[ period ], "pe_cons", out );
	XMLWriteElement( input[ period ], "input", out );
	XMLWriteElement( output[ period ], "output", out );
	XMLWriteElement( carbontaxpaid[ period ], "carbontaxpaid", out );
	
	// Write out the summary
	// summary[ period ].toDebugXML( period, out );

	// write out the subsector objects.
	for( vector<subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
		( *j )->toDebugXML( period, out );
	}

	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</supplysector>" << endl;
}

//! Create a market for the sector.
void sector::setMarket( const string& regionName ) {
	
	Marketplace* marketplace = scenario.getMarketplace();
	// name is resource name
    // market is the name of the regional market from the input file (i.e., global, region, regional group, etc.)
	
	if( marketplace->setMarket( regionName, market, name, Market::NORMAL ) ) {
		marketplace->setPriceVector( name, regionName, sectorprice );
	}
	/* The above is not quite right becuase there could be many sectors within the 
           same market, this would result in the prices being reset each time. But little
           practical effect -- see notes for demsector::setMarket.
	*/

	// This problem has actually been fixed, setMarket will only return true for a unique market.
}

//! Pass along carbon taxes.
void sector::applycarbontax(double tax, int per) {
	int i=0;
	for (i=0;i<nosubsec;i++) {
		subsec[i]->applycarbontax(tax,per);
	}
}

//! Set ghg tax to technologies.
void sector::addghgtax( const string ghgname, const string regionName, const int per)
{
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->addghgtax(ghgname,regionName,per);
	}
}

//! Calculate subsector shares, adjusting for capacity limits.
void sector::calc_share( const string regionName, const int per, const double gnp_cap )
{
	int i=0;
	double sum = 0.0;
	for (i=0;i<nosubsec;i++) {
		// determine subsector shares based on technology shares
		subsec[i]->calcShare( regionName, per );
		sum += subsec[i]->getShare(per);
	}
	// normalize subsector shares to total 100 %
	for (i=0;i<nosubsec;i++) {
		subsec[i]->normShare(sum, per);	
	}

    // Now adjust for capacity limits
      adjSharesCapLimit( per );
}


/*!
 * \brief Determine if any capacity limits are exceeded and adjust for shares if so.
 *
 * If a capacity limit comes into play the routine shifts the "excess" share (over the capacity limits) to the non limited sectors. This routine loops several times in case this shift then causes another sector to exceed a capacity limit.
 * 
 * The logic for the share adjustment is as follows:
 *     Sum_notlim(Si) + Sum_limited(Si) + sumSharesOverLimit = 1
 *     where:
 *         Sum_notlim(Si)  = sum of shares that are not capacity limited
 *         Sum_limited(Si) = sum of shares that are capacity limited
 *         sumSharesOverLimit = portion of shares that were over capacity limits
 *         
 *     Need to solve for the amount to increase shares to account for capacity limits
 *    if:
 *         newSi = a * Si, then a few lines of algebra gives:
 *         a = 1 + overelimit/Sum_notlim(Si)
 *
 * \author Steve Smith
 * \warning The routine assumes that shares are already normalized.
 * \param per Model period
 */
void sector::adjSharesCapLimit( const int per )
{
    double tempCapacityLimit;
    double tempSubSectShare;
    //bool capLimited = true;
    bool capLimited = false;
    int i=0;
    
    // check for capacity limits, repeating to take care of any knock-on effects. 
    // Do this a maximum of times equal to number of subsectors, 
    // which is the maximum number of times could possibly need to do this
    for (int n = 0;  n < nosubsec && capLimited; n++) {
        double sumSharesOverLimit = 0.0;		// portion of shares over cap limits
        double sumSharesNotLimited = 0.0;	// sum of shares not subject to cap limits
        capLimited = false;
       
        //  Check for capacity limits, looping through each subsector
        for ( i=0; i<nosubsec; i++ ) {
                tempCapacityLimit = subsec[i]->getCapacityLimit( per ); // call once, store these locally
                tempSubSectShare = subsec[i]->getShare( per ) ;
                
                 // if there is a capacity limit and are over then set flag and count excess shares
                if ( tempSubSectShare > tempCapacityLimit ) {
                    capLimited = true;
                    sumSharesOverLimit += tempSubSectShare - tempCapacityLimit;
                }
                
                // also sum shares under limit (but not those just at their limits)
               if ( tempSubSectShare < tempCapacityLimit ) {
                    sumSharesNotLimited += tempSubSectShare;
                }                 
        } // end of loop over sub-sectors
        
      // re-normalize subsector shares if capacity limits have been exceeded
      // See comments above for derivation of multiplier
      if ( capLimited ) {
            if ( sumSharesNotLimited > 0 ) {
                for ( i=0; i<nosubsec; i++ ) {
                    double multiplier = 1 + sumSharesOverLimit/sumSharesNotLimited;
                    subsec[i]->limitShares( multiplier, per );
                }
            }
            else { // If there are no sectors without limits and there are still shares to be re-distributed
                if ( sumSharesOverLimit > 0 ) {
                    // if there is no shares left then too much was limited!
                    cerr << "Insufficient capacity to meet demand" << endl;
                }
            }
      }
    
      // Check to make sure shares still equal 1
      if ( debugChecking ) {
            double sumshares = 0;
            for ( i=0; i<nosubsec; i++ ) {
                sumshares += subsec[i]->getShare(per) ;
            }
            if ( fabs(sumshares - 1) > 1e-6 ) {
                cerr << "ERROR: Shares do not sum to 1. Sum = " << sumshares << endl;
            }
      }
    
    } // end for loop
    
    // if have exited and still capacity limited, then report error
    if ( capLimited ) {
        cerr << "Capacity limit not resolved" << endl;
    }
}


//! Calculate weighted average price of subsectors.
void sector::price(int per)
{
	sectorprice[per]=0.0;
	for (int i=0;i<nosubsec;i++) {	
		sectorprice[per] += subsec[i]->getShare(per) * subsec[i]->getprice(per);
	}
}

//! return sector price
double sector::showprice(int per)
{
	return sectorprice[per];
}

//! Set output for sector (ONLY USED FOR energy service demand at present).
/*! Demand from the "dmd" parameter (could be energy or energy service) is passed to subsectors.
    This is then shared out at the technology level.
        In the case of demand, what is passed here is the energy service demand. 
        The technologies convert this to an energy demand.
    The demand is then summed at the subsector level (subsec[i].sumoutput) then
    later at the sector level (in region via supplysector[j].sumoutput(per))
    to equal the total sector output.
*/
void sector::setoutput(const string& regionName,double dmd, int per)
{
    int i;
	carbontaxpaid[per] = 0; // initialize carbon taxes paid
 
	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i]->setoutput(regionName,name,dmd,per);
		subsec[i]->sumoutput(per);
		carbontaxpaid[per] += subsec[i]->showcarbontaxpaid(per);
	}
}

//! Sum subsector outputs.
void sector::sumoutput(int per)
{
	output[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		output[per] += subsec[i]->getoutput(per);
	}
}

//! Set supply sector output.
/*! This routine takes the market demand and propagates that through the supply sub-sectors
    where it is shared out (and subsequently passed to the technology level within each sub-sector
     to be shared out) */
void sector::supply( const string regionName, const int per) {
	Marketplace* marketplace = scenario.getMarketplace();

	double mrkprice, mrkdmd;
    int i;
	double totalFixedSupply = 0; 
    double fixedSupply = 0;
    double shareVariable = 0; // original sum of shares of non-fixed subsectors   
    double shareVariableNew = 0; // new sum of shares of non-fixed subsectors   
	double shareRatio;  // ratio for adjusting shares of non-fixed subsectors

	carbontaxpaid[per] = 0; // initialize carbon taxes paid
	
	mrkprice = marketplace->showprice( name, regionName, per ); // price for the good produced by this sector
	mrkdmd = marketplace->showdemand( name, regionName, per ); // demand for the good produced by this sector

    if (mrkdmd < 0) {
        cerr << "ERROR: Demand value < 0 for good " << name << " in region " << regionName << endl;
    }
            
	// calculate output from technologies that have fixed outputs such as hydro electricity
    // Determine total fixed production and total var shares
    // Need to change the exog_supply function once new, general fixed supply method is available
	for (i=0;i<nosubsec;i++) {
        fixedSupply = subsec[i]->exogSupply(per);
		// add up subsector shares without fixed output
        if (fixedSupply == 0) { 
			shareVariable += subsec[i]->getShare(per);
		}
        totalFixedSupply += fixedSupply;
	}

	// Scale down fixed output if its greater than actual demand
	if ( totalFixedSupply > mrkdmd ) {
		for (i=0;i<nosubsec;i++) {
			subsec[i]->scaleFixedSupply( mrkdmd/totalFixedSupply, per ); 
		}
	}

    // Adjust shares for any fixed output
    if (totalFixedSupply > 0) {
		if (totalFixedSupply > mrkdmd) {
			// not used for fixed output
			shareVariableNew = 0;
		}
		else {
			// check for 0 so that shareVariableNew does not blow up
         // Note: This will still blow up -JPL
		    if (mrkdmd == 0) {
				cerr << "ERROR: Demand value = 0 for good " << name << " in region " << regionName << endl;
			}
			shareVariableNew = 1 - (totalFixedSupply/mrkdmd);
		}
         
        shareRatio = shareVariableNew/shareVariable;
        for (i=0;i<nosubsec;i++) {
            subsec[i]->adjShares( mrkdmd, shareRatio, totalFixedSupply, per ); 
        }
	}
	
	// This is where subsector and technology outputs are set
	for (i=0;i<nosubsec;i++) {
		// set subsector output from sector demand
		subsec[i]->setoutput( regionName, name, mrkdmd, per ); // CHANGED JPL
		subsec[i]->sumoutput( per );
		// for reporting only
		carbontaxpaid[per] += subsec[i]->showcarbontaxpaid( per );
	}
    
    if (debugChecking) {
        sumoutput(per); // Sum output just so its available below for an error check. sjs
        // If the model is working correctly this should never give an error
        // An error here means that the supply summed up from the supply sectors is not equal to the demand that was passed in 
		double mrksupply = getoutput(per);
		if (per > 0 && fabs(mrksupply - mrkdmd) > 0.01 && (regionName == "USA")) {
			mrksupply = mrksupply * 1.000000001;
			cout << regionName << " Market "<<  name<< " demand and derived supply are not equal by: ";
                        cout << fabs(mrksupply - mrkdmd) << ": ";
                        cout << "S: " << mrksupply << "  D: " << mrkdmd << endl;
		}
	}
    
}

void sector::show()
{
	int i=0;
	int m=0; // per = 0
	//write to file or database later
	cout << "Sector: " << name<< endl;
	cout << "Number of Subsectors: " << nosubsec << endl;
	for (i=0;i<nosubsec;i++)
		cout<<"Share["<<i<<"] "<<subsec[i]->getShare(m)<< endl;
	cout <<"Total Sector Output: " << output[m] << endl;

}

//! Prints to outputfile all technologies in each subsector.
void sector::showsubsec(int per, const char *ofile)
{
	int i=0;

	for (i=0;i<nosubsec;i++) {
		// shows subsector label (name and index)
		subsec[i]->showlabel(ofile);
		// write technology info in each subsector to file
		subsec[i]->showtechs(per, ofile);
	}
}

//! Prints to outputfile sector label (name and index).
void sector::showlabel(const char* ofile)
{
	ofstream outfile;

	outfile.open(ofile, ios::app);

		if (!outfile) {
			//open failed
			cerr<<"Cannot open file for output\n";
			exit(-1);
		}

	outfile << "Sector: " << name << endl;
	outfile << "Number of Subsectors: " << nosubsec << endl;
	outfile.close();
}

int sector::shownosubsec(void)
{
	return nosubsec;
}

//! returns sector output
double sector::getoutput(int per)
{
	return output[per]; 
}


//! Calculate GHG emissions for each sector from subsectors.
void sector::emission(int per)
{
	summary[per].clearemiss(); // clear emissions map
	summary[per].clearemfuelmap(); // clear emissions fuel map
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->emission(per,name);
		summary[per].updateemiss(subsec[i]->getemission(per));
		summary[per].updateemfuelmap(subsec[i]->getemfuelmap(per));
	}
}

//! Calculate indirect GHG emissions for each sector from subsectors.
void sector::indemission(int per)
{
	summary[per].clearemindmap(); // clear emissions map
	for (int i=0;i<nosubsec;i++) {
		subsec[i]->indemission(per);
		summary[per].updateemindmap(subsec[i]->getemindmap(per));
	}
}

//! Return sectoral primary energy consumption.
double sector::showpe_cons(int per)
{
	pe_cons[per] = 0;
	for (int i=0;i<nosubsec;i++) {
		pe_cons[per] += subsec[i]->showpe_cons(per);
	}
	return pe_cons[per];
}

//! Sums subsector primary and final energy consumption.
void sector::suminput(int per)
{
	input[per] = 0;
	for (int i=0;i<nosubsec;i++)
		input[per] += subsec[i]->showinput(per);
}

//! Returns sectoral energy consumption.
double sector::showinput(int per)
{
	return input[per];
}

//! Write sector output to database.
void sector::outputfile( const string& regname)
{
	// function protocol
	void fileoutput3(string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	// function arguments are variable name, double array, db name, table name
	// the function writes all years
	// total sector output
	fileoutput3(regname,name," "," ","production","EJ",output);
	// total sector eneryg input
	fileoutput3( regname,name," "," ","consumption","EJ",input);
	// sector price
	fileoutput3( regname,name," "," ","price","$/GJ",sectorprice);
	// sector carbon taxes paid
	fileoutput3( regname,name," "," ","C tax paid","Mil90$",carbontaxpaid);
}

//! Write out subsector results from demand sector.
void sector::MCoutput_subsec( const string& regname )	
{	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->MCoutputB(regname,name);
		subsec[i]->MCoutputC(regname,name);
	}
}

//! Write MiniCAM style sector output to database.
void sector::MCoutput(const string& regname ) {
	const Modeltime* modeltime = scenario.getModeltime();
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);
	int m;
        
	// total sector output
	dboutput4(regname,"Secondary Energy Prod","by Sector",name,"EJ",output);
	dboutput4(regname,"Secondary Energy Prod",name,"zTotal","EJ",output);
	
	int maxper = modeltime->getmaxper();
	vector<double> temp(maxper);
	string str; // temporary string

	// sector fuel consumption by fuel type
	typedef map<string,double>:: const_iterator CI;
	map<string,double> tfuelmap = summary[0].getfuelcons();
	for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_fmap_second(fmap->first);
		}
		dboutput4(regname,"Fuel Consumption",name,fmap->first,"EJ",temp);
	}

	// sector emissions for all greenhouse gases
	map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
	for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
		for (int m=0;m<maxper;m++) {
			temp[m] = summary[m].get_emissmap_second(gmap->first);
		}
		str = "Sec: "; // sector heading
		str+= name; // sector name
		dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
	}
	// CO2 emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emissmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss","by Sector",name,"MTC",temp);
	dboutput4(regname,"CO2 Emiss",name,"zTotal","MTC",temp);

	// CO2 indirect emissions by sector
	for (m=0;m<maxper;m++) {
		temp[m] = summary[m].get_emindmap_second("CO2");
	}
	dboutput4(regname,"CO2 Emiss(ind)",name,"zTotal","MTC",temp);

	// sector price
	dboutput4(regname,"Price",name,"zSectorAvg","$/GJ",sectorprice);
	// sector price
	dboutput4(regname,"Price","by Sector",name,"$/GJ",sectorprice);
	// sector carbon taxes paid
	dboutput4(regname,"General","CarbonTaxPaid",name,"$",carbontaxpaid);
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->MCoutputA(regname,name);
		subsec[i]->MCoutputC(regname,name);
	}
}
	
//! Write subsector output to database.
void sector::subsec_outfile( const string& regname )
{
	// do for all subsectors in the sector
	for (int i=0;i<nosubsec;i++) {
		// output or demand for each technology
		subsec[i]->outputfile(regname,name);
	}
}

void sector::set_ser_dmd(double dmd, int per)
{
	output[per] = dmd;
}

//! Return total sector carbon taxes paid.
double sector::showcarbontaxpaid(int per)
{
	return carbontaxpaid[per];
}

//! Get the fuel consumption map in summary object.
map<string, double> sector::getfuelcons(int per) 
{
	return summary[per].getfuelcons();
}

//!  Get the second fuel consumption map in summary object.
double sector::getfuelcons_second(int per,string key) 
{
	return summary[per].get_fmap_second(key);
}

//! Clear fuel consumption map in summary object.
void sector::clearfuelcons(int per) 
{
	summary[per].clearfuelcons();
}

//!  Get the ghg emissions map in summary object.
map<string, double> sector::getemission(int per) 
{
	return summary[per].getemission();
}

//! Get ghg emissions map in summary object.
map<string, double> sector::getemfuelmap(int per) 
{
	return summary[per].getemfuelmap();
}

//! update summaries for reporting
void sector::updateSummary( const int per )
{
	int i = 0;
	// clears sector fuel consumption map
	summary[per].clearfuelcons();

	for (i=0;i<nosubsec;i++) {
		// clears subsector fuel consumption map
		//subsec[i]->clearfuelcons(per);
		// call update summary for subsector
		subsec[i]->updateSummary(per);
		// sum subsector fuel consumption for sector fuel consumption
		summary[per].updatefuelcons(subsec[i]->getfuelcons(per)); 
	}

}



