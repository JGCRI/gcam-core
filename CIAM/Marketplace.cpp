#include "Definitions.h"
#include <iostream>
#include <fstream>
#include <cmath>
#include <algorithm>
#include "Market.h"
#include "Marketplace.h"
#include "modeltime.h"  // model runtime info
#include "XMLHelper.h"
#include "Configuration.h"
#include "World.h"

extern ofstream bugoutfile, sdcurvefile, logfile;	

extern Modeltime modeltime;
extern World world;

const double smnum = 1e-6; // constant small number to replace for null
const double priceMult = 1; // resolution enhancement for price markets
const double verysmnum = 1e-8; // constant small number to replace for null
const int maxMarkets = 80; // Maximum number of markets the solution mechanism can deal with
const double solTolerance = 0.01; // Tolerance for solution criteria
const int bugTracking = 0; //Turn on to enable bugout tracking in various solution routines
const int bugMinimal = 1; //Turn on minimal tracking of solution results

const int trackED = 0; // Turn on solution mechanism tracking (to cout)

//! Default constructor.
Marketplace::Marketplace() {
	uniqueNo = 0;
	nomrks = 0;
	nomrks_t = 0;
	nomrks_t_NR = 0;
	nodrscmrks = 0;
	nossecmrks = 0;
	noDemandMarkets = 0;
	noghgmrks = 0;
}

//! Write out XML for debugging purposes.
void Marketplace::toDebugXML( const int period, ostream& out ) const {
	
	// write the beginning tag.
	Tabs::writeTabs( out );
	out << "<Marketplace>" << endl;
	
	// increase the indent.
	Tabs::increaseIndent();
	
	// write the xml for the class members.
	XMLWriteElement( nomrks, "numberOfMarkets", out );
	XMLWriteElement( nomrks_t, "numberOfMarketsRequireSolving", out );
	XMLWriteElement( nomrks_t_NR, "numberOfMarketsForNR", out );
	//XMLWriteElement( nodrscmrks, "numberOfResourceMarkets", out );
	//XMLWriteElement( nossecmrks, "numberOfSupplyMarkets", out );
	//XMLWriteElement( noDemandMarkets, "numberOfDemandMarkets", out );
	//XMLWriteElement( noghgmrks, "numberOfGHGMarkets", out );
	// maybe add more to write out.
	
	// First write out the individual markets
	for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ){
		mrk[ i ][ period ].toDebugXML( period, out );
	}
	
	// finished writing xml for the class members.
	
	// decrease the indent.
	Tabs::decreaseIndent();
	
	// write the closing tag.
	Tabs::writeTabs( out );
	out << "</Marketplace>" << endl;
}

//! Function which adds a regional market into the 
/*! Returns true if a new market was added. False if the market already exists.
There is an important distinction here between the region name vs the market name. 
The key to the market is the goodName + market name.
The region_marketMap map, uses goodName + regionName to find that market.
*/
bool Marketplace::setMarket( const string& regionName, const string& mrkname, const string& goodName, const Market::marketType typeIn ) {
	int mrkNo;
	bool retValue;
	
	// create unique markets from distinct good and market region names
	string key = goodName + mrkname;
	
	if ( marketMap.find( key ) != marketMap.end() ) { // market exists, no unique number
		retValue = false;
		mrkNo = marketMap[ key ];
	} 
	else { // market does not exist, give unique number
		retValue = true;
		marketMap[ key ] = uniqueNo;
		Market tempMarket;
		tempMarket.name = goodName;
		tempMarket.region = mrkname;
		tempMarket.type = typeIn;
		
		// Need to set market parameters before vector is created.
		
        // create a vector of market objects, one for each period
		vector<Market> tempVector( modeltime.getmaxper(), tempMarket );
		
		// set the years for the markets.
		for( int i = 0; i < static_cast<int>( tempVector.size() ); i++ ){
			tempVector[ i ].year = modeltime.getper_to_yr( i );
		}
		
		mrk.push_back( tempVector ); // create individual markets
		//mrk is 2 dimentional
		mrkNo = uniqueNo;
		uniqueNo++;
		
		// increment market sizes
		nomrks++;
	}
	// market lookup from good and region names
	// mrkNo may not be unique
	region_marketMap[ goodName + regionName ] = mrkNo;
	return retValue;
}

int Marketplace::getMarketNumber( const string& goodName, const string& regionName ) const {
	map <string, int> :: const_iterator findIter = region_marketMap.find( goodName + regionName );
	
	if( findIter == region_marketMap.end() ) {
		return -1;
	}
	else {
		return findIter->second;
	}
}

//! Restructures a market to account for simultaneities
/*! Changes the named market to a price market, which suplies a trial price for a secondary good
Also adds a corresponding demand market that provides a trial value for demand.
Routines such as Marketplace::setdemand, getdemand, getprice, etc. are adjusted to act 
differently for PRICE and DEMAND markets so that these changes are transparent to the 
rest of the code.
*/
void Marketplace::resetToPriceMarket( const string& goodName, const string& regionName ) {
    int marketNumber = getMarketNumber(goodName, regionName);
    bool mktset;
    string marketName;
    string DemandGoodName;
    const bool setNewMarkets = false; // for debugging, set this to false to turn this off
    int first_period = 1;   // first period for price markets
	// skip first period since this is not really solved
    
    if( marketNumber == -1 ) {
        cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
    }
    else {
        if ( getType( marketNumber , first_period ) == Market::PRICE) {
            // do nothing, this market has already been restructured
        }
        else if (setNewMarkets) {
            // Setup the cooresponding demand markets
            marketName = getName( goodName, regionName );
            DemandGoodName = goodName + "Demand_int";
            mktset = setMarket( regionName, marketName, DemandGoodName, Market::DEMAND );
			
            // loop through time periods            
            for( int per = first_period; per < static_cast<int>( mrk[ marketNumber ].size() ); per++ ){                
                mrk[marketNumber][per].type = Market::PRICE;
                setMarketToSolve (goodName, regionName, per);
                setMarketToSolve (DemandGoodName, regionName, per);
                // this assumes that all markets have the same number of periods
            }
            
			// if (mktset) {
			//	cout << "Adjusted the "<< regionName<<" market for " << goodName << " for simultaneities" << endl;
			// }
        }
    }
}

//! Set the prices by period of a market from a vector.
void Marketplace::setPriceVector( const string& goodName, const string& regionName, const vector<double>& prices ){
	
	// determine what market the region and good are in.
	int marketNumber = region_marketMap[ goodName + regionName ];
	
	for( int i = 0; i < static_cast<int>( mrk[ marketNumber ].size() ) && i < static_cast<int>( prices.size() ); i++ ){
		mrk[ marketNumber ][ i ].price = prices[ i ];
	}
}

//! Initialize prices
/*! Supply and demand sector prices should always get set somewhere else, except for first period,
although no such guarantee for GHG markets, but initialize prices for all periods 
in all markets just to be safe if not already set. 

  Initialization also occurs for supply and demand markets that have prices read-in via routine:
  setPriceVector (in sector as supply & demand mkts are created).
  This has no effect for future periods as these prices are overwritten by 
  Marketplace::init_to_last
*/

void Marketplace::initXMLPrices(){
	
    // initialize supply and demand sector market prices to 1.
    for ( int i = 0; i < static_cast<int>( mrk.size() ); i++ ){               
        for( int j = 0; j < static_cast<int>( mrk[ i ].size() ); j++ ){
			
            switch( mrk[ i ][ 0 ].type ){
            case Market::GHG:
                mrk[ i ][ j ].price = 0;
                break;
            default:
                // Generally want a non-zero value as starting value for other markets
                if (mrk[ i ][ j ].price == 0) {	// don't set if already set by read-in
                    mrk[ i ][ j ].price = 1;	
                }
                break;
            }
        }
    }
}

//! Set the solve flag for this market for the given period
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName , const int per) {
    int marketNumber = getMarketNumber( goodName, regionName );
    if ( marketNumber != -1 ) {
        mrk[ marketNumber ][ per ].solveMarket = true;
	}
}

//! Set the solve flag for this market for ALL periods
void Marketplace::setMarketToSolve ( const string& goodName, const string& regionName) {
    int marketNumber = getMarketNumber( goodName, regionName );
	
	//   cout << "setMarketToSolve -- goodName: " << goodName << "  regionName: " << regionName;
	
    if ( marketNumber != -1 ) {
        for( int per = 0; per < static_cast<int>( mrk[ marketNumber ].size() ); per++ ){                 
            mrk[ marketNumber ][ per ].solveMarket = true;
        }
	}
}

//! initialize all market prices to 0
void Marketplace::nullprc( const int per ){
	for (int i=0;i<nomrks;i++)
		mrk[i][per].price = 0.0;
}

//! initialize all market demands to 0
void Marketplace::nulldem( const int per ){
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].demand = 0.0;
	}
}

//! initialize all market supplies to 0
void Marketplace::nullsup( const int per)
{
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].supply = 0.0;
		mrk[i][per].demMktSupply = 0.0;
	}
}

//! Set one market demand to 0.
/*! Not used at present */
void Marketplace::nulldem_imrk( const string& goodName, const string& regionName, const int per ){
	int marketNumber = getMarketNumber( goodName, regionName );
	
	if ( marketNumber != -1 ) {
		mrk[ marketNumber ][ per ].demand = 0;
	}
}

//! Set one market supply to 0
/*! Not used at present */
void Marketplace::nullsup_imrk( const string& goodName, const string& regionName, const int per ){
	int marketNumber = getMarketNumber( goodName, regionName );
	
	if ( marketNumber != -1 ) {
		mrk[ marketNumber ][ per ].supply = 0;
	}
}

//! initialize market price to 0
void Marketplace::showmrks( const int per ) const 
{
	for (int i=0;i<nomrks;i++) {
		cout<<"Market Year: "<<mrk[i][per].year<<"\n";
		cout<<"Market Name: "<<mrk[i][per].name<<"\n";
		cout<<"Market Region: "<<mrk[i][per].region<<"\n";
		cout<<"Market Price: "<<mrk[i][per].price<<"\n";
		cout<<"Market Supply: "<<mrk[i][per].supply<<"\n";
		cout<<"Market Demand: "<<mrk[i][per].demand<<"\n\n";
	}
}

//! set market price
/*! If this is a price market, then the price passed is used to set supply instead of price. 
The market price is also used to set the demand.
Use this convention for pseudo markets -- model values are supply, trail market values are used
for demand. */
void Marketplace::setprice( const string& goodName, const string& regionName, const double value, const int per ){
	int marketNumber = getMarketNumber( goodName, regionName );
	
	if ( marketNumber != -1 ) {
		// If is price market, then set "supply" to passed-in price
		if ( getType( marketNumber , per ) == Market::PRICE) {
			// Note, can't use function setsupply here since that would be re-directed
			//reversed (the other order does not work)
			mrk[ marketNumber ][ per ].demand = value * priceMult;
			mrk[ marketNumber ][ per ].supply = mrk[ marketNumber ][ per ].price * priceMult;
		}
		else {	
			// Otherwise, set the price as normal
			mrk[ marketNumber ][ per ].price = value;
		}
	}
}

//! Add to the supply for this market
void Marketplace::setsupply( const string& goodName, const string& regionName, const double value, const int per){
	int marketNumber = getMarketNumber( goodName, regionName );
	string marketName;
	string DemandGoodName;
	Market::marketType thisMarketType;
	
	
	if ( marketNumber != -1 ) {
		thisMarketType = getType( marketNumber, per );
		// If is price market, then set supply instead for the corresponding demand market
		if ( thisMarketType == Market::PRICE ) {
			marketName = getName( goodName, regionName );
			DemandGoodName = goodName + "Demand_int";
			setsupply ( DemandGoodName, regionName, value, per );
		}
		else if ( thisMarketType == Market::DEMAND ) {
			// if its a demand market then this is where the supply was set
			mrk[ marketNumber ][ per ].supply = mrk[ marketNumber ][ per ].price; 
			mrk[ marketNumber ][ per ].demMktSupply += value; // Store actual supply value to check later
		}
		else {
			// Otherwise, set supply as normal
			mrk[ marketNumber ][ per ].supply += value;
		}
	}
}

//! set market demand to used for solution mechanism
void Marketplace::setdemand( const string& goodName, const string& regionName, const double value , const int per){
	const int marketNumber = getMarketNumber( goodName, regionName );
	string marketName;
	string DemandGoodName;
	
	// The following condition used to be here, don't think this is needed any more
	//	if( goodName != "renewable" ){ // just a guess
	
	if (value < 0) {
		cerr << "ERROR: Demand value < 0 for market " << goodName << " in region " << regionName << endl;
	}
	
	if ( marketNumber != -1 ) {
		// If is price market, then set demand instead for the corresponding demand market
		if ( getType( marketNumber , per ) == Market::PRICE) {
			marketName = getName( goodName, regionName );
			DemandGoodName = goodName + "Demand_int";
			setdemand ( DemandGoodName, regionName, value, per );
		}
		else {
			// Otherwise, set demand as normal
			mrk[ marketNumber ][ per ].demand += value;
		}
	}
}

//! return market price
double Marketplace::showprice( const string& goodName, const string& regionName, const int per ) const 
{
	// returns market price
	//
	if( goodName == "renewable" ){ // total guess
		return 0;
	}
	else {
		int marketNumber = getMarketNumber( goodName, regionName );
		
		
		
		if ( marketNumber != -1 ) {
			if ( getType( marketNumber , per ) == Market::PRICE) {
				return mrk[ marketNumber ][ per ].price / priceMult ; }
			else {
				return mrk[ marketNumber ][ per ].price; }
		}
		else {
			return 0;
		}
	}
}

//! return market supply used for solution mechanism
double Marketplace::showsupply( const string& goodName, const string& regionName, const int per) const 
{
	int marketNumber = getMarketNumber( goodName, regionName );
	string marketName;
	string DemandGoodName;
	
	if ( marketNumber != -1 ) {
		// If is price market, then get supply instead for the corresponding demand market
		if ( getType( marketNumber , per ) == Market::PRICE) {
			marketName = getName( goodName, regionName );
			DemandGoodName = goodName + "Demand_int";
			return showsupply ( DemandGoodName, regionName, per );
		}
		else {
			// Otherwise, get supply as normal
			return mrk[ marketNumber ][ per ].supply;
		}
	}
	else {
		cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
		return 0;
	}
}

//! return supply for use in checking solution, including actual supply for demand market
/*! Not used now, could be used for debugging at some point*/
double Marketplace::checkSupply( const string& goodName, const string& regionName, const int per) const 
{
	int marketNumber = getMarketNumber( goodName, regionName );
	string marketName;
	string DemandGoodName;
	
	if ( marketNumber != -1 ) {
		// If is price market, then get supply instead for the corresponding demand market
		if ( getType( marketNumber , per ) == Market::DEMAND) {
			return mrk[ marketNumber ][ per ].demMktSupply;
		}
		else {
			// Otherwise, get supply as normal
			return mrk[ marketNumber ][ per ].supply;
		}
	}
	else {
		cerr << "ERROR: Called for supply of non-existant market "<< goodName << " in " << regionName << endl;
		return 0;
	}
}

//! return supply for use in checking solution, including actual supply for demand market
/*! Used to double-check solution. Should not use otherwise. */
double Marketplace::checkSupply( const int marketNumber, const int per) const 
{
	if ( marketNumber != -1 ) {
		// If is price market, then get supply instead for the corresponding demand market
		if ( getType( marketNumber, per ) == Market::DEMAND) {
			return mrk[ marketNumber ][ per ].demMktSupply;
		}
		else {
			// Otherwise, get supply as normal
			return mrk[ marketNumber ][ per ].supply;
		}
	}
	else {
		cerr << "ERROR: Called for supply of non-existant market # "<< marketNumber << endl;
		return 0;
	}
}

//! return market demand used for solution mechanism
/*! If this is a price market, then get the demand from the corresponding demand market.
    If this is a demand market (through the redirect), then get the demand from the trail value (.price)
     -- although this is never actually occurs at present. 
    */
double Marketplace::showdemand(  const string& goodName, const string& regionName, const int per) const 
{
	int marketNumber = getMarketNumber( goodName, regionName );
	string marketName;
	string DemandGoodName;
	
	if ( marketNumber != -1 ) {
                Market::marketType MType = getType( marketNumber , per );
		// If is price market, then get demand instead for the corresponding demand market
		if ( MType == Market::PRICE) {
			marketName = getName( goodName, regionName );
			DemandGoodName = goodName + "Demand_int";
			return showdemand ( DemandGoodName, regionName, per );
		}
		else if (MType == Market::DEMAND) {
			// Otherwise, get demand from the trial value
			return mrk[ marketNumber ][ per ].price;
		}
		else {
			// Otherwise, get demand as normal
			return mrk[ marketNumber ][ per ].demand;
		}
	}
	else {
		cerr << "ERROR: Called for demand of non-existant market "<< goodName << " in " << regionName << endl;
		return 0;
	}
}

//! return name (region group) of market
string Marketplace::getName( const string& goodName, const string& regionName) const {
	int marketNumber = getMarketNumber( goodName, regionName );
	
	if ( marketNumber != -1 ) {
		return mrk[ marketNumber ][ 0 ].region;
	}
	else {
		return "";
	}
}

//! return name (region group) of market
/*! Use for debugging only */
string Marketplace::getName( const int marketNumber) const {
	if ( marketNumber != -1 ) {
		return mrk[ marketNumber ][ 0 ].region;
	}
	else {
		return "";
	}
}

//! return name (region group) of market
/*! Use for debugging only */
string Marketplace::getGoodName( const int marketNumber) const 
{
	if ( marketNumber != -1 ) {
		return mrk[ marketNumber ][ 0 ].name;
	}
	else {
		return "";
	}
}

//! Return market type (NORMAL, PRICE, DEMAND, etc.)
Market::marketType Marketplace::getType( const string goodName, const string regionName, int per ) const
{
    int marketNumber = getMarketNumber(goodName, regionName);
    
    if( marketNumber == -1 ) {
        cerr << "ERROR: Market "<< goodName << " does not exist"  << endl;
        return Market::NORMAL;
    }
    else {
        return mrk[marketNumber][per].type;
    }
}

//! return market supply used for solution mechanism
/*! Use for debugging only */
Market::marketType Marketplace::getType( const int marketNumber, const int per) const 
{
	return mrk[ marketNumber ][ per ].type;
}

//! return market supply used for solution mechanism
/*! Use for debugging only */
double Marketplace::getRawSupply( const int marketNumber, const int per) const 
{
	return mrk[ marketNumber ][ per ].supply;
}

//! return market supply used for solution mechanism
/*! Use for debugging only */
double Marketplace::getRawDemand( const int marketNumber, const int per) const 
{
	return mrk[ marketNumber ][ per ].demand;
}

//! return market supply used for solution mechanism
/*! Use for debugging only */
double Marketplace::getRawPrice( const int marketNumber, const int per) const 
{
	return mrk[ marketNumber ][ per ].price;
}


//! calculates excess demand for all markets
void Marketplace::excessdemand( const int per ) 
{
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].exdmd = mrk[i][per].demand - mrk[i][per].supply; 
	}
}

//! calculates log of excess demand for all markets
void Marketplace::logED( const int per ) 
{
	int breakout = 0;
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].lexdmd = log10(max( mrk[i][per].demand, smnum )) 
			- log10(max(mrk[i][per].supply,smnum)); 
	}
}

//! calculates log of demand for all markets
void Marketplace::logDem( const int per ) 
{
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].ldem = log(max( mrk[i][per].demand ,smnum )); 
	}
}

//! calculates log of supply for all markets
void Marketplace::logSup( const int per ) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].lsup = log(max(mrk[i][per].supply,smnum));
}

//! Check to see that all markets actually solved.
bool Marketplace::checkMarketSolution( const double solTolerance, const int period ) const {
    bool solvedOK = true;
	
    for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
        if ( abs( checkSupply( i, period) - mrk[i][period].demand) > solTolerance ) {
            solvedOK = false;
            cout << "Market "<< i << " ("<< getGoodName(i)<< ") S: "<< checkSupply( i, period) << " D: " << mrk[i][period].demand <<endl;
        }
    }
	
	return solvedOK;
}

//! Select markets to solve.
int Marketplace::setMarketsToSolve( const int period ) {
	bool solvemkt = false;
	bool smalltest = false; // put all markets in array so that is large enough for all
	nomrks_t = 0;
	mrk_isol.clear();
	
	for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
		// Check if this market is supposed to be solved & if a significant demand exists
		solvemkt =  mrk[ i ][ period ].solveMarket;
		
		// if (smalltest) solvemkt =solvemkt && mrk[i][period].demand > smnum;
		
		// But don't solve if its a GHG market and there is no constraint
		if ( solvemkt && mrk[ i ][ period ].type == Market::GHG && mrk[ i ][ period ].supply < 0 ) {
			solvemkt = false; 
		}
		
		// Add markets to the "normal" solution list
		if ( solvemkt ) {
			nomrks_t++;
			mrk_isol.push_back( i );
		}
	}
	
	return nomrks_t;
}

//! set markets to solve
int Marketplace::setMarketsToSolveNR( const int period ){
	bool solvemkt = false;
	bool smalltest = true;
	
	nomrks_t_NR = 0;
	mrk_isol_NR.clear();
	
	
	for( int i = 0; i < static_cast<int>( mrk.size() ); i++ ) {
		// double tempDemand = mrk[ i ][ period ].demand;
		// double tempSupply = mrk[ i ][ period ].supply;
		cout << mrk[ i ][ period ].name << ": Supply: " << mrk[ i ][ period ].supply << " Demand: " << mrk[ i ][ period ].demand << " SM: " << mrk[ i ][ period ].solveMarket << endl;
		solvemkt = false;
		// Check if this market is supposed to be solved & if a significant demand exists
		// solvemkt =  mrk[ i ][ period ].solveMarket;
		// if (smalltest ) solvemkt =solvemkt && mrk[i][period].demand > smnum;
		if ( smalltest 
			&& mrk[ i ][ period ].solveMarket 
			&& mrk[ i ][ period ].supply > smnum 
			&& mrk[ i ][ period ].demand > smnum ) {
			solvemkt = true;
		}
		// But if its a GHG market .... 
		if ( solvemkt && mrk[ i ][ period ].type == Market::GHG ){
			//  don't solve if there is no constraint 
			if ( (mrk[ i ][ period ].supply < 0) ||  	
				// or don't solve if exdmd < 0 & price is really small 
				(mrk[ i ][ period ].price < smnum && mrk[ i ][ period ].exdmd < 0) || 
				// or don't solve if price is zero
				( mrk[ i ][ period ].price == 0 ) ) { 	
				
				solvemkt = false; 
			}
		} // end GHG block
		
		// Add markets to NR solution list
		if ( solvemkt ) {
			nomrks_t_NR++;
			mrk_isol_NR.push_back( i );
		}
	} // end for
	
	return nomrks_t_NR;
}


//! Return market number when given solution market index
/*! Used for debugging. */
int Marketplace::showmrk_sol( const int id ) const {
	return 	mrk_isol[id];
}

//! checks if supply > 0 for all markets
/*! not used at present */
bool Marketplace::checkprod( const int per ) const
{
    cout << "This routine will not work 100% correctly with simultaneous markets" << endl;
	for (int i=0;i<nomrks_t;i++)
		if ((mrk[i][per].supply == 0) ||
			(mrk[i][per].demand == 0))
			return false; // breaks out as soon as true
		return true;
}

//! return market with largest excess demand
int Marketplace::worstED( const int per ) const {
	int worstID = 0;
	double temp = fabs(mrk[0][per].exdmd);
	
	for (int i=1;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		if (fabs(mrk[j][per].exdmd) > temp) { // need to check constraint case
			worstID = i;
			temp = fabs(mrk[j][per].exdmd);
		}
	}
	return worstID;
}

//! returns largest excess demand
double Marketplace::maxED( const int per ) const {
	double largest = 0;
	double temp = 0;
	
	// if price is less than small number or null then
	// constraint case where constraint is greater than demand
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		temp = fabs(mrk[j][per].exdmd);
		if (mrk[j][per].price > smnum && 
			temp > largest) {
			largest = temp;
		}
	}
	return largest;
}

//! set new solution prices for all markets
void Marketplace::setPRC( const vector<double>& prices, const int per ) {
	// set prices only for markets in price vector
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		mrk[j][per].price = prices[i];
		//   if (j == 9) {cout << "e price:" << prices[i]<<", sup:"<<mrk[j][per].supply<<", dem:"<<mrk[j][per].demand << endl;}
	}
}

//! set new solution prices for all markets in Newton-Rhapson
void Marketplace::setPRC_NR( const vector<double>& prices, const int per )
{
	// set prices only for markets in price vector
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		mrk[j][per].price = prices[i];
	}
}

//! Initizliae marketplace price
/*! Use last period demand price as starting point for next period */
void Marketplace::init_to_last( const int per )
{ 
	// only after the starting period
	if (per > 0)
		for (int i=0;i<nomrks;i++) {
			//mrk[i][per].demand = mrk[i][per-1].demand;
			//mrk[i][per].supply = mrk[i][per-1].supply;
			mrk[i][per].price = mrk[i][per-1].price;
		}
}

//! For debugging, print out all prices
void Marketplace::prices_to_bugout( const int per ) const
{
    bugoutfile << ",,,,";
    for (int i=0;i<nomrks;i++) bugoutfile << i << ",";
    bugoutfile << endl;
    bugoutfile << ",Per,"<< per <<",Prices:,";
	for (int j=0;j<nomrks;j++) 
		bugoutfile << mrk[j][per].price << ",";
    bugoutfile << endl;
}

//! For debugging, print out all supplies
void Marketplace::supply_to_bugout( const int per ) const
{
    bugoutfile << ",Per,"<< per <<",Supplies:,";
	for (int i=0;i<nomrks;i++) 
		bugoutfile << mrk[i][per].supply << ",";
    bugoutfile << endl;
}

//! For debugging, print out all demands
void Marketplace::demand_to_bugout( const int per ) const
{
    bugoutfile << ",Per,"<< per <<",Demands:,";
	for (int i=0;i<nomrks;i++) 
		bugoutfile << mrk[i][per].demand << ",";
    bugoutfile << endl;
}

//! Initizliae marketplace price
/*! Use last period demand, supply and price
as starting point for next period */
void Marketplace::storeto_last( const int per )
{
	// only after the starting period
	if (per > 0)
		for (int i=0;i<nomrks;i++) {
			mrk[i][per].tdemand = mrk[i][per-1].demand;
			mrk[i][per].tsupply = mrk[i][per-1].supply;
			mrk[i][per].tprice = mrk[i][per-1].price;
		}
}
/*
// Store original demand, supply and price
// Used for calculation of derivative
void Marketplace::storeinfo( const int per )
{
// store only for markets that need solving
for (int i=0;i<nomrks_t;i++) {
if (mrk[i][per].demand < smnum)
mrk[i][per].tdemand = smnum;
else
mrk[i][per].tdemand = mrk[i][per].demand;
if (mrk[i][per].supply < smnum)
mrk[i][per].tsupply = smnum;
else
mrk[i][per].tsupply = mrk[i][per].supply;
if (mrk[i][per].price < smnum)
mrk[i][per].tprice = smnum;
else
mrk[i][per].tprice = mrk[i][per].price;
}
}
*/

//! Store original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::storeinfo( const int per )
{
	// store only for markets that need solving
	//for (int i=0;i<nomrks_t;i++) {
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].tdemand = mrk[i][per].demand;
		mrk[i][per].tsupply = mrk[i][per].supply;
		mrk[i][per].tprice = mrk[i][per].price;
	}
}

//! Restore original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::restoreinfo( const int per )
{
	// store only for markets that need solving
	//for (int i=0;i<nomrks_t;i++) {
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].demand = mrk[i][per].tdemand;
		mrk[i][per].supply = mrk[i][per].tsupply;
		mrk[i][per].price = mrk[i][per].tprice;
	}
}

//! Restore original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::restoreprc( const int per )
{
	// store only for markets that need solving
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		mrk[j][per].price = mrk[j][per].tprice;
	}
}

//! Restore original demand, supply and price
/*! Used for calculation of derivative */
void Marketplace::restoreprc_NR( const int per )
{
	// store only for markets that need solving
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		mrk[j][per].price = mrk[j][per].tprice;
	}
}

//! returns vector of market prices
const vector<double> Marketplace::showPRC( const int per ) const {
	vector<double> prices(nomrks_t);
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		prices[i] = mrk[j][per].price;
	}
	return prices;
}

//! returns vector of market prices
const vector<double> Marketplace::showPRC_NR( const int per ) const {
	vector<double> prices(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		prices[i] = mrk[j][per].price;
	}
	return prices;
}

//! returns vector of market excess demands
const vector<double> Marketplace::showED( const int per ) const {
	vector<double> ED(nomrks_t);
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		ED[i] = mrk[j][per].exdmd;
	}
	
	return ED;
}

//! returns vector of market excess demands
const vector<double> Marketplace::showED_NR( const int per ) const {
	vector<double> ED(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		ED[i] = mrk[j][per].exdmd;
	}
	
	return ED;
}

//! returns vector of log of market excess demands
const vector<double> Marketplace::showlogED( const int per ) const {
	vector<double> ED(nomrks_t);
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		ED[i] = mrk[j][per].lexdmd;
	}
	
	return ED;
}

//! returns vector of log of market excess demands
const vector<double> Marketplace::showlogED_NR( const int per ) const {
	vector<double> ED(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		ED[i] = mrk[j][per].lexdmd;
	}
	
	return ED;
}

//! returns vector of log of market demands
const vector<double> Marketplace::showlogDem( const int per ) const {
	vector<double> Dem(nomrks_t);
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		Dem[i] = mrk[j][per].ldem;
	}
	
	return Dem;
}

//! returns vector of log of market demands
const vector<double> Marketplace::showlogDem_NR( const int per ) const {
	vector<double> Dem(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		Dem[i] = mrk[j][per].ldem;
	}
	
	return Dem;
}

//! returns vector of log of market supplys
const vector<double> Marketplace::showlogSup( const int per ) const {
	vector<double> Sup(nomrks_t);
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		Sup[i] = mrk[j][per].lsup;
	}
	
	return Sup;
}

//! returns vector of log of market supplys
const vector<double> Marketplace::showlogSup_NR( const int per ) const {
	vector<double> Sup(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		Sup[i] = mrk[j][per].lsup;
	}
	
	return Sup;
}

//! Calculate the derivatives, elasticities or Jacobian
const vector<double> Marketplace::jacobian( const int k, const int per ) const {
	double ddemand,dsupply,dprice;
	vector<double> JFD(nomrks_t_NR);
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		if(mrk[j][per].demand == 0) 
			ddemand = 0;
		else
			ddemand = (mrk[j][per].demand - mrk[j][per].tdemand)
			/mrk[j][per].tdemand;
		if(mrk[j][per].tsupply == 0) 
			dsupply = 0;
		else
			dsupply = (mrk[j][per].supply - mrk[j][per].tsupply)
			/mrk[j][per].tsupply;
		if(mrk[j][per].tprice == 0) 
			dprice = 0;
		else
			dprice = (mrk[k][per].price - mrk[k][per].tprice)
			/mrk[k][per].tprice;
		JFD[i] = (ddemand - dsupply)
			/dprice; 
	}
	return JFD;
}

//! Calculate demand elasticities
const vector<double> Marketplace::dem_elas( const int k, const int per ) const {
	double ddemand,dprice;
	vector<double> JFD(nomrks_t);
	
	int kk = mrk_isol[k]; // market index 
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		if(mrk[j][per].tdemand == 0) 
			ddemand = smnum;
		else
			ddemand = log(mrk[j][per].demand) - log(mrk[j][per].tdemand);
		if(mrk[kk][per].tprice == 0) 
			dprice = smnum;
		else
			dprice = log(mrk[kk][per].price)- log(mrk[kk][per].tprice);
		JFD[i] = ddemand/dprice; 
	}
	return JFD;
}

//! Calculate demand elasticities
const vector<double> Marketplace::dem_elas_NR( const int k, const int per ) const {
	double ddemand,dprice;
	vector<double> JFD(nomrks_t_NR);
	
	int kk = mrk_isol_NR[k]; // market index 
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		if(mrk[j][per].tdemand == 0) 
			ddemand = smnum;
		else
			ddemand = log(mrk[j][per].demand) - log(mrk[j][per].tdemand);
		if(mrk[kk][per].tprice == 0) 
			dprice = smnum;
		else
			dprice = log(mrk[kk][per].price)- log(mrk[kk][per].tprice);
		JFD[i] = ddemand/dprice; 
	}
	return JFD;
}

//! Calculate supply elasticities
const vector<double> Marketplace::sup_elas( const int k, const int per ) const {
	double dsupply,dprice;
	vector<double> JFS(nomrks_t);
	
	int kk = mrk_isol[k]; // market index 
	
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		if(mrk[j][per].tsupply == 0) 
			dsupply = smnum;
		else
			dsupply = log(mrk[j][per].supply) - log(mrk[j][per].tsupply);
		if(mrk[kk][per].tprice == 0) 
			dprice = smnum;
		else
			dprice = log(mrk[kk][per].price) - log(mrk[kk][per].tprice);
		JFS[i] = dsupply/dprice; 
	}
	return JFS;
}

//! Calculate supply elasticities for NR solution method
const vector<double> Marketplace::sup_elas_NR( const int k, const int per ) const {
	double dsupply,dprice;
	vector<double> JFS(nomrks_t_NR);
	
	int kk = mrk_isol_NR[k]; // market index 
	
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index	(array fixed, changed from mrk_isol, 3/14/03 -- sjs)
		if(mrk[j][per].tsupply == 0) 
			dsupply = smnum;
		else
			dsupply = log(mrk[j][per].supply) - log(mrk[j][per].tsupply);
		if(mrk[kk][per].tprice == 0) 
			dprice = smnum;
		else
			dprice = log(mrk[kk][per].price) - log(mrk[kk][per].tprice);
		JFS[i] = dsupply/dprice; 
	}
	return JFS;
}

//! write out market info to database
void Marketplace::MCoutput() const {
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
		string uname,vector<double> dout);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	int m;
	// write market prices, supply and demand
	for (int i=0;i<nomrks;i++) {
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].price;
		dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"1_price","$/GJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].supply;
		dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"2_supply","EJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].demand;
		dboutput4(mrk[i][0].region,"Market",mrk[i][0].name,"3_demand","EJ",temp);
	}
}

//! write out market info to file
void Marketplace::outputfile() const {
	// function protocol
	void fileoutput2(string var1name,string var2name,string var3name,
		string var4name,string var5name,vector<double> dout,string uname);
	void fileoutput3(string var1name,string var2name,string var3name,
		string var4name,string var5name,string uname,vector<double> dout);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	int m;
	
	// Function arguments are variable name, double array, db name, and
	// table name.
	// The function writes all years.
	
	// write market prices and supply (or demand)
	for (int i=0;i<nomrks;i++) {
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].price;
		fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","price","$/GJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].supply;
		fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","supply","EJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].demand;
		fileoutput3(mrk[i][0].region,"market",mrk[i][0].name," ","demand","EJ",temp);
	}
}

//! write out market info to file for debugging
void Marketplace::bugout( const int per, const int iteration) const {
	// write market prices and supply (or demand)
	bugoutfile<<"Period,"<< per <<",Nth Iteration:,"<<iteration<<"\n";
	for (int i=0;i<nomrks;i++) {
		bugoutfile<<"market:,"<<i<<","<<mrk[i][0].name<<",price:,"<<mrk[i][per].price<<",$/GJ,";
		bugoutfile<<"supply:,"<<mrk[i][per].supply<<",EJ,demand:,"<<mrk[i][per].demand<<",EJ,";
		bugoutfile<<"solved:,"<<mrk[i][per].solveMarket<<"\n";
	}
	bugoutfile<<"\n";
}

//! write out market price supply and demand info to file for debugging
void Marketplace::sdcurves( const int per, const int iteration) const {
	// write market prices and supply (or demand)
	for (int i=0;i<nomrks;i++) {
		sdcurvefile<<i<<","<<mrk[i][0].name<<","<<mrk[i][per].price<<",";
		sdcurvefile<<mrk[i][per].supply<<","<<mrk[i][per].demand<<",";
	}
	sdcurvefile<<"\n";
}

//! Bracketing function only, does not find solution
int Marketplace::Bracket( const double Tol, vector<solinfo>& sol, bool& allbracketed, bool& firsttime,int& n, const int per ) {
	int i, j;
	int nmrks = sol.size(); // number of markets to solve
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand
	
	logfile << ",,Bracketing function called.\n";
	
	//         bugoutfile<<"\nMarket,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,Supply,Price,Demand,Tolerance\n";
	//bugoutfile<<5<<","<<sol[5].X<<","<<sol[5].XL<<","<<sol[5].XR<<",";
	//bugoutfile<<sol[5].ED<<","<<sol[5].EDL<<","<<sol[5].EDR<<","<<-1<<","<< -1<<","<<-1<<","<<Tol<<","<<-1<<"\n"; 
	
	// Loop is done at least once.
	do {
		// bracketed array is either 0 or 1
		j=0; // counter
		for (i=0;i<nmrks;i++)
			if (sol[i].bracketed) j++;
			if (j == nmrks) {
				allbracketed = true;
				// store original brackeded information
				if(firsttime) {
					for (i=0;i<nmrks;i++) {
						sol[i].XL_org = sol[i].XL; 
						sol[i].EDL_org = sol[i].EDL;
						sol[i].XR_org = sol[i].XR; 
						sol[i].EDR_org = sol[i].EDR;
					}
					firsttime = false;
				}
			}
			else
				allbracketed = false;
			
			// Bracketing of prices; done first regardless of choice of solution
			// algorithm.
			if (!allbracketed)
				for (i=0; i<nmrks; ++i) {
					if (!sol[i].bracketed) {
						if (sign(sol[i].ED) == sign(sol[i].EDL)) { //first will always be true
							if (sol[i].ED < 0) {
								sol[i].XL = sol[i].X; 
								sol[i].EDL = sol[i].ED;
								if (sign(sol[i].EDL) == sign(sol[i].EDR)) sol[i].X *= 0.5; 
								else sol[i].bracketed=true;
							}
							else {
								if (sol[i].X == 0) {
									sol[i].X = 10.0; // starting value
									int stop=1;
								}
								else {
									sol[i].XR = sol[i].X; 
									sol[i].EDR = sol[i].ED;
									if (sign(sol[i].EDL) == sign(sol[i].EDR)) sol[i].X *= 1.5;
									else sol[i].bracketed=true;
								}
							}
						}
						else { // branch for bracket
							if (sol[i].ED < 0) {
								sol[i].XL = sol[i].X; 
								sol[i].EDL = sol[i].ED;
								if (sol[i].XL<=sol[i].XR) { //XL is always greater than XR
									sol[i].X = sol[i].XL*0.9;
									sol[i].bracketed=false;
								}
								else
									sol[i].bracketed=true;
							}
							else {
								if (sol[i].X == 0) {
									sol[i].X = 10.0; // starting value
									int stop=1;
								}
								else {
									sol[i].XR = sol[i].X; 
									sol[i].EDR = sol[i].ED; 
									if (sol[i].XL<=sol[i].XR) { //XL is always greater than XR
										sol[i].X = sol[i].XR*1.1;
										sol[i].bracketed=false;}
									else
										sol[i].bracketed=true;
								}
							}
						}
						if (sol[i].X < 0) sol[i].X = smnum; // cannot have negative prices
					}
					if (sol[i].XL == sol[i].XR) sol[i].bracketed=false;
					
					// Check if market has fixed constraint and 
					// if demand is always below contraint
					if (sol[i].X < smnum && sol[i].ED < 0) {
						sol[i].X = sol[i].XL = sol[i].XR = 0;
						sol[i].bracketed = true;
					}
					
					// case when supply is zero and cannot bracket (ie. resource runs out)
					if(!sol[i].bracketed && (fabs(sol[i].ED)<Tol)) sol[i].bracketed=true;
				}
				// bracketed array is either 0 or 1
				j=0; // counter
				for (i=0;i<nmrks;i++) {
					if (sol[i].bracketed) j++;
				}
				if (j == nmrks) {
					allbracketed = true;
					// store original brackeded information
					if(firsttime) {
						for (i=0;i<nmrks;i++) {
							sol[i].XL_org = sol[i].XL; 
							sol[i].EDL_org = sol[i].EDL;
							sol[i].XR_org = sol[i].XR; 
							sol[i].EDR_org = sol[i].EDR;
						}
						firsttime = false;
					}
				}
				else
					allbracketed = false;
				
				for (i=0;i<nmrks;i++)
					Xtemp[i]=sol[i].X;
				
				setPRC(Xtemp,per); // set new prices
				nulldem(per);	// null demand
				nullsup(per); // null supply
				
				world.calc(per); // call world object to recalculate supply and demand
				
				logED(per); // calculate log of excess demand
				excessdemand(per); // calculate excess demand
				// using log of ED does not matter for bracketing
				// since only sign matters
				//logED =  showlogED(per); // show log of excess demand
				EDtemp = showED(per); // show excess demand
				for (i=0;i<nmrks;i++)
					sol[i].ED = EDtemp[i];
				
				
				// for debugging
				int bug = 0; // debugging on(1) or off(0)
				if (bug) {
					bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
					for (i=0; i<nmrks; ++i) {
						bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
							<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
					}
					bugout(per,nn);
					sdcurves(per,nn);
				}
				
	} // end do loop		
	while (++nn < 30 && !allbracketed);			// report sucess, 1
	Code = (allbracketed ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

//! function to check bracketing -- ???(see details)
/*! if not solving and bracketed prices are converging,
???? what does this mean?*/
void Marketplace::CheckBracket( const double Tol, vector<solinfo>& sol, bool& allbracketed ){
	logfile << ",,Check brackets function called.\n";
	int nmrks = sol.size(); // number of markets to solve
	int reset = 0;
	// try rebracketing by setting bracketed array to false
	for(int i=0;i<nmrks;i++) {
		//if (fabs(sol[i].XL-sol[i].XR)<verysmnum && fabs(sol[i].ED>Tol)) {
		if (fabs(sol[i].dX) < smnum) {
			allbracketed = false;
			sol[i].bracketed = false;
			sol[i].XL = sol[i].XR = sol[i].X; 
			sol[i].EDL = sol[i].EDR = sol[i].ED; 
		}
	}
}

//! Bisection Solution Mechanism (all markets)
int Marketplace::Bisection_all( const double Tol, const int IterLimit, vector<solinfo>& sol,
	int& n, const int per ) {

	int i, j;
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	int nmrks = sol.size(); // number of markets to solve
	double M; // maximum equality value
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand
	bool breakout;	// var to allow various conditions to exit bisection routine
        double breakOutThreshold = 0.001;
        double previousEDvalue = -1;
        double maxEDvalue = 0;
        int maxInt = 0;
	
	const int bug = 0; // debugging on(1) or off(0)
	
	if (bug) {
		bugoutfile<<"\nMarket,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,Supply,Price,Demand,Tolerance\n";
	}
	// bugoutfile<<5<<","<<sol[5].X<<","<<sol[5].XL<<","<<sol[5].XR<<",";
	// bugoutfile<<sol[5].ED<<","<<sol[5].EDL<<","<<sol[5].EDR<<","<<-1<<","<< //-1<<","<<-1<<","<<Tol<<","<<-1<<"\n"; 
	
    if (trackED) { 
		cout << endl <<"Bisection begin " <<endl; 
	}
  	logfile << ",,Bisection_all function called.\n";
	// solve all markets
	do {
        breakout = false; //default is not to breakout of bisection routine
        maxEDvalue = 0;
		if (bugMinimal) {
			bugoutfile << " Bisect "<<nn;
		}
        if (bugTracking) {
			prices_to_bugout(per);
		}
		for (i=0; i<nmrks; ++i) {
			if (fabs(sol[i].ED) > Tol) { // if haven't solved
				// Move the left bracket in if Supply > Demand
				if (sol[i].ED < 0) {
					sol[i].XL = sol[i].X;
					sol[i].EDL = sol[i].ED;
				}
				// Move the right bracket in if Demand >= Supply
				else {
					sol[i].XR = sol[i].X;
					sol[i].EDR = sol[i].ED;
				}
				// Set new trial value to center
				sol[i].X = (sol[i].XL + sol[i].XR)/2;
				// Set new change in excess demand
				sol[i].dX = sol[i].XR - sol[i].XL;
			}	
			// price=0 and supply>demand
			// only true for constraint case
			// other markets cannot have supply>demand as price->0
			if (fabs(sol[i].X)<smnum && sol[i].ED<0) { 
				sol[i].X = 0; 
				sol[i].dX = 0;
			} 
			Xtemp[i]=sol[i].X; // copy prices to temporary vector
		}
		setPRC(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand
		
		logED(per); // calculate log of excess demand
		excessdemand(per); // calculate excess demand
		EDtemp = showED(per); // show excess demand
		for (i=0;i<nmrks;i++) {
			sol[i].ED = EDtemp[i];
			j = showmrk_sol(i); 
			// if price market, then check if XL < Demand, if so move XL
			/// BAD, BAD, BAD
			// This is not the way to do this, need a more general bracket check
			if (getType(j,per) == Market::PRICE && sol[i].XL < getRawDemand(j,per) ) {
				sol[i].XL = getRawDemand(j,per)* 1.5;
				// print out to remind us to re-do this correctly
				if (per == 8) {
					cout << "   Shifted XL for market " << j << " ("<< getName(j)<< "-"
						<< getGoodName(j)<< ")" << endl;
				}
			}
                        
            // debugging code that tracks ED
            if (i< 10 && 1==2) {
               cout << getGoodName(j)<<": "<<sol[i].XL <<"("<<getRawPrice(j,per) <<") "<< sol[i].XR;
               if (!(getType(j,per) == Market::NORMAL)) {
                  cout <<"  S,D: ["<<getRawSupply(j,per)<<","<< getRawDemand(j,per) <<"]";
                  if (sol[i].XL < getRawDemand(j,per) ) {
					  cout << "***";
				  } 
               }
               else {
                  cout <<"  ED: ["<<getRawDemand(j,per) - getRawSupply(j,per)<<","<< sol[i].ED<<"]";
               }
               cout <<endl;
            }
                    
		}
		M = maxED(per);
		
		// for debugging
		if (bug) {
			//  bugoutfile<<"\nMarket,X-unknown,L-Brack,R-Brack,Ex Dem,EDL-Brac,EDR-Brac,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				j =  showmrk_sol(i); 
				double bugP,  bugD, bugS;
				if (j == 1) {
					bugP = getRawPrice(j,per);
					bugS = getRawSupply(j,per);
					bugD = getRawDemand(j,per);
					//     cout <<nn<< " sup:"<<bugS<<", p:" << bugP<<", dem:"<<bugD << endl;
					
					bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR<<",";
					bugoutfile<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<bugS<<","<< bugP<<","<<bugD<<","<<Tol<<","<<nn<<","<<j<<"\n"; 
				}
			}
			//  bugout(per,nn);
			//  sdcurves(per,nn); 
		}
        if (trackED) { 
           maxInt = worstED(per);
           cout << "maxED: "<<M<<" ("<< getName(maxInt)<< getGoodName(maxInt) << ")"  << endl; 
        }
        if (nn > 5) {	// always bisect a few times
           // If the worst ED is not changing too much then breakout of bisection and let NR try to solve this
           if (abs(M-previousEDvalue)/previousEDvalue < breakOutThreshold)  {  
              breakout = true; 
           }
        }
            
        previousEDvalue = M;
            
	} // end do loop		
	while (++nn < IterLimit && M >= Tol && !breakout);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
        
    if (trackED) { cout << endl; }

	return Code;
}

//! Bisection Solution Mechanism (single market)
int Marketplace::Bisection_i( const int i, const double Tol, vector<solinfo>& sol,int& n, const int per ){
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	vector<double> Xtemp= showPRC(per); // temporary prices
	vector<double> EDtemp= showED(per); // temporary excess demand
	
	logfile << ",,Bisection_i function called.\n";
    // solve only one market
	do {
		if (fabs(sol[i].ED) > Tol) {
			if (sol[i].ED < 0) {
				sol[i].XL = sol[i].X;
				sol[i].EDL = sol[i].ED; 
			}
			else {
				sol[i].XR = sol[i].X;
				sol[i].EDR = sol[i].ED; 
			}
			sol[i].X = (sol[i].XL + sol[i].XR)/2;
			sol[i].dX = sol[i].XR - sol[i].XL;
		}			
		Xtemp[i]=sol[i].X; // copy prices to temporary vector
		// only current market should have different prices
		setPRC(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand
		
		logED(per); // calculate log of excess demand
		excessdemand(per); // calculate excess demand
		EDtemp = showED(per); // show excess demand
		for (int j=0;j<EDtemp.size();j++)
			sol[j].ED = EDtemp[j];
		
		M = fabs(sol[i].ED); // only solving one market
		
		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			bugout(per,nn);
			sdcurves(per,nn);
		}
		
	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

//! False Position Solution Mechanism (all markets)
int Marketplace::FalsePos_all( const double Tol,vector<solinfo>& sol,int& n, const int per ) {
	int i;
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int nmrks = sol.size(); // number of markets to solve
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand
	
	logfile << ",,FalsePos_all function called.\n";
	// solve all markets
	do {
		for (i=0; i<nmrks; ++i) {
			if (fabs(sol[i].ED) > Tol) {
				if (sol[i].ED < 0) {
					sol[i].XL = sol[i].X;
					sol[i].EDL = sol[i].ED; 
				}
				else {
					sol[i].XR = sol[i].X;
					sol[i].EDR = sol[i].ED; 
				}
				sol[i].dX = sol[i].XR - sol[i].XL;
				sol[i].X = sol[i].XL + (sol[i].dX*sol[i].EDL)/(sol[i].EDL-sol[i].EDR);
			}		
			Xtemp[i] = sol[i].X;
		}
		setPRC(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand
		
		logED(per); // calculate log of excess demand
		excessdemand(per); // calculate excess demand
		EDtemp =  showED(per); // show excess demand
		for (i=0;i<EDtemp.size();i++)
			sol[i].ED = EDtemp[i];
		
		M = *max_element(EDtemp.begin(), EDtemp.end());
		
		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
					<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			bugout(per,nn);
			sdcurves(per,nn);
		}
		
	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

//! Secant Solution Mechanism (all markets)
int Marketplace::Secant_all( const double Tol,vector<solinfo>& sol,int& n, const int per ) {
	int i;
	int iSec=0; 
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int nmrks = sol.size(); // number of markets to solve
	vector<double> Xtemp(nmrks); // temporary prices
	vector<double> EDtemp(nmrks); // temporary excess demand
	
	logfile << ",,Secant_all function called.\n";
	// solve all markets
	do {
		if (iSec == 0) { // initial starting point; done once
			for (i=0; i<nmrks; ++i) {
				if (fabs(sol[i].ED) < fabs(sol[i].EDR)) {
					sol[i].X = sol[i].XL;
					sol[i].XL = sol[i].XR;
					double swap = sol[i].EDL;
					sol[i].EDL = sol[i].EDR;
					sol[i].ED = swap; 
				}
				else 
					sol[i].X = sol[i].XR;
			}
		}
		for (i=0; i<nmrks; i++) {
			if (fabs(sol[i].ED) > Tol) {
				sol[i].dX = (sol[i].XL - sol[i].X)*sol[i].ED/(sol[i].ED-sol[i].EDL);
				sol[i].XL = sol[i].X;
				sol[i].EDL = sol[i].ED;
				if (fabs(sol[i].dX)<fabs(sol[i].X))
					sol[i].X += sol[i].dX;
			}
			Xtemp[i] = sol[i].X;
		}
		iSec++;
		
		setPRC(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand
		
		logED(per); // calculate log of excess demand
		excessdemand(per); // calculate excess demand
		EDtemp =  showED(per); // show excess demand
		for (i=0;i<EDtemp.size();i++)
			sol[i].ED = EDtemp[i];
		
		M = *( max_element(EDtemp.begin(), EDtemp.end() )); // Max returns largest sol[i].ED
		
		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<nmrks; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
					<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<Tol<<"\n";
			}
			bugout(per,nn);
			sdcurves(per,nn);
		}
		
	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	n+=nn-1;
	return Code;
}

//! Function to calculate derivative
//void JFunction(valarray<double> prices, Matrix& JFDM, int per)
void Marketplace::JFunction( vector<double> prices, double** JFDM, int& n, int const per ) {
	int m = prices.size();
	double deltap = 1e-4; // What is the proper value for delta?
	vector<double> tprices = prices; // define and initialize prices for storage
	vector<double> tmpJFD(m);
	
	storeinfo(per); // store original market info before perturbing price
	for (int j=0;j<m;++j) {	// j is column index
		prices[j] *= (1 + deltap);	  // add price times deltap
		//prices[j] += deltap;	  // add price times deltap
		setPRC_NR(prices,per);	// set new price for one market
		nulldem(per);	// null demand
		nullsup(per);	// null supply
		world.calc(per); // call world object to recalculate supply and demand
		int col = j; // function calculates rows for each column
		tmpJFD =  jacobian(col,per); // calculate elasticities or Jacobian
		for (int i=0;i<m;++i) // copy column vector to Jacobian Matrix
			JFDM[i][j] = tmpJFD[i]; // i is row index
		restoreprc_NR(per); // restore perturbed market price
		prices[j] = tprices[j]; //  restore perturbed market price
		n++;
	}
}

//! Function to calculate derivative for Ron's version
void Marketplace::Derivatives( vector<double> prices, double** JFDM, double** JFSM, int& n, const int per ) {
	int m = prices.size();
	//double deltap = 1e-4; // Orginal, What is the proper value for delta?
	double deltap = 1e-5; // What is the proper value for delta?
	vector<double> tprices = prices; // define and initialize prices for storage
	vector<double> tmpJFD(m),tmpJFS(m);
	
	storeinfo(per); // store original market info before perturbing price
	for (int j=0;j<m;++j) {	// j is column index
		prices[j] *= (1 + deltap);	  // add price times deltap
		setPRC_NR(prices,per);	// set new price for one market
		nulldem(per);	// null demand
		nullsup(per);	// null supply
		world.calc(per); // call world object to recalculate supply and demand
		int col = j; // function calculates rows for each column
		tmpJFD =  dem_elas_NR(col,per); // calculate demand elasticities
		tmpJFS =  sup_elas_NR(col,per); // calculate supply elasticities
		for (int i=0;i<m;++i) {// copy column vector to Jacobian Matrix
			JFDM[i][j] = tmpJFD[i]; // i is row index
			JFSM[i][j] = tmpJFS[i]; // i is row index
		}
		restoreprc_NR(per); // restore perturbed market price
		prices[j] = tprices[j]; //  restore perturbed market price
		n++;
	}
}

//! Newton Raphson Solution Mechanism (all markets)
int Marketplace::NewtRap( const double Tol, vector<solinfo>& sol, double** JF, double** bb, int& n, const int per ){
	int i;
	int NRn=0; // calls to calculate elasticities
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value
	int m =  setMarketsToSolveNR(per); // number of markets to solve
	vector<double> DP(m); // adjustment value
	vector<double> logEDVec =  showlogED_NR(per); // show log of excess demand
	vector<double> Xtemp= showPRC_NR(per); // temporary prices
	vector<double> EDtemp(m); // temporary excess demand
	
	// function protocol
	//void gaussj(Matrix& a,int n,Matrix& b,int m); // function protocol
	void gaussj(double** a,int n,double** b,int m);
	
	logfile << ",,Newton-Raphson function called.\n";
	// solve all markets
	do {
		if ((NRn < 5) && (per < modeltime.getmaxper())) { // do only once for each period
			JFunction(Xtemp,JF,n,per); //recalculate Jacobian matrix, returns JF matrix
			gaussj(JF,m,bb,1); // inverse of JF matrix by reference
		}
		NRn++;
		for (i=0; i<m; ++i) {
			// Calculate new price based on NR
			DP[i] = 0;
			for (int j=0; j<m; ++j)
				DP[i] -= JF[i][j]*logEDVec[j];
			if (DP[i] < -1) DP[i] = -0.9;
			Xtemp[i]*=(1+DP[i]); // new price
		}
		
		setPRC_NR(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		
		world.calc(per); // call world object to recalculate supply and demand
		logED( per ); // calculate log of excess demand
		excessdemand(per); // calculate excess demand
		logEDVec =  showlogED_NR(per); // show log of excess demand
		EDtemp =  showED_NR(per); // show excess demand
		
		M =  maxED(per); // Max returns largest ED[i]
		
		// for debugging
		int bug = 0; // debugging on(1) or off(0)
		if (bug) {
			bugoutfile<<"\nMarket,X,DP,ED,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<Xtemp[i]<<","<<DP[i]<<","<<EDtemp[i]<<","<<Tol<<"\n";
			}
			bugout(per,nn);
			sdcurves(per,nn); 
		}
		// if solution moves in wrong direction
		if(M>1500) {
			logfile << ",,Exit Newton-Raphson function M>1500.\n";
			return 0;
		}
	} // end do loop		
	while (++nn < 30 && M >= Tol);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	// resize and reasign all solution prices and ED's
	// need to copy prices and ED to sol
	Xtemp =  showPRC(per);
	EDtemp =  showED(per);
	for (i=0; i<Xtemp.size(); i++) {
		sol[i].X = Xtemp[i];
		sol[i].ED = EDtemp[i];
	}
	n+=nn-1;
	return Code;
}

//! Ron's version of the Newton Raphson Solution Mechanism (all markets)
int Marketplace::NR_Ron( const double Tol,vector<solinfo>& sol, double** JF, double** bb, int& n, const int per ) {
	int i;
	int NRn=0; // calls to calculate elasticities
	int nn = 0; // number of iterations
	int Code = 2; // Code that reports success 1 or failure 0
	double M; // maximum equality value 
	int m =  setMarketsToSolveNR(per ); // number of markets to solve
	vector<double> NP(m); // adjustment value
	vector<double> KD(m); // k values demand
	vector<double> KS(m); // k values supply
	vector<double> KDS(m); // k values demand - supply
	vector<double> Xtemp= showPRC_NR(per); // temporary prices
	vector<double> EDtemp(m); // temporary excess demand
	
	bool breakout = false;	// var to allow various conditions to exit NR routine
    int maxInt = 0;
    double beforeEDvalue = -1;
    double previousEDvalue = -1;
    double breakOutThreshold = 0.001;
        
    if (trackED) { cout <<"NR_Ron begin "; }

	double** aaa = new double*[ m ];
	double** bbb = new double*[ m ];
	double **JFDM = new double*[ m ];
	double **JFSM = new double*[ m ];
	
	for( i = 0; i < m; i++ ) {
		aaa[ i ] = new double[ m ];
		bbb[ i ] = new double[ m ];
		JFDM[ i ] = aaa[ i ];
		JFSM[ i ] = bbb[ i ];
	}
	
	// function protocol
	void gaussj(double** a,int n,double** b,int m);
	
	logfile << ",,Ron's version of the Newton-Raphson function called.\n";
	// solve all markets
	do {
		if (bugTracking) {
			bugoutfile<<"Number of Markets:  "<< m << "\n";
			bugoutfile << "Ron_NR "<<nn;  prices_to_bugout(per);
		}

		logDem(per); // calculate log of demand
		logSup(per); // calculate log of supply
		
		if ((NRn < 1) && (per < modeltime.getmaxper())) { // control no of times derivatives are calculated
            if (trackED) { cout <<" ... "; }
			Derivatives(Xtemp,JFDM,JFSM,n,per); //recalculate Jacobian matrix, returns JF matrix
			for(i=0;i<m;++i) {
				for(int j=0;j<m;++j) {
					JF[i][j] = JFSM[i][j]-JFDM[i][j];
				}
			}
			gaussj(JF,m,bb,1); // inverse of JF matrix by reference
            if (trackED) {
				cout <<" End Derivatives " <<endl;
			}
		}
		NRn++;

		if (bugTracking) {
			bugoutfile << "--after gaussj "<<nn;  prices_to_bugout(per);
			bugoutfile <<",,,Xtemp1,";
		}
		
		// initialize KD and KS as logs of original demand and supply
		KD =  showlogDem_NR(per); // return log of demand
		KS =  showlogSup_NR(per); // return log of supply
		for (i=0; i<m; ++i) {
			for (int j=0; j<m; ++j) {
				KD[i] -= (log(max(Xtemp[j],smnum))*JFDM[i][j]);
				KS[i] -= (log(max(Xtemp[j],smnum))*JFSM[i][j]);
			}
			KDS[i] = KD[i] - KS[i];
		}
		
		if (bugMinimal) {
			bugoutfile << endl<<",,,Xtemp2,";
		}
		// Calculate new log price based on NR
		for (i=0; i<m; ++i) {
			NP[i] = 0;
			for (int j=0; j<m; ++j) {
				NP[i] += JF[i][j]*KDS[j];
			}
			Xtemp[i] = exp(NP[i]); // new price
		}
		
		setPRC_NR(Xtemp,per); // set new prices
		nulldem(per);	// null demand
		nullsup(per); // null supply
		if (bugTracking) {
			bugoutfile <<endl<< "--before .calc "<<nn;  prices_to_bugout(per);
		}
		
		world.calc(per); // call world object to recalculate supply and demand
		
		if (bugTracking) {
			bugoutfile << "--after .calc "<<nn;  prices_to_bugout(per);
			supply_to_bugout(per);  demand_to_bugout(per);
		}
		
		excessdemand(per); // calculate excess demand
		EDtemp =  showED_NR(per); // show excess demand
		
		M =  maxED(per); // Max returns largest ED[i]

		// for debugging
		bool bug = false; // debugging on or off
		bug = (per == 2 ? false : false);
		
		if (bug) {
			bugoutfile<<"\nMarket,X,DP,ED,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<Xtemp[i]<<","<<NP[i]<<","<<EDtemp[i]<<","<<Tol<<"\n";
			}
			bugout(per,nn);
			sdcurves(per,nn); 
		}
                
        maxInt = worstED(per);
        if (trackED) {
			cout << "maxED: "<<M<<" ("<< getName(maxInt)<< getGoodName(maxInt) << ")" << endl;
		}

        if (nn > 3) {	
        // If the worst ED is not changing too much then breakout
        // double tempVal = abs(M-previousEDvalue)/previousEDvalue;
        //  cout << " diff: "<<tempVal<<" ";
			if (abs(M-previousEDvalue)/previousEDvalue < breakOutThreshold) {  // not converging very fast
				breakout = true;
                if (trackED) {
					cout << "slow convergance" << endl;
				}
			}
            if (abs(M-beforeEDvalue)/beforeEDvalue < 0.01 && !breakout) {  // oscillating
				breakout = true; 
                if (trackED) {
					cout << "oscillating" << endl;
				}
                logfile << ",,,Exited NR due to oscillation" << endl;
            }
        }      
                  
        beforeEDvalue = previousEDvalue;
        previousEDvalue = M;
                   
		// if solution moves in wrong direction
		if(M>1500) {
			logfile << ",,Exit Newton-Raphson function M>1500. "<< endl;
       //   logfile << ", Due to market " << getName(maxInt)<< "-"<< getGoodName(maxInt) <<"\n";
            if (trackED && per > 0) {
				cout << "Exit Newton-Raphson function M>1500" << endl;
			}
            for (i=0; i<nomrks; ++i) {
                if (fabs(mrk[i][per].exdmd) > M/100) {
					if (trackED) {
						cout << "ED: ("<<getName(i)<< "-"<< getGoodName(i)<<") - " 
						<< mrk[i][per].exdmd << endl;
					}
					logfile << ",,,Due to market "<<getName(i)<< "-"<< getGoodName(i)<<" - ED: " 
						<< mrk[i][per].exdmd << endl;
                }
            }
			return 0;
		}
	} // end do loop	
	
	while (++nn < 35 && M >= Tol && !breakout);			// report sucess, 1
	Code = (M < Tol ? 1 : 0);				// or failure, 0, 
	// resize and reasign all solution prices and ED's
	// need to copy prices and ED to sol
	Xtemp =  showPRC(per);
	EDtemp =  showED(per);
	for (i=0; i<Xtemp.size(); i++) {
		sol[i].X = Xtemp[i];
		sol[i].ED = EDtemp[i];
	}
	n+=nn-1;
	logfile << ",Number of Newton-Raphson iterations: n="<<nn<<"\n";

	// free up memory when done
	
	for( i = 0; i < m; i++ ) {
		delete[] aaa[ i ];
		delete[] bbb[ i ];
	}
	
	delete[] aaa;
	delete[] bbb;
	delete[] JFDM;
	delete[] JFSM;
	return Code;
}


void Marketplace::solve( const int per ) {
	bool allbracketed = false;
	bool prod_not_null = false;
	const bool useBisect = false;
	const bool useFP = false;
	const bool useSecant = false;
	const bool useNR = false;
	const bool useNR_Ron = true;
	bool firsttime = true;
	int i=0; // some index
	int n=0; // index for solution iteration
	int bn=0; // counter for bisection routine
	int Code = 2; // Code that reports success 1 or failure 0
	int solved = 0; // Code that reports success 1 or failure 0
	// double solTolerance = 0.0001; // Tolerance for solution criteria
	// Extra high tolerance to get to solution faster
	//double solTolerance = 0.1; // Tolerance for solution criteria
	double M; // temporary maximum value of equality condition			 
	vector<double> X,ED,logEDVec; // price, excess demand and log of excess demand vectors
	
	excessdemand(per); // first calculate excess demand for all markets
	const int m =  setMarketsToSolve(per ); // set number of markets to solve
	
	logfile << ",Starting Solution. Solving for " << m << " markets." << endl;
	
	// if m = 0, no markets to solve, break out of solution.
	if (m==0) {
		cout<< "Model solved with last period's prices"; 
		return;
	}
	
	// For Newton-Raphson Solution Mechanism
	double** a = new double*[ m ];
	double** b = new double*[ m ];
	
	double** JF = new double*[ m ];
	double** bb = new double*[ m ];
	
	for ( i = 0; i < m; i++ ) {
		a[ i ] = new double[ m ];
		b[ i ] = new double[ m ];
		JF[ i ] = a[ i ];
		bb[ i ] = b[ i ];
	}
	
	logED(per); // calculate log of excess demand
	X =  showPRC(per); // showPRC returns a vector of prices	
	logEDVec =  showlogED(per); // showlogED returns a vector of logED
	ED =  showED(per); // showED returns a vector of ED
	
	vector<solinfo> sol(m); // create vector of solution information
	// initialize solution information
	for (i=0;i<m;i++) {
		sol[i].XL = sol[i].XR = sol[i].X = X[i];
		sol[i].EDL = sol[i].EDR = sol[i].ED = ED[i]; 
		sol[i].bracketed = 0;
	}
	
	// for debugging
	if (bugMinimal) {
		bugoutfile<<"\nSolution() Begin. Per "<< per << "\n";
		bugoutfile<<"Number of Markets:  "<< m << "\n";
		bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
		for (i=0; i<m; ++i) {
			bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solTolerance<<"\n";
		}
		bugout(per,n);
		sdcurves(per,n);
	}
	
	// Loop is done at least once.
	do {
		if (!allbracketed) {
			bool wasFirstTime = firsttime;
            int didBracket;
			didBracket = Bracket(solTolerance,sol,allbracketed,firsttime,n,per);
                        
            // In case initial prices are way out of wack, bisect a few times and re-bracket
            if (wasFirstTime && !solved) { 
               solved = Bisection_all(solTolerance,15,sol,n,per); 
          //   cout << endl <<"********Rebracketing*******" << endl;
          //   if ( !solved ) { didBracket = Bracket(solTolerance,sol,allbracketed,firsttime,n,per); }
            }
		}
		
		// Bisect method
		if (allbracketed && useBisect) {
			if (bn < 1) {
				int maxIter = 30;
				solved = Bisection_all(solTolerance,maxIter,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++)
						CheckBracket(solTolerance,sol,allbracketed);
				}
				++bn;
			}
			else {
				i =  worstED(per);
				solved = Bisection_i(i,solTolerance,sol,n,per);
				if (solved) {
					bn = 2;
				}
				else {
					for (i=0;i<m;i++) {
						CheckBracket(solTolerance,sol,allbracketed);
					}
				}
			}
		}
		// False position method
		if (allbracketed && useFP) {
			if (bn < 1) {
				solved = FalsePos_all(solTolerance,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++) {
						CheckBracket(solTolerance,sol,allbracketed);
					}
				}
				++bn;
			}
			else {
				i =  worstED(per);
				solved = Bisection_i(i,solTolerance,sol,n,per);
				if (solved) {
					bn = 0;
				}
				else {
					CheckBracket(solTolerance,sol,allbracketed);
				}
			}
		}
		
		// Secant method
		if (allbracketed && useSecant) {
			if (bn < 1) {
				solved = Secant_all(solTolerance,sol,n,per);
				if (!solved) {
					for (i=0;i<m;i++) {
						CheckBracket(solTolerance,sol,allbracketed);
					}
				}
				++bn;
			}
			else {
				i =  worstED(per);
				solved = Bisection_i(i,solTolerance,sol,n,per);
				if (solved) {
					bn = 0;
				}
				else {
					CheckBracket(solTolerance,sol,allbracketed);
				}
			}
		}
		
		// Use Newton-Raphson only if all markets are bracketed
		// and production is not zero.
		if (allbracketed && useNR) {
			//int maxIter = 30;
			int maxIter = 15;
			solved = Bisection_all(solTolerance,maxIter,sol,n,per);
			logfile <<",Number of iterations: n = "<<n<<"\n";
			M =  maxED(per); // Max returns largest ED[i]
			// Bisection returns ED, not log of ED
			if(!solved && M<1500) {
				solved = NewtRap(solTolerance,sol,JF,bb,n,per);
			}
			if (!solved) { 
				CheckBracket(solTolerance,sol,allbracketed);
			}
		}
		
		// Ron's version of the NR routine
		if ( allbracketed && useNR_Ron ) {
			//int maxIter = 30;
			int maxIter = 15;
			solved = Bisection_all( solTolerance, maxIter, sol, n, per );
			logfile << ",Number of iterations: n = " << n << endl;
			M =  maxED(per); // Max returns largest ED[i]
			if(!solved && M<1500) {
			//if(!solved) {
				solved = NR_Ron(solTolerance,sol,JF,bb,n,per);
				if (bugMinimal) { 
					bugoutfile << "After Ron_NR "<<n;
				}
                if (bugTracking) {
					prices_to_bugout(per);
				}
			}
			if (!solved) {
				CheckBracket(solTolerance,sol,allbracketed);
				if (bugMinimal) {
					bugoutfile << "After NR CheckB "<<n;  prices_to_bugout(per);
				}
                if (bugTracking) {
					prices_to_bugout(per);
				}
			}
		}
		
		//sdfile<<"Iteration: "<<n<<"\n"; //supply & demand info
		
		// make sure that ED, NOT Log of ED, is checked against tolerance
		ED =  showED(per);
		M =  maxED(per); // Max returns largest ED[i]
		
		// for debugging
		if (bugTracking) {
			bugoutfile<<"\nSolution() loop. N: "<< n << "\n";
			bugoutfile<<"\nMarket,X,XL,XR,ED,EDL,EDR,Tolerance\n";
			for (i=0; i<m; ++i) {
				bugoutfile<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solTolerance<<"\n";
			}
			bugout(per,n);
			sdcurves(per,n);
		}
		
	} // end do loop		
	while (M >= solTolerance && ++n < 1000);			// report sucess, 0
	
	Code = (M < solTolerance ? 0 : -1);				// or failure, -1, 
	
	if (! checkMarketSolution(  solTolerance, per ) && (Code == 0)) {
		cerr << "ERROR: Supplies and Demands are NOT equal" << endl;
	}
	
	switch (Code) {
	case 0:
		cout<< "Model solved normally:	World Calc n = " << n << endl;
		logfile<< ",Model solved normally:  World Calc n = " << n << endl;
		break;
	case -1:
		cout<< "Model did not solve within set iteration	" << n << endl;
		logfile<< ",Model did not solve within set iteration " << n << endl;
		
		logfile<<",Market,X,XL,XR,ED,EDL,EDR,Tolerance" << endl;
		for (i=0; i<m; ++i) {
			logfile<<","<<i<<","<<sol[i].X<<","<<sol[i].XL<<","<<sol[i].XR
				<<","<<sol[i].ED<<","<<sol[i].EDL<<","<<sol[i].EDR<<","<<solTolerance<< endl;
		}
		break;
	case 2:
		cout << "Original Code has not been changed" << endl;
		logfile << ",Original Code has not been changed" << endl;
		break;
	default:
		cout<< "Case for Code not found" << endl;
		logfile<< ",Case for Code not found" << endl;
		break;
	}
	
	// free up memory when done
	for( i = 0; i < m; i++ ) {
		delete[] a[ i ];
		delete[] b[ i ];
	}
	
	delete[] a;
	delete[] b;
	delete[] JF;
	delete[] bb;
}


