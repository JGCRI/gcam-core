/* market.cpp                                   *
 * Method definition for market class.          *
 *                                              *
 *                                              *
 * SHK 9/21/00                                  */

#include <iostream>
#include <fstream>
#include <math.h>
//#include <algorithm>
#include <valarray>
//using namespace std; // enables elimination of std::
//#include <string.h>
//** Database Headers *****
#include <afxdisp.h>
#include <dbdao.h>
#include <dbdaoerr.h>

#include "market.h"
#include "str_indexname.h" // get index and name from database
#include "modeltime.h"  // model runtime info
 
extern CdbDatabase db;
extern ofstream bugoutfile,outfile,sdcurvefile,sdfile;	

extern const char *dbfile;
extern const char *dbtghg; // GHG table
extern const char *dbtdrsc; // resource table
extern const char *dbtsupsec; // supply sector table
extern const char *dbtgen;
extern bool Minicam;  // run Minicam(true) or full CGE(false)
extern CdbRecordset drscrst,drscrst_ct,suprst,suprst_ct,demrst,demrst_ct;
extern Modeltime modeltime;

const double smnum = 1e-6; // constant small number to replace for null

// market class method definition
Marketplace::Marketplace(void) // default constructor
{
}

Marketplace::~Marketplace(void) // destructor
{
	cout << "Deleting all markets...\n";

}

// set the number of all markets based on number of regional markets
// and dynamically creates an array of individual market objects
void Marketplace::setmrks(void)
{
	int count_mrks = 0, count_ctry = 0;
	int count_fuels = 0;
	string str;
	vector<int> no_ctry; // vector of number of countries in each market
	vector<int> sectorid;
	vector<string> marketname, sectorname;
	vector< vector<string> > ctryname;
	vector< vector<int> > ctryid;
	CdbRecordset rst;

	//**** Get Number of Markets from Resource Table
	str = "SELECT DISTINCT Market,Sector,SectorName,Region,RegionName FROM ";
	str += dbtdrsc;
	str += " ORDER BY Market,Sector,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and sector name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("SectorName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("Sector").intVal;
		count_ctry = 0;
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("SectorName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			//str = sectorname[count_mrks-1] + rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			//mrkmap[str] = count_mrks-1;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	nodrscmrks = count_mrks; // resource markets

	//**** Get Number of Markets from Supply Sector Table
	str = "SELECT DISTINCT Market,Sector,SectorName,Region,RegionName FROM ";
	str += dbtsupsec;
	str += " ORDER BY Market,Sector,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		// not reinitialized so count continues from Resouce Table
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and sector name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("SectorName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("Sector").intVal;
		count_ctry = 0;
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("SectorName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	nossecmrks = count_mrks - nodrscmrks; // supply sector markets


	//**** Get Number of Markets from GHG Table
	str = "SELECT DISTINCT Market,GHG,GHGName,Region,RegionName FROM ";
	str += dbtghg;
	str += " ORDER BY Market,GHG,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		// not reinitialized so count continues from Supply Sector Table
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and GHG name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("GHGName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("GHG").intVal;
		count_ctry = 0; // reset to 0 to count countries in each market
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("GHGName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	noghgmrks = count_mrks - (nossecmrks + nodrscmrks); // GHG markets

	nomrks = count_mrks; // total number of markets
	
	
	// create market objects
	int maxper = modeltime.getmaxper();
	mrk.resize(nomrks);
	for(int i=0;i<mrk.size();i++) mrk[i].resize(maxper);

	// initialize market objects with market name, region, number of countries
	// in each market, and countries names
	for (int m=0;m<maxper;m++) {
		for (int i=0;i<nomrks;i++) {
			strcpy(mrk[i][m].name, sectorname[i].c_str());
			strcpy(mrk[i][m].region, marketname[i].c_str());
			mrk[i][m].type = sectorid[i];
			mrk[i][m].year = modeltime.getstartyr() + m*modeltime.gettimestep(m); 
			if (m==0) {
				mrk[i][m].nocountry = no_ctry[i]; // number of countries
				mrk[i][m].countryid.resize(no_ctry[i]);
				for (int j=0;j<mrk[i][m].nocountry;j++) {
					mrk[i][m].countryid[j] = ctryid[i][j]; // list of countries
				}
			}
			else {
				mrk[i][m].nocountry = mrk[i][0].nocountry;
				mrk[i][m].countryid = mrk[i][0].countryid;
			}
		}
	}

	//mrk_index.resize(nomrks+1);
	//mrk_index[0].resize(20);
	for (i=0;i<nomrks;i++) {
		//if (i==0) mrk_index[i].resize(no_ctry[i]+1);
		//else if ((i > 0) && (no_ctry[i] > no_ctry[i-1]))
		//	mrkindex[i].resize(no_ctry[i]+1);
		for (int j=0;j<no_ctry[i];j++) {
			// get market index from fuel and region
			mrk_index[sectorid[i]][ctryid[i][j]] = i;
		}
	}
		
}

// create two markets for every secondary good
// set the number of all markets based on number of regional markets
// and dynamically creates an array of individual market objects
void Marketplace::setmrks2(void)
{
	int count_mrks = 0, count_ctry = 0;
	int count_fuels = 0;
	string str;
	vector<int> no_ctry; // vector of number of countries in each market
	vector<int> sectorid;
	vector<string> marketname, sectorname;
	vector< vector<string> > ctryname;
	vector< vector<int> > ctryid;
	CdbRecordset rst;

	//**** Get Number of Markets from Resource Table
	str = "SELECT DISTINCT Market,Sector,SectorName,Region,RegionName FROM ";
	str += dbtdrsc;
	str += " ORDER BY Market,Sector,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and sector name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("SectorName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("Sector").intVal;
		count_ctry = 0;
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("SectorName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	nodrscmrks = count_mrks; // resource markets

	//**** Get Number of Markets from Supply Sector Table
	str = "SELECT DISTINCT Market,Sector,SectorName,Region,RegionName FROM ";
	str += dbtsupsec;
	str += " ORDER BY Market,Sector,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		// not reinitialized so count continues from Resouce Table
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and sector name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("SectorName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("Sector").intVal;
		count_ctry = 0;
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("SectorName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	nossecmrks = count_mrks - nodrscmrks; // supply sector markets


	//**** Get Number of Markets from GHG Table
	str = "SELECT DISTINCT Market,GHG,GHGName,Region,RegionName FROM ";
	str += dbtghg;
	str += " ORDER BY Market,GHG,Region";
	// each record is distinct
	rst = db.OpenRecordset(str.c_str());

	while(!rst.GetEOF()) {
		// count number of markets
		// not reinitialized so count continues from Supply Sector Table
		count_mrks++;
		marketname.resize(count_mrks);
		sectorname.resize(count_mrks);
		sectorid.resize(count_mrks);
		ctryname.resize(count_mrks);
		ctryid.resize(count_mrks);
		// market and GHG name distinguishes each market
		marketname[count_mrks-1] = rst.GetField("Market").pcVal;
		sectorname[count_mrks-1] = rst.GetField("GHGName").pcVal;
		sectorid[count_mrks-1] = rst.GetField("GHG").intVal;
		count_ctry = 0; // reset to 0 to count countries in each market
		while(!rst.GetEOF() &&
			marketname[count_mrks-1] == rst.GetField("Market").pcVal &&
			sectorname[count_mrks-1] == rst.GetField("GHGName").pcVal) {
			// count nubmer of countries in each market
			count_ctry++;
			ctryname[count_mrks-1].resize(count_ctry);
			ctryid[count_mrks-1].resize(count_ctry);
			ctryname[count_mrks-1][count_ctry-1] = 
				rst.GetField("RegionName").pcVal;
			ctryid[count_mrks-1][count_ctry-1] = 
				rst.GetField("Region").intVal;
			rst.MoveNext();
		}
		no_ctry.resize(count_mrks);
		no_ctry[count_mrks-1] = count_ctry;
	}
	rst.Close();
	noghgmrks = count_mrks - (nossecmrks + nodrscmrks); // GHG markets

	nomrks = count_mrks; // total number of markets
	
	// number of extra markets for each secondary goods
	nomrks += nossecmrks;

	// create market objects
	int maxper = modeltime.getmaxper();
	mrk.resize(nomrks); // add extra markets
	for(int i=0;i<mrk.size();i++) mrk[i].resize(maxper);

	// initialize market objects with market name, region, number of countries
	// in each market, and countries names
	for (int m=0;m<maxper;m++) {
		for (int i=0;i<(nodrscmrks+nossecmrks);i++) {
			strcpy(mrk[i][m].name, sectorname[i].c_str());
			strcpy(mrk[i][m].region, marketname[i].c_str());
			mrk[i][m].type = sectorid[i];
			mrk[i][m].year = modeltime.getstartyr() + m*modeltime.gettimestep(m); 
			if (m==0) {
				mrk[i][m].nocountry = no_ctry[i]; // number of countries
				mrk[i][m].countryid.resize(no_ctry[i]);
				for (int j=0;j<mrk[i][m].nocountry;j++) {
					mrk[i][m].countryid[j] = ctryid[i][j]; // list of countries
				}
			}
			else {
				mrk[i][m].nocountry = mrk[i][0].nocountry;
				mrk[i][m].countryid = mrk[i][0].countryid;
			}
		}
		// copy info to extra secondary markets
		for (i=(nodrscmrks+nossecmrks);i<(nomrks-noghgmrks);i++) {
			int i2 = i - nossecmrks; // index of original supply sector
			strcpy(mrk[i][m].name, sectorname[i2].c_str());
			strcpy(mrk[i][m].region, marketname[i2].c_str());
			mrk[i][m].type = sectorid[i2];
			mrk[i][m].year = modeltime.getstartyr() + m*modeltime.gettimestep(m); 
			if (m==0) {
				mrk[i][m].nocountry = no_ctry[i2]; // number of countries
				mrk[i][m].countryid.resize(no_ctry[i2]);
				for (int j=0;j<mrk[i][m].nocountry;j++) {
					mrk[i][m].countryid[j] = ctryid[i2][j]; // list of countries
				}
			}
			else {
				mrk[i][m].nocountry = mrk[i][0].nocountry;
				mrk[i][m].countryid = mrk[i][0].countryid;
			}
		} // end copy info for extra secondary markes
		// ghg markets
		for (i=(nomrks-noghgmrks);i<nomrks;i++) {
			int i2 = i - nossecmrks;
			strcpy(mrk[i][m].name, sectorname[i2].c_str());
			strcpy(mrk[i][m].region, marketname[i2].c_str());
			mrk[i][m].type = sectorid[i2];
			mrk[i][m].year = modeltime.getstartyr() + m*modeltime.gettimestep(m); 
			if (m==0) {
				mrk[i][m].nocountry = no_ctry[i2]; // number of countries
				mrk[i][m].countryid.resize(no_ctry[i2]);
				for (int j=0;j<mrk[i][m].nocountry;j++) {
					mrk[i][m].countryid[j] = ctryid[i2][j]; // list of countries
				}
			}
			else {
				mrk[i][m].nocountry = mrk[i][0].nocountry;
				mrk[i][m].countryid = mrk[i][0].countryid;
			}
		} 
	}

	for (i=0;i<(nodrscmrks+nossecmrks);i++) {
		for (int j=0;j<no_ctry[i];j++) {
			mrk_index[sectorid[i]][ctryid[i][j]] = i;
		}
	}
	for (i=(nodrscmrks+nossecmrks);i<(nodrscmrks+nossecmrks+noghgmrks);i++) {
		int i2=i-nossecmrks;
		for (int j=0;j<no_ctry[i2];j++) {
			mrk_index[sectorid[i2]][ctryid[i2][j]] = i;
		}
	}
/*	for (i=(nomrks-noghgmrks);i<nomrks;i++) {
		int i2=i-nossecmrks;
		for (int j=0;j<no_ctry[i2];j++) {
			mrk_index[sectorid[i2]][ctryid[i2][j]] = i;
		}
	}
*/	
}

// initializes all markets
void Marketplace::initprices(void)
{
	// function protocol
	void dbmodelread(double *temp,string region,string var1name,string var2name);
	// variable declaration
	double tmpval[1];
	int maxper = modeltime.getmaxper();

	// reads in market prices for primary fuels
	// setmrks() created names already
	for (int i=0;i<nodrscmrks;i++) {
		dbmodelread(tmpval,"USA","price",mrk[i][0].name);
		mrk[i][0].price = tmpval[0]; // set base year prices to all periods
	}
	// initialize the rest of the markets to 1 (supply sectors and ghg markets)
	for (i=nodrscmrks;i<nomrks;i++) {
		mrk[i][0].price = 1;
	}
	// overrides ghg markets to 0
	// initialize ghg prices to zero if ghg markets exist
	for (i=0;i<noghgmrks;i++) {
		int j = nomrks - i - 1;
		for (int m=0;m<maxper;m++) 
			mrk[j][m].price = 0;
	}
}

// initialize all market prices to 0
void Marketplace::nullprc(int per) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].price = 0.0;
}

// initialize all market demands to 0
void Marketplace::nulldem(int per)
{
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].demand = 0.0;
		mrk[i][per].demand_good = 0.0;
	}
}

// set one market demand to 0
void Marketplace::nulldem_imrk(int good_id, int country_id, int per)
{
	int i = mrk_index[good_id][country_id];
	mrk[i][per].demand = 0.0;
}

// initialize all market supplies to 0
void Marketplace::nullsup(int per)
{
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].supply = 0.0;
		mrk[i][per].supply_good = 0.0;
	}
}

// set one market supply to 0
void Marketplace::nullsup_imrk(int good_id, int country_id, int per)
{
	int i = mrk_index[good_id][country_id];
	mrk[i][per].demand = 0.0;
}

// initialize market price to 0
int Marketplace::shownoghgmrks(void) 
{
	return noghgmrks;
}
 
// initialize market price to 0
void Marketplace::showmrks(int per) 
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
 
// set market price
void Marketplace::setprice(int good_id, int country_id, double value, int per)
{
	// sets market price
	int i = mrk_index[good_id][country_id];
	mrk[i][per].price = value;
}

// set market price
void Marketplace::setprice_d(int good_id, int country_id, double value, int per)
{
	// sets market price
	int i = mrk_index[good_id][country_id]+nossecmrks+noghgmrks;
	mrk[i][per].price = value;
}

// set market supply to used for solution mechanism
void Marketplace::setsupply(int good_id, int country_id, double value, int per) 
{
	// sets market supply
	int i = mrk_index[good_id][country_id];
	mrk[i][per].supply += value; // note increment operator

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// set market supply of goods and services
void Marketplace::setsupply_good(int good_id, int country_id, double value, int per) 
{
	// sets market supply
	int i = mrk_index[good_id][country_id];
	mrk[i][per].supply_good += value; // note increment operator

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// override market supply to used for solution mechanism
void Marketplace::override_supply(int good_id, int country_id, double value, int per) 
{
	// sets market supply
	int i = mrk_index[good_id][country_id];
	mrk[i][per].supply = value;

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// override market supply to used for solution mechanism
void Marketplace::override_supply_d(int good_id, int country_id, double value, int per) 
{
	// sets market supply
	int i = mrk_index[good_id][country_id]+nossecmrks+noghgmrks;
	mrk[i][per].supply = value;

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// set market demand to used for solution mechanism
void Marketplace::setdemand(int good_id, int country_id, double value, int per) 
{
	// sets market demand
	// good_id = 0 represents renewables and does not contribute to fuel demand
	if (good_id != 0) {
		int i = mrk_index[good_id][country_id];
		mrk[i][per].demand += value; // note += operator adds to previous demand
		//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
	}
}

// set market demand of goods and services
void Marketplace::setdemand_good(int good_id, int country_id, double value, int per) 
{
	// sets market demand
	// good_id = 0 represents renewables and does not contribute to fuel demand
	if (good_id != 0) {
		int i = mrk_index[good_id][country_id];
		mrk[i][per].demand_good += value; // note += operator adds to previous demand
		//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
	}
}

// override market demand to used for solution mechanism
void Marketplace::override_demand(int good_id, int country_id, double value, int per) 
{
	// sets market demand
	int i = mrk_index[good_id][country_id];
	mrk[i][per].demand = value;

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// override market demand to used for solution mechanism
void Marketplace::override_demand_d(int good_id, int country_id, double value, int per) 
{
	// sets market demand
	int i = mrk_index[good_id][country_id]+nossecmrks+noghgmrks;
	mrk[i][per].demand = value;

	//sdfile<<mrk[i][per].name<<","<<value<<","; //supply & demand info
}

// return market price
double Marketplace::showprice(int good_id, int country_id, int per) 
{
	// returns market price
	// good_id = 0 represents renewables and does not have fuel costs
	if (good_id == 0)
		return 0;
	else {
		int i = mrk_index[good_id][country_id];
		return mrk[i][per].price;
	}
}

// return market price
double Marketplace::showprice_d(int good_id, int country_id, int per) 
{
	// returns market price
	// good_id = 0 represents renewables and does not have fuel costs
	if (good_id == 0)
		return 0;
	else {
		int i = mrk_index[good_id][country_id]+nossecmrks+noghgmrks;
		return mrk[i][per].price;
	}
}

// return market supply used for solution mechanism
double Marketplace::showsupply(int good_id, int country_id, int per) 
{
	// returns market supply
	// else return a value of 9999 for the supply
	int i = mrk_index[good_id][country_id];
	return mrk[i][per].supply;
}

// return market supply of goods and services
double Marketplace::showsupply_good(int good_id, int country_id, int per) 
{
	// returns market supply
	// else return a value of 9999 for the supply
	int i = mrk_index[good_id][country_id];
	return mrk[i][per].supply_good;
}

// return market demand used for solution mechanism
double Marketplace::showdemand(int good_id, int country_id, int per)
{
	// returns market demand
	// else return a value of 9999 for the demand
	int i = mrk_index[good_id][country_id];
	return mrk[i][per].demand;
}

// return market demand of goods and services
double Marketplace::showdemand_good(int good_id, int country_id, int per)
{
	// returns market demand
	// else return a value of 9999 for the demand
	int i = mrk_index[good_id][country_id];
	return mrk[i][per].demand_good;
}

// calculates excess demand for all markets
void Marketplace::excessdemand(int per) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].exdmd = mrk[i][per].demand - mrk[i][per].supply;
}

// calculates log of excess demand for all markets
void Marketplace::logED(int per) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].lexdmd = log10(_MAX(mrk[i][per].demand,smnum)) 
						   - log10(_MAX(mrk[i][per].supply,smnum));
}

// calculates log of demand for all markets
void Marketplace::logDem(int per) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].ldem = log(_MAX(mrk[i][per].demand,smnum));
}

// calculates log of supply for all markets
void Marketplace::logSup(int per) 
{
	for (int i=0;i<nomrks;i++)
		mrk[i][per].lsup = log(_MAX(mrk[i][per].supply,smnum));
}

// set markets to solve
int Marketplace::setmrks_sol(int per, double tol)
{
	int i=0, j=0;

	// initialize nomrks_t to 0
	nomrks_t = 0;
	mrk_isol.resize(nomrks);
	// check all markets and store the number and index of
	// markets that require solving

	if (Minicam) {
		// solve primary resource market only
		for (i=0;i<nodrscmrks;i++) { 
			// abs is integer absolute value
			// fabs is double absolute value
			//if ((mrk[i][per].demand > smnum) && (fabs(mrk[i][per].exdmd) > tol)) {
			if (mrk[i][per].demand > smnum) {
				nomrks_t++; // count markets that need solving
				mrk_isol[j++] = i; // set index for markets
			}
		}

		// ghg markets to solve
		// ghg marekts added at end of all markets
		for (i=(nomrks-noghgmrks);i<nomrks;i++) { 
			// demand is emission for ghg markets (demand = emission)
			// supply is constraint for ghg markets (supply = constraint)
			// ghg market not added to markets to solve if emission = 0
			// ghg market not added to markets to solve if supply < 0
			if (mrk[i][per].demand > smnum && mrk[i][per].supply >= 0) {
				nomrks_t++; // count markets that need solving
				mrk_isol[j++] = i; // set index for markets
			}
		}

	}
	else {
		// solve all goods, primary and intermediate
		for (i=0;i<(nomrks-noghgmrks);i++) { 
			// abs is integer absolute value
			// fabs is double absolute value
			//if ((mrk[i][per].demand > smnum) && (fabs(mrk[i][per].exdmd) > tol)) {
			if (mrk[i][per].demand > smnum) {
				nomrks_t++; // count markets that need solving
				mrk_isol[j++] = i; // set index for markets
			}
		}
		// ghg markets to solve
		// ghg marekts added at end of all markets
		for (i=(nomrks-noghgmrks);i<nomrks;i++) { 
			// demand is emission for ghg markets (demand = emission)
			// supply is constraint for ghg markets (supply = constraint)
			// ghg market not added to markets to solve if emission = 0
			// ghg market not added to markets to solve if supply < 0
			if (mrk[i][per].demand > smnum && mrk[i][per].supply >= 0) {
				nomrks_t++; // count markets that need solving
				mrk_isol[j++] = i; // set index for markets
			}
		}
	}

	return nomrks_t;
}

// set markets to solve
int Marketplace::setmrks_sol_NR(int per, double tol)
{
	int i=0, j=0;

	// initialize nomrks_t to 0
	nomrks_t_NR = 0;
	mrk_isol_NR.resize(nomrks);
	// check all markets and store the number and index of
	// markets that require solving

	if (Minicam) {
		// solve primary resource market only
		for (i=0;i<nodrscmrks;i++) { 
			// abs is integer absolute value
			// fabs is double absolute value
			//if ((mrk[i][per].demand > smnum) && (fabs(mrk[i][per].exdmd) > tol)) {
			if (mrk[i][per].demand > smnum) {
				nomrks_t_NR++; // count markets that need solving
				mrk_isol_NR[j++] = i; // set index for markets
			}
		}

		// ghg markets to solve
		// ghg marekts added at end of all markets
		for (i=(nomrks-noghgmrks);i<nomrks;i++) { 
			// demand is emission for ghg markets (demand = emission)
			// supply is constraint for ghg markets (supply = constraint)
			// ghg market not added to markets to solve if emission = 0
			// ghg market not added to markets to solve if supply < 0
			if (mrk[i][per].demand > smnum && mrk[i][per].supply >= 0) {
				if (mrk[i][per].price < smnum && mrk[i][per].exdmd < 0) ;
				else if (mrk[i][per].price == 0) ;
				else {
					nomrks_t_NR++; // count markets that need solving
					mrk_isol_NR[j++] = i; // set index for markets
				}
			}
		}

	}
	else {
		// solve all goods, primary and intermediate
		for (i=0;i<(nomrks-noghgmrks);i++) { 
			// abs is integer absolute value
			// fabs is double absolute value
			//if ((mrk[i][per].demand > smnum) && (fabs(mrk[i][per].exdmd) > tol)) {
			if (mrk[i][per].demand > smnum) {
				nomrks_t_NR++; // count markets that need solving
				mrk_isol_NR[j++] = i; // set index for markets
			}
		}
		// ghg markets to solve
		// ghg marekts added at end of all markets
		for (i=(nomrks-noghgmrks);i<nomrks;i++) { 
			// demand is emission for ghg markets (demand = emission)
			// supply is constraint for ghg markets (supply = constraint)
			// ghg market not added to markets to solve if emission = 0
			// ghg market not added to markets to solve if supply < 0
			if (mrk[i][per].demand > smnum && mrk[i][per].supply >= 0) {
				if (mrk[i][per].price < smnum && mrk[i][per].exdmd < 0) ;
				else if (mrk[i][per].price == 0) ;
				else {
					nomrks_t_NR++; // count markets that need solving
					mrk_isol_NR[j++] = i; // set index for markets
				}
			}
		}
	}

	return nomrks_t_NR;
}

// returns market index that requires solving
int Marketplace::showmrk_sol(int id)
{
	return 	mrk_isol[id];
}
				
// checks if supply > 0 for all markets
bool Marketplace::checkprod(int per) 
{
	for (int i=0;i<nomrks_t;i++)
		if ((mrk[i][per].supply == 0) ||
			(mrk[i][per].demand == 0))
			return false; // breaks out as soon as true
	return true;
}

// return market with largest excess demand
int Marketplace::worstED(int per) 
{
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

// returns largest excess demand
double Marketplace::maxED(int per) 
{
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

// set new solution prices for all markets
void Marketplace::setPRC(vector<double> prices,int per)
{
	// set prices only for markets in price vector
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		mrk[j][per].price = prices[i];
	}
}

// set new solution prices for all markets in Newton-Rhapson
void Marketplace::setPRC_NR(vector<double> prices,int per)
{
	// set prices only for markets in price vector
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		mrk[j][per].price = prices[i];
	}
}

// Use last period demand, supply and price
// as starting point for next period
void Marketplace::init_to_last(int per)
{
	// only after the starting period
	if (per > 0)
		for (int i=0;i<nomrks;i++) {
			//mrk[i][per].demand = mrk[i][per-1].demand;
			//mrk[i][per].supply = mrk[i][per-1].supply;
			mrk[i][per].price = mrk[i][per-1].price;
		}
}

// Use last period demand, supply and price
// as starting point for next period
void Marketplace::storeto_last(int per)
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
void Marketplace::storeinfo(int per)
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

// Store original demand, supply and price
// Used for calculation of derivative
void Marketplace::storeinfo(int per)
{
	// store only for markets that need solving
	//for (int i=0;i<nomrks_t;i++) {
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].tdemand = mrk[i][per].demand;
		mrk[i][per].tsupply = mrk[i][per].supply;
		mrk[i][per].tprice = mrk[i][per].price;
	}
}

// Restore original demand, supply and price
// Used for calculation of derivative
void Marketplace::restoreinfo(int per)
{
	// store only for markets that need solving
	//for (int i=0;i<nomrks_t;i++) {
	for (int i=0;i<nomrks;i++) {
		mrk[i][per].demand = mrk[i][per].tdemand;
		mrk[i][per].supply = mrk[i][per].tsupply;
		mrk[i][per].price = mrk[i][per].tprice;
	}
}

// Restore original demand, supply and price
// Used for calculation of derivative
void Marketplace::restoreprc(int per)
{
	// store only for markets that need solving
	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		mrk[j][per].price = mrk[j][per].tprice;
	}
}

// Restore original demand, supply and price
// Used for calculation of derivative
void Marketplace::restoreprc_NR(int per)
{
	// store only for markets that need solving
	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		mrk[j][per].price = mrk[j][per].tprice;
	}
}

// returns vector of market prices
vector<double> Marketplace::showPRC(int per) 
{
	vector<double> prices(nomrks_t);

	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		prices[i] = mrk[j][per].price;
	}
	return prices;
}

// returns vector of market prices
vector<double> Marketplace::showPRC_NR(int per) 
{
	vector<double> prices(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		prices[i] = mrk[j][per].price;
	}
	return prices;
}

// returns vector of market excess demands
vector<double> Marketplace::showED(int per) 
{
	vector<double> ED(nomrks_t);

	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		ED[i] = mrk[j][per].exdmd;
	}

	return ED;
}

// returns vector of market excess demands
vector<double> Marketplace::showED_NR(int per) 
{
	vector<double> ED(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		ED[i] = mrk[j][per].exdmd;
	}

	return ED;
}

// returns vector of log of market excess demands
vector<double> Marketplace::showlogED(int per) 
{
	vector<double> ED(nomrks_t);

	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		ED[i] = mrk[j][per].lexdmd;
	}

	return ED;
}

// returns vector of log of market excess demands
vector<double> Marketplace::showlogED_NR(int per) 
{
	vector<double> ED(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		ED[i] = mrk[j][per].lexdmd;
	}

	return ED;
}

// returns vector of log of market demands
vector<double> Marketplace::showlogDem(int per) 
{
	vector<double> Dem(nomrks_t);

	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		Dem[i] = mrk[j][per].ldem;
	}

	return Dem;
}

// returns vector of log of market demands
vector<double> Marketplace::showlogDem_NR(int per) 
{
	vector<double> Dem(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		Dem[i] = mrk[j][per].ldem;
	}

	return Dem;
}

// returns vector of log of market supplys
vector<double> Marketplace::showlogSup(int per) 
{
	vector<double> Sup(nomrks_t);

	for (int i=0;i<nomrks_t;i++) {
		int j = mrk_isol[i];	// look up index
		Sup[i] = mrk[j][per].lsup;
	}

	return Sup;
}

// returns vector of log of market supplys
vector<double> Marketplace::showlogSup_NR(int per) 
{
	vector<double> Sup(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		Sup[i] = mrk[j][per].lsup;
	}

	return Sup;
}

// Calculate the derivatives, elasticities or Jacobian
vector<double> Marketplace::jacobian(int k, int per)
{
	double ddemand,dsupply,dprice;
	vector<double> JFD(nomrks_t_NR);

	for (int i=0;i<nomrks_t_NR;i++) {
		int j = mrk_isol_NR[i];	// look up index
		if(mrk[j][per].tdemand == 0) 
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

// Calculate demand elasticities
vector<double> Marketplace::dem_elas(int k, int per)
{
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

// Calculate demand elasticities
vector<double> Marketplace::dem_elas_NR(int k, int per)
{
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

// Calculate supply elasticities
vector<double> Marketplace::sup_elas(int k, int per)
{
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

// Calculate supply elasticities
vector<double> Marketplace::sup_elas_NR(int k, int per)
{
	double dsupply,dprice;
	vector<double> JFS(nomrks_t_NR);

	int kk = mrk_isol_NR[k]; // market index 

	for (int i=0;i<nomrks_t_NR;i++) {
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

// write out market info to database
void Marketplace::outputdb(void)
{
	// function protocol
	void dboutput2(string varreg,string var1name,string var2name,string var3name,
			  string var4name,vector<double> dout,string uname);

	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// Function arguments are variable name, double array, db name, and
	// table name.
	// The function writes all years.

	// write market prices and supply (or demand)
	for (int i=0;i<nomrks;i++) {
		for (int m=0;m<maxper;m++)
			temp[m] = mrk[i][m].price;
		//dboutput2(99,mrk[i][0].region,"market"," ",mrk[i][0].name,"price",temp,"$/GJ");
		dboutput2(mrk[i][0].region,"market",mrk[i][0].name," ","price",temp,"$/GJ");
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].supply;
		//dboutput2(99,mrk[i][0].region,"market"," ",mrk[i][0].name,"supply",temp,"EJ");
		dboutput2(mrk[i][0].region,"market",mrk[i][0].name," ","supply",temp,"EJ");
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].demand;
		//dboutput2(99,mrk[i][0].region,"market"," ",mrk[i][0].name,"demand",temp,"EJ");
		dboutput2(mrk[i][0].region,"market",mrk[i][0].name," ","demand",temp,"EJ");
	}
}

// write out market info to database
void Marketplace::MCoutput(void)
{
	// function protocol
	void dboutput4(string var1name,string var2name,string var3name,string var4name,
			   string uname,vector<double> dout);

	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// write market prices, supply and demand
	for (int i=0;i<nomrks;i++) {
		for (int m=0;m<maxper;m++)
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

// write out market info to file
void Marketplace::outputfile(void)
{
	// function protocol
	void fileoutput2(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,vector<double> dout,string uname);
	void fileoutput3(int regno,string var1name,string var2name,string var3name,
				  string var4name,string var5name,string uname,vector<double> dout);
	
	int maxper = modeltime.getmaxper();
	vector<double> temp(maxper);
	
	// Function arguments are variable name, double array, db name, and
	// table name.
	// The function writes all years.

	// write market prices and supply (or demand)
	for (int i=0;i<nomrks;i++) {
		for (int m=0;m<maxper;m++)
			temp[m] = mrk[i][m].price;
		fileoutput3(99,mrk[i][0].region,"market",mrk[i][0].name," ","price","$/GJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].supply;
		fileoutput3(99,mrk[i][0].region,"market",mrk[i][0].name," ","supply","EJ",temp);
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].demand;
		fileoutput3(99,mrk[i][0].region,"market",mrk[i][0].name," ","demand","EJ",temp);
	/*	for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].supply_good;
		fileoutput2(99,mrk[i][0].region,"market"," ",mrk[i][0].name,"supply_good",temp,"EJ");
		for (m=0;m<maxper;m++)
			temp[m] = mrk[i][m].demand_good;
		fileoutput2(99,mrk[i][0].region,"market"," ",mrk[i][0].name,"demand_good",temp,"EJ"); */
	}
}

// write out market info to file for debugging
void Marketplace::bugout(int per,int iter)
{
	// write market prices and supply (or demand)
	bugoutfile<<"Period,"<< per <<",Nth Iteration:,"<<iter<<"\n";
	for (int i=0;i<nomrks;i++) {
		bugoutfile<<"market:,"<<i<<","<<mrk[i][0].name<<",price:,"<<mrk[i][per].price<<",$/GJ,";
		bugoutfile<<"supply:,"<<mrk[i][per].supply<<",EJ,demand:,"<<mrk[i][per].demand<<",EJ\n";
	}
	bugoutfile<<"\n";
}

// write out market price supply and demand info to file for debugging
void Marketplace::sdcurves(int per,int iter)
{
	// write market prices and supply (or demand)
	for (int i=0;i<nomrks;i++) {
		sdcurvefile<<i<<","<<mrk[i][0].name<<","<<mrk[i][per].price<<",";
		sdcurvefile<<mrk[i][per].supply<<","<<mrk[i][per].demand<<",";
	}
	sdcurvefile<<"\n";
}
