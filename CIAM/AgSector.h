#ifndef _AGSECTOR_
#define _AGSECTOR_
#pragma once

/*! 
* \file AgSector.h
* \ingroup CIAM
* \brief The AgSector class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOM.hpp>
#include <iostream>
#include <vector>
#include <string>
#include <map>

using namespace std;
using namespace xercesc;

/*! 
* \ingroup CIAM
* \brief A class which defines the agricultural sector of a region. 
* \author Josh Lurz
*/

class AgSector {
	
private:
	static int regionCount; //!< Tracks how many AgSectors have been instantiated.
	static const int numAgMarkets; //!<Number of internally solved ag markets.
	static bool init; //!< Whether the static data has been initialized.
	static map<string, int> nameToIndiceMap; //! Converts market name into market indice.
	static vector<string> marketNameVector; //! Contains the names of all agLu markets.
	static map<int, string> indiceToNameMap; //! Contains a mapping of indice to name.
	
	string name; //!< Name of the agricultural sector.
	int regionNumber; //!< The region number of the container region.
	vector<double> gnp; //!< Contains the gnps passed to the AgLu model.
	vector<double> population; //!< Contains population passed to the AgLu model.
	double biomassPrice; //! Initial market biomass price passed to the AgLu model.
	vector<double> CO2Emissions; //! Co2 emissions by period returned from the AgLu model.
	vector< vector<double> > prices; //! Market prices passed into the agLU model.
	vector< vector<double> > supplies; //! Market supplies returned from the AgLu model.
	vector< vector<double> > demands; //! Market demands returned from the AgLu model.
	static void staticInitialize();
public:
	AgSector();
	void clear();
	void XMLParse( const DOMNode* node );
	static int getNumAgMarkets();
	void toXML( ostream& out ) const;
	void toDebugXML( const int period, ostream& out ) const;
	void setGNP( const vector<double>& gnpsIn );
	void setPop( const vector<double>& popsIn );
	void setBiomassPrice( const double bioPriceIn );
	void carbLand( const int period, const string& regionName );
	void runModel( const int period, const string& regionName );
	void setMarket( const string& regname ); // creates markets
	void initMarketPrices( const string& regionName, const vector<double>& pricesIn );
	static void internalOutput();
	static void transposeArray( double array[][14], int dimension1, int dimension2 );
};
#endif