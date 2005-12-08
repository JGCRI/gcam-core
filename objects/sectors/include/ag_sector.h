#ifndef _AG_SECTOR_
#define _AG_SECTOR_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file ag_sector.h
* \ingroup Objects
* \brief The AgSector class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <map>
#include "util/base/include/iround_trippable.h"

/*! 
* \ingroup Objects
* \brief A class which defines the agricultural sector of a region. 
* \author Josh Lurz
*/

class AgSector: public IRoundTrippable {

private:
    void setMarket( const std::string& regname );

    static int regionCount; //!< Tracks how many AgSectors have been instantiated.
    static const int numAgMarkets; //!<Number of internally solved ag markets.
    static bool init; //!< Whether the static data has been initialized.
    static std::map<std::string, int> nameToIndiceMap; //! Converts market name into market indice.
    static std::vector<std::string> marketNameVector; //! Contains the names of all agLu markets.
    static std::map<int, std::string> indiceToNameMap; //! Contains a mapping of indice to name.
	const static std::string XML_NAME; //!< node name for toXML methods

    std::string name; //!< Name of the agricultural sector.
    int regionNumber; //!< The region number of the container region.
    std::vector<double> gdp; //!< Contains the gnps passed to the AgLu model.
    std::vector<double> population; //!< Contains population passed to the AgLu model.
    double biomassPrice; //!< Initial market biomass price passed to the AgLu model.
    std::vector<double> CO2Emissions; //!< Co2 emissions by period returned from the AgLu model.
    std::vector< std::vector<double> > prices; //!< Market prices passed into the agLU model.
    std::vector< std::vector<double> > supplies; //!< Market supplies returned from the AgLu model.
    std::vector< std::vector<double> > demands; //!< Market demands returned from the AgLu model.
    static void staticInitialize();
    const std::string& getXMLName() const;
public:
    AgSector();
    ~AgSector();
    void XMLParse( const xercesc::DOMNode* node );
    static int getNumAgMarkets();
    void toInputXML( std::ostream& out, Tabs* tabs ) const;
    void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	static const std::string& getXMLNameStatic();
    void completeInit( const std::string& regionName );
    void setGNP( const std::vector<double>& gnpsIn );
    void setPop( const std::vector<double>& popsIn );
    void setBiomassPrice( const double bioPriceIn );
    void carbLand( const int period, const std::string& regionName );
    double getLandUseEmissions( const int aPeriod ) const;
    void runModel( const int period, const std::string& regionName );
    void initMarketPrices( const std::string& regionName, const std::vector<double>& pricesIn );
    static void internalOutput();
};
#endif // _AG_SECTOR_H_
