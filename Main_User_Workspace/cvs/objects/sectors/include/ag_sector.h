#ifndef _AG_SECTOR_
#define _AG_SECTOR_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
 */


/*! 
* \file ag_sector.h
* \ingroup Objects
* \brief The AgSector class header file.
* \author Josh Lurz
*/

#include <xercesc/dom/DOMNode.hpp>
#include <vector>
#include <map>
#include "util/base/include/iround_trippable.h"
#include "util/base/include/ivisitable.h"

/*! 
* \ingroup Objects
* \brief A class which defines the agricultural sector of a region. 
* \author Josh Lurz
*/

class AgSector: public IRoundTrippable,
                public IVisitable 
{
private:
    void setMarket( const std::string& regname );

    static int regionCount; //!< Tracks how many AgSectors have been instantiated.
    static const int numAgMarkets; //!<Number of internally solved ag markets.
    static bool init; //!< Whether the static data has been initialized.
    static std::map<std::string, int> nameToIndiceMap; //! Converts market name into market index.
    static std::vector<std::string> marketNameVector; //! Contains the names of all agLu markets.
    static std::map<int, std::string> indiceToNameMap; //! Contains a mapping of index to name.
	const static std::string XML_NAME; //!< node name for toXML methods

    std::string name; //!< Name of the agricultural sector.
    int regionNumber; //!< The region number of the container region.
    std::vector<double> gdp; //!< Contains the gnps passed to the AgLu model.
    std::vector<double> population; //!< Contains population passed to the AgLu model.
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
    void carbLand( const int period, const std::string& regionName );
    double getLandUseEmissions( const int aPeriod ) const;
    void runModel( const int period, const std::string& regionName );
    void initMarketPrices( const std::string& regionName, const std::vector<double>& pricesIn );
    virtual void accept( IVisitor* aVisitor, const int aPeriod ) const;
    static void internalOutput();
};
#endif // _AG_SECTOR_H_
