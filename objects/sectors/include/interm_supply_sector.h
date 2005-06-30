#ifndef _INTERM_SUPPLY_SECTOR_H_
#define _INTERM_SUPPLY_SECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file interm_supply_sector.h
* \ingroup Objects
* \brief The Intermittent SupplySector class header file.
*  Intended for computing and demanding a backup supply sector to
*  add to the supply of an intermittent resource
*
* \author Marshall Wise
* \date $Date$
* \version $Revision$
*/
#include <string>
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"

/*!
* \ingroup Objects
* \brief The derived Intermittent Resource Supply Sector.
* Intended for wind and solar.  Takes an intermittent resource and determines the demand for
* supply from a back-up sector, especially if needed for electricity.
* \author Marshall Wise
*/
class IntermittentSupplySector: public SupplySector
{
public:
    IntermittentSupplySector ( const std::string regionNameIn );
    static const std::string& getXMLNameStatic();
    void initCalc( const int period, const MarketInfo* aRegionInfo,
                           NationalAccount& nationalAccount, Demographic* aDemographics );
protected:
    bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node );
    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;  
    void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    const std::string& getXMLName() const;
    //! electricity reserve margin.  For regional electricity sector.
    double elecReserveMargin;
    //! resource backup cost in 1975 $/kW/yr   (value is and should be annualized)
    double backupCost;
    //! average capacity factor of total electric system to convert to total grid capacity
    double aveGridCapacityFactor;
    //! Capacity factor for backup capacity (to convert energy output to capacity
    double backupCapacityFactor; 
private:
    const static std::string XML_NAME; //!< node name for toXML methods
};




#endif // _INTERM_SUPPLY_SECTOR_H_
