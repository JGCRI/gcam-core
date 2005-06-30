#ifndef _INTERM_SUBSECTOR_H_
#define _INTERM_SUBSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*!
* \file interm_subsector.h
* \ingroup CIAM
* \brief The intermittent supply subsector class header file.
* \author Marshall Wise
* \date $Date$
* \version $Revision$
*/

#include <string>
#include "sectors/include/subsector.h"


/*!
* \ingroup Objects
* \brief The derived Intermittent Supply SubSector.
* Intended for wind and solar.  Takes an intermittent resource and determines the demand for
* supply from a back-up sector, especially if needed for electricity.
* \author Marshall Wise
*/
class IntermittentSubsector: public Subsector {
public:
    IntermittentSubsector( const std::string regionName, const std::string sectorName);
    static const std::string& getXMLNameStatic();
    void completeInit(DependencyFinder* aDependencyFinder );
    void initCalc(  const MarketInfo* aSectorInfo, NationalAccount& aNationalAccount,
        Demographic* aDemographics, const MoreSectorInfo* aMoreSectorInfo, const int aPeriod );
    void calcPrice( const int period );
    void calcTechShares ( const GDP* gdp, const int period );
    void MCoutputSupplySector() const;
protected:
	//! percent of reserve capacity per unit of intermittent capacity (e.g., GW/GW)
	std::vector<double> backupCapacityFraction;
	//! reserve capacity per intermittent electricity resource output (GW/EJ)
    std::vector<double> backupCapacityPerEnergyOutput; 

    //! name of the electricity sector it will supply to
    std::string electricSectorName; 

    //  block of member variables with values passed down from Sector level in Sectorinfo object
    //! electricity reserve margin 
    double elecReserveMargin;
    //! resource backup cost in 1975 $/kW/yr   (value is and should be annualized)
    double backupCost;
    //! average capacity factor of total electric system to convert to capacity
    double aveGridCapacityFactor;
    //! Capacity factor for backup capacity
    double backupCapacityFactor; 
    //  end of block passed from Sectorinfo object

    //! ordering number of technology with main Resource input (typically 0)
    unsigned int resourceTechNumber;
    //! ordering number of technology with backupsector input (typically 1)
    unsigned int backupTechNumber; 

    const std::string& getXMLName() const;
	void calcBackupFraction(const int per); //compute demand for backup supply for intermittency
    virtual bool XMLDerivedClassParseAttr( const xercesc::DOMNode* node );
    virtual bool XMLDerivedClassParse( const std::string nodeName, const xercesc::DOMNode* curr );
    virtual void toOutputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

private:
    const static std::string XML_NAME; //!< node name for toXML methods
};



#endif // _INTERM_SUBSECTOR_H_
