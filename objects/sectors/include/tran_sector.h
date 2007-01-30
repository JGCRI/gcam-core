#ifndef _TRANSECTOR_H_
#define _TRANSECTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file tran_sector.h
* \ingroup Objects
* \brief The Transportation Demand Sector header file. 
* \author Sonny Kim, Josh Lurz, Steve Smith, Marshall Wise
*/

#include <vector>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/demand_sector.h"

// Forward declarations
class GDP;

/*! 
* \ingroup Objects
* \brief This class represents a detailed transportation demand sector
* \brief and is derived from the Demand Sector Class.
* \author Marshall Wise, Sonny Kim, Josh Lurz
*/
class TranSector : public DemandSector
{
public:
    explicit TranSector( const std::string& aRegionName );
    virtual void initCalc( NationalAccount* aNationalAccount,
                           const Demographic* aDemographics,
                           const int aPeriod );

    virtual void calcAggregateDemand( const GDP* aGDP,
                                      const Demographic* aDemographics,
                                      const int aPeriod );

    static const std::string& getXMLNameStatic();

    virtual void operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
        const int aPeriod ){}; // Passing demographic here is not good.
protected:
    std::vector<double> percentLicensed; //!< Percent of population licensed
    
    void checkSectorCalData( const int period );
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
private:
	static const std::string XML_NAME; //!< node name for toXML methods
};

#endif // _TRANSSECTOR_H_

