#ifndef _TOTAL_SECTOR_EMISSIONS_H_
#define _TOTAL_SECTOR_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file total_sector_emissions.h
* \ingroup Objects
* \brief TotalSectorEmissions header file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <string>
#include <vector>
#include "util/base/include/value.h"

class Sector;
class Tabs;

/*! \brief Sector level object responsible for calculating an aggregate
*          emissions factor for a set of sectors.
* \details This object allows a total emissions level to be read in for a
*          specified set of sectors. This is then used to calculate an average
*          emissions factor for all of the sectors based on their output in a
*          given year.
*/
class TotalSectorEmissions
{
public:
	TotalSectorEmissions();

	void XMLParse( const xercesc::DOMNode* aNode );
    
    void toInputXML( std::ostream& aOut,
                     Tabs* aTabs ) const;
    
    void toDebugXML( const int aPeriod,
                     std::ostream& aOut,
                     Tabs* aTabs ) const;

    const std::string& getName() const;

	void setAggregateEmissionFactor( const std::vector<Sector*>& aSectors,
                                     IInfo* aRegionInfo,
                                     const int aPeriod ) const;
	
    double getEmissionFactor() const;

	static const std::string& getXMLNameStatic();
	static const std::string& aggrEmissionsPrefix();
private:


    //! Type of sector that this GHG will be emitted from
	std::string mType;

    //! Name of ghg
	std::string mName;

    //! Aggregate emissions of the ghg
	Value mAggregateEmissions;
    
    //! Year in which to set aggregate emissions
	int mApplicableYear;
};

#endif // _TOTAL_SECTOR_EMISSIONS_H_

