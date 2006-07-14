#ifndef _MORE_SECTOR_INFO_H_
#define _MORE_SECTOR_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file more_sector_info.h
* \ingroup Objects
* \brief MoreSectorInfo class header file.
* \author Sonny Kim
*/

#include <string>
#include <vector>
#include <map>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/iround_trippable.h"

/*! 
* \ingroup Objects
* \brief A container which contains additional information about an SGM sector.
* \details TODO
* \author Sonny Kim
*/

class MoreSectorInfo: public IRoundTrippable
{
public:
    enum MoreSectorInfoType {
        ENERGY_CURRENCY_CONVERSION,
        INVEST_TAX_CREDIT_RATE,
        CORP_INCOME_TAX_RATE,
        IND_BUS_TAX_RATE,
        MAX_CORP_RET_EARNINGS_RATE,
        CORP_RET_EARNINGS_RATE,
        HH_RET_EARNINGS_RATE,
        RET_EARNINGS_PARAM,
		TRANSPORTATION_COST,
		TRAN_COST_MULT,
		PRICE_ADJUST_MULT,
		PROPORTIONAL_TAX_RATE,
		ADDITIVE_TAX
    };

	MoreSectorInfo();
	void XMLParse( const xercesc::DOMNode* node );
	void toInputXML( std::ostream& out, Tabs* tabs ) const;
	void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;
	static const std::string& getXMLNameStatic();
	void reset();
	void setType( const MoreSectorInfoType aType, const double aValue );
    double getValue( const MoreSectorInfoType aType ) const;

private:
    const std::string& getXMLName() const;
    const std::string enumToName( const MoreSectorInfoType aType ) const;
    const std::string enumToXMLName( const MoreSectorInfoType aType ) const;
	static const std::string XML_NAME; //!< node name for toXML methods
	std::map<MoreSectorInfoType, double> mSectorInfoMap; //!< Map relating additional sector info by type to value
    typedef std::map<MoreSectorInfoType, double>::const_iterator CInfoTypeIterator;
};

#endif // _MORE_SECTOR_INFO_H_
