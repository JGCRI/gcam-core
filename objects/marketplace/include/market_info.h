#ifndef _MARKET_INFO_H_
#define _MARKET_INFO_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file market_info.h
* \ingroup CIAM
* \brief The MarketInfo class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>

/*!
* \ingroup CIAM
* \brief This class contains a set of information related to a particular market.
* \author Josh Lurz
*/

class MarketInfo
{
public:
    MarketInfo();
    ~MarketInfo();
    void toDebugXML( std::ostream& out ) const;
    void addItem( const std::string& itemName, const double itemValue );
    double getItemValue( const std::string& itemName ) const;
private:
    std::map<std::string,double> infoMap; //!< Internal storage mapping item names to item values.
};

#endif // _MARKET_INFO_H_