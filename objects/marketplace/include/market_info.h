#ifndef _MARKET_INFO_H_
#define _MARKET_INFO_H_
#if defined(_MSC_VER_)
#pragma once
#endif

/*! 
* \file market_info.h
* \ingroup Objects
* \brief The MarketInfo class header file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include <map>
class Tabs;

/*!
* \ingroup Objects
* \brief This class contains a set of information related to a particular market.
* \author Josh Lurz
*/

class MarketInfo
{
public:
    MarketInfo();
    void toDebugXML( std::ostream& out, Tabs* tabs ) const;
    void addItem( const std::string& aName, const double aValue );
    double getItemValue( const std::string& aName, bool aMustExist ) const;
private:
    std::map<std::string, double> mInfoMap; //!< Internal storage mapping item names to item values.
    typedef std::map<std::string, double>::iterator InfoIterator;
    typedef std::map<std::string, double>::const_iterator CInfoIterator;
};

#endif // _MARKET_INFO_H_
