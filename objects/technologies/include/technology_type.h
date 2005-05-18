#ifndef _TECHNOLOGY_TYPE_H_
#define _TECHNOLOGY_TYPE_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file technology_type.h
* \ingroup Objects
* \brief The TechnologyType class header file.
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include <map>
#include <climits>
// Forward declaration
class BaseTechnology;

/*! 
* \ingroup Objects
* \brief This class groups vintages of the same type for simplification of subsector level routines.
* \author Josh Lurz
*/

class TechnologyType
{
public:
    TechnologyType();
    bool addVintage( BaseTechnology* aTech );
    double getTotalCapitalStock( const int aUpToYear = INT_MAX ) const;
    void initializeTechsFromBase( const int aTechYear );
    BaseTechnology* initOrCreateTech( const int aNewTechYear, const int aCurrTechYear );
    double setTotalInvestment( const std::string& aRegionName, const int aPrevYear, const int aCurrentYear,
                               const double aAnnualInvestment, const int aPeriod );
private:
    std::map<int, BaseTechnology*> mVintages;
    typedef std::map<int, BaseTechnology*>::const_iterator CVintageIterator;
    typedef std::map<int, BaseTechnology*>::iterator VintageIterator;
};

#endif // _TECHNOLOGY_TYPE_H_
