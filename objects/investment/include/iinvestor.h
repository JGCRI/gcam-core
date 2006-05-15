#ifndef _IINVESTOR_H_
#define _IINVESTOR_H_
#if defined(_MSC_VER)
#pragma once
#endif

/* 
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

/*! 
 * \file iinvestor.h
 * \ingroup Objects
 * \brief The IInvestor interface file.
 * \author Josh Lurz
 */
#include <string>

class Tabs;
class Demographic;
class IInvestable;
class NationalAccount;

/*! 
 * \ingroup Objects
 * \brief An interface to a class which controls SGM sector level investment in
 *        new technologies.
 * \details The IInvestor interface represents a sector level decision maker
 *          which controls calculating the total investment for a
 *          ProductionSector, and distributing it to the subsectors. The
 *          IInvestor will make this decision once per iteration. Each
 *          ProductionSector has it's own independent IInvestor, although the
 *          actions of the IInvestors are linked through the investment market.
 * \author Josh Lurz
 */
class IInvestor
{
public:
    IInvestor();
    virtual ~IInvestor();

    /*!
     * \brief Complete the initialization of an IInvestor.
     * \details Finishes initializes an IInvestor before it is used.
     * \param aRegionName Region containing the investor.
     * \param aSectorName Sector for which the IInvestor is determining
     *        investment.
     */
    virtual void completeInit( const std::string& aRegionName,
                               const std::string& aSectorName ) = 0;

    // TODO: Inherit and make documentation inherited.
    virtual void XMLParse( const xercesc::DOMNode* node ) = 0; 
    virtual void toDebugXML( const int period, std::ostream& out,
                             Tabs* tabs ) const = 0;
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const = 0;
    
    /*!
     * \brief Calculate a total investment level for the sector and distribute
     *        it to the subsectors of the sector.
     * \details This is the main investment method. It is called by the
     *          ProductionSector to determine the total quantity of investment
     *          for the sector and to distribute the investment to the
     *          subsectors of the ProductionSector.
     * \param aInvestables The subsectors of the ProductionSector as an
     *        IInvestable vector.
     * \param aNationalAccount Regional national accounts container.
     * \param aDemographic Regional demograpics container.
     * \param aPeriod Model period for which to calculate investment.
     */
    virtual double calcAndDistributeInvestment( std::vector<IInvestable*>& aInvestables,
                                                NationalAccount& aNationalAccount, 
                                                const Demographic* aDemographic,
                                                const int aPeriod ) = 0;
};

//! Constructor
inline IInvestor::IInvestor(){
}

//! Destructor
inline IInvestor::~IInvestor(){
}

#endif // _IINVESTOR_H_
