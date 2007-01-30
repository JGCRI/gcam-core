#ifndef _GLOBAL_TECHNOLOGY_H_
#define _GLOBAL_TECHNOLOGY_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
* \file global_technology.h
* \ingroup Objects
* \brief GlobalTechnology header file.
* \author Pralit Patel
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/itechnology_info.h"

// Forward declarations
class Tabs;

/*! 
* \ingroup Objects
* \brief This class holds information that is shared between all technologies.
*
* \details Holds common information for technologies, only one instance of this class
*          will exist for all technologies that request it.  This will help reduce 
*          memory and input size.  Note that this class is immutable, the set methods will
*          all fail.  The GlobalTechnology is read from XML conforming 
*          to the following specification:
*
*          <b>XML specification for GlobalTechnology</b>
*          - XML name: \c globalTechnology
*          - Contained by: GlobalTechnologyDatabase
*          - Parsing inherited from class: None
*          - Attributes:
*              - \c name GlobalTechnology::name
*              - \c year GlobalTechnology::year
*          - Elements (All optional):
*              - \c fuelname GlobalTechnologyDatase::fuelname
*              - \c efficiency GlobalTechnologyDatase::mBaseEfficiency
*              - \c nonenergycost GlobalTechnologyDatase::mBaseNonEnergyCost
*              - \c efficiencyPenalty GlobalTechnologyDatase::effPenalty
*              - \c neCostPenalty GlobalTechnologyDatase::neCostPenalty
*              - \c fMultiplier GlobalTechnologyDatase::fMultiplier
*              - \c fuelprefElasticity GlobalTechnologyDatase::fuelPrefElasticity
* \author Pralit Patel
*/
class GlobalTechnology : public ITechnologyInfo
{
public:
    GlobalTechnology();

    virtual ITechnologyInfo* clone();

    virtual void completeInit();
    
    virtual void XMLParse( const xercesc::DOMNode* tempnode ); // initialize technology with xml data

    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

    static const std::string& getXMLNameStatic();
    static const std::string parseIdentifier( const xercesc::DOMNode* tempnode );

    const int getYear() const;

    // getters and setters, setters do nothing here
    virtual const std::string& getName() const;
    virtual const std::string& getFuelName() const;
    virtual const double getEfficiency() const;
    virtual const double getNonEnergyCost() const;

    virtual const double getFMultiplier() const;
    virtual const double getFuelPrefElasticity() const;

    virtual void setFuelName( const std::string& aFuelName );
    virtual void setEfficiency( const double aEfficiency );
    virtual void setNonEnergyCost( const double aNonEnergyCost );

    virtual void setFMultiplier( const double aFMultiplier );
    virtual void setFuelPrefElasticity( const double aFuelPrefElasticity );

protected:
    int year;
    std::string name; //!< technology name
    std::string fuelname; //!< name of fuel used
    
    //! Base read-in efficiency.
    double mBaseEfficiency;

    //! Base non-fuel costs read in(levelized).
    double mBaseNonEnergyCost;

    double fMultiplier; //!< multiplier on fuel cost or price
    double fuelPrefElasticity; //!< Fuel preference elasticity

};
#endif // _GLOBAL_TECHNOLOGY_H_
