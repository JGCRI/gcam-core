#ifndef _GENERIC_TECHNOLOGY_INFO_H_
#define _GENERIC_TECHNOLOGY_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
* \file generic_technology_info.h
* \ingroup Objects
* \brief GenericTechnologyInfo header file.
* \author Pralit Patel
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "technologies/include/itechnology_info.h"

// Forward declarations
class Tabs;

/*! 
* \ingroup Objects
* \brief This class holds information that should not change for a technology.
*
* Holds information that would not change however differers between regions or
* periods.  If this data won't differ consider using GlobalTechnology.
*
* \author Pralit Patel
*/
class GenericTechnologyInfo : public ITechnologyInfo
{
public:
    GenericTechnologyInfo( const std::string& aName );

    virtual ITechnologyInfo* clone();

    virtual void completeInit();

    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const;

    // getters   
    virtual const std::string& getName() const;
    virtual const std::string& getFuelName() const;
    virtual const double getEfficiency() const;
    virtual const double getNonEnergyCost() const;
    virtual const double getEffPenalty() const;
    virtual const double getNECostPenalty() const;
    virtual const double getFMultiplier() const;
    virtual const double getFuelPrefElasticity() const;

    // setters
    virtual void setFuelName( const std::string& aFuelName );
    virtual void setEfficiency( const double aEfficiency );
    virtual void setNonEnergyCost( const double aNonEnergyCost );
    virtual void setEffPenalty( const double aEffPenalty );
    virtual void setNECostPenalty( const double aNECostPenalty );
    virtual void setFMultiplier( const double aFMultiplier );
    virtual void setFuelPrefElasticity( const double aFuelPrefElasticity );

protected:
    std::string name; //!< technology name
    std::string fuelname; //!< name of fuel used
    
    //! Base read-in efficiency.
    double mBaseEfficiency;

    //! Base non-fuel costs read in(levelized).
    double mBaseNonEnergyCost;
    
    double effPenalty; //!< energy efficiency penalty

    double neCostPenalty; //!< penalty on non-fuel costs 

    double fMultiplier; //!< multiplier on fuel cost or price
    double fuelPrefElasticity; //!< Fuel preference elasticity

};
#endif // _GENERIC_TECHNOLOGY_INFO_H_
