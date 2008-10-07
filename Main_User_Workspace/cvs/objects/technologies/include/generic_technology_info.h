#ifndef _GENERIC_TECHNOLOGY_INFO_H_
#define _GENERIC_TECHNOLOGY_INFO_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * LEGAL NOTICE
 * This computer software was prepared by Battelle Memorial Institute,
 * hereinafter the Contractor, under Contract No. DE-AC05-76RL0 1830
 * with the Department of Energy (DOE). NEITHER THE GOVERNMENT NOR THE
 * CONTRACTOR MAKES ANY WARRANTY, EXPRESS OR IMPLIED, OR ASSUMES ANY
 * LIABILITY FOR THE USE OF THIS SOFTWARE. This notice including this
 * sentence must appear on any copies of this computer software.
 * 
 * EXPORT CONTROL
 * User agrees that the Software will not be shipped, transferred or
 * exported into any country or used in any manner prohibited by the
 * United States Export Administration Act or any other applicable
 * export laws, restrictions or regulations (collectively the "Export Laws").
 * Export of the Software may require some form of license or other
 * authority from the U.S. Government, and failure to obtain such
 * export control license may result in criminal liability under
 * U.S. laws. In addition, if the Software is identified as export controlled
 * items under the Export Laws, User represents and warrants that User
 * is not a citizen, or otherwise located within, an embargoed nation
 * (including without limitation Iran, Syria, Sudan, Cuba, and North Korea)
 *     and that User is not otherwise prohibited
 * under the Export Laws from receiving the Software.
 * 
 * All rights to use the Software are granted on condition that such
 * rights are forfeited if User fails to comply with the terms of
 * this Agreement.
 * 
 * User agrees to identify, defend and hold harmless BATTELLE,
 * its officers, agents and employees from all liability involving
 * the violation of such Export Laws, either directly or indirectly,
 * by User.
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

    virtual const double getFMultiplier() const;
    virtual const double getFuelPrefElasticity() const;

    // setters
    virtual void setFuelName( const std::string& aFuelName );
    virtual void setEfficiency( const double aEfficiency );
    virtual void setNonEnergyCost( const double aNonEnergyCost );

    virtual void setFMultiplier( const double aFMultiplier );
    virtual void setFuelPrefElasticity( const double aFuelPrefElasticity );

protected:
    std::string name; //!< technology name
    std::string fuelname; //!< name of fuel used
    
    //! Base read-in efficiency.
    double mBaseEfficiency;

    //! Base non-fuel costs read in(levelized).
    double mBaseNonEnergyCost;

    double fMultiplier; //!< multiplier on fuel cost or price
    double fuelPrefElasticity; //!< Fuel preference elasticity

};
#endif // _GENERIC_TECHNOLOGY_INFO_H_
