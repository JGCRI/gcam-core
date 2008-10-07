#ifndef _ITECHNOLOGY_INFO_H_
#define _ITECHNOLOGY_INFO_H_
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
* \file itechnology_info.h
* \ingroup Objects
* \brief ITechnologyInfo header file.
* \author Pralit Patel
*/

#include <string>

// Forward declaration
class Tabs;

/*! 
* \ingroup Objects
* \brief Interface for a technology info object
*
* Defines the methods necessary for a technology to be able to
* use a global or generic technology info, each of which contains
* data that should remain constant for a technology.
*
* \author Pralit Patel
*/
class ITechnologyInfo
{

public:

    /*! Creates a clone of the ITechnologyInfo
     * \return The pointer to the clone.
     */
    virtual ITechnologyInfo* clone() = 0;

    /*! Called after XML Parsing, to verify validity of
     *  data read.
     */
    virtual void completeInit() =0;

    /*! Print values in XML to replicate the input files read.
     *
     * \param out Stream to write values to.
     * \param tabs The class responsible for correct tabbing.
     */
    virtual void toInputXML( std::ostream& out, Tabs* tabs ) const = 0;

    /*! Print values in XML for debbuging purposes.
     *
     * \param out Stream to write values to.
     * \param tabs The class responsible for correct tabbing.
     */
    virtual void toDebugXML( const int period, std::ostream& out, Tabs* tabs ) const = 0;

    /*!
     * \brief Get the name of this technology.
     * \return The name.
     */
    virtual const std::string& getName() const = 0;
    
    /*!
     * \brief Get the fuelname of this technology.
     * \return The fuelname.
     */
    virtual const std::string& getFuelName() const = 0;
    
    /*!
     * \brief Get the efficiency of this technology.
     * \return The efficiency.
     */
    virtual const double getEfficiency() const = 0;
    
    /*!
     * \brief Get the non energy cost of this technology.
     * \return The non energy cost.
     */
    virtual const double getNonEnergyCost() const = 0;

    /*!
     * \brief Get the fMultiplier of this technology.
     * \return The fMultiplier.
     */
    virtual const double getFMultiplier() const = 0;

    /*!
     * \brief Get the fuel pref elasticity of this technology.
     * \return The fuel pref elasticity.
     */
    virtual const double getFuelPrefElasticity() const = 0;

    /*!
     * \brief Set the fuelname of this technology
     * \param aName The fuelname to set.
     */
    virtual void setFuelName( const std::string& aFuelName ) = 0;

    /*!
     * \brief Set the efficiency of this technology
     * \param aName The efficiency to set.
     */
    virtual void setEfficiency( const double aEfficiency ) = 0;

    /*!
     * \brief Set the non energy cost of this technology
     * \param aName The non energy cost to set.
     */
    virtual void setNonEnergyCost( const double aNonEnergyCost ) = 0;

    /*!
     * \brief Set the fmultiplier of this technology
     * \param aName The fmultiplier to set.
     */
    virtual void setFMultiplier( const double aFMultiplier ) = 0;

    /*!
     * \brief Set the fuel pref elasticity of this technology
     * \param aName The fuel pref elasticity to set.
     */
    virtual void setFuelPrefElasticity( const double aFuelPrefElasticity ) = 0;

};
#endif // _ITECHNOLOGY_INFO_H_
