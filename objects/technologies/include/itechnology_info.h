#ifndef _ITECHNOLOGY_INFO_H_
#define _ITECHNOLOGY_INFO_H_
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
