#ifndef _AEMISSONS_DRIVER_H_
#define _AEMISSONS_DRIVER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*!
 * \file aemissions_driver.h
 * \ingroup Objects
 * \brief AEmissionsDriver header file.
 * \author Jim Naslund
 */


/*! 
 * \ingroup Objects
 * \brief An abstract emissions driver class.
 * \details The class defines the behavior of an emissions driver.
 *          The class has one method and no members.
 * \author Jim Naslund
 */
class AEmissionsDriver{

public:
    /* \brief Returns an appropriate emissions driver.
     * \return A double representing an appropriate emissions driver.
     */
    virtual double calcEmissionsDriver( const double aInputIn, const double aOutputIn ) const = 0;
    //! Clone operator.
    virtual AEmissionsDriver* clone() const = 0;
    /*
     * \brief Static method to get a string representing the type of driver.
     * \return A string representing the type of driver.
     */
    virtual const std::string& getXMLName() const = 0;
};


#endif // _AEMISSONS_DRIVER_H_
