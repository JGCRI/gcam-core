#ifndef _READ_EMISSIONS_COEF_H_
#define _READ_EMISSIONS_COEF_H_
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
 * \file read_emissions_coef.h
 * \ingroup Objects
 * \brief ReadEmissionsCoef header file.
 * \author Jim Naslund
 */

#include "emissions/include/aemissions_coef.h"

class IInfo;

/*! 
 * \ingroup Objects
 * \brief A concrete class that represents a read-in emissions coefficient.
 * \details This class represents an emissions coefficient that is read from input.
 *          Its methods are empty.
 * \author Jim Naslund
 */
class ReadEmissionsCoef : public AEmissionsCoef {
public:
    ReadEmissionsCoef( const double aEmissionsCoef );
    virtual ReadEmissionsCoef* clone() const;

    virtual void initCalc( const IInfo* aSubsectorInfo, const std::string& aName );
    virtual void overrideCoef( const double aEmissionsCoef );
        
    virtual const std::string& getXMLName() const;
};


#endif // _READ_EMISSIONS_COEF_H_
