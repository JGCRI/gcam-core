#ifndef _GENERIC_EMISSIONS_H_
#define _GENERIC_EMISSIONS_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Labratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the express
 * written authorization from Battelle. All rights to the software are reserved
 * by Battelle. Battelle makes no warranty, express or implied, and assumes no
 * liability or responsibility for the use of this software.
 */

/*! 
 * \file generic_emissions.h
 * \ingroup Objects
 * \brief GenericEmissions class header file.
 * \author Jim Naslund
 */

#include "emissions/include/acomplex_emissions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

/*! 
 * \ingroup Objects
 * \brief A class that represents generic emissions.
 * \author Jim Naslund
 */
class GenericEmissions: public AComplexEmissions { 
public:
    virtual ~GenericEmissions();
    GenericEmissions* clone() const;
    static const std::string& getXMLNameStatic();
    virtual const std::string& getName() const;

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aFuelName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );
protected:
    virtual const std::string& getXMLName() const;
    virtual void parseName( const std::string& aNameAttr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;

    std::string mName; //!< name of ghg gas
};

#endif // _GENERIC_EMISSIONS_H_

