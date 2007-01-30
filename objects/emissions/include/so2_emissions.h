#ifndef _SO2_EMISSIONS_H_
#define _SO2_EMISSIONS_H_
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
* \file so2_emissions.h
* \ingroup Objects
* \brief SO2Emissions class header file.
* \author
*/

#include "emissions/include/acomplex_emissions.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

/*! 
 * \ingroup Objects
 * \brief A class that represents SO2 emissions.
 * \warning SO2Emissions will not work with an emissions coefficient of a type
 *          other than ReadEmissionsCoef.
 * \todo Currently this method overrides initCalc and sets the emissions coefficient
 *       delegate to 1.  The emissions coefficient is contained in the emissionsDriver
 *       function.  This is a hack and should be refactored.
 * \author Jim Naslund
 */
class SO2Emissions: public AComplexEmissions { 
public:
    SO2Emissions();
    ~SO2Emissions();
    SO2Emissions* clone() const;
    static const std::string& getXMLNameStatic();
    virtual const std::string& getName() const;

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aFuelName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );

protected:
    virtual const std::string& getXMLName() const;
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void parseName( const std::string& aNameAttr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const;
    void setAdjMaxCntrl();
    virtual void adjustMaxCntrl(const double GDPcap);
    virtual double emissionsDriver( const double inputIn, const double outputIn ) const;
    
    double ashRetention; //!< percentage of output that remains in ash form
    double percentSulfur; //!< sulfur content of input (percentage)  
    double gjPerTonne; //!< fuel energy(in GJ) per metric ton
    double finalSulfur; // Asymptotic final sulfur content (percentage) 
};

#endif // _SO2_EMISSIONS_H_

