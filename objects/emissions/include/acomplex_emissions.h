#ifndef _ACOMPLEX_EMISSIONS_H_
#define _ACOMPLEX_EMISSIONS_H_
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
 * \file acomplex_emissions.h
 * \ingroup Objects
 * \brief AComplexEmissions class header file.
 * \author Jim Naslund
 */

#include "emissions/include/aghg.h"
#include <string>
#include <xercesc/dom/DOMNode.hpp>

class AEmissionsCoef;
class GhgMAC;

/*! 
 * \ingroup Objects
 * \brief An abstract class that represents complex emissions.
 * \author Jim Naslund
 */
class AComplexEmissions: public AGHG {
public:
    virtual ~AComplexEmissions();
    virtual AComplexEmissions* clone() const = 0;

    virtual void copyGHGParameters( const AGHG* prevGHG );

    virtual void initCalc( const std::string& aRegionName,
                           const std::string& aFuelName,
                           const IInfo* aLocalInfo,
                           const int aPeriod );

    virtual double getGHGValue( const std::string& aRegionName,
                                const std::string& aFuelName,
                                const std::vector<IOutput*>& aOutputs,
                                const double aEfficiency,
                                const ICaptureComponent* aSequestrationDevice,
                                const int aPeriod ) const;

    virtual void calcEmission( const std::string& aRegionName,
                               const std::string& aFuelname,
                               const double aInput,
                               const std::vector<IOutput*>& aOutputs,
                               const GDP* aGDP,
                               ICaptureComponent* aSequestrationDevice,
                               const int aPeriod );
protected:

    AComplexEmissions();
    AComplexEmissions( const AComplexEmissions& other );
    AComplexEmissions& operator=( const AComplexEmissions& other );
    
    double maxCntrl; //!<  final control fraction for ghg's
    double gdpcap0; //!< User inputed variable- represents midpoint of curve for control function
    double tau; //!< User inputed timescale parameter in control function
    double gdpCap; //!< Saved value for GDP per capita. Needed to adjust control.
    double techDiff; //!< technological change parameter- represents percent reduction in gdp0 per year;
    double adjMaxCntrl; //!< multiplier to maxCntrl, keeping current emissions constant
    double multMaxCntrl; //!< multiplier to maxCntrl -- changes current emissions
    double emAdjust; //!< User inputed adjustment to emissions values(0 to 1)
    double finalEmissCoef; //!< user input final emissions factor that is approached asymptotically
    
    //! Global warming potential of the gas.
    double gwp;

    std::auto_ptr<GhgMAC> ghgMac; //!< Marginal Abatement Cost Curve Object
    std::auto_ptr<AEmissionsCoef> mEmissionsCoef; //!< emissions coefficient delegate

    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual void toInputXMLDerived( std::ostream& out, Tabs* tabs ) const = 0;
    virtual void toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const = 0;
    double adjustControlParameters( const double gdpCap, const double emissDrive, const double macReduction,
                                    const int period );
    virtual void adjustMaxCntrl(const double GDPcap);

    double controlFunction( const double maxCntrlIn, const double tauIn, const double gdpcap0In, const double gdpCapIn );
    double calcTechChange( const int period );

private:
    void copy( const AComplexEmissions& other );
    
};

#endif // _ACOMPLEX_EMISSIONS_H_

