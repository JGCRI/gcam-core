/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial
 * Institute. Battelle has certain unperfected rights in the software which
 * should not be copied or otherwise disseminated outside your organization
 * without the express written authorization from Battelle. All rights to the
 * software are reserved by Battelle. Battelle makes no warranty, express or
 * implied, and assumes no liability or responsibility for the use of this
 * software.
 */

#ifndef _PHASED_SHUTDOWN_DECIDER_H_
#define _PHASED_SHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file phased_shutdown_decider.h
 * \ingroup Objects
 * \brief The PhasedShutdownDecider header file.
 * \author Josh Lurz
 */
#include "technologies/include/ishutdown_decider.h"

#include <string>
struct ProductionFunctionInfo;
class Tabs;

/*! 
 * \ingroup Objects
 * \brief This object shuts down production for a vintage using a single
 *        power-law decay with zero production after the technology lifetime.
 * \details This object uses a power-law decay to reduce the production of a
 *          vintage based on a read-in shutdown rate and the number of periods a
 *          vintage has been active. The class calculates a scaling factor which
 *          is used to decrease production as the maximum lifetime of the
 *          technology is approached. The technology will remove the tail of the
 *          production by shutting itself down when it reaches the end of its
 *          lifetime. The coefficient is calculated as 1 / ( ( 1 + rate ) ^
 *          number of years ).
 *
 *          <b>XML specification for PhasedShutdownDecider</b>
 *          - XML name: \c phased-shutdown-decider
 *          - Contained by: Technology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements:
 *              - \c shutdown-rate PhasedShutdownDecider::mShutdownRate
 *     
 * \author Josh Lurz
 */
class PhasedShutdownDecider: public IShutdownDecider
{
    friend class ShutdownDeciderFactory;
public:
    // IParsedComponent methods.
    virtual PhasedShutdownDecider* clone() const;

    virtual bool isSameType( const std::string& aType ) const;

    virtual const std::string& getName() const;

    virtual bool XMLParse( const xercesc::DOMNode* aNode );

    virtual void toInputXML( std::ostream& aOut,
                             Tabs* aTabs ) const;

    virtual void toDebugXML( const int aPeriod,
                             std::ostream& aOut,
                             Tabs* aTabs ) const;
    
    // IShutdownDecider methods.
    virtual double calcShutdownCoef( const ProductionFunctionInfo* aFuncInfo,
                                     const double aCalculatedProfits,
                                     const std::string& aRegionName,
                                     const std::string& aSectorName,
                                     const int aInitialTechYear,
                                     const int aPeriod ) const;
private:
    //! The annual rate at which to shutdown production. This rate may be zero
    //! which is the equivalent to not reading in the phased shutdown decider.
    double mShutdownRate;

    PhasedShutdownDecider();

    static const std::string& getXMLNameStatic();
};

#endif // _PHASED_SHUTDOWN_DECIDER_H_
