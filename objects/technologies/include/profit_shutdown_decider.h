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

#ifndef _PROFIT_SHUTDOWN_DECIDER_H_
#define _PROFIT_SHUTDOWN_DECIDER_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
 * \file profit_shutdown_decider.h
 * \ingroup Objects
 * \brief The ProfitShutdownDecider header file.
 * \author Josh Lurz
 */
#include "technologies/include/ishutdown_decider.h"

#include <string>
struct ProductionFunctionInfo;

/*! 
 * \ingroup Objects
 * \brief This object begins to shut down a vintage when its profit rate falls
 *        below a minimum level.
 * \details This object mimics the Legacy-SGM shutdown decision by calculating a
 *          current profit rate as determined by total profits divided by
 *          capital. It begins to shutdown the vintage when this profit rate is
 *          less than a minimum level(as determined by the MIN_PROFIT_RATE
 *          constant). Complete shutdown occurs at the zero profit point. This
 *          short term shutdown decision includes the payments to capital. The
 *          object returns a scaling factor which is used to decrease and
 *          eventually shutdown profit and output of a vintage as it is nearing
 *          zero profit. A smoothing is calculated between the MIN_PROFIT_RATE
 *          and zero to prevent a vintage from switching on and off abruptly. At
 *          the point of zero profits the vintage is shutdown fully.
 *
 *          <b>XML specification for ProfitShutdownDecider</b>
 *          - XML name: \c profit-shutdown-decider
 *          - Contained by: Technology and BaseTechnology
 *          - Parsing inherited from class: None
 *          - Attributes: None
 *          - Elements: None
 *   
 * \author Josh Lurz
 */
class ProfitShutdownDecider: public IShutdownDecider
{
    friend class ShutdownDeciderFactory;

    // Allow SGM technology to create the ProfitShutdownDecider directly.
    friend class ProductionTechnology;
public:
    // IParsedComponent methods.
    virtual ProfitShutdownDecider* clone() const;

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
    ProfitShutdownDecider();

    static const std::string& getXMLNameStatic();
};

#endif // _PROFIT_SHUTDOWN_DECIDER_H_
