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
