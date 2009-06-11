#ifndef _FOREST_SUPPLY_SUBSECTOR_H_
#define _FOREST_SUPPLY_SUBSECTOR_H_
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
* \file forest_supply_subsector.h
* \ingroup CIAM
* \brief The ForestSupplySubsector class header file.
* \author James Blackwood
*/

#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup Objects
* \brief A class which defines a single ForestSupplySubsector of the model.
* \details The forest supply subsector exists solely to create
*          ForestProductTechnologies.
* \author James Blackwood
*/

class ForestSupplySubsector : public Subsector {
public:
    ForestSupplySubsector( const std::string& aRegionName,
                           const std::string& aSectorName );

    virtual ~ForestSupplySubsector();
    static const std::string& getXMLNameStatic();

    virtual double calcShare( const int aPeriod,
                              const GDP* aGDP ) const;

protected:
    virtual bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr );
    virtual const std::string& getXMLName() const;
    bool isNameOfChild( const std::string& nodename ) const;

    virtual ITechnology* createChild( const std::string& aTechType,
                                     const std::string& aTechName,
		                             const int aTechYear ) const;
private:
};
#endif // _FOREST_SUPPLY_SUBSECTOR_H_
