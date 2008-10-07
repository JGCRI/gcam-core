#ifndef _BUILDING_SUPPLY_SUBSECTOR_H_
#define _BUILDING_SUPPLY_SUBSECTOR_H_
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
* \file building_supply_subsector.h
* \ingroup CIAM
* \brief The BuildingSupplySubSector class header file.
* \author Steve Smith
*/

#include <xercesc/dom/DOMNode.hpp>
#include "sectors/include/subsector.h"

/*! 
* \ingroup CIAM
* \brief A class which defines a building supply Subsector of the model.

* This subsector supplies building services. The main differences 
* to provide calibration and internal load values to the building demand subsectors and techs

* \author Steve Smith
*/

class BuildingSupplySubSector : public Subsector
{
public:
    BuildingSupplySubSector( const std::string& regionName, const std::string& sectorName );
    static const std::string& getXMLNameStatic();
protected:
    virtual const std::string& getXMLName() const;
    bool isNameOfChild  ( const std::string& nodename ) const;

    virtual ITechnology* createChild( const std::string& aTechType,
                                      const std::string& aTechName,
                                      const int aTechYear ) const;

    bool XMLDerivedClassParse( const std::string& nodeName, const xercesc::DOMNode* curr ); 
    double unitInternalLoads; //!< internal loads per unit service
private:
    static const std::string XML_NAME; //!< node name for toXML methods
};
#endif // _BUILDING_SUPPLY_SUBSECTOR_H_
