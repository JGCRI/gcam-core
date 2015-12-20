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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


/*! 
* \file ghg_factory.cpp
* \ingroup Objects
* \brief GHGFactory source file.
* \author Jim Naslund
*/

#include "util/base/include/definitions.h"

#include <string>

#include "emissions/include/ghg_factory.h"
#include "emissions/include/co2_emissions.h"
#include "emissions/include/nonco2_emissions.h"

using namespace std;

/*!
 * \brief Return a new instance of a component of the requested type.
 * \return A newly created GHG wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<AGHG> GHGFactory::create( const string& aType ){
    auto_ptr<AGHG> toReturn;
    if( aType == CO2Emissions::getXMLNameStatic() ){
        toReturn.reset( new CO2Emissions );
    }
    else if( aType == NonCO2Emissions::getXMLNameStatic() ){
        toReturn.reset( new NonCO2Emissions );
    }
    return toReturn;
}

/*!
 * \brief Returns whether the type is a valid GHG type.
 * \details Returns whether the type is a valid GHG type.  This is useful because
 *          when additional GHG types are created they only need to be added here.
 * \param aType type
 * \return boolean indicating whether the type of node is a valid GHG type
 */
bool GHGFactory::isGHGNode( const string& aType ){
    return aType == CO2Emissions::getXMLNameStatic()
           || aType == NonCO2Emissions::getXMLNameStatic();
}
