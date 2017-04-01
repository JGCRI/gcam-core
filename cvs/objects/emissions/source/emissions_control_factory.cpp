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
* \file emissions_control_factory.cpp
* \ingroup Objects
* \brief EmissionsControlFactory source file.
* \author Kate Calvin
*/

#include "util/base/include/definitions.h"

#include <string>

#include "emissions/include/emissions_control_factory.h"
#include "emissions/include/mac_control.h"
#include "emissions/include/gdp_control.h"
#include "emissions/include/linear_control.h"
#include "emissions/include/aemissions_control.h"

using namespace std;

/*!
 * \brief Return a new instance of a component of the requested type.
 * \return A newly created EmissionsDriver wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<AEmissionsControl> EmissionsControlFactory::create( const string& aType ){
    if( aType == MACControl::getXMLNameStatic() ){
        return auto_ptr<AEmissionsControl>( new MACControl );
    }
    else if( aType == GDPControl::getXMLNameStatic() ){
        return auto_ptr<AEmissionsControl>( new GDPControl );
    }
    else if( aType == LinearControl::getXMLNameStatic() ){
        return auto_ptr<AEmissionsControl>( new LinearControl );
    }
    return auto_ptr<AEmissionsControl>();
}

bool EmissionsControlFactory::isEmissionsControlNode( const string& aNodeName ){
    return aNodeName == MACControl::getXMLNameStatic() ||
           aNodeName == GDPControl::getXMLNameStatic() ||
           aNodeName == LinearControl::getXMLNameStatic();
}
