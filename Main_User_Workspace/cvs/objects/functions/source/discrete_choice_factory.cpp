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
* \file discrete_choice_factory.cpp
* \ingroup objects
* \brief DiscreteChoiceFactor class source file
* \author Pralit Patel
*/

#include "util/base/include/definitions.h"
#include "functions/include/discrete_choice_factory.hpp"
#include "util/logger/include/ilogger.h"

// IDiscreteChoice subtypes here
#include "functions/include/relative_cost_logit.hpp"
#include "functions/include/absolute_cost_logit.hpp"

using namespace std;

/*!
 * \brief Returns whether the requested type is a type the factory knows how to create.
 * \param aType Type to determine if the factory can create.
 * \return Whether the factory can create the type.
 */
bool DiscreteChoiceFactory::isOfType( const string& aType ) {
    return aType == RelativeCostLogit::getXMLNameStatic() ||
           aType == AbsoluteCostLogit::getXMLNameStatic();
}

/*!
 * \brief Return a new instance of a discrete choice function of the requested type.
 * \param aType Type of discrete choice function to return.
 * \return A newly created discrete choice function wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<IDiscreteChoice> DiscreteChoiceFactory::create( const string& aType ) {
    auto_ptr<IDiscreteChoice> ret;
    if( aType == RelativeCostLogit::getXMLNameStatic() ) {
        ret.reset( new RelativeCostLogit );
    }
    else if( aType == AbsoluteCostLogit::getXMLNameStatic() ) {
        ret.reset( new AbsoluteCostLogit );
    }
    else {
        // Unknown type.
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Could not create discrete choice function of type " << aType << "." << endl;
    }

    return ret;
}
