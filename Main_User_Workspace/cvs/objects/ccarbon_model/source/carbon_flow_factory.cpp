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
 * \file carbon_flow_factory.cpp
 * \ingroup objects
 * \brief CarbonFlowFactory class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include "ccarbon_model/include/carbon_flow_factory.h"
#include "util/logger/include/ilogger.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/box_flow.h"
#include "ccarbon_model/include/luc_flow_out.h"
#include "ccarbon_model/include/luc_flow_in_to_box.h"

using namespace std;

/*!
 * \brief Creates a carbon flow.
 * \details Creates a carbon flow based on the string that is passed in.
 *          Returns an auto_ptr that is initialized to null otherwise.
 * \warning This may return a null pointer.
 * \param aType a string representing the type of flow to create.
 * \return an auto_ptr to a carbon flow.
 */
auto_ptr<ACarbonFlow> CarbonFlowFactory::create( const string& aType ){
    if( aType == BoxFlow::getXMLNameStatic() ){
        return auto_ptr<ACarbonFlow>( new BoxFlow() );
    }
    if( aType == LUCFlowOut::getXMLNameStatic() ){
        return auto_ptr<ACarbonFlow>( new LUCFlowOut() );
    }
    if( aType == LUCFlowInToBox::getXMLNameStatic() ){
        return auto_ptr<ACarbonFlow>( new LUCFlowInToBox() );
    }

    // Unknown type.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create CarbonFlow of type " << aType << "." << endl;
    return auto_ptr<ACarbonFlow>( 0 );
}

/*!
 * \brief Determines if a node is a derived type of ACarbonFlow.
 * \detail Determines if the string represents a type of carbon flow
 *         that can be created with this factory.
 * \param aType a string representing the type of carbon flow to check.
 * \return a boolean representing whether the string is a type of carbon
 *         flow or not.
 */
bool CarbonFlowFactory::isOfType( const string& aType ){
    return aType == BoxFlow::getXMLNameStatic() ||
           aType == LUCFlowInToBox::getXMLNameStatic() ||
           aType == LUCFlowOut::getXMLNameStatic();
}
