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
