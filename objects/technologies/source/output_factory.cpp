/*
 * This software, which is provided in confidence, was prepared by employees of
 * Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
 * Battelle has certain unperfected rights in the software which should not be
 * copied or otherwise disseminated outside your organization without the
 * express written authorization from Battelle. All rights to the software are
 * reserved by Battelle. Battelle makes no warranty, express or implied, and
 * assumes no liability or responsibility for the use of this software.
 */

/*! 
 * \file output_factory.cpp
 * \ingroup Objects
 * \brief OutputFactory source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "technologies/include/output_factory.h"
#include "technologies/include/ioutput.h"
#include "util/logger/include/ilogger.h"

// Add new types here.
#include "technologies/include/secondary_output.h"
// #include "emissions/include/by_product.h"

using namespace std;

/*!
 * \brief Returns whether the requested type is a type the factory knows how to
 *          create.
 * \param aType Type to determine if the factory can create.
 * \return Whether the factory can create the type.
 */
bool OutputFactory::isOfType( const string& aType ) {
    // Search the list of known types.
    return ( aType == SecondaryOutput::getXMLNameStatic() );
    // TODO: Enable byproducts code when nuclear is committed.
    //   || ( aType == ByProduct::getXMLNameStatic() );
}

/*!
 * \brief Return a new instance of a component of the requested type.
 * \param aType Type of capture component to return.
 * \return A newly created capture component wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<IOutput> OutputFactory::create( const string& aType ) {
    // Search the list of known types.
    if( aType == SecondaryOutput::getXMLNameStatic() ){
        return auto_ptr<IOutput>( new SecondaryOutput );
    }
    // if( aType == ByProduct::getXMLNameStatic() ){
    //    return auto_ptr<IOutput>( new ByProduct );
    //}
    // Check for consistency.
    assert( !isOfType( aType ) );

    // Unknown type.
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create output of type " << aType << "." << endl;
    return auto_ptr<IOutput>();
}
