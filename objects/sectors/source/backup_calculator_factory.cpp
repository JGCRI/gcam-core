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
 * \file backup_calculator_factory.cpp
 * \ingroup Objects
 * \brief BackupCalculatorFactory source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "sectors/include/backup_calculator_factory.h"
#include "util/logger/include/ilogger.h"
#include <cassert>

// Add new types here.
#include "sectors/include/wind_backup_calculator.h"
#include "sectors/include/capacity_limit_backup_calculator.h"

using namespace std;

/*!
 * \brief Returns whether the requested type is a type the factory knows how to
 *        create.
 * \param aType Type to determine if the factory can create.
 * \return Whether the factory can create the type.
 */
bool BackupCalculatorFactory::isOfType( const string& aType ) {
    // Search the list of known types.
    return ( aType == WindBackupCalculator::getXMLNameStatic() 
        || aType == CapacityLimitBackupCalculator::getXMLNameStatic() );
}

/*!
 * \brief Return a new instance of a component of the requested type.
 * \param aType Type of shutdown decider to return.
 * \return A newly created shutdown decider wrapped in an auto_ptr. The pointer
 *         is null if the type is unknown.
 */
auto_ptr<IBackupCalculator> BackupCalculatorFactory::create( const string& aType ) {
    // Search the list of known types.
    if( aType == WindBackupCalculator::getXMLNameStatic() ) {
        return auto_ptr<IBackupCalculator>( new WindBackupCalculator );
    }
    if( aType == CapacityLimitBackupCalculator::getXMLNameStatic() ){
        return auto_ptr<IBackupCalculator>( new CapacityLimitBackupCalculator );
    }

    // Unknown type. Check for consistency between methods.
    assert( !isOfType( aType ) );
    ILogger& mainLog = ILogger::getLogger( "main_log" );
    mainLog.setLevel( ILogger::ERROR );
    mainLog << "Could not create backup calculator of type " << aType << "." << endl;
    return auto_ptr<IBackupCalculator>();
}
