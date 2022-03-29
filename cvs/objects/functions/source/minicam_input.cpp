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
 * \file minicam_input.cpp
 * \ingroup Objects
 * \brief The MiniCAMInput class source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <cassert>
#include "functions/include/minicam_input.h"
#include "util/base/include/ivisitor.h"
#include "util/logger/include/ilogger.h"

using namespace std;

//! Default Constructor
MiniCAMInput::MiniCAMInput(): mTypeFlags( 0 )
{
}

//! Destructor
MiniCAMInput::~MiniCAMInput() {
}

void MiniCAMInput::copy( const MiniCAMInput& aOther ) {
    mName = aOther.mName;
    mKeywordMap = aOther.mKeywordMap;
    mTypeFlags = aOther.mTypeFlags;
}

const string& MiniCAMInput::getName() const {
    return mName;
}

const string& MiniCAMInput::getMarketName( const string& aRegionName ) const {
    return aRegionName;
}

/*! \brief Initialize the type flags.
 * \see setFlagsByName
 */
void MiniCAMInput::initializeTypeFlags() {
    
    // Initialize the flag.
    mTypeFlags |= IInput::INITIALIZED;
}

/*! \brief Add the correct flags to the mTypeFlags for the given type name.
 * \details This allows us to flexibly initialize the type flags based on
 *          input names or read in flags.
 * \param aTypeName The name to use when determining the type flags.
 */
void MiniCAMInput::setFlagsByName( const string& aTypeName ) {
    // Initialize the type.
    if( aTypeName == "Energy" ){
        mTypeFlags |= IInput::ENERGY;
    }
    else if( aTypeName == "Resource" ){
        mTypeFlags |= IInput::RESOURCE;
    }
    else if( aTypeName == "BackupEnergy" ){
        mTypeFlags |= IInput::BACKUP_ENERGY;
    }
    else{
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Flag Error " << aTypeName << endl;
    }
}

string MiniCAMInput::getFlagName( const int aTypeFlag ) const {
    if( aTypeFlag & IInput::ENERGY ){
        return ("Energy");
    }
    else if( aTypeFlag & IInput::RESOURCE ){
        return ("Resource");
        
    }
    else if( aTypeFlag & IInput::BACKUP_ENERGY ){
        return ("BackupEnergy");
    }
    else {
        return ("");
    }
}

bool MiniCAMInput::hasTypeFlag( const int aTypeFlag ) const {
    
    /*! \pre The type flags must be initialized. */
    
    return ( ( aTypeFlag & ~mTypeFlags ) == 0 );
}

double MiniCAMInput::getConversionFactor( const int aPeriod ) const {
    return 0; // check this.
}

double MiniCAMInput::getCurrencyDemand( const int aPeriod ) const {
    return 0;
}

// Return 0 for base class.
double MiniCAMInput::getCarbonContent( const int aPeriod ) const {
    return 0;
}

void MiniCAMInput::setCurrencyDemand( double aCurrencyDemand,
                                      const string& aRegionName,
                                      const int aPeriod )
{
    // MiniCAM cannot set currency demand directly.
}

double MiniCAMInput::getPricePaid( const string& aRegionName,
                                   const int aPeriod ) const
{
    // In MiniCAM, price, price paid, and price received are all equal.
    return getPrice( aRegionName, aPeriod );
}

void MiniCAMInput::setPricePaid( double aPricePaid, const int aPeriod ) {
    // MiniCAM cannot directly set price paid.
    assert( false );
}

double MiniCAMInput::getPriceReceived( const string& aRegionName,
                                       const int aPeriod ) const
{
    // In MiniCAM, price, price paid, and price received are all equal.
    return getPrice( aRegionName, aPeriod );
}

double MiniCAMInput::getPriceAdjustment() const {
    return 0;
}

void MiniCAMInput::doInterpolations( const int aYear, const int aPreviousYear,
                                     const int aNextYear, const IInput* aPreviousInput,
                                     const IInput* aNextInput )
{
    // most inputs will not need to do anything
}

void MiniCAMInput::accept( IVisitor* aVisitor, const int period ) const
{
    aVisitor->startVisitMiniCAMInput( this, period );
    aVisitor->endVisitMiniCAMInput( this, period );
}

void MiniCAMInput::copyParamsInto( NodeInput& aNodeInput,
                                   const int aPeriod ) const
{
    // This should never be called.
    assert( false );
}

