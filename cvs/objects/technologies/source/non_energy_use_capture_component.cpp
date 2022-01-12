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
 * \file non_energy_use_capture_component.cpp
 * \ingroup Objects
 * \brief NonEnergyUseCaptureComponent source file.
 * \author Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include "util/base/include/xml_helper.h"
#include "technologies/include/non_energy_use_capture_component.h"
#include "util/logger/include/ilogger.h"

using namespace std;

/*!
 * \brief Constructor.
 * \details Protected constructor which prevents the capture component from
 *          being created without using the CaptureComponentFactory.
 */
NonEnergyUseCaptureComponent::NonEnergyUseCaptureComponent()
{
    mRemoveFraction = 0;
}

NonEnergyUseCaptureComponent::~NonEnergyUseCaptureComponent() {
}

// Documentation inherits.
NonEnergyUseCaptureComponent* NonEnergyUseCaptureComponent::clone() const {
    NonEnergyUseCaptureComponent* clone = new NonEnergyUseCaptureComponent();
    clone->copy( *this );
    return clone;
}

void NonEnergyUseCaptureComponent::copy( const NonEnergyUseCaptureComponent& aOther ) {
    mRemoveFraction = aOther.mRemoveFraction;
    mTargetGas = aOther.mTargetGas;
}

// Documentation inherits.
bool NonEnergyUseCaptureComponent::isSameType( const string& aType ) const {
    return aType == getXMLNameStatic();
}

/*!
 * \brief Get the XML node name in static form for comparison when parsing XML.
 * \details This public function accesses the private constant string, XML_NAME.
 *          This way the tag is always consistent for both read-in and output
 *          and can be easily changed. The "==" operator that is used when
 *          parsing, required this second function to return static.
 * \note A function cannot be static and virtual.
 * \author Josh Lurz, James Blackwood
 * \return The constant XML_NAME as a static.
 */
const string& NonEnergyUseCaptureComponent::getXMLNameStatic() {
    const static string XML_NAME = "non-energy-use-capture-component";
    return XML_NAME;
}

const string& NonEnergyUseCaptureComponent::getXMLName() const {
    return getXMLNameStatic();
}

const string& NonEnergyUseCaptureComponent::getName() const {
    // Capture components are not named as only one can exist per Technology.
    return getXMLNameStatic();
}

void NonEnergyUseCaptureComponent::toDebugXML( const int aPeriod,
                                               ostream& aOut,
                                               Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );
    XMLWriteElement( mRemoveFraction, "remove-fraction", aOut, aTabs );
    XMLWriteElement( mTargetGas, "target-gas", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void NonEnergyUseCaptureComponent::completeInit( const string& aRegionName,
                                                 const string& aSectorName )
{
    // Does not need to add any dependencies. Check that the remove fraction is
    // valid.
    if( mRemoveFraction < 0 || mRemoveFraction > 1 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Invalid removal fraction of " << mRemoveFraction << "." << endl;
        mRemoveFraction = 0;
    }

    // Default the target gas to CO2.
    if( mTargetGas.empty() ){
        mTargetGas = "CO2";
    }
}

void NonEnergyUseCaptureComponent::initCalc( const string& aRegionName,
                                             const string& aSectorName,
                                             const string& aFuelName,
                                             const int aPeriod )
{
}

double NonEnergyUseCaptureComponent::getStorageCost( const string& aRegionName,
                                                     const string& aGHGName,
                                                     const int aPeriod ) const
{
    // Emissions stored in the product so there is zero cost.
    return 0;
}

double NonEnergyUseCaptureComponent::getRemoveFraction( const string& aGHGName ) const {
	return aGHGName == mTargetGas ? mRemoveFraction : 0;
}

double NonEnergyUseCaptureComponent::calcSequesteredAmount( const string& aRegionName,
                                                            const string& aGHGName,
										                    const double aTotalEmissions, const int aPeriod )
{
	double capturedAmount = 0;
	if( aGHGName == mTargetGas ){
		// Calculate the amount.
		capturedAmount = mSequesteredAmount[ aPeriod ] = mRemoveFraction * aTotalEmissions;
	}
	return capturedAmount;
}

double NonEnergyUseCaptureComponent::getSequesteredAmount( const string& aGHGName,
                                                           const bool aGetGeologic,
										                   const int aPeriod ) const 
{
	// Only return emissions if non-geologic emissions are requested.
    if( !aGetGeologic && aGHGName == mTargetGas ){
		return mSequesteredAmount[ aPeriod ];
	}
	return 0;
}

void NonEnergyUseCaptureComponent::adjustInputs( const string& aRegionName,
                                                 vector<IInput*>& aInputs,
                                                 const int aPeriod ) const
{
    // Non-energy use capture components do not need to adjust input costs or
    // efficiencies.
}
