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
* \file satiation_demand_function.cpp
* \ingroup Objects
* \brief The SatiationDemandFunction class source file.
* \author Pralit Patel
* \author Jiyong Eom
*/

#include "util/base/include/definitions.h"
#include <cmath>
#include <cassert>

#include "functions/include/satiation_demand_function.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;

extern Scenario* scenario;

SatiationDemandFunction::SatiationDemandFunction()
{
    mParsedSatiationAdder = 0;
}

SatiationDemandFunction* SatiationDemandFunction::clone() {
    SatiationDemandFunction* clone = new SatiationDemandFunction();
    clone->copy( *this );
    return clone;
}

void SatiationDemandFunction::copy( const SatiationDemandFunction& aOther ) {
    mBaseYearSatiationMultiplier = aOther.mBaseYearSatiationMultiplier;
    mParsedSatiationLevel = aOther.mParsedSatiationLevel;
    mSatiationLevel = aOther.mSatiationLevel;
    mSatiationImpedance = aOther.mSatiationImpedance;
    mParsedSatiationAdder = aOther.mParsedSatiationAdder;
    mSatiationAdder = aOther.mSatiationAdder;
	mParsedSatiationImpedance = aOther.mParsedSatiationImpedance;
}

const string& SatiationDemandFunction::getXMLNameStatic() {
    const static string XML_NAME = "satiation-demand-function";
    return XML_NAME;
}

const string& SatiationDemandFunction::getName() const {
    return getXMLNameStatic();
}

/*!
 * \brief Evaluate the satiation function at the given driver level.
 * \param aDemandDriver The value at which to calculate the function.
 * \return The value of the function at the given demand driver.
 */
double SatiationDemandFunction::calcDemand( const double aDemandDriver ) const {

	double SatiationLevel = SatiationDemandFunction::mParsedSatiationLevel;
	double SatiationImpedance = SatiationDemandFunction::mParsedSatiationImpedance;
	double SatiationAdder = SatiationDemandFunction::mParsedSatiationAdder;
	

    const double log2 = log( 2.0 );
    return (SatiationLevel)
        * ( 1 - exp( -log2 / SatiationImpedance * aDemandDriver ) ) + SatiationAdder;

	
}

/*!
 * \brief Calibrate the satiation impedance given the data point (aDemand, aDemandDriver).
 * \details With the given data point, satiation adder (subsistence level), and satiation level
 *          (asymptote) then the last shape parameter satiation impedance can be determined.
 *          If the user specified the satiation level as a base year demand increase
 *          that value can also be determined now.
 * \param aDemand The calibrated output of this function.
 * \param aDemandDriver The driver for the calibrated demand level.
 * \param aPeriod The model period.
 */
