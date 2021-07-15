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
* \file gompertz_function.cpp
* \ingroup Objects
* \brief The GompertzDemandFunction class source file.
* \author Jon Sampedro
*/

#include "util/base/include/definitions.h"
#include <cmath>
#include <cassert>

#include "functions/include/gompertz_function.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

GompertzDemandFunction::GompertzDemandFunction()
{

}

GompertzDemandFunction* GompertzDemandFunction::clone() {
	GompertzDemandFunction* clone = new GompertzDemandFunction();
    clone->copy( *this );
    return clone;
}

void GompertzDemandFunction::copy( const GompertzDemandFunction& aOther ) {
	mParsedUnadjustSatiation = aOther.mParsedUnadjustSatiation;
	mUnadjustSatiation = aOther.mUnadjustSatiation;
	mParsedHabitableLand = aOther.mParsedHabitableLand;
	mHabitableLand = aOther.mHabitableLand;
	mParsedLandDensityParam = aOther.mParsedLandDensityParam;
	mParsedBaseFloorspaceParam = aOther.mParsedBaseFloorspaceParam;
	mBaseFloorspaceParam = aOther.mBaseFloorspaceParam;
	mParsedIncomeParam = aOther.mParsedIncomeParam;
	mIncomeParam = aOther.mIncomeParam;
	mParsedBiasAdjustParam = aOther.mParsedBiasAdjustParam;
	mBiasAdjustParam = aOther.mBiasAdjustParam

}

const string& GompertzDemandFunction::getXMLNameStatic() {
    const static string XML_NAME = "gompertz-function";
    return XML_NAME;
}

const string& GompertzDemandFunction::getName() const {
    return getXMLNameStatic();
}

bool GompertzDemandFunction::XMLParse( const DOMNode* aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );

    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
		else if (nodeName == "unadjust_satiation") {
			mParsedUnadjustSatiation.set(XMLHelper<double>::getValue(curr));
		}
        else if( nodeName == "land_density_param" ) {
			mParsedLandDensityParam.set( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "base_floorspace_param" ) {
			mParsedBaseFloorspaceParam.set( XMLHelper<double>::getValue( curr ) );
		}
		else if (nodeName == "income_param") {
			mParsedIncomeParam.set(XMLHelper<double>::getValue(curr));
		}
		else if (nodeName == "bias_adjust_param") {
			mParsedBiasAdjustParam.set(XMLHelper<double>::getValue(curr));
		}
		else if (nodeName == "habitable_land") {
			mParsedHabitableLand.set(XMLHelper<double>::getValue(curr));
		            
            assert(mParsedHabitableLand > 1.0 );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
    
    return true;
}

/*!
 * \brief Evaluate the satiation function at the given driver level.
 * \param aDemandDriver The value at which to calculate the function.
 * \return The value of the function at the given demand driver.
 */
double GompertzDemandFunction::calcDemand( const double aDemandDriver ) const {

	    return (mParsedUnadjustSatiation - mParsedLandDensityParam*log(POP/ mParsedHabitableLand))

        * exp(mParsedBaseFloorspaceParam*log(BASEpcFLSP)

		* exp(mParsedIncomeParam*log(GDPpc))

		+ mParsedBiasAdjustParam;
}

