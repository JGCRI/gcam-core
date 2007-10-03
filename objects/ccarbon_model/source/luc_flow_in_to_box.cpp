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
 * \file luc_flow_in_to_box.cpp
 * \ingroup objects
 * \brief LUCFlowInToBox class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"

#include "ccarbon_model/include/luc_flow_in_to_box.h"
#include "ccarbon_model/include/summer_box.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_container.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
LUCFlowInToBox::LUCFlowInToBox(){
}

LUCFlowInToBox::LUCFlowInToBox( ICarbonContainer* aTarget, const int aFraction )
: ACarbonFlow( eLUCFlowIn )
{
    mFraction = aFraction;
    mTarget = aTarget;
}

LUCFlowInToBox::LUCFlowInToBox( const LUCFlowInToBox& aLUCFlowInToBox )
:ACarbonFlow( aLUCFlowInToBox ){
}

LUCFlowInToBox* LUCFlowInToBox::clone() const {
	return ( new LUCFlowInToBox( *this ) );
}

/*!
 * \brief Constructor that sets the target.
 * \details Constructors a LUCFlowInToBox and sets the target to ther
 *          passed in parameter.
 * \param aTarget the target.
 */
LUCFlowInToBox::LUCFlowInToBox( ICarbonContainer* aTarget )
: ACarbonFlow( eLUCFlowIn )
{
    mTarget = aTarget;
}

LUCFlowInToBox::~LUCFlowInToBox(){
}

bool LUCFlowInToBox::XMLDerivedClassParse( const xercesc::DOMNode* aNode ){
    return false;
}

void LUCFlowInToBox::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowInToBox::toDebugXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowInToBox::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                               const int aYear, const BoxType aBoxType ){
    // This multiplies the value that should go to the box by a fraction
    // read in from input.  This is used in case there is more than one type of
    // a box (i.e vegetationA and vegetationB) and they should receieve different
    // percentages.
								
	mTarget->acceptTransfer( aValue * mFraction, aYear, aBoxType );
}

const std::string& LUCFlowInToBox::getXMLNameStatic(){
    // static const string XML_NAME = "luc-flow-into-box";  // origianl parsing sentence
	static const string XML_NAME = "luc-flow-in";
    return XML_NAME;
}

void LUCFlowInToBox::completeInit( const std::map< const std::string, ICarbonContainer* > aNamesToBoxes,
                                   int aKey ){
}

const string& LUCFlowInToBox::getXMLName() const {
    return getXMLNameStatic();
}
