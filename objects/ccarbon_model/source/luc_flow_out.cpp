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
 * \file luc_flow_out.cpp
 * \ingroup objects
 * \brief LUCFlowOut class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"

#include "ccarbon_model/include/luc_flow_out.h"
#include "ccarbon_model/include/luc_carbon_summer.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_container.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
LUCFlowOut::LUCFlowOut():
ACarbonFlow( eLUCFlowOut )
{
mFraction = 1;
}

LUCFlowOut::LUCFlowOut( const LUCFlowOut& aLUCFlowOut )
: ACarbonFlow( aLUCFlowOut ){
}

LUCFlowOut::~LUCFlowOut(){
}

LUCFlowOut* LUCFlowOut::clone() const{
    return ( new LUCFlowOut( *this ) );
}

bool LUCFlowOut::XMLDerivedClassParse( const xercesc::DOMNode* aNode ){
    return false;
}

void LUCFlowOut::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowOut::toDebugXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowOut::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                           const int aYear, const BoxType aBoxType ){
    //! notice, the aBoxType is not used here. We are not transfering to same box type
    //! we are making transfer from the box to the specified target type in every
    //! LUCFlowOut
    mTarget->acceptTransfer( aValue * mFraction, aYear, aBoxType );
}

const std::string& LUCFlowOut::getXMLNameStatic(){
    static const string XML_NAME = "luc-flow-out";
    return XML_NAME;
}

const string& LUCFlowOut::getXMLName() const {
    return getXMLNameStatic();
}
