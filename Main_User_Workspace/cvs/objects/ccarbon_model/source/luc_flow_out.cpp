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
