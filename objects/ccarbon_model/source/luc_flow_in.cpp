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
 * \file luc_flow_in.cpp
 * \ingroup objects
 * \brief LUCFlowIn class source file.
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "ccarbon_model/include/luc_flow_in.h"
#include "ccarbon_model/include/summer_box.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_container.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
LUCFlowIn::LUCFlowIn( ICarbonContainer* aTarget, const int aFraction )
: ACarbonFlow( eLUCFlowIn )
{
    mFraction = aFraction;
    mTarget = aTarget;
}

/*!
 * \brief Constructor that sets the target.
 * \details Constructors a LUCFlowIn and sets the target to ther
 *          passed in parameter.
 * \param aTarget the target.
 */
LUCFlowIn::LUCFlowIn( ICarbonContainer* aTarget )
: ACarbonFlow( eLUCFlowIn )
{
    mTarget = aTarget;
}

LUCFlowIn::~LUCFlowIn(){
}

bool LUCFlowIn::XMLDerivedClassParse( const xercesc::DOMNode* aNode ){
    return false;
}

void LUCFlowIn::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowIn::toDebugXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowIn::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                          const int aYear, const BoxType aBoxType ){
    // The EnvironmentalInfo pointer points to the EnvironmentalInfo object
    // that is owned a carbon box model that is owned by a land leaf under
    // the correct conceptual root.  Thus, the EnvironmentalInfo's key can
    // be used to access the appropriate SummerSubContainer.
    double getLandLoss = SummerBox::getInstance()->getLandLoss( aEnvInfo->getKey() );
    mTarget->acceptTransfer( ( aValue / abs( getLandLoss ) ) * mFraction,
                             aYear, aBoxType );
}

const std::string& LUCFlowIn::getXMLNameStatic(){
    static const string XML_NAME = "luc-flow-in";
    return XML_NAME;
}

void LUCFlowIn::completeInit( const std::map< const std::string, ICarbonContainer* > aNamesToBoxes,
                              int aKey ){
}

const string& LUCFlowIn::getXMLName() const {
    return getXMLNameStatic();
}
