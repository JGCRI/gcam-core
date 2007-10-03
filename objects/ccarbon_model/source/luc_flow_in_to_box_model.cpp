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
 * \file luc_flow_in_to_box_model.cpp
 * \ingroup objects
 * \brief LUCFlowInToBoxModel class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"

#include "ccarbon_model/include/luc_flow_in_to_box_model.h"
#include "ccarbon_model/include/summer_box.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_container.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
LUCFlowInToBoxModel::LUCFlowInToBoxModel( ICarbonContainer* aTarget, const int aFraction )
: ACarbonFlow( eLUCFlowIn )
{
    mFraction = aFraction;
    mTarget = aTarget;
}

LUCFlowInToBoxModel::LUCFlowInToBoxModel(const LUCFlowInToBoxModel &aLUCFlowInToBoxModel)
:ACarbonFlow( aLUCFlowInToBoxModel ){
	this->mFraction = aLUCFlowInToBoxModel.mFraction;
	this->mTarget = aLUCFlowInToBoxModel.mTarget;
}

LUCFlowInToBoxModel* LUCFlowInToBoxModel::clone() const {
	return ( new  LUCFlowInToBoxModel( *this )  );
}
/*!
 * \brief Constructor that sets the target.
 * \details Constructors a LUCFlowInToBoxModel and sets the target to ther
 *          passed in parameter.
 * \param aTarget the target.
 */
LUCFlowInToBoxModel::LUCFlowInToBoxModel( ICarbonContainer* aTarget )
: ACarbonFlow( eLUCFlowIn )
{
    mTarget = aTarget;
}

LUCFlowInToBoxModel::~LUCFlowInToBoxModel(){
}

bool LUCFlowInToBoxModel::XMLDerivedClassParse( const xercesc::DOMNode* aNode ){
    return false;
}

void LUCFlowInToBoxModel::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowInToBoxModel::toDebugXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void LUCFlowInToBoxModel::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                                    const int aYear, const BoxType aBoxType ){
    // The EnvironmentalInfo pointer points to the EnvironmentalInfo object
    // that is owned a carbon box model that is owned by a land leaf under
    // the correct conceptual root.  Thus, the EnvironmentalInfo's key can
    // be used to access the appropriate SummerSubContainer.
    // NOTE: mFraction will always be 1.
		
		double getLandLoss = SummerBox::getInstance()->getLandLoss( aEnvInfo->getKey() );
		mTarget->acceptTransfer( ( aValue / abs( getLandLoss ) ) * mFraction,
								   aYear, aBoxType );
}

const std::string& LUCFlowInToBoxModel::getXMLNameStatic(){
    static const string XML_NAME = "luc-flow-in";
    return XML_NAME;
}

void LUCFlowInToBoxModel::completeInit( const std::map< const std::string, ICarbonContainer* > aNamesToBoxes,
                                        int aKey ){
}

const string& LUCFlowInToBoxModel::getXMLName() const {
    return getXMLNameStatic();
}
