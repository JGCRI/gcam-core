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
#include "ccarbon_model/include/luc_carbon_summer.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/icarbon_container.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
LUCFlowInToBox::LUCFlowInToBox()
: ACarbonFlow( eLUCFlowIn )
{
mFraction = 1.0;
}


/*! LUCFlowInToBox( ICarbonContainer* aTarget, const int aFraction )
 * \brief a Constructor for LUCFlowInToBox
 * \details create a LUCFlowInToBox object and initialize the member variable
            with parameter value instead of default value
 * \param aTarget ICarbonContainer* target carbon box
         aFraction int fraction value of carbon stock transfer
 * \author Ming Chang
 */
LUCFlowInToBox::LUCFlowInToBox( ICarbonContainer* aTarget, const int aFraction )
: ACarbonFlow( eLUCFlowIn ) 
{                            
    mFraction = aFraction;
    mTarget = aTarget;
}

/*! LUCFlowInToBox( const LUCFlowInToBox& aLUCFlowInToBox )
 * \brief a Copy Constructor for LUCFlowInToBox
 * \details create a LUCFlowInToBox object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param aLUCFlowInToBox const reference of a LUCFlowInToBox object
 * \author Ming Chang
 */
LUCFlowInToBox::LUCFlowInToBox( const LUCFlowInToBox& aLUCFlowInToBox )
:ACarbonFlow( aLUCFlowInToBox ){//! initialization line must call copy constructor for base class.
                                //! Otherwise, it will use default constructor.
}                                

/*! clone()
 * \brief a Virtual Copy Constructor
 * \details this is a virtual function, it will return a newly created object
               of LUCFlowInToBox by calling the copy constructor with the parameter referenced to iself
 * \return - LUCFlowInToBox*: pointer points to the address of newly generated LUCFlowInToBox object.
 * \author Ming Chang
 */
LUCFlowInToBox* LUCFlowInToBox::clone() const {
    return ( new LUCFlowInToBox( *this ) );
}

/*! LUCFlowInToBox( ICarbonContainer* aTarget )
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

/*! transfer( const double aValue, const EnvironmentalInfo* aEnvInfo, const int aYear, const BoxType aBoxType )
 * \brief carbon stock shifting from summerbox subcontainer type to the carbon box in Carbon Model
 * \details calling acceptTransfer for Target in LUC-Flow-In. It will transfer the carbon stock from
 *              summerbox subcontainer with same box type of target in Carbon Model.
 * \param aValue double carbon stock value 
             aEnvInfo EnvironmentalInfo* for the current conceptual root
             aYear int working year
             aBoxType BoxType target box type from enum
 * \author Ming Chang and Jim Naslund
 */
void LUCFlowInToBox::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                               const int aYear, const BoxType aBoxType ){
    // This multiplies the value that should go to the box by a fraction
    // read in from input.  This is used in case there is more than one type of
    // a box (i.e vegetationA and vegetationB) and they should receieve different
    // percentages.
                                
    mTarget->acceptTransfer( aValue * mFraction, aYear, aBoxType );
}

const std::string& LUCFlowInToBox::getXMLNameStatic(){
    static const string XML_NAME = "luc-flow-in";
    return XML_NAME;
}

void LUCFlowInToBox::completeInit( CarbonBoxModel& aParentBoxModel ){
    setTargetFlowType();
}

const string& LUCFlowInToBox::getXMLName() const {
    return getXMLNameStatic();
}
