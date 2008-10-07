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
 * \file box_flow.cpp
 * \ingroup objects
 * \brief BoxFlow class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include "ccarbon_model/include/box_flow.h"
#include "ccarbon_model/include/carbon_box.h"

using namespace std;

/*!
 * \brief Default constructor.
 */
BoxFlow::BoxFlow()
: ACarbonFlow( eBoxFlow )
{
}
/*! BoxFlow( const ACarbonFlow& aCarbonFlow )
 * \brief a Copy Constructor for BoxFlow
 * \details create a newBoxFlow object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param aBoxFlow const reference of BoxFlow object
 * \author Ming Chang
 */
BoxFlow::BoxFlow( const BoxFlow& aBoxFlow ):ACarbonFlow( aBoxFlow ){
    /*
     * \notice: the initilization line must includes the base class copy constructor with the 
                 derived object in your parameter. It will automatically dynamic_cast back into the
                 base class. This step must be done in order for Virtual Copy Constructor to work.
     */
}

BoxFlow::~BoxFlow(){
}

/*! clone()
 * \brief a Virtual Copy Constructor
 * \details this is a virtual function, it will return a newly created object
               of BoxFlow by calling the copy constructor with the parameter referenced to iself
 * \return BoxFlow* pointer points to the address of newly generated BoxFlow object.
 * \author Ming Chang
 */
BoxFlow* BoxFlow::clone() const {
    return ( new BoxFlow( *this ) );
}

bool BoxFlow::XMLDerivedClassParse( const xercesc::DOMNode* aNode ){
    return false;
}

void BoxFlow::toInputXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

void BoxFlow::toDebugXMLDerived( ostream& aOut, Tabs* aTabs ) const {
}

/*! transfer( const double aValue, const EnvironmentalInfo* aEnvInfo, const int aYear, const BoxType aBoxType )
 * \brief carbon stock shifting from one box to another or to the same type in Summer Box
 * \details calling acceptTransfer for Target in BoxFlow and LUC-Flow-Out
 * \param aValue carbon stock value
           aEnvInfo EnvironmentalInfo* for the current conceptual root
           aYear working year
           aBoxType BoxType from enum
 * \author Ming Chang
 */
void BoxFlow::transfer( const double aValue, const EnvironmentalInfo* aEnvInfo,
                        const int aYear, const BoxType aBoxType ){
    // Passing any enum because it doesn't matter for box flows.
    mTarget->acceptTransfer( aValue * mFraction, aYear, eAnyBox );
}

const std::string& BoxFlow::getXMLNameStatic(){
    static const string XML_NAME = "box-flow";
    return XML_NAME;
}

const string& BoxFlow::getXMLName() const {
    return getXMLNameStatic();
}
