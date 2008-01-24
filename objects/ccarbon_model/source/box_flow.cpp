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
