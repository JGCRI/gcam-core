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

BoxFlow::BoxFlow( const BoxFlow& aBoxFlow ):ACarbonFlow( aBoxFlow ){
}

BoxFlow::~BoxFlow(){
}

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
