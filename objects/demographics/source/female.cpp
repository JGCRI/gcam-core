/*! 
* \file female.cpp
* \ingroup Objects-SGM
* \brief Female class source file.
* \author Sonny Kim
*/

#include <xercesc/dom/DOMNode.hpp>
#include "demographics/include/female.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;
// static initialize
const string Female::XML_NAME = "female";

//! Default constructor
Female::Female() : Gender(){
    mFertilityRate = 0;
    mMaleBirth = 0;
    mFemaleBirth = 0;
    mMaleBirthFrac = 0;
}

//! Parse xml file for data
bool Female::XMLDerivedClassParse( const string &nodeName, const DOMNode* curr ) {
    if ( nodeName == "fertilityRate" ) {
        mFertilityRate = XMLHelper<double>::getValue( curr );
    }
    else if ( nodeName == "maleBirthFrac" ) {
        mMaleBirthFrac = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

//! For derived classes to output XML data
void Female::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElementCheckDefault( mFertilityRate, "fertilityRate", out, tabs );
    XMLWriteElementCheckDefault( mMaleBirthFrac, "maleBirthFrac", out, tabs );
    XMLWriteElementCheckDefault( mFemaleBirth, "femaleBirth", out, tabs );
    XMLWriteElementCheckDefault( mMaleBirth, "maleBirth", out, tabs );
}

//! Output debug info for derived class
void Female::toDebugXMLDerived( ostream& out, Tabs* tabs ) const {
    XMLWriteElement( mFertilityRate, "fertilityRate", out, tabs );
    XMLWriteElement( mMaleBirthFrac, "maleBirthFrac", out, tabs );
    XMLWriteElement( mFemaleBirth, "femaleBirth", out, tabs );
    XMLWriteElement( mMaleBirth, "maleBirth", out, tabs );
}

// calculate male births
double Female::calcMaleBirth() {
    assert( mPopulation != -1 );
    mMaleBirth = mPopulation * mFertilityRate * mMaleBirthFrac;
    return mMaleBirth;
}

// calculate female births
double Female::calcFemaleBirth() {
    assert( mPopulation != -1 );
    mFemaleBirth = mPopulation * mFertilityRate * ( 1 - mMaleBirthFrac );
    return mFemaleBirth;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Female::getXMLName() const{
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& Female::getXMLNameStatic(){
    return XML_NAME;
}

/*! \brief Update a visitor with information about a Female.
* \param aVisitor Visitor to update.
* \param aPeriod Period for which to update.
*/
void Female::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitFemale( this, aPeriod );
	// Update the parent class.
	Gender::accept( aVisitor, aPeriod );
	aVisitor->endVisitFemale( this, aPeriod );
}
