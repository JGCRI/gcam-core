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
 * \file npp.cpp
 * \ingroup Objects
 * \brief NPP class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "ccarbon_model/include/npp.h"
#include "util/base/include/xml_helper.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/environmental_info.h"
#include "util/base/include/util.h"
#include "util/base/include/configuration.h"

using namespace std;
using namespace xercesc;

/*! 
 * \brief Default Constructor
 */
NPP::NPP()
: mNPP( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 ),
  mNPPOverArea( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{
    this->mInitialNPP = 0;
    this->mCurrentNPP = 0;
    this->mUnitPerArea = false;
    this->mCarbonBoxType = eAnyBox;
}

/*! NPP( const NPP& aNPP )
 * \brief a Copy Constructor for NPP
 * \details create a new NPP object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param NPP const reference of NPP object
 * \author Ming Chang
 */
NPP::NPP( const NPP& aNPP ):
mNPP( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 ),
  mNPPOverArea( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear(), 0 )
{

    this->mInitialNPP = aNPP.mInitialNPP;
    this->mCurrentNPP = aNPP.mCurrentNPP;
    this->mCarbonBoxType = aNPP.mCarbonBoxType;
    this->mUnitPerArea = aNPP.mUnitPerArea;
    //! using STD copy function. Param 1 = the resource starting index
    //!                             Param 2 = the resource ending index
    //!                             Param 3 = the destined variable starting index
    std::copy( aNPP.mNPP.begin(), aNPP.mNPP.end(), this->mNPP.begin() );
    std::copy( aNPP.mNPPOverArea.begin(), aNPP.mNPPOverArea.end(), this->mNPPOverArea.begin() );
}

/*! clone()
 * \brief a Virtual Copy Constructor
 * \details this is a virtual function, it will return a newly created object
 *              of NPP by calling the copy constructor with the parameter referenced to iself
 * \return NPP* pointer points to the address of newly generated NPP object.
 * \author Ming Chang
 */
NPP* NPP::clone() const {
    return new NPP( *this );
}

NPP::~NPP(){
}

bool NPP::XMLParse( const xercesc::DOMNode* aNode ){
    // assume we were passed a valid node.
    assert( aNode );

    // Whether parsing is successful.
    bool success = true;
    
    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "initial-value" ) {
            mInitialNPP = XMLHelper<double>::getValue( curr );
            mCurrentNPP = mInitialNPP;
            mNPP[ CarbonModelUtils::getStartYear() ] = mInitialNPP;
            mNPPOverArea[ CarbonModelUtils::getStartYear() ] = mInitialNPP;
            mUnitPerArea = false;
        }
        else if ( nodeName == "initial-value-per-area" ) {
            mInitialNPP = 0;
            mCurrentNPP = mInitialNPP;
            mNPP[ CarbonModelUtils::getStartYear() ] = mInitialNPP;
            mNPPOverArea[ CarbonModelUtils::getStartYear() ] = XMLHelper<double>::getValue( curr );
            mUnitPerArea = true;
        }
        else {
            success = false;
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing NPP." << endl;
        }
    }

    return success;
}

const string& NPP::getXMLNameStatic(){
    static const string XML_NAME = "npp";
    return XML_NAME;
}

void NPP::toInputXML( std::ostream& aOut, Tabs* aTabs ) const {    
    /*!
     * \ write the element for each carbon_stock to the
     * \ output.xml
     */

    // write the opening tag
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "");

    // start write the xml for the class members
    if ( mUnitPerArea == false ) {
        XMLWriteElement( mInitialNPP, "initial-value", aOut, aTabs, 0, "");
    }
    else {
        XMLWriteElement( mInitialNPP, "initial-value-per-area", aOut, aTabs, 0, "" );
    }


    // finished writing xml for the class members

    // write the closing tag
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

void NPP::toDebugXML( const int aPeriod, std::ostream& aOut,
                      Tabs* aTabs ) const {
    // write the opening tag
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "");

    // start write the xml for the class members
    if ( mUnitPerArea == false ) {
        XMLWriteElement( mInitialNPP, "initial-value", aOut, aTabs );
    }
    else {
        XMLWriteElement( mInitialNPP, "initial-value-per-area", aOut, aTabs );
    }

    // finished writing xml for the class members
    const Modeltime* modeltime = scenario->getModeltime();
    int aYear = modeltime->getper_to_yr( aPeriod );
    
    XMLWriteElement( mNPP[ aYear ] , "NPP", aOut, aTabs );
    XMLWriteElement( mNPPOverArea[ aYear ] , "NPPperUnitArea", aOut, aTabs );
    
    // write the closing tag
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

void NPP::accept( IVisitor* aVisitor, const int aPeriod ) const {
}
/*! transfer
 * \brief return double - current NPP value or 0
 * \param aEnvInfo - environmental information for current carbon model
 *           aFlowType - current flow type
 *           aYear - current year
 * \details This function calculates the current NPP value in
            box flow, luc-flow-in, and luc-flow-out
 * \author Ming Chang and Jim Naslund
 */
double NPP::transfer( const EnvironmentalInfo* aEnvInfo,
                      FlowType aFlowType, const int aYear ){
    //This sets the value for the current year.  This is done because the
    //array is initialized full of zeros
    double currLandUseValue = aEnvInfo->getLandUse( aYear );
    if ( mUnitPerArea == true && aYear > CarbonModelUtils::getStartYear() ) {
        mNPPOverArea[ aYear ] = mNPPOverArea[ aYear - 1 ];
        mCurrentNPP = mNPPOverArea[ aYear ] * currLandUseValue;
    }

    if( aYear >= CarbonModelUtils::getStartYear() &&
        aYear <= CarbonModelUtils::getEndYear()) {
        mNPP[ aYear ] = mCurrentNPP;
    }

    // NPP never loses its store so it simply returns its inital amount
    // every time this is called.  This should only happen for BoxTransfers.
    if( aFlowType == eBoxFlow ) {
        return mCurrentNPP;
    }
    else if( aFlowType == eLUCFlowOut || aFlowType == eLUCFlowIn ){
        double prevLandUse = aEnvInfo->getLandUse( aYear - 1 );
        double currLandUse = aEnvInfo->getLandUse( aYear );
        double landChange = currLandUse - prevLandUse;

        if( util::isEqual( prevLandUse, (double)0 ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            // Exits and does nothing -- not sure if this is appropriate.
     //       mainLog << "Divide by zero because land allocation is zero." << endl;
            return 0;
        }

        double percentageLandChange = landChange / prevLandUse;
        if( percentageLandChange != 0 ) {
                        
            mCurrentNPP *= ( currLandUse / prevLandUse );
            
            mNPP[ aYear ] = mCurrentNPP;
            
        } //! end of ( percentageLandChange != 0 )
        else {
            return 0;
        } //! end of else

//        if ( mUnitPerArea == false && aYear == CarbonModelUtils::getStartYear() ) {
        if ( mUnitPerArea == false ) {
            double currLandUseValue = aEnvInfo->getLandUse( aYear );
            if ( currLandUse > util::getVerySmallNumber() ) {
                mNPPOverArea[ aYear ] = mNPP[ aYear ] / currLandUseValue;
            }
        }

    }

    return 0;
}

void NPP::addToStock( double aValueToAdd, const int aYear ){
    // NPP should not be added to.
    assert( false );
}

const double NPP::getStock( const int aYear ){
    if( aYear >= CarbonModelUtils::getStartYear() &&
        aYear <= CarbonModelUtils::getEndYear()) {
        return mNPP[ aYear ];
    }
    return 0;
}

void NPP::setCurrentStock( const int aYear ){
    mCurrentNPP = mNPP[ max( CarbonModelUtils::getStartYear(), aYear - 1 ) ];
}

/*! getNPPOverAreaRatio
 * \brief return double 
 * \param aYear
 * \details This function returns the NPP per unit by the giving year
 * \author Ming Chang
 */
double NPP::getNPPOverAreaRatio( const int aYear ){
    return mNPPOverArea[ aYear ];
}


/*! deductFromStock
 * \brief reduce stock value
 * \details This function dedict the current carbon stock value with the new value from parameter
 *
 * \param aDeductValue the value used to subtract the current carbon stock
 *        aYear the year of the carbon stock
 * \author Ming Chang
 */
void NPP::deductFromStock( const double aDeductValue, const int aYear ){
    mCurrentNPP -= aDeductValue;
    mNPP[ aYear ] = mCurrentNPP;
    return;
}

/*! modifyCarbonStock
 * \brief replace stock value
 * \details This function modify the current carbon stock value with the new value from parameter
 *
 * \param aNewValue the value used to replace the current carbon stock
 *        aYear the year of the carbon stock
 * \author Ming Chang
 */
void NPP::modifyCarbonStock( const double aNewValue, const int aYear ) {
    mCurrentNPP = aNewValue;
    mNPP[ aYear ] = mCurrentNPP;
    return;
}; // end of void NPP::modifyCarbonStock( const double aNewValue, const int aYear )

/*! getTurnoverTimescale
 * \brief return mTurnoverTimescale
 * \details This function returns the turnover timescale value of each stock.
 *          Hence the NPP does not have one, it will return 0
 * \return the turnoverTimescale value in double
 * \author Ming Chang
 */
double NPP::getTurnoverTimescale() {
    return 0;
} // end of double NPP::getTurnoverTimescale() 

/*! copyBoxType
 * \brief copy the Box Type for the associated flow
 * \param BoxType aCarbonBoxType
 * \details This function copy the box type that perform the npp flow
 * \author Ming Chang
 */

void NPP::copyBoxType( BoxType aCarbonBoxType ) {
    mCarbonBoxType = aCarbonBoxType;
    return;
}
BoxType NPP::returnBoxType() {
    return mCarbonBoxType;
}
