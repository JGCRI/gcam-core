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
 * \file acarbon_flow.cpp
 * \ingroup Objects
 * \brief The ACarbonFlow class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "ccarbon_model/include/acarbon_flow.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "ccarbon_model/include/environmental_info.h"
#include "util/base/include/ivisitor.h"
#include "ccarbon_model/include/summer_box.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Default constructor.
 */
ACarbonFlow::ACarbonFlow()
{
}
ACarbonFlow::ACarbonFlow( FlowType aType ):
mType( aType )
{
}
ACarbonFlow::ACarbonFlow( const ACarbonFlow& aCarbonFlow ){
	this->mBoxType = aCarbonFlow.mBoxType;
	this->mType = aCarbonFlow.mType;
	this->mFraction = aCarbonFlow.mFraction;
	this->mTargetName = aCarbonFlow.mTargetName;
	this->mTarget = aCarbonFlow.mTarget;	//! this part might cause error, might point to same address as template.
											//! need call completeInit to reset target
}

ACarbonFlow::~ACarbonFlow(){
}


bool ACarbonFlow::XMLParse( const xercesc::DOMNode* aNode ){
    // assume we were passed a valid node.
    assert( aNode );

    // Whether parsing is successful.
    bool success = true;
    
    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();
    mTargetName = XMLHelper<string>::getAttr( aNode, "target" );
    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if( nodeName == "fraction" ){
            mFraction = XMLHelper<double>::getValue( curr );
        }
        else if( XMLDerivedClassParse( curr ) ){
        }
        else {
            success = false;
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing CarbonBox." << endl;
        }
    }
    return success;
}

void ACarbonFlow::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    //TODO: implement
	if ( getXMLName() == "box-flow" || getXMLName() == "luc-flow-out"){
		XMLWriteElementTag( getXMLName(), aOut, aTabs, "target", getTargetName() );
		XMLWriteElement( getFraction(), getXMLNameStatic(), aOut, aTabs, 0, "" );
		XMLWriteClosingTag( getXMLName(), aOut, aTabs );
	}
	else{
		XMLWriteOpeningTag( getXMLName(), aOut, aTabs, "");
		XMLWriteElement( getFraction(), getXMLNameStatic(), aOut, aTabs, 0, "" );
		XMLWriteClosingTag( getXMLName(), aOut, aTabs );
	}
}

void ACarbonFlow::toDebugXML( const int aPeriod, ostream& aOut,
                              Tabs* aTabs ) const {
    //TODO: implement
	if ( getXMLName() == "box-flow" ){
	XMLWriteElementTag( getXMLName(), aOut, aTabs, "target", getTargetName() );
	XMLWriteElement(getFraction(), getXMLNameStatic(), aOut, aTabs, aPeriod , "");
	XMLWriteClosingTag( getXMLName(), aOut, aTabs );
	}
	else{
	XMLWriteOpeningTag( getXMLName(), aOut, aTabs, "");
	XMLWriteElement(getFraction(), getXMLNameStatic(), aOut, aTabs, aPeriod, "");
	XMLWriteClosingTag( getXMLName(), aOut, aTabs );
	}
}

void ACarbonFlow::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitCarbonFlow( this, aPeriod );
    aVisitor->endVisitCarbonFlow( this, aPeriod );
}

/*!
 * \brief Returns a double used for error checking.
 * \details Returns this flow's fraction if its type matches the passed in type;
 *          0 otherwise.  This function is used by carbon containers to sum
 *          the fractions of a given flow for error checking purposes.
 * \param aFlowType the type of flow that is being error checked.
 */
double ACarbonFlow::getNumForErrorChecking( FlowType aFlowType ) const {
    return aFlowType == mType ? mFraction : 0;
}

/*!
 * \brief Returns a double used for error checking.
 * \details Returns this flow's fraction if its type matches the passed in type;
 *          0 otherwise.  This function is used by carbon containers to sum
 *          the fractions of a given flow for error checking purposes.
 * \param aFlowType the type of flow that is being error checked.
 */
bool ACarbonFlow::matches( FlowType aFlowType ) const {
    return aFlowType == mType;
}

/*!
 * \brief Completes initializations.
 * \details Initializes the mTarget member of this flow using the map that is
 *          stored in the parent carbon box model.  If the target of the flow
 *          is "stay", it means the flow should point to the summer container
 *          for this land leaf's conceptual root.
 * \param aParent the parent carbon box model.
 */
void ACarbonFlow::completeInit( CarbonBoxModel& aParent ){

	//! Find out whether the flow is either a LUC-Flow-Out or a Box-flow
	//! If it is a LUC-Flow-Out, the target should be setup to the SummerBox
	//! else set the target based by the mTargetName

	if ( CarbonModelUtils::flowTypeToString( this->getFlowType() ) == "LUCFlowOut") {
		
		std::string tempTargetName = this->getTargetName();
		std::transform( tempTargetName.begin(),
						tempTargetName.end(),
						tempTargetName.begin(),
						::tolower );
		if ( tempTargetName == "atmosphere" ) {
			mTarget = aParent.getBoxByName( tempTargetName );
		}
		else {
			mTarget = SummerBox::getInstance()->getContainer( aParent.getKey() );
		}

	} // end of if ( CarbonModelUtils::flowTypeToString( this->getFlowType() ) == "LUCFlowOut")
	else {
		//! possible case: if the target is not set or set to stay
		//! provide a Warning message

		if ( ( mTargetName == "stay" ) || ( mTargetName == "" ) ) {
			std::string tempTargetName = CarbonModelUtils::boxTypeToString( mBoxType );
			std::transform( tempTargetName.begin(),
							tempTargetName.end(),
							tempTargetName.begin(),
							::tolower );
			if ( mTargetName == "stay" ) {
				ILogger& mainLog = ILogger::getLogger( "main_log" );
				mainLog.setLevel( ILogger::WARNING );
				mainLog << "target is not correctly specified: " << mTargetName
					    << ". Set the target for Box-Flow to itself, "<< tempTargetName
						<< ", to preserve Carbon." << endl;
			} // end of if ( mTargetName == "stay" )
			else {
				ILogger& mainLog = ILogger::getLogger( "main_log" );
				mainLog.setLevel( ILogger::WARNING );
				mainLog << "target is not specified: Set the target for Box-Flow to itself, "
						<< tempTargetName
						<< ", to preserve Carbon." << endl;
			} // end of else for if ( mTargetName == "stay" )

			mTarget = aParent.getBoxByName( tempTargetName );
		} // end of if ( ( mTargetName == "stay" ) || ( mTargetName == "" ) )
		else {
			mTarget = aParent.getBoxByName( mTargetName ); 
		} // end of else for if ( ( mTargetName == "stay" ) || ( mTargetName == "" ) )
    } // end of else for if ( CarbonModelUtils::flowTypeToString( this->getFlowType() ) == "LUCFlowOut")
}

/*! getTargetName
 * \brief Returns the name of this flow's target.
 * \return name of this flow's target.
 */
const string& ACarbonFlow::getTargetName() const {
    return mTargetName;
}

/*! TransferIfOfType
 * \brief Transfers carbon from this box to another if it the right type.
 * \details Transfers the passed in value to this flow's target if the flow
 *          matches the passed in type.  This function simply calls transfer()
 *          or returns.
 * \param aValue the amount to transfer.
 * \param aEnvInfo environmental info object.
 * \param aYear the year.
 * \param aBoxType the type of box.
 * \param aFlowType the type of flow this flow must match for the transfer
 *        to be performed.
 */
void ACarbonFlow::transferIfOfType( const double aValue, const EnvironmentalInfo* aEnvInfo,
                                    const int aYear, const BoxType aBoxType,
                                    const FlowType aFlowType ){
										
	if( aFlowType == mType ) {
		if ( getFlowType() == eLUCFlowOut ) {
			transfer( aValue, aEnvInfo, aYear, CarbonModelUtils::stringBoxNameToType( this->getTargetName() ) );		 
		}
		else
		{	
			transfer( aValue, aEnvInfo, aYear, aBoxType );
		}
    }
}

/*! getFraction
 * \brief Returns the fraction of this flow.
 * \details Returns the fraction of the carbon container's carbon that should
 *          be sent to the target carbon container.
 * \return the fraction.
 */
double ACarbonFlow::getFraction() const {
    return mFraction;
}
/*! getXMLNamestatic
 * \brief Returns the XML_NAME
 * \details Returns the XML_NAME
 */
const std::string& ACarbonFlow::getXMLNameStatic() {
	static const string XML_NAME = "fraction";
	return XML_NAME;
}

FlowType ACarbonFlow::getFlowType() const{
	return mType;
}

void ACarbonFlow::setTargetName( const std::string aTargetName ) {
	mTargetName = aTargetName;
}
void ACarbonFlow::copyBoxType( BoxType aCarbonBoxType ){
	mBoxType = aCarbonBoxType ;
	return;
}

/*! setDefaultLUCFlowOut()
 * \brief initialize LUCFlowOut object
 * \details set the flow targetName to the specified targetNAme
			set the fraction value to 1
			set the flow type to eLUCFlowOut
 * \param: aTargetName - - const string
 */
void ACarbonFlow::setDefaultLUCFLowOut( const std::string aTargetName ) {
	this->mFraction = 1;
	this->setTargetName( aTargetName );
	this->mType = eLUCFlowOut;
}
