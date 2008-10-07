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
#include "ccarbon_model/include/luc_carbon_summer.h"

using namespace std;
using namespace xercesc;

/*!
 * \brief Default constructor.
 */
ACarbonFlow::ACarbonFlow()
{
    mTargetType = eAnyBox;
}
ACarbonFlow::ACarbonFlow( FlowType aType ):
mType( aType )
{
}

/*! ACarbonFlow( const ACarbonFlow& aCarbonFlow )
 * \brief a Copy Constructor for ACarbonFlow
 * \details create a new ACarbonFlow object or derived class and initialize all the
 *             variables with either deep copy of the member object variable or shallow copy
 *             of the member variable.
 * \param aCarbonFlow const reference of ACarbonFlow object
 * \author Ming Chang
 */
ACarbonFlow::ACarbonFlow( const ACarbonFlow& aCarbonFlow ){
    this->initialization();    //! calling initialization 
    this->mBoxType = aCarbonFlow.mBoxType;
    this->mType = aCarbonFlow.mType;
    this->mFraction = aCarbonFlow.mFraction;
    this->mTargetName = aCarbonFlow.mTargetName;
    this->mTarget = aCarbonFlow.mTarget;    //! this part might cause error, might point to same address as template.
                                            //! need call completeInit to reset target
}

/*! initialize()
 * \brief Initialization wrapper function
 * \details Initialize member variables of this object
 * \author Ming Chang
 */
void ACarbonFlow::initialization(){
    this->mBoxType = eAnyBox;
    this->mFraction = 0;
    this->mTarget = NULL;
    this->mType = eAnyFlow;
    this->mTargetName = "stay";
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

/*!
 * \brief Write out the object information to output.xml
 * \details Run through the objects and display the ouput to output.xml
 * \param aOut a ostream of the output file
 *          aTabs tab indent for each sentech of output
 * \author Ming Chang
 */
void ACarbonFlow::toInputXML( ostream& aOut, Tabs* aTabs ) const {
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
bool ACarbonFlow::matches( FlowType aFlowType ) const {
    return aFlowType == mType;
}

/*!
 * \brief Completes initializations.
 * \details Initializes the mTarget member of this flow using the map that is
 *          stored in the parent carbon box model.  If the target of the flow
 *          is "stay", it means the flow should point to the summer container
 *          for this land leaf's conceptual root.
 * \param aParentBoxModel the parent carbon box model.
 * \author Ming Chang
 */
void ACarbonFlow::completeInit( CarbonBoxModel& aParentBoxModel ){

    //! Find out whether the flow is either a LUC-Flow-Out or a Box-flow
    //! If it is a LUC-Flow-Out, the target should be setup to the CarbonSummer
    //! else set the target based by the mTargetName
    //! TODO? in case of code optimization, maybe we can change the name list from
    //!          vector into a hash table or a map. So, the target name verification
    //!          will not need to perform O(k) searches but just do a O(1) search


    //! Obtain a list of boxes name from parent Model and check if the target name
    //! matches any boxes in the model.

    std::string targetName = this->getTargetName();
    //! obtain a list of boxes name from this carbon model
    std::vector<std::string> boxesName = aParentBoxModel.getBoxNames();
    bool verify = false;
    
    boxesName.push_back( "" );    //! empty targetName
    boxesName.push_back( "stay" ); //! special case stay


    //! check if any carbon boxes matched with the target name
    for ( std::vector<std::string>::const_iterator vecIter = boxesName.begin();
          vecIter != boxesName.end(); vecIter++ ){
              if ( targetName == (*vecIter) ){
                  verify = true;
                  break;
              } //! end of if (targetName == *vecIter )
    } //! end of for ( std::vector<std::string>::const_iterator vecIter = boxesName.begin(); vecIter != boxesName.end(); vecIter++ )
    
    //! set the target name to stay if target name is not correct
    if ( verify == false ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "target name is not correctly specified: " << mTargetName
                        << ". Set the target name to stay in order to preserve Carbon." << endl;
        this->mTargetName = "stay";
    } //! end of if ( verify == false )
    
    if ( CarbonModelUtils::flowTypeToString( this->getFlowType() ) == "LUCFlowOut") {
        
        std::string tempTargetName = this->getTargetName();
        std::transform( tempTargetName.begin(),
                        tempTargetName.end(),
                        tempTargetName.begin(),
                        ::tolower );

        std::string atmName = CarbonModelUtils::boxTypeToString( eAtmosphere );
        std::transform( atmName.begin(),
                        atmName.end(),
                        atmName.begin(),
                        ::tolower );
        if ( tempTargetName == atmName ) {
            mTarget = aParentBoxModel.getBoxByName( tempTargetName );
        }
        else {
            mTarget = CarbonSummer::getInstance()->getContainer( aParentBoxModel.getKey() );
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

            mTarget = aParentBoxModel.getBoxByName( tempTargetName );
        } // end of if ( ( mTargetName == "stay" ) || ( mTargetName == "" ) )
        else {
            mTarget = aParentBoxModel.getBoxByName( mTargetName ); 
        } // end of else for if ( ( mTargetName == "stay" ) || ( mTargetName == "" ) )
    } // end of else for if ( CarbonModelUtils::flowTypeToString( this->getFlowType() ) == "LUCFlowOut")
    
    setTargetFlowType();
}

/*! getTargetName
 * \brief Returns the name of this flow's target.
 * \return name of this flow's target
 * \author Ming Chang  
 */
const string& ACarbonFlow::getTargetName() const {
    return mTargetName;
}

/*! 
 * \brief Set the box type of this flow's target.
 * \return box type of this flow's target
 * \author Steve Smith
 */
void ACarbonFlow::setTargetFlowType( ) {
    mTargetType = CarbonModelUtils::stringBoxNameToType( mTargetName );
}

/*! getTargetName
 * \brief Returns the box type of this flow's target.
 * \return box type of this flow's target
 * \author Steve Smith  
 */
BoxType ACarbonFlow::getTargetFlowType() const {
    return mTargetType;
}

/*! TransferIfOfType
 * \brief Transfers carbon from this box to another if it the right type.
 * \details Transfers the passed in value to this flow's target if the flow
 *          matches the passed in type.  This function simply calls transfer()
 *          or returns.
 * \param aValue the amount to transfer.
           aEnvInfo environmental info object.
           aYear the working year.
           aBoxType the type of box.
           aFlowType the type of flow this flow must match for the transfer
                    to be performed.
 * \author Ming Chang and Jim Naslund
 */
void ACarbonFlow::transferIfOfType( const double aValue, const EnvironmentalInfo* aEnvInfo,
                                    const int aYear, const BoxType aBoxType,
                                    const FlowType aFlowType ){
                                        
    if( aFlowType == mType ) {
        if ( getFlowType() == eLUCFlowOut ) {
            transfer( aValue, aEnvInfo, aYear, this->getTargetFlowType() );    
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
 * \author Ming Chang
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

/*! getFlowType
 * \brief Returns the type of this flow.
 * \details Returns the type of the this flow in enum format
 * \return Flowtype the enum of flows.
 * \author Ming Chang
 */
FlowType ACarbonFlow::getFlowType() const{
    return mType;
}

/*! setTargetName( const std::string aTargetName )
 * \brief set the mTargetName variable to user defined target
 * \details reset the targetName variable to the user defined target name passed
 * \          by parameter.
 * \param aTargetName the target box's name
 * \author Ming Chang
 */
void ACarbonFlow::setTargetName( const std::string aTargetName ) {
    mTargetName = aTargetName;
}

/*!
 * \brief set the fraction value for this target
 * \param aFraction the target fraction
 * \author Steve Smith
 */
void ACarbonFlow::setFraction( const double aFraction ) {
    mFraction = aFraction;
}

/*! copyBoxType( BoxType aCarbonBoxType )
 * \brief copy the current box type to the flow objects
 * \details store the Box Type enum to the carbon flow object
 * \param aCarbonBoxType box type from enum
 * \author Ming Chang
 */
void ACarbonFlow::copyBoxType( BoxType aCarbonBoxType ){
    mBoxType = aCarbonBoxType ;
    return;
}

/*! setDefaultLUCFlowOut()
 * \brief initialize LUCFlowOut object
 * \details set the flow targetName to the specified targetNAme
            set the fraction value to 1
            set the flow type to eLUCFlowOut
 * \param aTargetName target box's name
 * \author Ming Chang
 */
void ACarbonFlow::setDefaultLUCFLowOut( const std::string aTargetName ) {
    this->mFraction = 1;
    this->setTargetName( aTargetName );
    this->mType = eLUCFlowOut;
}
