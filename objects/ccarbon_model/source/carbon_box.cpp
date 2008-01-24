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
 * \file carbon_box.cpp
 * \ingroup objects
 * \brief CarbonBox class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "ccarbon_model/include/carbon_box.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/carbon_flow_factory.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/dependency_finder.h"
#include "ccarbon_model/include/npp.h"
#include "ccarbon_model/include/luc_flow_in_to_box.h"
#include "ccarbon_model/include/carbon_stock.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "util/base/include/util.h"
#include "ccarbon_model/include/luc_carbon_summer_stock.h"
#include "ccarbon_model/include/luc_flow_out.h"

using namespace std;
using namespace xercesc;

typedef boost::ptr_list<ACarbonFlow>::iterator CarbonFlowIter;
typedef boost::ptr_list<ACarbonFlow>::const_iterator CarbonFlowConstIter;

/*!
 * \brief Default constructor.
 */
CarbonBox::CarbonBox()
: mStock( 0 )
{
}
/*! CarbonBox( const CarbonBox& aCarbonBox )
 * \brief  copy constructor
 * \details This is a copy constructor for CarbonBox. It will take in a reference of 
 * \        a CarbonBox object and make a exact copy of it in a newly allocated memory.
 * \param aCarbonBox - a const reference of a CarbonBoxModel
 * \return a newly generated CarbonBox object with exact copied value of this CarbonBox
 */
CarbonBox::CarbonBox( const CarbonBox& aCarbonBox ):mStock( aCarbonBox.mStock->clone() ){
    //! assign the value for mName from argument aCarbonBox
    this->mName = aCarbonBox.mName;
    
    //! assign the value for mType from argument aCarbonBox
    this->mType = aCarbonBox.mType;
    
    //! a iterator for ACarbonFlow ptr_list
    boost::ptr_list<ACarbonFlow>::const_iterator ptrListIter;
    
    //! copy the CarbonFlow from argument aCarbonBox and add to mCarbonFlow
    for ( ptrListIter = aCarbonBox.mCarbonFlows.begin(); 
          ptrListIter != aCarbonBox.mCarbonFlows.end();
          ptrListIter++ ){
              auto_ptr<ACarbonFlow> newFlow ( ptrListIter->clone() );
              this->internalAddFlow( newFlow );
    }    
}

/*!
 * \brief Destructor.
 * \details Deallocates stock, and all ACarbonFlow that were created on the heap.
 */
CarbonBox::~CarbonBox(){
    //! free the CarbonStock
    ICarbonStock* tempStock = this->mStock.release();
    delete( tempStock );

    //! free the CarbonFlows
    this->mCarbonFlows.clear();    //! All pointers held in list are deleted.
}

/*! clone()
 * \brief  virtual copy constructor
 * \details Notice, there is no virtual copy constructor or virtual constructor in C++.
 * \        Therefore, clone() is a virtual function that will perform desired performance as
 * \        virtual copy constructor by indirectly calling the copy constructor through this
 * \        virtual function.
 * \return a newly generated CarbonBox object with exact copied value of this CarbonBox
 */
CarbonBox* CarbonBox::clone() const {
    return ( new CarbonBox( *this ) );
}

/*! Constructor 
 * \brief Constructor to create boxes that are not parsed from XML
 * \details construct a carbon box with stock as SummerCarbonStock inside CarbonSummer
 * \param aType- a BoxType 
 */
CarbonBox::CarbonBox( BoxType aType )
: mStock( new SummerCarbonStock ),
  mType( aType ){
}

bool CarbonBox::XMLParse( const xercesc::DOMNode* aNode ){
    // assume we were passed a valid node.
    assert( aNode );

    // Whether parsing is successful.
    bool success = true;
    
    // get the children of the node.
    DOMNodeList* nodeList = aNode->getChildNodes();

    mName = XMLHelper<string>::getAttr( aNode, XMLHelper<string>::name() );
    assignEnum( XMLHelper<string>::getAttr( aNode, "type" ) );

    
    // loop through the children
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == XMLHelper<void>::text() ) {
            continue;
        } //! end of  if( nodeName == XMLHelper<void>::text() ) 
        else if( CarbonFlowFactory::isOfType( nodeName ) ){

            // Create a carbon box flow by allowing it to parse itself
            // and add it to the list of carbon boxes.
            // The list will handle deallocation.

            auto_ptr<ACarbonFlow> newFlow( CarbonFlowFactory::create( nodeName ) );
            if( newFlow.get() == 0 ){
                // Unknown carbon flow, an error has already been printed so
                // attempt to stop without crashing.
                break;
            }
            newFlow->XMLParse( curr );
            std::string tempBoxName = this->getName();
            std::transform( tempBoxName.begin(), 
                            tempBoxName.end(),    
                            tempBoxName.begin(),
                            ::tolower );
            //! check to see if target has appropriately setup in luc-flow-out
            if ( newFlow->getXMLName() == "luc-flow-out" ) {

    
                if ( tempBoxName == "atmosphere" || tempBoxName == "npp" ) {
                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                    mainLog.setLevel( ILogger::WARNING );
                    mainLog << this->getName()
                            <<" box should not have any LUC-Flow-Out tag: "
                            << " LUC-Flow-Out will not proceed for Carbon Box, "
                            << this->getName()<<"."<<endl;
                }
                else {
                    if ( newFlow->getTargetName() == "" ) {
                        ILogger& mainLog = ILogger::getLogger( "main_log" );
                        mainLog.setLevel( ILogger::WARNING );
                        mainLog << "Target for LUC-flow-Out is missing: "
                                << "set the target for CarbonBox, "<< mName
                                << ", to its own."<< endl;
                        newFlow->setTargetName( "stay" );
                    } //! end of if ( newFlow->getTargetName() == "" )
                }
            } //! end of if ( newFlow->getXMLName() == "luc-flow-out" )
            else {
                if ( tempBoxName == "atmosphere" ) {
                    if ( newFlow->getXMLName() == "box-flow" ) {
                        ILogger& mainLog = ILogger::getLogger( "main_log" );
                        mainLog.setLevel( ILogger::WARNING );
                        mainLog << this->getName()
                                <<" box should not have any box-flow tag: "
                                << " box-flow will not proceed for Carbon Box, "
                                << this->getName()<<"."<<endl;
                    }
                }
            }


            if (  newFlow->getXMLName() == "luc-flow-out" ) {
                if ( tempBoxName == "atmosphere" || tempBoxName == "npp" ) {
                    newFlow.release();
                }
                else {
                    internalAddFlow( newFlow );
                }
            }
            else {    // else if it is a box-flow
                if ( tempBoxName == "atmosphere" ) {
                    newFlow.release();
                }
                else {
                    internalAddFlow( newFlow );
                }
            }
        } //! end of if( CarbonFlowFactory::isOfType( nodeName ) )
        else if( nodeName == CarbonStock::getXMLNameStatic() ){
            mStock.reset( new CarbonStock );
            mStock->XMLParse( curr );

        } //! end of  if( nodeName == CarbonStock::getXMLNameStatic() )
        else if( nodeName == NPP::getXMLNameStatic() ){
            mStock.reset( new NPP );
            mStock->XMLParse( curr );
        } //! end of  if( nodeName == NPP::getXMLNameStatic() )
        else {
            success = false;
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing CarbonBox." << endl;
        } //! end of else
    } //! end of  for ( unsigned int i = 0; i < nodeList->getLength(); i++ )
    return success;
}

/*!
 * \brief Set this box's enum.
 * \details Sets this box's enum bassed on the passed in string.
 * \param aTypeString the type of box.
 */
void CarbonBox::assignEnum( const string& aTypeString ){
    if( aTypeString == "vegetation" ){
        mType = eVegetation;
    }
    else if( aTypeString == "soil" ){
        mType = eSoil;
    }
    else if( aTypeString == "litter" ){
        mType = eLitter;
    }
    else if( aTypeString == "NPP" ){
        mType = eNPP;
    }
    else if( aTypeString == "atmosphere" ){
        mType = eAtmosphere;
    }
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Unrecognized CarbonBox type." << endl;
    }
}

void CarbonBox::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    /*! 
        \ Write the Carbon Box stock to output.xml
     */

    //! Write an opening tag for Carbon-box
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName(), 0, getName() );
    
    //! Write the carbon stock
    if ( mStock.get() ){
        mStock->toInputXML( aOut, aTabs );
    }

    //! run through the box-flows 
    for ( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
          flowIter != mCarbonFlows.end(); flowIter++){
                flowIter->toInputXML( aOut, aTabs );
    } //! end of for
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

void CarbonBox::toDebugXML( const int aPeriod, ostream& aOut,
                            Tabs* aTabs ) const {

    //! Write an opening tag for Carbon-box
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName(), 0, getName() );
    
    //! Write the carbon stock
    if ( mStock.get() ){
        mStock->toDebugXML( aPeriod, aOut, aTabs );
    }

    //! run through the box-flows 
    for ( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
          flowIter != mCarbonFlows.end(); flowIter++){
                flowIter->toDebugXML( aPeriod, aOut, aTabs );
    } //! end of for

    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

const string& CarbonBox::getXMLNameStatic(){
    static const string XML_NAME = "carbon-box";
    return XML_NAME;
}

void CarbonBox::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitCarbonBox( this, aPeriod );
    for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
         flowIter != mCarbonFlows.end(); ++flowIter ){
             flowIter->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitCarbonBox( this, aPeriod );
}

/*! matches()
 * \brief  see if current object matches with the aBoxType argument
 * \details user may use it to check if current box matches with the desired BoxType 
             of the parameter: eVegetation, eSoil, eLitter, eNPP, eAtmosphere, and eAnyBox
 * \return true or false
 */
bool CarbonBox::matches( const BoxType aBoxType ) const {
    return mType == aBoxType || aBoxType == eAnyBox;
}

/*!
 * \brief Complete initialization for this box.
 * \details Ensures that the sum of the fractions of each type of flow
 *          out of this box are valid. Calls completeInit on all the flows
 *          out of this box.
 *          During parsing LUCFlowIns were read from input.  These represent
 *          the fraction of carbon this box should get from a land use change
 *          flow in.  Thus, flows need to be added to this box's parent 
 *          carbon box model.
 * \param aNamesToBoxes a map of strings to carbon boxes representing flow targets.
 * \param aKey the parent carbon box model's unique key.
 * \param aParent a pointer to the parent carbon box model.
 */
void CarbonBox::completeInit( CarbonBoxModel& aParent ){
    //! Complete initialize the carbon flow in the Carbon Box 
    //! and also scan through to check if there is a flow for LUC-Flow-Out
    //! If not, create a carbon flow object with fraction to 1 and target
    //! to itself by default. Notice, only NPP box will not create a LUC-Flow-Out default box
    int LUCFlowOutCounter( 0 );
        
    std::string tempBoxName = this->getName();
    std::transform( tempBoxName.begin(),
                    tempBoxName.end(),
                    tempBoxName.begin(),
                    ::tolower );

    for( CarbonFlowIter flowIter = mCarbonFlows.begin();
        flowIter != mCarbonFlows.end(); ++flowIter ){
            flowIter->copyBoxType( this->getCarbonBoxType() );
            LUCFlowOutCounter += flowIter->matches( eLUCFlowOut ) ? 1 : 0;
            if ( flowIter->getTargetName() == "stay" ) { //! default condition if the target is not specified
                flowIter->setTargetName( this->getName() );
            }
            flowIter->completeInit( aParent );
    }
    
    //! If a LUC-Flow-Out does not exist from every physical land carbon box,
    //! create a default LUC_Flow-Out object here.
    if ( tempBoxName != "npp" && tempBoxName != "atmosphere" ) {
        if ( LUCFlowOutCounter == 0 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Missing a LUC-Flow-Out flow object for box,"
                    <<getName()
                    <<". Creating a default flow container" << endl;
            auto_ptr<ACarbonFlow> newFlow( CarbonFlowFactory::create( "luc-flow-out" ) );
            newFlow->setDefaultLUCFLowOut( this->getName() );
            internalAddFlow( newFlow );
            //! for project performance, I simply take a pointer for the last
            //! ACarbonFlow I add to mCarbonFlows vector list.
            //! I know the last component will be the newFlow. Therefore, 
            //! I will just call the completeInit for it. 
            //! In case of code clarity, this might not easy to understand as
            //! a simply for loop to find eLUCFlowOut instance in the vector.
            CarbonFlowIter flowIter = mCarbonFlows.end();
            flowIter--;
            flowIter->completeInit( aParent );
        }
    }

    int numBoxFlows = 0;
    int numLUCInFlows = 0;
    int numLUCOutFlows = 0;
    
    // Verify sum of flows (if flows exist).
    if( mCarbonFlows.size() > 0 ){
        // Count the number of flows.
        for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
            flowIter != mCarbonFlows.end(); ++flowIter ){
            numBoxFlows += flowIter->matches( eBoxFlow ) ? 1 : 0;
            numLUCOutFlows += flowIter->matches( eLUCFlowOut ) ? 1 : 0;
            numLUCInFlows += flowIter->matches( eLUCFlowIn ) ? 1 : 0;
        }
        
        double boxSum = 0;
        double LUCOutSum = 0;
        double LUCInSum = 0;
        //Sum the fractions if there are flows.
        for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
             flowIter != mCarbonFlows.end(); ++flowIter ) {
                boxSum += flowIter->matches( eBoxFlow ) ? flowIter->getFraction() : 0;
                LUCInSum += flowIter->matches( eLUCFlowIn ) ? flowIter->getFraction() : 0;
                LUCOutSum += flowIter->matches( eLUCFlowOut ) ? flowIter->getFraction() : 0;
        }

        // Write error if sum of the fractions of all the box flows do not add
        // up to 1.
        if( !util::isEqual( boxSum, 1.0 ) && numBoxFlows > 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Carbon box: " << mName << " box flows have invalid"
                    << " sum of " << boxSum << "." << endl;
        }

        // Write error if sum of the fractions of all the LUCIn flows do not add
        // up to 1.
        if( !util::isEqual( LUCInSum, 1.0 ) && numLUCInFlows > 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Carbon box: " << mName << " LUC-Flows-In has invalid"
                    << " sum of " << LUCInSum << "." << endl;
        }

        // Write error if sum of the fractions of all the LUCOut flows do not add
        // up to 1.
        if(  !util::isEqual( LUCOutSum, 1.0 ) && numLUCOutFlows > 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Carbon Box: " << mName << " LUC-Flow-Out has invalid"
                    << " sum of " << LUCOutSum << "." << endl;
        }
    }
    
    // Create new LUCIn flows and move into the parent carbon box container.
    // LUCIn flows need to be replaced because any current flows are pointing to 
    // the template carbon box model in the parent node instead of to this model.
    
    // The explicit copy breaks encapsulation as this will not copy any new parameters.
    // Would be good to find a better way to do this.
    
    while( numLUCInFlows > 0 ) {
        for( CarbonFlowIter flowIter = mCarbonFlows.begin();
             flowIter != mCarbonFlows.end(); ++flowIter ) {
            if ( flowIter->matches( eLUCFlowIn ) ) {
                // Get fraction from the current LUC Flow
                double currentFraction = flowIter->getFraction();

                // Delete old flow
                mCarbonFlows.release( flowIter );

                //Create new flow, which will point to this carbon box
                auto_ptr<ACarbonFlow> newFlow( new LUCFlowInToBox( this ) );
                
                // Set fraction
                newFlow->setFraction( currentFraction );

                // Note mType is not used unless flow is being added to SummerSubContainer
                // Move flow to parent
                aParent.addFlow( newFlow, mType );
                
                numLUCInFlows -= 1;
                break; // break out of for loop since iter is now not valid
            }
        }
    }
}


/*! setCurrentStock
 * \brief  stock assigner
 * \details assign the current stock value to the selected year
 * \param aYear - integer of selected year
 * \return void
 */
void CarbonBox::setCurrentStock( const int aYear ) {    
    mStock->setCurrentStock( aYear );
}

/*! doTransfersconst( EnvironmentalInfo* aEnvInfo, FlowType aFlowType, const int aYear )
 * \brief  actual carbon stock transfer process in between boxes
 * \details This is the actual carbon stock transfering process. It will first call the transfer()
 * \ to obtain the amount depleted from the carbon boxes and transfer the amounts in between each 
 * \ boxes based on the target.
 * \param aEnvInfo : EnvironmentalInfo* - a Land Usage History information object pointer
           aFlowType : FlowType 
           aYear : int
 */
void CarbonBox::doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                             const int aYear ){

    //! assign the current box type to the next level child, mStock.
    //! in this case, mStock knows the parent box type
    mStock->copyBoxType( this->getCarbonBoxType() );

    //! calling the transfer() to obtain the amount of carbon lost in boxFlow or LUC-Flow-Out
    double amountDepleted = mStock->transfer( aEnvInfo, aFlowType, aYear );
    
    if( amountDepleted < -util::getVerySmallNumber() ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Negative amount of carbon depleted from a carbon store. FlowType: " 
                << CarbonModelUtils::flowTypeToString( aFlowType ) << endl
                << " Amount Depleted: " << amountDepleted << endl;
    }
    //! This if condition is here for run-time optimization reason.
    //! if the amountDepleted == 0, the function will simply terminate instead continue
    //! processing the remaining loop of flow->transferIfOfType() with 0 amount lost or gaining.
    if( amountDepleted == 0 ){
        return;
    }
    
    //! process the carbon stock transfering in between current box to another sibling in same level
    //! of hierarchy.
    for( CarbonFlowIter flowIter = mCarbonFlows.begin();
         flowIter != mCarbonFlows.end(); ++flowIter ){
             flowIter->transferIfOfType( amountDepleted, aEnvInfo, aYear, mType, aFlowType );
    }
}

/*! acceptTransfer( double aCarbonValue, const int aYear, const BoxType aBoxType )
 * \brief  add the carbon value to the mStock
 * \details take in the carbon value and add tot he current carbon stock and modify the
 *            mstock vector for the assigned year, if and only if the BoxType belongs to the
 *            enum, BoxType.
 * \param aCarbonValue : double
           aYear : int
           aBoxType : BoxType
 */
void CarbonBox::acceptTransfer( double aCarbonValue, const int aYear,
                                const BoxType aBoxType ){

    if( matches( aBoxType ) ){
        double prevStock = mStock->getStock( aYear );
        mStock->addToStock( aCarbonValue, aYear );
    }
}

/*! addFlow( auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxTyp )
 * \brief  add a Flow into a ptr_list container
 * \details calling internalAddFlow to push a ACarbonFlow object into mCarbonFlows.
 * \param aBoxType - BoxType of the flow
 * \           aCarbonFlow - ACarbonFlow auto_ptr for an object
 */
void CarbonBox::addFlow( auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType ){
    internalAddFlow( aCarbonFlow );
}

void CarbonBox::addDependencies( DependencyFinder& aDepFinder ) const {
    for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
         flowIter != mCarbonFlows.end(); ++flowIter ){
            // Dependencies are only relevant for box flows
            if ( flowIter->matches( eBoxFlow ) ) {
                aDepFinder.addDependency( flowIter->getTargetName(), getName() );
            }
    }
}

/*!
 * \brief Adds a flow to this box.
 * \param aCarbonFlow the flow to add.
 */
void CarbonBox::internalAddFlow( auto_ptr<ACarbonFlow> aCarbonFlow ){
    mCarbonFlows.push_back( aCarbonFlow.release() );
}

/*!
 * \brief Returns the name of this box.
 * \return name of this box.
 */
const string& CarbonBox::getName() const {
    return mName;
}

/*! getCarbonStock
 * \brief Return the carbonStock object pointer
 * \return the CarbonStock object pointer
 */
ICarbonStock* CarbonBox::getCarbonStock() {
    return mStock.get();
}

/*! getCarbonStock
 * \brief Return the carbonStock object pointer
 * \return the CarbonStock object pointer
 */
const double CarbonBox::getStock( const int aYear ) {
    return mStock->getStock( aYear );
}

/*! getCarbonBoxFlow
 * \brief Return the ACarbonFlow object pointer as the carbon flow of a selected carbon box
 * \return the CarbonFlowConstIter
 *
 * \param aTargetName -  a string of box flow target name
 */

const CarbonFlowConstIter CarbonBox::getCarbonBoxFlow( std::string aTargetName ) {

        for (CarbonFlowConstIter boxFlowIter = mCarbonFlows.begin(); 
         boxFlowIter != mCarbonFlows.end();
         boxFlowIter++) {
             if ( boxFlowIter->getTargetName() == aTargetName )
             {
                 return boxFlowIter;
             }
        }
        
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Unrecognized target name: " << aTargetName
                    << " Make sure the input file is correct." << endl;
        return mCarbonFlows.end();
}


/*! carbonBoxFlowNotFound
 * \brief Return the end of carbon flow object as indication flow is not found
 * \return the mCarbonFlow.end()
 */
const CarbonFlowConstIter CarbonBox::carbonBoxFlowNotFound() {
    return mCarbonFlows.end();
}

/*! getCarbonBoxType()
 * \brief  return BoxType.
 * \details return the BoxType of current CarbonBox object.
 * \return mType- a BoxType variable represent the ID of current CarbonBox.
 */
BoxType CarbonBox::getCarbonBoxType() {
    return mType;
}
