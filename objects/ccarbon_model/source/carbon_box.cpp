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
#include "ccarbon_model/include/summer_carbon_stock.h"
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
 * \brief: copy constructor
 * \detail: This is a copy constructor for CarbonBox. It will take in a reference of 
 * \		a CarbonBox object and make a exact copy of it in a newly allocated memory.
 * \param: aCarbonBox - a const reference of a CarbonBoxModel
 * \return: a newly generated CarbonBox object with exact copied value of this CarbonBox
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
	
	//! copy the FlowsFromParentBoxModel from argument aCarbonBox and add to mFlowsFromParentBoxModel
	for ( ptrListIter = aCarbonBox.mFlowsFromParentBoxModel.begin();
		  ptrListIter != aCarbonBox.mFlowsFromParentBoxModel.end();
		  ptrListIter++ ){
			  auto_ptr<ACarbonFlow> newFlow ( ptrListIter->clone() );
			  this->mFlowsFromParentBoxModel.push_back( newFlow.release() );
	}

	//! copy the LucFlowInbox from argument aCarbonBox and add to mLucFlowInBox
	for ( ptrListIter = aCarbonBox.mLucFlowInBox.begin();
		  ptrListIter != aCarbonBox.mLucFlowInBox.end();
		  ptrListIter++ ){			  
			  auto_ptr<ACarbonFlow> newFlow ( ptrListIter->clone() );
			  this->mLucFlowInBox.push_back( newFlow.release() );
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
	this->mCarbonFlows.clear();	//! All pointers held in list are deleted.
	this->mFlowsFromParentBoxModel.clear(); //! All pointers held in list are deleted.
	this->mLucFlowInBox.clear(); //! All pointers held in list are deleted.
}

/*! clone()
 * \brief: virtual copy constructor
 * \detail: Notice, there is no virtual copy constructor or virtual constructor in C++.
 * \		Therefore, clone() is a virtual function that will perform desired performance as
 * \		virtual copy constructor by indirectly calling the copy constructor through this
 * \		virtual function.
 * \return: a newly generated CarbonBox object with exact copied value of this CarbonBox
 */
CarbonBox* CarbonBox::clone() const {
	return ( new CarbonBox( *this ) );
}

/*! Constructor 
 * \brief Constructor to create boxes that are not parsed from XML
 * \detail: construct a carbon box with stock as SummerCarbonStock inside SummerBox
 * \param: aType- a BoxType 
 */
CarbonBox::CarbonBox( BoxType aType )
: mStock( new SummerCarbonStock ),
  mType( aType )
{
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
			else {	// else if it is a box-flow
				if ( tempBoxName == "atmosphere" ) {
					newFlow.release();
				}
				else {
					internalAddFlow( newFlow );
				}
			}
        } //! end of if( CarbonFlowFactory::isOfType( nodeName ) )

        // This is to create a flow from the containing box model to this box to control
        // what fraction of LUCFlowInToBoxModel for this box's type goes to this box.
        else if( nodeName == LUCFlowInToBox::getXMLNameStatic() ){
			
			std::string tempBoxName = this->getName();
			std::transform( tempBoxName.begin(),
							tempBoxName.end(),
							tempBoxName.begin(),
							::tolower );
			if ( tempBoxName != "atmosphere" && tempBoxName != "npp" ) {
				auto_ptr<ACarbonFlow> newFlow( new LUCFlowInToBox( this ) );
				newFlow->XMLParse( curr );
				// extra container used to preserve the value */
				mLucFlowInBox.push_back( newFlow.get() );
				internalAddParentFlow( newFlow );
			}
			else {
				ILogger& mainLog = ILogger::getLogger( "main_log" );
				mainLog.setLevel( ILogger::WARNING );
				mainLog << this->getName()
						<< " box should not have any LUC-Flow-In tag:"
						<< " LUC-Flow-In will not proceed for Carbon Box, "
						<< this->getName()<<"."<<endl;
			}

        } //! end of if( nodeName == LUCFlowInToBox::getXMLNameStatic() )
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

	//! run through the box-flow and write the box-flow fraction
	//! it will not print anything if the iterator is empty
	for ( CarbonFlowConstIter boxFlowIter = mCarbonFlows.begin();
		  boxFlowIter != mCarbonFlows.end();
		  boxFlowIter++){
			 if ( boxFlowIter->getXMLName() != "luc-flow-out" ) {
				 boxFlowIter->toInputXML( aOut, aTabs );
			 } //! end of if ( boxFlowIter->getXMLName() != "luc-flow-out" )
			 else {
				//! run through the luc-flow-in box and write the luc-flow fraction 
				//! it will not print anything if the iterator is empty
				for (CarbonFlowConstIter LucFlowInBoxIter = mLucFlowInBox.begin(); 
					 LucFlowInBoxIter != mLucFlowInBox.end();
					 LucFlowInBoxIter++){
						 LucFlowInBoxIter->toInputXML( aOut, aTabs );
				} //! end of (CarbonFlowConstIter boxFlowIter = mLucFlowInBox.begin(); boxFlowIter != mLucFlowInBox.end(); boxFlowIter++)
				boxFlowIter->toInputXML( aOut, aTabs );
			 } //! end of else
	} //! end of for( CarbonFlowConstIter boxFlowIter = mCarbonFlows.begin(); boxFlowIter != mCarbonFlows.end(); boxFlowIter++)
	
	XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

void CarbonBox::toDebugXML( const int aPeriod, ostream& aOut,
                            Tabs* aTabs ) const {
    /*! 
		\ Write the Carbon Box stock to output.xml
	 */

	//! Write an opening tag for Carbon-box
	XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName(), 0, getName() );
	
	//! Write the carbon stock
	if ( mStock.get() ){
		
		mStock->toDebugXML( aPeriod, aOut, aTabs );
	}

	//! run through the box-flow and write the box-flow fraction
	//! it will not print anything if the iterator is empty
	for (CarbonFlowConstIter boxFlowIter = mCarbonFlows.begin();
		 boxFlowIter != mCarbonFlows.end();
		 boxFlowIter++){

			 boxFlowIter->toDebugXML( aPeriod, aOut, aTabs );
	}

	//! run through the luc-flow box and write the luc-flow fraction 
	//! it will not print anything if the iterator is empty
	for (CarbonFlowConstIter boxFlowIter = mLucFlowInBox.begin(); 
		 boxFlowIter != mLucFlowInBox.end();
		 boxFlowIter++){

			 boxFlowIter->toDebugXML( aPeriod, aOut, aTabs );
	} 
	
	// TODO: I might want to empty the mLucFlowInBox container to save up the memory.
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
 * \brief: see if current object matches with the aBoxType argument
 * \detail: user may use it to check if current box matches with the desired BoxType 
 *\			of the parameter: eVegetation, eSoil, eLitter, eNPP, eAtmosphere, and eAnyBox
 * \return: true or false
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
			if ( flowIter->getTargetName() == "stay" ) { //! default condition if the target is not clearly specified
				flowIter->setTargetName( this->getName() );
			}
			flowIter->completeInit( aParent );
	}

	//! If the LUC-Flow-Out is not existing in the current model from every carbon box,
	//! it will automatically creat a LUC_Flow-Out object here.
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
	
	
	// Verify sum of flows (if flows exist).
    if( mCarbonFlows.size() > 0 ){
        int numBoxFlows = 0;
        int numLUCFlows = 0;
        // Count the number of flows.
        for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
            flowIter != mCarbonFlows.end(); ++flowIter ){
            numBoxFlows += flowIter->matches( eBoxFlow ) ? 1 : 0;
			numLUCFlows += flowIter->matches( eLUCFlowOut ) ? 1 : 0;
        }
		
		numLUCFlows += mFlowsFromParentBoxModel.size();

        double boxSum = 0;
        double LUCOutSum = 0;
		double LUCInSum = 0;
        //Sum the fractions if there are flows.
        if( numBoxFlows > 0 ){
            for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
                 flowIter != mCarbonFlows.end(); ++flowIter ){
                boxSum += flowIter->getNumForErrorChecking( eBoxFlow );
            }
            // Check to make sure the sum of the fractions of all the box flows
            // adds up to exactly 1.
            if( !util::isEqual( boxSum, 1.0 ) ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Carbon box: " << mName << " box flows have invalid"
                        << " sum of " << boxSum << "." << endl;
            }
        }

		//! verify if all the box flows, LUC-Flow-In, or LUC-Flow-Out does add up max to 1
		if( numLUCFlows > 0 ){
			for( CarbonFlowConstIter flowIter = mLucFlowInBox.begin();
				flowIter != mLucFlowInBox.end(); ++flowIter ){
					LUCInSum += flowIter->getNumForErrorChecking( eLUCFlowIn );
			}

			for ( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
				flowIter != mCarbonFlows.end(); ++flowIter ) {
					if ( flowIter->matches( eLUCFlowOut ) ) {
						LUCOutSum += flowIter->getNumForErrorChecking( eLUCFlowOut );
					}
			}

			// Check to make sure the sum of fractions of all the LUC flows adds
            // up to less than 1.
            if( !util::isEqual( LUCInSum, 1.0 ) ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::ERROR );
                mainLog << "Carbon box: " << mName << " LUC-Flows-In has invalid"
                        << " sum of " << LUCInSum << "." << endl;
            }

			if(  !util::isEqual( LUCOutSum, 1.0 ) ){
				ILogger& mainLog = ILogger::getLogger( "main_log" );
				mainLog.setLevel( ILogger::ERROR );
				mainLog << "Carbon Box: " << mName << " LUC-Flow-Out has invalid"
						<< " sum of " << LUCOutSum << "." << endl;
			}
		}
    }
	
    // Move all the flows that should be from the parent to this object into the parent
    // carbon box container.
    while( !mFlowsFromParentBoxModel.empty() ){
        boost::ptr_list<ACarbonFlow>::auto_type ptr =
        mFlowsFromParentBoxModel.release( mFlowsFromParentBoxModel.begin() );
        auto_ptr<ACarbonFlow> aptr( ptr.release() );
        aParent.addFlow( aptr, mType );
    }
}

/*! setCurrentStock
 * \brief: stock assigner
 * \detail: assign the current stock value to the selected year
 * \param: aYear - integer of selected year
 * \return: void
 */
void CarbonBox::setCurrentStock( const int aYear ) {    
    mStock->setCurrentStock( aYear );
}

void CarbonBox::doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                             const int aYear ){

    mStock->copyBoxType( this->getCarbonBoxType() );
	double amountDepleted = mStock->transfer( aEnvInfo, aFlowType, aYear );
	
    if( amountDepleted < 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Negative amount of carbon depleted from a carbon store. FlowType: " << aFlowType
                << "Amount Depleted: " << amountDepleted << endl;
    }
    //TODO: do we need this?
    if( amountDepleted == 0 ){
        return;
    }

    for( CarbonFlowIter flowIter = mCarbonFlows.begin();
         flowIter != mCarbonFlows.end(); ++flowIter ){
			 flowIter->transferIfOfType( amountDepleted, aEnvInfo, aYear, mType, aFlowType );
    }
}

void CarbonBox::acceptTransfer( double aCarbonValue, const int aYear,
                                const BoxType aBoxType ){

    if( matches( aBoxType ) ){
        mStock->addToStock( aCarbonValue, aYear );
    }
}

/*! addFlow()
 * \brief: add a Flow into a ptr_list container
 * \detail: calling internalAddFlow to push a ACarbonFlow object into mCarbonFlows.
 * \param: aBoxType - BoxType of the flow
 * \		   aCarbonFlow - ACarbonFlow auto_ptr for an object
 * \return: a newly generated CarbonBoxModel object with exact copied value of this CarbonBoxModel
 */
void CarbonBox::addFlow( auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType ){
    internalAddFlow( aCarbonFlow );
}

void CarbonBox::addDependencies( DependencyFinder& aDepFinder ) const {
    for( CarbonFlowConstIter flowIter = mCarbonFlows.begin();
         flowIter != mCarbonFlows.end(); ++flowIter ){
             aDepFinder.addDependency( flowIter->getTargetName(), getName() );
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
 * \brief Adds a flow to the list of flows that belong in the parent box
 *        model.
 * \details See documentation for completeInit to see why this is neccessary.
 * \param aCarbonFlow the flow to add.
 */
void CarbonBox::internalAddParentFlow( auto_ptr<ACarbonFlow> aCarbonFlow ){
	mFlowsFromParentBoxModel.push_back( aCarbonFlow.release() );
}

/*!
 * \brief Returns the name of this box.
 * \return name of this box.
 */
const string& CarbonBox::getName() const {
    return mName;
}

/*!
 * \brief Return the total value of luc-flow-in in integer
 * \return number of luc-flow-in
 */
int CarbonBox::getLucflowValue() {
	int flowValue(0);
	for (CarbonFlowConstIter boxFlowIter = mLucFlowInBox.begin(); 
		 boxFlowIter != mLucFlowInBox.end();
		 boxFlowIter++){
			 flowValue += (int) boxFlowIter->getFraction();
	}
	return flowValue;
}


/*! getCarbonStock
 * \brief Return the carbonStock object pointer
 * \return the CarbonStock object pointer
 */
ICarbonStock* CarbonBox::getCarbonStock() {
	return mStock.get();
}

/*! getCarbonBoxFlow
 * \brief Return the ACarbonFlow object pointer as the carbon flow of a selected carbon box
 * \return the CarbonFlowConstIter
 *
 * \param: aTargetName -  a string of box flow target name
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
 * \brief: return BoxType.
 * \detail: return the BoxType of current CarbonBox object.
 * \return: mType- a BoxType variable represent the ID of current CarbonBox.
 */
BoxType CarbonBox::getCarbonBoxType() {
	return mType;
}
