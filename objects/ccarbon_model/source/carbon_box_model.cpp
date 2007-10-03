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
 * \file carbon_box_model.cpp
 * \ingroup objects
 * \brief CarbonBoxModel class source file.
 * \author Jim Naslund and Ming Chang
 */
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "ccarbon_model/include/carbon_box_model.h"
#include "ccarbon_model/include/carbon_box.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "ccarbon_model/include/environmental_info.h"
#include "util/logger/include/ilogger.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/util.h"
#include "reporting/include/complex_carbon_dot_printer.h"
#include "reporting/include/complex_carbon_printer.h"
#include "util/base/include/auto_file.h"
#include "containers/include/dependency_finder.h"
#include "containers/include/icycle_breaker.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include <boost/lexical_cast.hpp>
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "ccarbon_model/include/luc_flow_in_to_box_model.h"
#include "ccarbon_model/include/summer_box.h"
#include "ccarbon_model/include/environmental_info.h"
#include "land_allocator/include/land_use_history.h"
#include <typeinfo>

extern Scenario* scenario; // for modeltime

using namespace std;
using namespace xercesc;

typedef boost::ptr_list<ACarbonFlow>::iterator CarbonFlowIter;
typedef boost::ptr_list<ACarbonFlow>::const_iterator CarbonFlowConstIter;

/*!
 * \brief Default constructor.
 */
CarbonBoxModel::CarbonBoxModel()
: mAboveGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mBelowGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mCalculated( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mCalculatedLUC( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mAtmDeductionCondition( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() )
{
    mEnvironmentalInfo.reset( new EnvironmentalInfo( scenario->getClimateModel() ) );
}

/*! CarbonBoxModel( const CarbonBoxModel& aCarbonBoxModel )
 * \brief: copy constructor
 * \detail: This is a copy constructor for CarbonBoxModel. It will take in a reference of 
 * \		a CarbonBoxModel object and make a exact copy of it in a newly allocated memory.
 * \param: aCarbonBoxModel - a const reference of a CarbonBoxModel
 * \return: a newly generated CarbonBoxModel object with exact copied value of this CarbonBoxModel
 */
CarbonBoxModel::CarbonBoxModel( const CarbonBoxModel& aCarbonBoxModel ) //! initialization list 
: mAboveGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ), 
  mBelowGroundCarbon( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mCalculated( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mCalculatedLUC( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  mAtmDeductionCondition( CarbonModelUtils::getStartYear(), CarbonModelUtils::getEndYear() ),
  ICarbonCalc( aCarbonBoxModel )
{
	
	try{	//! major section of codes for CarbonBoxModel copy constructor
		
		//! empty mNamesToBoxes map
		this->mNamesToBoxes = std::map<const std::string, ICarbonContainer*>();
		//! copy the EnvironmentalInfo from mEnvironmental
		this->mEnvironmentalInfo.reset( new EnvironmentalInfo( *aCarbonBoxModel.mEnvironmentalInfo.get() ) );

		//! copy the content from mAtmDeductionCondition
		//! if condition verified the member variable type before calling the std::copy()
		if ( typeid( aCarbonBoxModel.mAtmDeductionCondition ) == typeid( this->mAtmDeductionCondition ) ){
			std::copy( aCarbonBoxModel.mAtmDeductionCondition.begin(), 
					   aCarbonBoxModel.mAtmDeductionCondition.end(), 
					   this->mAtmDeductionCondition.begin() );
		}

		//! copy the content from mBelowGroundCarbon
		//! if condition verified the member variable type before calling the std::copy()
		if ( typeid( aCarbonBoxModel.mBelowGroundCarbon ) == typeid( this->mBelowGroundCarbon ) ){
			std::copy( aCarbonBoxModel.mBelowGroundCarbon.begin(),
					   aCarbonBoxModel.mAboveGroundCarbon.end(),
					   this->mBelowGroundCarbon.begin() );
		}
		
		//! copy the content from mAboveGroundCarbon
		//! if condition verified the member variable type before calling the std::copy()
		if ( typeid( aCarbonBoxModel.mAboveGroundCarbon ) == typeid( this->mAboveGroundCarbon ) ){
			std::copy( aCarbonBoxModel.mAboveGroundCarbon.begin(),
					   aCarbonBoxModel.mAboveGroundCarbon.end(),
					   this->mAboveGroundCarbon.begin() );
		}

		//! copy the content from mCalculatedLUC
		//! if condition verified the member variable type before calling the std::copy()
		if ( typeid( aCarbonBoxModel.mCalculatedLUC ) == typeid( this->mCalculatedLUC ) ){
			std::copy( aCarbonBoxModel.mCalculatedLUC.begin(),
					   aCarbonBoxModel.mCalculatedLUC.end(),
					   this->mCalculatedLUC.begin() );
		}

		//! copy the content from mCalculated
		//! if condition verified the member variable type before calling the std::copy()
		if ( typeid( aCarbonBoxModel.mCalculated ) == typeid( this->mCalculated ) ){
			std::copy( aCarbonBoxModel.mCalculated.begin(),
					   aCarbonBoxModel.mCalculated.end(),
					   this->mCalculated.begin() );
		}
		
		//! copy the mCarbonFlow list
		boost::ptr_list<ACarbonFlow>::const_iterator carbonFlowIter;
		for ( carbonFlowIter = aCarbonBoxModel.mCarbonFlows.begin();
			  carbonFlowIter != aCarbonBoxModel.mCarbonFlows.end();
			  carbonFlowIter++ ){
				  auto_ptr<ACarbonFlow> newFlow( carbonFlowIter->clone() );
				  cout<<CarbonModelUtils::flowTypeToString( newFlow->getFlowType() )<<endl;
				  this->mCarbonFlows.push_back( newFlow.release() );
		}

		//! copy the mCarbonBoxes and create new mNamesToBoxes map
		//! notice: it will not directly copy the mNamesToBoxes from aCarbonBoxModel.
		//!			The reason is that the 2nd component of the map should be the pointer of 
		//!			newly generated boxes instead of the pointer to the template boxes.
		std::vector<CarbonBox*>::const_iterator carbonBoxIter;
		for ( carbonBoxIter = aCarbonBoxModel.mCarbonBoxes.begin();
			  carbonBoxIter != aCarbonBoxModel.mCarbonBoxes.end();
			  carbonBoxIter++){
				  auto_ptr<CarbonBox> newBox ( (*carbonBoxIter)->clone() );
				  this->addBox( newBox );
		}
		
		//! calling the completeInit for each newly constructed CarbonBox
		//! this will reset the target for each carbon flow to accurate target
		//! CAUTION: CarbonBoxMoedl most call the completeInit() after copy constructor
		//!			 instead in the copy constructor.

	}
	catch( char* str ){
		cout<<"Exception raised"<<str<<endl;
	}
}

/*!
 * \brief Destructor.
 * \details Deallocates all the boxes that were created on the heap.
 */
CarbonBoxModel::~CarbonBoxModel(){
    for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
             delete *boxIter;
    }
}

/*! clone()
 * \brief: virtual copy constructor
 * \detail: Notice, there is no virtual copy constructor or virtual constructor in C++.
 * \		Therefore, clone() is a virtual function that will perform desired performance as
 * \		virtual copy constructor by indirectly calling the copy constructor through this
 * \		virtual function.
 * \return: a newly generated CarbonBoxModel object with exact copied value of this CarbonBoxModel
 */
CarbonBoxModel* CarbonBoxModel::clone() const{
	return ( new CarbonBoxModel( *this ) );
}

/*! XMLParse()
 * \brief: interpretate the system informations from XML file
 * \detail: Create new model or scenario from XML file. 
 * \		It will take a xercesc node objects and create a CarbonBox object with the XML data stored
 * \		in the node.
 * \param: aNode- xercesc::DOMNode*, holds the XML nodes information- in this case, it's CarbonBox information
 * \return: true or false
 */
bool CarbonBoxModel::XMLParse( const xercesc::DOMNode* aNode ){
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
        else if( nodeName == "carbon-box" ){
            // Create a carbon box by allowing it to parse itself
            // and add it to the list of carbon boxes.
            // The list will handle deallocation.
            checkForDuplicateName( XMLHelper<string>::getAttr( curr, "name" ) );
            auto_ptr<CarbonBox> newBox ( new CarbonBox );
            newBox->XMLParse( curr );
            addBox( newBox );
        }
        else {
            success = false;
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing CarbonBoxModel." << endl;
        }
    }
    return success;
}

/*! checkForDuplicateName()
 * \brief: check if CarbonBox name is already presented in the model
 * \detail: write to Warning Dialog if and only if newly parsed XML tag has same carbon box name which is
 * \		already presented in mCarbonBox vector of the same model.
 * \param: aName- string, the CarbonBox Name
 */
void CarbonBoxModel::checkForDuplicateName( string aName ) const {
    for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        if( (*boxIter)->getName() == aName ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Duplicate carbon box name " << aName << "." << endl;
            return;
        }
    }
}

/*! toInputXML()
 * \brief: a writeout function called by XMLDBOutputer visitor
 * \detail: directly write out the CarbonBox information into input.xml in XML format
 * \param: aOut - ostream&, output stream reference
		   aTabs - Tabs*, Tab pointer 
 */
void CarbonBoxModel::toInputXML( ostream& aOut, Tabs* aTabs ) const {
	/*!
		\ function that write the original input file to 
		\ output.xml.
	 */

	// write out an opening tag
	XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "" );// missing year;

	// write the CarbonBox objects to the output.xml
	for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
		boxIter != mCarbonBoxes.end(); ++boxIter ){
			(*boxIter)->toInputXML( aOut, aTabs);			
	}

	// write out an closing tag
	XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! toDebugXML()
 * \brief: a writeout function called by XMLDBOutputer visitor
 * \detail: directly write out the CarbonBox information into debug.xml in XML format
 * \param: aOut - ostream&, output stream reference
		   aTabs - Tabs*, Tab pointer 
 */
void CarbonBoxModel::toDebugXML( const int aPeriod, ostream& aOut,
                                 Tabs* aTabs ) const {
	XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "" );
	for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
		boxIter != mCarbonBoxes.end(); ++boxIter ){
			(*boxIter)->toDebugXML( aPeriod, aOut, aTabs );
	}
	XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

/*! accept()
 * \brief: a process function called by Ivisitor visitor
 * \detail: Loop through each CarbonBox accept()
 * \param: aVisitor - IVisitor*
		   aPeriod -  const int
 */
void CarbonBoxModel::accept( IVisitor* aVisitor,
                             const int aPeriod ) const {
    aVisitor->startVisitCarbonCalc( this, aPeriod );
    for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
             (*boxIter)->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitCarbonCalc( this, aPeriod );
}

/*! getXMLNameStatic()
 * \brief: obtain the XML name
 * \detail: static function which will return a XML tag name "carbon-box-model"
 * \return: XML_Name - const string&
 */
const string& CarbonBoxModel::getXMLNameStatic(){
    static const string XML_NAME = "carbon-box-model";
    return XML_NAME;
}

/*!
 * \CompletesInit()
 * \brief: complete initialization of Carbon Box
 * \details This function creates a dependency finder and passes it to all the
 *          boxes in the model.  It uses this dependency finder to reorder the
 *          box vector so that transfers occur the in correct order.  Box D is
 *          dependant on box P if box D receives a transfer from box P.  Box
 *          D must be after box P in the vector.
 *
 *          It also creates flows that point from this CarbonBoxModel to the
 *          appropriate container in the Summer.  This is done using the 
 *          mKey member which is a unique key generated by the CarbonBoxModel's
 *          leaf node's conceptual root.
 * \param aKey the unique key.
 */
void CarbonBoxModel::completeInit( int aKey ){
    // They key is stored in mEnvironmental info so it can be easily accessed
    // by objects owned by this model (boxes and flows).
    mEnvironmentalInfo->setKey( aKey );
    // Add an entry to the singleton Summer if needed
    SummerBox::getInstance()->addContainer( aKey );
    DependencyFinder depFinder( 0 );
    // Create a LUC In transfer pointing to this box model from the summer
    // for all three types of boxes.
    // TODO: Simplify?

	int vegFlowIn( 0 ), soilFlowIn( 0 ), litterFlowIn( 0 );
	
	// retrieve the luc-flow-in value from each carbon box
	// only soil, vegetation, and litter are the available through transfer
	for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
			 if( ( *boxIter )->getName() == "vegetation" ){
				  vegFlowIn = ( *boxIter )->getLucflowValue();
			 }
			 else if( ( *boxIter )->getName() == "litter" ){
				 litterFlowIn = ( *boxIter )->getLucflowValue();
			 }
			 else if( ( *boxIter )->getName() == "soil" ){
					 soilFlowIn = ( *boxIter )->getLucflowValue();
			}
	}
	
    auto_ptr<ACarbonFlow> newFlowVeg = auto_ptr<ACarbonFlow>( new LUCFlowInToBoxModel( this, vegFlowIn ) );
    auto_ptr<ACarbonFlow> newFlowSoil = auto_ptr<ACarbonFlow>( new LUCFlowInToBoxModel( this, soilFlowIn ) );
    auto_ptr<ACarbonFlow> newFlowLitter = auto_ptr<ACarbonFlow>( new LUCFlowInToBoxModel( this, litterFlowIn ) );
    SummerBox::getInstance()->getContainer( aKey )->addFlow( newFlowVeg, eVegetation );
    SummerBox::getInstance()->getContainer( aKey )->addFlow( newFlowSoil, eSoil );
    SummerBox::getInstance()->getContainer( aKey )->addFlow( newFlowLitter, eLitter );
	
    
	// Pass the dependency finder to e-ach box so it can add dependencies for
    // all the boxes that depend on it.

    for( CarbonBoxConstIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        (*boxIter)->addDependencies( depFinder );
    }
	depFinder.createOrdering();
    util::reorderContainer<CarbonBoxIterator, CarbonBox*>(
                            mCarbonBoxes.begin(), mCarbonBoxes.end(),
                            depFinder.getOrdering() );

    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
         (*boxIter)->completeInit( *this );
    }


}


// For debugging purposes.
void CarbonBoxModel::printGraph( const std::string& fileName ) const {
    AutoOutputFile landAllocatorStream( fileName );
    ComplexCarbonDotPrinter debugPrinter = ComplexCarbonDotPrinter( "whatever", *landAllocatorStream );
    accept( &debugPrinter, 1 );
}

// For debugging purposes.
void CarbonBoxModel::printGraphOneFile( const std::string& fileName,
                                        const int aPeriod ) const {
    static AutoOutputFile landAllocatorStream( fileName );
    static ComplexCarbonPrinter debugPrinter = ComplexCarbonPrinter( "whatever", *landAllocatorStream );
    accept( &debugPrinter, aPeriod );
}

/*! initLandUseHistory
 * \brief Initializes land use history.
 * \details Initializes land use history and share with the passed in values.
 * \param aHistory the land use history object.
 * \param aShare the share.
 */
void CarbonBoxModel::initLandUseHistory( const LandUseHistory* aHistory,
                                         const double aShare ){
	mEnvironmentalInfo->setLandUseHistory( aHistory );
    mEnvironmentalInfo->setHistoricalShare( aShare );
}

/*! calc
 * \brief Performs all box flows out of this box model.
 * \details Box flows are transfers to other boxes <strong> in this carbon
 *              box model </strong> that are not driven by land use change.
 * \param aYear the year.
 */
void CarbonBoxModel::calc( const int aYear ){
    doTransfers( mEnvironmentalInfo.get(), eBoxFlow, aYear );
	//! Important, the original idea was to deduct the carbon stock with the NPP value
	//! However, the calculation does not fit at the begging of starting year(1700)
	//! Therefore, I created a new function which extracts the neccessary values to calculate a new
	//! atmosphere stock and replace the value to atmosphere box. The key point for this function is
	//! all the carbon stock must be accurated for calculation.
	
	double tempNPPValue (0);

		for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
			 boxIter != mCarbonBoxes.end(); ++boxIter ) {
				if ( (*boxIter)->matches( eNPP ) ) {
					tempNPPValue = (*boxIter)->getCarbonStock()->getStock(aYear);
				}
		}
		
    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
		boxIter != mCarbonBoxes.end(); ++boxIter ) {
			if ( (*boxIter)->getName() == "atmosphere" ) {
				(*boxIter)->getCarbonStock()->deductFromStock(tempNPPValue,aYear);
			}
	}
	
	//doAtmosphereCalculation( aYear );	// debug purpose
    //printGraphOneFile("ccarbon-model-stocks.xml", aYear );
}
/*!
 *\ doAtmosphereCalculation()
 *\ brief: calculate the atmosphere carbonstock of current year
 *\ detail: This is a debug function used to verify if the carbon stock is
 *\			correct in atmosphere box. It will calculate the carbon stock by performing
 *\			stock calculation.
 *\param: aYear- int
 *\
 */
void CarbonBoxModel::doAtmosphereCalculation( const int aYear ){
	double tempNPPValue (0);
	double tempAtmValue (0);
	double tempLitter (0);
	double tempSoil (0);
	double tauLitter (0);
	double tauSoil (0);
	double fracLitToSoil (0);
	double newAtm (0);

		for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
			 boxIter != mCarbonBoxes.end(); ++boxIter ) {
				if ( (*boxIter)->matches( eNPP ) ) {
					tempNPPValue = (*boxIter)->getCarbonStock()->getStock(aYear);
				}
				else if ( (*boxIter)->matches( eAtmosphere) ) {
					tempAtmValue = (*boxIter)->getCarbonStock()->getStock(aYear-1);
				}
				else if ( (*boxIter)->matches( eLitter ) ) {
					tempLitter = (*boxIter)->getCarbonStock()->getStock(aYear-1);
					tauLitter = (*boxIter)->getCarbonStock()->getTurnoverTimescale();
					boost::ptr_list<ACarbonFlow>::const_iterator tempBoxFlow;
					tempBoxFlow = ( *boxIter )->getCarbonBoxFlow( "soil" );

					if ( tempBoxFlow == ( *boxIter )->carbonBoxFlowNotFound() ) {
						// if box is not found, preset the flow from soil to atmosphere to 0%
						fracLitToSoil = 0;
					}
					else {
						fracLitToSoil = tempBoxFlow->getFraction();
					}

				}
				else if ( (*boxIter)->matches( eSoil ) ) {
					tempSoil =  (*boxIter)->getCarbonStock()->getStock(aYear-1);
					tauSoil = (*boxIter)->getCarbonStock()->getTurnoverTimescale();
				}
		}
		if ( aYear >= 1700 ) {
			cout<<"Year = "<<aYear<<endl;
			cout<<"NPP = "<<tempNPPValue<<endl;
			cout<<"soil = "<<tempSoil<<endl;
			cout<<"tauSoil = "<<tauSoil<<endl;
			cout<<"Litter = "<<tempLitter<<endl;
			cout<<"tauLitter = "<<tauLitter<<endl;
			cout<<"fracLiToSoil = "<<fracLitToSoil<<endl;
			cout<<"Atmosphere = "<<tempAtmValue<<endl;
		}
		if ( aYear < 1700 ) {
			newAtm = 0;
		}
		else {
			newAtm = ( tempAtmValue + ( 1 - fracLitToSoil )*( tempLitter / tauLitter ) + ( tempSoil / tauSoil ) - tempNPPValue );
		}

		if ( aYear >= 1700 ) {
			cout<<"New Atmosphere = "<< newAtm <<endl;
		}

		for ( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
			  boxIter != mCarbonBoxes.end(); ++boxIter ) {
			if ( ( *boxIter )->getName() == "atmosphere" ) {
				( *boxIter )->getCarbonStock()->modifyCarbonStock( newAtm, aYear );
			} // end of if ( ( *boxIter )->getName() == "atmosphere" )
		} // end of for ( CarbonBoxIterator boxIter = mCarbonBoxes.begin();boxIter != mCarbonBoxes.end(); ++boxIter )
}

/*!
 * \brief Sets the current stock.
 * \param aYear the year
 */
void CarbonBoxModel::setCurrentStock( const int aYear ){
    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        (*boxIter)->setCurrentStock( aYear );
    }
}

/*! calcLandUseChange()
 * \brief: calling all the doTransfer() to perfrom Carbon Flow Calculation
 * \detail: set the currentStock value if it is eLUCFlowOut,
			call SummerBox to perform flow transfering if it is eLUCFlowIn
			otherwise, it will just call dotransfer() to perform flow transfer in eBoxFlow
 * \param: aYear - const int
		   aFlowType - FlowType
 */
void CarbonBoxModel::calcLandUseChange( const int aYear, FlowType aFlowType ){
    //TODO: I don't like this if/else, maybe this should be in the calling method
    //      instead?  The Summer is singleton and can be accessed from anywhere.
    //      This could be moved to a better place higher up.

    //TODO: better place
    if( aFlowType == eLUCFlowOut ){
        setCurrentStock( aYear );
    }
    // Land-use change IN (from SummerBox to CarbonBoxModels) is special because
    // it must be invoked on the SummerBox.
    if( aFlowType == eLUCFlowIn ){
        SummerBox::getInstance()->getContainer( 
            mEnvironmentalInfo->getKey() )->doTransfers( 
            mEnvironmentalInfo.get(), aFlowType, aYear );
    }
    // Any other type of transfer will be invoked on the containers owned
    // by this CarbonBoxModel.
    else {
        doTransfers( mEnvironmentalInfo.get(), aFlowType, aYear );
    }
}

/*! doTransfers()
 * \brief: Carbon flow calculation by calling doTransfers()
 * \detail: calling doTransfers to perform eBoxFlow transfering
 * \param: aEnvInfo - const EnvironmentalInfo*
		   aFlowType - FlowType
		   int year - int
 */
void CarbonBoxModel::doTransfers( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType,
                                  int aYear ) {

    // Calculate this year.
    for( CarbonBoxIterator boxIter = mCarbonBoxes.begin();
         boxIter != mCarbonBoxes.end(); ++boxIter ){
        (*boxIter)->doTransfers( mEnvironmentalInfo.get(), aFlowType, aYear );
    }
}

double CarbonBoxModel::getNetLandUseChangeEmission( const int aYear ) const {
    //TODO: This is just a stub, needs to be addressed during integration.
    return 0;
}

double CarbonBoxModel::getNetTerrestrial( const int aYear ) const {
    //TODO: This is just a stub. This is the major output function that needs to interface with
    //      the atmosphere box, needs to be addressed during integration.
    return 0;
}

void CarbonBoxModel::setTotalLandUse( const double aLandUse,
                                      const int aPeriod ){
    
    mEnvironmentalInfo->setLandUse( aLandUse, aPeriod );
}

double CarbonBoxModel::getPotentialAboveGroundCarbon( const int aYear ) const {
    return mAboveGroundCarbon[ aYear ];
}

/*!
 * \warning If this is called on unmanaged land it will mess things up
 */
void CarbonBoxModel::setUnitAboveGroundCarbon( const double aAboveGroundCarbon,
                                               const int aPeriod ){
    mAboveGroundCarbon[ aPeriod ] = aAboveGroundCarbon;
}

double CarbonBoxModel::getPotentialBelowGroundCarbon( const int aYear ) const {
    return mBelowGroundCarbon[ aYear ];
}

/*!
 * \warning If this is called on unmanaged land it will mess things up
 */
void CarbonBoxModel::setUnitBelowGroundCarbon( const double aBelowGroundCarbon,
                                               const int aPeriod ){
    mBelowGroundCarbon[ aPeriod ] = aBelowGroundCarbon;
}

/*! addBox
 * \brief Adds a box to this box model.
 * \details Adds a box to this carbon model.  This function transfers ownership.
 *          This function also adds the box's name to names to boxes map.
 * \param aCarbonBox the box to add.
 */
void CarbonBoxModel::addBox( auto_ptr<CarbonBox> aCarbonBox ){
    mNamesToBoxes[ aCarbonBox->getName() ] =  aCarbonBox.get();
    mCarbonBoxes.push_back( aCarbonBox.release() );
}

void CarbonBoxModel::addFlow( std::auto_ptr<ACarbonFlow> aCarbonFlow, const BoxType aBoxType ){
    mCarbonFlows.push_back( aCarbonFlow.release() );
}

void CarbonBoxModel::acceptTransfer( double aCarbonValue, const int aYear,
                                     const BoxType aBoxType ){
	double prevLandUse =
        CarbonModelUtils::getLandUse( aYear - 1,
                                      mEnvironmentalInfo->getLandUseHistory(),
                                      mEnvironmentalInfo->getHistoricalShare(),
                                      mEnvironmentalInfo->getLandUse() );

    double currLandUse = 
        CarbonModelUtils::getLandUse( aYear,
                                      mEnvironmentalInfo->getLandUseHistory(),
                                      mEnvironmentalInfo->getHistoricalShare(),
                                      mEnvironmentalInfo->getLandUse() );
    double landChange = currLandUse - prevLandUse;
    // We only want to accept this transfer if this box gained land.
    if( landChange > 0 ){
        for( CarbonFlowIter flowIter = mCarbonFlows.begin();
             flowIter != mCarbonFlows.end(); ++flowIter ){
            // transferIfOfType is used here because this loop goes through and performs
            // all the LUCFlowInToBoxModel transfers from this carbon box model to
            // its the carbon boxes it contains.  We don't want to perform
            // other types of transfers.
            flowIter->transferIfOfType( landChange * aCarbonValue,
                                        mEnvironmentalInfo.get(),
                                        aYear, aBoxType, eLUCFlowIn );
        }
    }
}

void CarbonBoxModel::addDependencies( DependencyFinder& aDepFinder ) const {
    assert( false );
}

/*! getKey
 * \brief Returns the carbon box model's key.
 * \details Returns this box model's key.
 * \returns this box model's key.
 */
int CarbonBoxModel::getKey() const {
    return mEnvironmentalInfo->getKey();
}

/*! getBoxByName
 * \brief Returns a pointer to a box that is contained by this box model.
 * \details Uses the internal map to return a pointer to the box that matches
 *          the passed in name.  This function is used by flows when they
 *          initialize their target.
 * \return pointer to a carbon container.
 */
ICarbonContainer* CarbonBoxModel::getBoxByName( const std::string& aName ) const {
	// possible error with luc-flow-out, reading aName is error
    return mNamesToBoxes.find( aName )->second;
}
