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
 * \file carbon_stock.cpp
 * \ingroup Objects
 * \brief CarbonStock class source file.
 * \author Jim Naslund and Ming Chang
 */

#include "util/base/include/definitions.h"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include "ccarbon_model/include/carbon_stock.h"
#include "ccarbon_model/include/carbon_model_utils.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_helper.h"
#include "ccarbon_model/include/environmental_info.h"
#include "ccarbon_model/include/summer_box.h"

using namespace std;
using namespace xercesc;

/*! 
 * \brief Default Constructor
 */
CarbonStock::CarbonStock()
{
	this->mTurnoverTimescale = 0;
}

CarbonStock::CarbonStock( const CarbonStock& aCarbonStock ):ACarbonStock( aCarbonStock ){
	this->mTurnoverTimescale = aCarbonStock.mTurnoverTimescale;
}

CarbonStock* CarbonStock::clone() const{
	return ( new CarbonStock( *this ) );
}
CarbonStock::~CarbonStock() {
}
bool CarbonStock::XMLParse( const xercesc::DOMNode* aNode ){
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
            mCurrentStock = XMLHelper<double>::getValue( curr );
			mInitialStock = mCurrentStock;
            mCarbonStock[ CarbonModelUtils::getStartYear() ] = mCurrentStock;
        }
        else if( nodeName == "turnover-timescale" ) {
            mTurnoverTimescale = XMLHelper<double>::getValue( curr );
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

const string& CarbonStock::getXMLNameStatic(){
    static const string XML_NAME = "carbon-stock";
    return XML_NAME;
}

double CarbonStock::transfer( const EnvironmentalInfo* aEnvInfo,
                              FlowType aFlowType, const int aYear ){

    //TODO: I'm not sure I like these if statements...  
    if( aFlowType == eBoxFlow ){
        //TODO: Is this needed?  It was put here because during testing LUC was dropping
        //      the carbon store below zero which was causing major problems.
		
		//! Notice: There is no box transfer in Atmosphere box. Atmosphere box will only 
		//!			pass the carbon lost to NPP box. Therefore, we will have to skip the 
		//!			atmosphere box by passing a 0 and to calculate the deduction after
		//!			the transfering	process.
		if ( mCarbonBoxType  != eAtmosphere ) {
		
			double carbonLost = max( mCurrentStock / ( mTurnoverTimescale ),  0.0 );
	
			mCurrentStock = mCurrentStock - carbonLost;
			mCarbonStock[ aYear ] = mCurrentStock;
			
			return carbonLost;	// return the actual carbon lost for boxes other than atmosphere
		} // end of if ( CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "Atmosphere" )
		else {
			return 0;	//! return 0 carbon lost for atmosphere box
		}
    }
    else if( aFlowType == eLUCFlowOut ){
		//! Notice: Land-Use-Change carbon flow out will only happen in vegetation, litter, and soil.
		//!			Therefore, we are going to skip the LUC-flow-out calculatioin for atmosphere and
		//!			simply returns a 0.
		if ( CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "Atmosphere"
			&& CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "NPP" ){

			double prevLandUse 
				= CarbonModelUtils::getLandUse( aYear - 1, aEnvInfo->getLandUseHistory(),
	                                            aEnvInfo->getHistoricalShare(),
		                                        aEnvInfo->getLandUse() );
    
			double currLandUse 
				= CarbonModelUtils::getLandUse( aYear, aEnvInfo->getLandUseHistory(),
					                            aEnvInfo->getHistoricalShare(),
						                        aEnvInfo->getLandUse() );



			double landLost = currLandUse - prevLandUse;

	        double landChangeFraction = landLost / prevLandUse;
			
		    if( landChangeFraction < 0 ) {
			    double carbonLost = fabs( mCurrentStock * landChangeFraction );
				mCurrentStock -= carbonLost;
				mCarbonStock[ aYear ] = mCurrentStock;
				SummerBox::getInstance()->sumLandLoss( landLost, aEnvInfo->getKey() );
				return carbonLost;
			}	// end of if ( landChangeFraction < 0 )
			else {
	            return 0;
		   }	// end of else for if ( landChangeFraction < 0 )
		}	// end of if ( CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "Atmosphere" 
			//				&& CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "NPP" 
		return 0;
    }	// end of else if( aFlowType == eLUCFlowOut )
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Carbon Stock encountered unknown flow type of " << aFlowType << "." << endl;
        return 0;
    }	// end of else if( aFlowType == eBoxFlow )/ else if( aFlowType == eLUCFlowOut )
}

void CarbonStock::addToStock( double aValueToAdd, const int aYear ){
    
	mCurrentStock += aValueToAdd;
    mCarbonStock[ aYear ] = mCurrentStock;
}

void CarbonStock::toInputXML(std::ostream &aOut, Tabs *aTabs) const{
	/*!
		/ write the carbon stock for each carbon box model
	 */

	// write the opening tag
	XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "");

	// write the elements

	XMLWriteElement( mInitialStock, "initial-value", aOut, aTabs, 0, "");
	XMLWriteElement( mTurnoverTimescale, "turnover-timescale", aOut, aTabs, 0, "");

	// write the closing tag
	XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}

void CarbonStock::toDebugXML( const int aPeriod, std::ostream& aOut,
							 Tabs* aTabs ) const{
	// TODO::write debug output
	// write the opening tag
	XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, "");

	// write the elements
	XMLWriteElement( mInitialStock, "initial-value", aOut, aTabs, aPeriod, "");
	XMLWriteElement( mTurnoverTimescale, "turnover-timescale", aOut, aTabs, aPeriod, "");
	XMLWriteElement( mCurrentStock, "final stock", aOut, aTabs, aPeriod, "");

	// write the closing tag
	XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}
 
/*! deductFromCurrentCarbonStock
 * \brief reduce current carbonstock.
 * \details This function takes in a deduct value as parameter and 
 *          reduce the current stock value with the deduct value.
 *
 * \param aDeductValue the value subtracts to the current stock value
 */

void CarbonStock::deductFromCurrentCarbonStock( const double aDeductValue) {
	mCurrentStock -= aDeductValue;
} // end of void CarbonStock::deductFromCurrentCarbonStock( const double aDeductValue) 

/*! getCurrentStockValue
 * \brief get current stock.
 * \details This function returns the current Carbon stock value
 *
 */
double CarbonStock::getCurrentStockValue() {
	return mCurrentStock;
} // end of double CarbonStock::getCurrentStockValue() 

/*! getTurnoverTimescale
 * \brief return the turnover timescale value.
 * \details This function returns the turnover timescale value of each stock
 *
 */
double CarbonStock::getTurnoverTimescale() {
	return mTurnoverTimescale;
} // end of double CarbonStock::getTurnoverTimescale() 
