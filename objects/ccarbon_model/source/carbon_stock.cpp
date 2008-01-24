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
#include "ccarbon_model/include/luc_carbon_summer.h"

using namespace std;
using namespace xercesc;

/*! 
 * \brief Default Constructor
 */
CarbonStock::CarbonStock()
{
    this->mTurnoverTimescale = 0;
}

/*! CarbonStock( const CarbonStock& aCarbonStock )
 * \brief a Copy Constructor for CarbonStock
 * \details create a newBoxFlow object or derived class and initialize all the
              variables with either deep copy of the member object variable or shallow copy
              of the member variable.
 * \param aCarbonStock const reference of a CarbonStock object
 * \author Ming Chang
 */
CarbonStock::CarbonStock( const CarbonStock& aCarbonStock ):ACarbonStock( aCarbonStock ){
    /*
     * \notice: the initilization line must includes the base class copy constructor with the 
                 derived object from your parameter list. It will automatically dynamic_cast back into the
                 base class. This step must be done in order for Virtual Copy Constructor to work.
     */
    this->mTurnoverTimescale = aCarbonStock.mTurnoverTimescale;
}

/*! clone()
 * \brief a Virtual Copy Constructor
 * \details this is a virtual function, it will return a newly created object
 *              of CarbonStock by calling the copy constructor with the parameter referenced to iself
 * \return CarbonStock* pointer points to the address of newly generated CarbonStock object.
 * \author Ming Chang
 */
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
            mInitialStock = XMLHelper<double>::getValue( curr );
            mCarbonStock[ CarbonModelUtils::getStartYear() ] = mInitialStock;
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

/*! transfer( const EnvironmentalInfo* aEnvInfo, FlowType aFlowType, const int aYear)
 * \brief carbon stock shifting from one box to another or to the same type in Summer Box
 * \details calculate the carbonLost value and deduct the current carbon stock with the
 * \lost value and return the carbon lost value. It will return 0 if and only if it's a box flow from
 * \atmosphere or a LUC-Flow-Out from NPP and Atmosphere.
 * \param aEnvInfo EnvironmentalInfo* for the current conceptual root
 *            aFlowtype FlowType transfer flow typ
 *            aYear working year
 * \return double Carbon Lost value.
 * \author Ming Chang and Jim Naslund
 */
double CarbonStock::transfer( const EnvironmentalInfo* aEnvInfo,
                              FlowType aFlowType, const int aYear ){

    //TODO: I'm not sure I like these if statements...  
    if( aFlowType == eBoxFlow ){
        //TODO: Is this needed?  It was put here because during testing LUC was dropping
        //      the carbon store below zero which was causing major problems.
        
        //! Notice: There is no box transfer in Atmosphere box. Atmosphere box will only 
        //!            pass the carbon lost to NPP box. Therefore, we will have to skip the 
        //!            atmosphere box by passing a 0 and to calculate the deduction after
        //!            the transfering    process.
        if ( mCarbonBoxType  != eAtmosphere ) {
        
            double carbonLost = max( mCarbonStock[ aYear ] / ( mTurnoverTimescale ),  0.0 );
    
            mCarbonStock[ aYear ] -= carbonLost;
            
            return carbonLost;    // return the actual carbon lost for boxes other than atmosphere
        } // end of if ( CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "Atmosphere" )
        else {
            return 0;    //! return 0 carbon lost for atmosphere box
        }
    }
    else if( aFlowType == eLUCFlowOut ){
        //! Notice: Land-Use-Change carbon flow out will only happen in vegetation, litter, and soil.
        //!            Therefore, we are going to skip the LUC-flow-out calculatioin for atmosphere and
        //!            simply returns a 0.
        if( ( this->mCarbonBoxType != eAtmosphere ) && ( this->mCarbonBoxType != eNPP ) ){

            double prevLandUse = aEnvInfo->getLandUse( aYear - 1 );
            double currLandUse = aEnvInfo->getLandUse( aYear );

            double landLost = currLandUse - prevLandUse;
            double landChangeFraction = landLost / prevLandUse;
            
            if( landChangeFraction < 0 ) {
                double carbonLost = fabs( mCarbonStock[ aYear ] * landChangeFraction );
                mCarbonStock[ aYear ] -= carbonLost;
                return carbonLost;
            }    // end of if ( landChangeFraction < 0 )
            else {
                return 0;
           }    // end of else for if ( landChangeFraction < 0 )
        }    // end of if ( CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "Atmosphere" 
            //                && CarbonModelUtils::boxTypeToString( mCarbonBoxType ) != "NPP" 
        return 0;
    }    // end of else if( aFlowType == eLUCFlowOut )
    else {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Carbon Stock encountered unknown flow type of " << aFlowType << "." << endl;
        return 0;
    }    // end of else if( aFlowType == eBoxFlow )/ else if( aFlowType == eLUCFlowOut )
}

/*! addToStock( double aValueToAdd, const int aYear )
 * \brief add carbon value to the current carbon stock
 * \details add the value to current carbon stock and modify the carbon stock in the designed year
 * \param aValueToAdd double the carbon stock value
 *            aYear int the working year
 */
void CarbonStock::addToStock( double aValueToAdd, const int aYear ){
    
    mCarbonStock[ aYear ]  += aValueToAdd;
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
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteElement( mCarbonStock[ modeltime->getper_to_yr( aPeriod ) ], "final stock", aOut, aTabs, aPeriod, "");

    // write the closing tag
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs);
}
 
/*! getTurnoverTimescale()
 * \brief return the turnover timescale value.
 * \details This function returns the turnover timescale value of each stock
 *
 */
double CarbonStock::getTurnoverTimescale() {
    return mTurnoverTimescale;
} // end of double CarbonStock::getTurnoverTimescale() 
