/*
	This software, which is provided in confidence, was prepared by employees
	of Pacific Northwest National Laboratory operated by Battelle Memorial
	Institute. Battelle has certain unperfected rights in the software
	which should not be copied or otherwise disseminated outside your
	organization without the express written authorization from Battelle. All rights to
	the software are reserved by Battelle.  Battelle makes no warranty,
	express or implied, and assumes no liability or responsibility for the 
	use of this software.
*/

/*! 
* \file more_sector_info.cpp
* \ingroup Objects-SGM
* \brief The MoreSectorInfo class source file.
*
*  Detailed Description.
*
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "sectors/include/more_sector_info.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/util.h"

using namespace std;
using namespace xercesc;

const string MoreSectorInfo::XML_NAME = "moreSectorInfo";

//!< Default Constructor
MoreSectorInfo::MoreSectorInfo() {
}

//! parse SOME xml data
void MoreSectorInfo::XMLParse( const DOMNode* node ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get all child nodes.
    const DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        // Skip text nodes, whitespace or comments.
        if( curr->getNodeType() == DOMNode::TEXT_NODE ){
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        if( nodeName == "corpIncomeTaxRate" ) {
			setType( MoreSectorInfo::CORP_INCOME_TAX_RATE, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "energyToCurrencyConversion" ) {
			setType( MoreSectorInfo::ENERGY_CURRENCY_CONVERSION, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "maxCorpRERate" ) {
			setType( MoreSectorInfo::MAX_CORP_RET_EARNINGS_RATE, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "investTaxCreditRate" ) {
			setType( MoreSectorInfo::INVEST_TAX_CREDIT_RATE, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "indirectBusinessTaxRate" ) {
			setType( MoreSectorInfo::IND_BUS_TAX_RATE, XMLHelper<double>::getValue( curr ) );
		}
		else if  ( nodeName == "transportationCost" ) {
			setType( MoreSectorInfo::TRANSPORTATION_COST, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "tranCostMult" ) {
			setType( MoreSectorInfo::TRAN_COST_MULT, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "priceAdjustMult" ) {
			setType( MoreSectorInfo::PRICE_ADJUST_MULT, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "proportionalTaxRate" ) {
			setType( MoreSectorInfo::PROPORTIONAL_TAX_RATE, XMLHelper<double>::getValue( curr ) );
		}
		else if( nodeName == "additiveTax" ) {
			setType( MoreSectorInfo::ADDITIVE_TAX, XMLHelper<double>::getValue( curr ) );
		}
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << "." << endl;
		}
	}
}

//! Output to XML data
/*! \todo This may print several extra values. */
void MoreSectorInfo::toInputXML( ostream& out, Tabs* tabs ) const {
	// write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs );
    
    for( CInfoTypeIterator value = mSectorInfoMap.begin(); value != mSectorInfoMap.end(); ++value ){
        XMLWriteElementCheckDefault( value->second, enumToXMLName( value->first ), out, tabs );
    }
	// write the closing tag.
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Output debug info to XML data
void MoreSectorInfo::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
	// write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs );
    
    for( CInfoTypeIterator value = mSectorInfoMap.begin(); value != mSectorInfoMap.end(); ++value ){
        XMLWriteElementCheckDefault( value->second, enumToXMLName( value->first ), out, tabs );
    }
	// write the closing tag.
	XMLWriteClosingTag( getXMLName(), out, tabs );
}

const string& MoreSectorInfo::getXMLName() const {
	return XML_NAME;
}

const string& MoreSectorInfo::getXMLNameStatic() {
	return XML_NAME;
}

//! Get the value for the national account specified by the account type key. 
double MoreSectorInfo::getValue( const MoreSectorInfoType aType ) const {
    return util::searchForValue( mSectorInfoMap, aType );
}

//! Reset all MoreSectorInfo. This still exposes the underlying map too much.
void MoreSectorInfo::reset() {
	mSectorInfoMap.clear();
}

void MoreSectorInfo::setType( const MoreSectorInfoType aType, const double aValue ) {
	mSectorInfoMap[ aType ] = aValue;
}

//! Convert the enumerator value to the name used for printing output files.
const string MoreSectorInfo::enumToName( const MoreSectorInfoType aType ) const {
	switch( aType ) {
		case MoreSectorInfo::ENERGY_CURRENCY_CONVERSION: 
			return "Energy to Currency Conversion";
		case MoreSectorInfo::INVEST_TAX_CREDIT_RATE: 
			return "Investment Tax Credit Rate";
		case MoreSectorInfo::CORP_INCOME_TAX_RATE: 
			return "Corporate Income Tax Rate";
		case MoreSectorInfo::IND_BUS_TAX_RATE: 
			return "Indirect Business Tax Rate";
		case MoreSectorInfo::MAX_CORP_RET_EARNINGS_RATE:
			return "Max Corp Retained Earnings Rate";
		default:
			return "";
	}
}

//! Convert the enumerator to the XML name of the value.
const string MoreSectorInfo::enumToXMLName( const MoreSectorInfoType aType ) const {
    switch( aType ){
        case ENERGY_CURRENCY_CONVERSION:
            return "energyToCurrencyConversion";
        case INVEST_TAX_CREDIT_RATE:
            return "investTaxCreditRate";        
        case CORP_INCOME_TAX_RATE:
            return "corpIncomeTaxRate";
        case IND_BUS_TAX_RATE:
            return "indirectBusinessTaxRate";
        case MAX_CORP_RET_EARNINGS_RATE:
            return "maxCorpRERate";
        case CORP_RET_EARNINGS_RATE:
            return "corpRERate";
        case HH_RET_EARNINGS_RATE:
            return "houseHoldRetainedEarningsRate";
        case RET_EARNINGS_PARAM:
            return "retainedEarningsParam";
        case TRANSPORTATION_COST:
            return "transportationCost";
        case TRAN_COST_MULT:
            return "tranCostMult";
        case PRICE_ADJUST_MULT:
            return "priceAdjustMult";
        case PROPORTIONAL_TAX_RATE:
            return "proportionalTaxRate";
        case ADDITIVE_TAX:
            return "additiveTax";
        default:
            return "invalid-type enum=\"" + util::toString( aType ) + "\"";
    }
}
