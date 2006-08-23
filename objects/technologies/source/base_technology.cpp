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
* \file base_technology.cpp
* \ingroup Objects
* \brief The BaseTechnology class source file.
*
*  Detailed Description.
*
* \author Pralit Patel
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <string>
#include <vector>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "technologies/include/base_technology.h"
#include "functions/include/input.h"
#include "functions/include/production_input.h"
#include "functions/include/demand_input.h"
#include "functions/include/ifunction.h"
#include "sectors/include/more_sector_info.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "functions/include/function_manager.h"
#include "util/base/include/model_time.h"
#include "technologies/include/expenditure.h"
#include "marketplace/include/marketplace.h"
#include "emissions/include/ghg.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//!< Default Constructor
BaseTechnology::BaseTechnology() {
    const int maxper = scenario->getModeltime()->getmaxper();
    mOutputs.resize( maxper );
    prodDmdFn = 0;
}

//!< Destructor
BaseTechnology::~BaseTechnology() {
    clear();
}

BaseTechnology::BaseTechnology( const BaseTechnology& baseTechIn ) {
    const int maxper = scenario->getModeltime()->getmaxper();
    mOutputs.resize( maxper );
    copy( baseTechIn );
}

BaseTechnology& BaseTechnology::operator =( const BaseTechnology& baseTechIn ) {
    if ( this != &baseTechIn ) {
        clear();
        copy( baseTechIn );
    }
    return *this;
}

void BaseTechnology::copy( const BaseTechnology& baseTechIn ) {
    name = baseTechIn.name;
    categoryName = baseTechIn.categoryName;
    prodDmdFnType = baseTechIn.prodDmdFnType;
    prodDmdFn = baseTechIn.prodDmdFn;
    inputNameToNo = baseTechIn.inputNameToNo;
    mGhgNameMap = baseTechIn.mGhgNameMap;
    // year? expenditure?

    for ( unsigned int i = 0; i < baseTechIn.input.size(); i++) {
        input.push_back( baseTechIn.input[i]->clone() );
    }
    for( vector<Ghg*>::const_iterator ghg = baseTechIn.mGhgs.begin(); ghg != baseTechIn.mGhgs.end(); ++ghg ){
        mGhgs.push_back( (*ghg)->clone() );
    }
}

void BaseTechnology::copyParam( const BaseTechnology* baseTechIn ) {
    name = baseTechIn->name;
    categoryName = baseTechIn->categoryName;
    prodDmdFnType = baseTechIn->prodDmdFnType;
    prodDmdFn = baseTechIn->prodDmdFn;
    
    // First loop through and remove any inputs that are not in the copy-from technology.
    for( InputIterator iter = input.begin(); iter != input.end(); ++iter ){
        if( !isCoefBased() && (*iter)->getDemandCurrency() != 0 ){
            continue;
        }
        // Doesn't exist in the other one. 
        if( !util::hasValue( baseTechIn->inputNameToNo, (*iter)->getName() ) ){
            // Remove the map entry for it. This is not strictly neccessary.
            map<string,int>::iterator mapEntry = inputNameToNo.find( (*iter)->getName() );
            assert( mapEntry != inputNameToNo.end() ); 
            inputNameToNo.erase( mapEntry );
            // Remove it from the vector.
            delete *iter;
            input.erase( iter-- );
        }
    }
    
    // Need to reset the map because the indexes will no longer be correct as they will account for
    // items removed from the vector. 
    resetMapIndices( input, inputNameToNo );

    // For each input, check if it exists in the current technology. 
    for ( CInputIterator iter = baseTechIn->input.begin(); iter != baseTechIn->input.end(); ++iter ) {
        if( !util::hasValue( inputNameToNo, (*iter)->getName() ) ){
            input.push_back( (*iter)->clone() );
            inputNameToNo[ (*iter)->getName() ] = static_cast<int>( input.size() - 1 );
        }
        // It already exists, we need to copy into.
        else {
            input[ util::searchForValue( inputNameToNo, (*iter)->getName() ) ]->copyParam( *iter );
        }
    } // end for
    
    // For each Ghg check if it exists in the current technology.
    for ( vector<Ghg*>::const_iterator ghg = baseTechIn->mGhgs.begin(); ghg != baseTechIn->mGhgs.end(); ++ghg ) {
        if( !util::hasValue( mGhgNameMap, (*ghg)->getName() ) ){
            mGhgs.push_back( (*ghg)->clone() );
            // Add it to the map.
            mGhgNameMap[ (*ghg)->getName() ] = static_cast<int>( mGhgs.size() ) - 1;
        }
        // It already exists, we need to copy into.
        // For now just leave the current one. It should already be in the map since it was parsed.
        else {
            // TODO: Add copy into code here.
        }
    } // end for
}

void BaseTechnology::clear() {
    for( InputIterator iter = input.begin(); iter != input.end(); ++iter ) {
        delete *iter;
    }
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        delete *ghg;
    }
}

//! parse SOME xml data
void BaseTechnology::XMLParse( const DOMNode* node ) {
    /*! \pre make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( node, "name" );
    year = XMLHelper<int>::getAttr( node, "year" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if ( nodeName == ProductionInput::getXMLNameStatic() ) {
            parseContainerNode( curr, input, inputNameToNo, new ProductionInput() );
        }
        else if (nodeName == DemandInput::getXMLNameStatic() ) {
            parseContainerNode( curr, input, inputNameToNo, new DemandInput() );
        }
        else if ( nodeName == "prodDmdFnType" ) {
            prodDmdFnType = XMLHelper<string>::getValue( curr );
        }
        else if ( nodeName == "categoryName" ) {
            categoryName = XMLHelper<string>::getValue( curr );
        }
        else if( nodeName == Ghg::getXMLNameStatic() ){
            parseContainerNode( curr, mGhgs, mGhgNameMap, new Ghg() );
        }
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing " << getXMLName() << "." << endl;
        }
    }
}

//! Output to XML data
void BaseTechnology::toInputXML( ostream& out, Tabs* tabs ) const {

    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name, year, "");

    //XMLWriteElement( year, "year", out, tabs );
    XMLWriteElement( prodDmdFnType, "prodDmdFnType", out, tabs );
    
    for( unsigned int iter = 0; iter < input.size(); iter++ ){
        input[ iter ]->toInputXML( out, tabs );
    }
    
    for( vector<Ghg*>::const_iterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->toInputXML( out, tabs );
    }
    toInputXMLDerived( out, tabs );

    // write the closing tag.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Output debug info to XML data
void BaseTechnology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    // write the beginning tag.
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name, year );

    XMLWriteElement( prodDmdFnType, "prodDmdFnType", out, tabs );

    for( unsigned int iter = 0; iter < input.size(); iter++ ){
        input[ iter ]->toDebugXML( period, out, tabs );
    }
    for( vector<Ghg*>::const_iterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->toDebugXML( period, out, tabs );
    }
    expenditure.toDebugXML( period, out, tabs );

    XMLWriteElement( mOutputs[ period ], "output", out, tabs );

    toDebugXMLDerived( period, out, tabs );

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Complete the initialization of the BaseTechnology object.
void BaseTechnology::completeInit( const std::string& regionName ) {
    for( InputIterator anInput = input.begin(); anInput != input.end(); ++anInput ) {
         // This does not appear to do anything, might not work if it did due to later copying.
        (*anInput)->completeInit();
    }

    // Check if CO2 is missing. 
    if( !util::hasValue( mGhgNameMap, string( "CO2" ) ) ){
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        mGhgs.push_back( new Ghg( "CO2", "MTC", 1 ) ); // at least CO2 must be present
        mGhgNameMap[ "CO2" ] = static_cast<int>( mGhgs.size() ) - 1;
    }
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        // (*ghg)->completeInit();
    }
    initProdDmdFn();
}

void BaseTechnology::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                               const string& aSectorName, NationalAccount& nationalAccount,
                               const Demographic* aDemographics, const double aCapitalStock, const int aPeriod )
{
    // Only calculate price paid if the current year is equal or less than the technologies year.
    if( scenario->getModeltime()->getper_to_yr( aPeriod ) >= year ){
        calcPricePaid( aMoreSectorInfo, aRegionName, aSectorName, aPeriod );
    }

    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        // (*ghg)->initCalc();
    }
}

/*! \brief Clear out empty inputs.
* \details Loop through the set of inputs for a technology and remove all inputs
*          which have a currency demand of zero. Resets the name to number map
*          to contain the correct mappings.
*/
void BaseTechnology::removeEmptyInputs(){
    for( InputIterator currInput = input.begin(); currInput != input.end(); ++currInput ){
        if( (*currInput)->getDemandCurrency() == 0 ){
            // Remove the input object.
            delete *currInput;
            // Remove the dangling pointer and empty vector position. The erase
            // operation invalidates any iterators at or after the position that
            // was erased. The postfix decrement returns the current iterator to
            // the vector delete function, then decrements the iterator. This
            // allows the erasure of the current position without invalidating
            // the iterator.
            if( currInput == input.begin() ){
                input.erase( currInput );
                currInput = input.begin();
            }
            else {
                input.erase( currInput-- );
            }
        }
    }

    // Rewrite the map.
    resetMapIndices( input, inputNameToNo );
}

//! Ininialize prodDmdFn by choosing the appropriate type
void BaseTechnology::initProdDmdFn() {
    assert( !prodDmdFnType.empty() );
    prodDmdFn = FunctionManager::getFunction( prodDmdFnType );
}

//! get technology name
const string& BaseTechnology::getName() const {
    return name;
}

//! get technology year
int BaseTechnology::getYear() const {
    return year;
}

void BaseTechnology::setYear( int newYear ) {
    year = newYear;
}

/*! \brief Get the output of the technology in a given period.
* \param aPeriod Period to get output for.
* \return The output for the given period.
* \author Josh Lurz
* \note Currently the output vector is calculated differently depending on whether the new 
* investments are being included in subsector level output. 
*/
double BaseTechnology::getOutput( const int aPeriod ) const {
    return mOutputs[ aPeriod ];
}

/*! \brief Calculates and sets the price paid for each input of the the
*          technology.
* \param aMoreSectorInfo Additional sector level information needed for
*        technology.
* \param aRegionName The name of the region.
* \param aSectorName The name of the sector.
* \param aPeriod The period for which to calculate expected price paid.
* \author Sonny Kim
*/
void BaseTechnology::calcPricePaid( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                                    const string& aSectorName, const int aPeriod )
{
    // initialize so that aMoreSectorInfo does not have any impact on price 
    // and override only if aMoreSectorInfo is not null
    double transportationAdder = 0;
    double transportationMult = 1;
    double proportionalTax = 1;
    double additiveTax = 0;

    // if pointer is not null
    if(aMoreSectorInfo) {
        transportationAdder = aMoreSectorInfo->getValue(MoreSectorInfo::TRANSPORTATION_COST);
        transportationMult = aMoreSectorInfo->getValue(MoreSectorInfo::TRAN_COST_MULT);
        proportionalTax = aMoreSectorInfo->getValue(MoreSectorInfo::PROPORTIONAL_TAX_RATE);
        additiveTax = aMoreSectorInfo->getValue(MoreSectorInfo::ADDITIVE_TAX);
    }

    Marketplace* marketplace = scenario->getMarketplace();
    for( unsigned int i = 0; i < input.size(); i++ ) {
        // Could this be simplified some by moving it into input? Possibly a derived class for capital?
        // fails without using inputName in augument, don't know the cause
        double tempPricePaid = 0;
        if( input[ i ]->isCapital() ){
            tempPricePaid = input[ i ]->getPrice( aRegionName, aPeriod ) + input[i]->getPriceAdjustment();
        }
        else if( input[ i ]->isNumeraire() ){
            tempPricePaid = 1;
        }
        else {
            // Calculate GHG taxes.
            double carbonTax = 0;
            for( vector<Ghg*>::const_iterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
                carbonTax += (*ghg)->getGHGValue( input[ i ], aRegionName, aSectorName, aPeriod );
            }
            tempPricePaid = ( ( input[ i ]->getPrice( aRegionName, aPeriod ) + 
                ( transportationAdder * transportationMult ) ) * proportionalTax + additiveTax + carbonTax ) * 
                input[i]->getPriceAdjustment();
        }
        input[i]->setPricePaid( tempPricePaid );
    }
}

void BaseTechnology::updateMarketplace( const string& sectorName, const string& regionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    double totalDemand = 0;
    for( InputIterator curr = input.begin(); curr != input.end(); ++curr ) {
        double tempDemand = (*curr)->getDemandCurrency();
        if( tempDemand < 0 ){
            cout << "Error trying to add negative demand currency to marketplace from BaseTechnology." << endl;
        }
        marketplace->addToDemand( (*curr)->getName(), regionName, tempDemand, period );
        totalDemand += tempDemand;
    }
    marketplace->addToSupply( sectorName, regionName, totalDemand, period );
}

void BaseTechnology::csvSGMOutputFile( ostream& aFile, const int period ) const {
    aFile <<  "Commodity" << ',' << "Consumption" << ',' << "Price Paid" << endl;
    for( CInputIterator curr = input.begin(); curr != input.end(); ++curr ) {
        (*curr)->csvSGMOutputFile( aFile, period );
    }
    aFile << "Total Consumption" << ',' << mOutputs[ period ] << endl << endl;
}

void BaseTechnology::accept( IVisitor* aVisitor, const int period ) const
{
	aVisitor->updateBaseTechnology( this );
	for( CInputIterator cInput = input.begin(); cInput != input.end(); ++cInput ) {
		(*cInput)->accept( aVisitor, period );
	}
}

const string BaseTechnology::getIdentifier() const {
    return createIdentifier( name, year );
}

const string BaseTechnology::createIdentifier( const string& aName, int aYear ){
    return aName + util::toString( aYear );
}
