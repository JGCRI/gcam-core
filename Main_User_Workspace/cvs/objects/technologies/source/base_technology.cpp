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
#include "functions/include/iinput.h"
#include "technologies/include/ioutput.h"
#include "technologies/include/sgm_output.h"
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
#include "emissions/include/aghg.h"
#include "util/base/include/ivisitor.h"
#include "emissions/include/ghg_factory.h"
#include "emissions/include/co2_emissions.h"
#include "functions/include/function_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef vector<IInput*>::iterator InputIterator;
typedef vector<IInput*>::const_iterator CInputIterator;
typedef vector<AGHG*>::const_iterator CGHGIterator;
typedef vector<AGHG*>::iterator GHGIterator;

//!< Default Constructor
BaseTechnology::BaseTechnology() {
    const int maxper = scenario->getModeltime()->getmaxper();
    expenditures.resize( maxper );
    prodDmdFn = 0;
}

//!< Destructor
BaseTechnology::~BaseTechnology() {
    clear();
}

BaseTechnology::BaseTechnology( const BaseTechnology& baseTechIn ) {
    const int maxper = scenario->getModeltime()->getmaxper();
    expenditures.resize( maxper );
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
    for ( unsigned int i = 0; i < baseTechIn.mOutputs.size(); i++) {
        mOutputs.push_back( baseTechIn.mOutputs[i]->clone() );
    }
    for( CGHGIterator ghg = baseTechIn.mGhgs.begin(); ghg != baseTechIn.mGhgs.end(); ++ghg ){
        mGhgs.push_back( (*ghg)->clone() );
    }
}

void BaseTechnology::copyParam( const BaseTechnology* baseTechIn,
                                const int aPeriod )
{
    name = baseTechIn->name;
    categoryName = baseTechIn->categoryName;
    prodDmdFnType = baseTechIn->prodDmdFnType;
    prodDmdFn = baseTechIn->prodDmdFn;
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int initialYear = max( modeltime->getStartYear(), year );
    const int initialPeriod = modeltime->getyr_to_per( initialYear );
    
    // First loop through and remove any inputs that are not in the copy-from technology.
    for( InputIterator iter = input.begin(); iter != input.end(); ++iter ){
        if( !isCoefBased() && (*iter)->getCurrencyDemand( initialPeriod ) != 0 ){
            continue;
        }
        // Doesn't exist in the other one. 
        if( !util::hasValue( baseTechIn->inputNameToNo, (*iter)->getName() ) ){
            // Remove the map entry for it. This is not strictly necessary.
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
            IInput* newInput = (*iter)->clone();
            // TODO: Figure out if we need to initialize here.
            // newInput->completeInit( initialPeriod );
            input.push_back( newInput );
            inputNameToNo[ (*iter)->getName() ] = static_cast<int>( input.size() - 1 );
        }
        // It already exists, we need to copy into.
        else {
            IInput* newInput = input[ util::searchForValue( inputNameToNo, (*iter)->getName() ) ];
            newInput->copyParam( *iter, aPeriod );

            // TODO: Figure out if we need to initialize here.
            // newInput->completeInit( initialPeriod );
        }
    } // end for
    
    // For each Ghg check if it exists in the current technology.
    for ( CGHGIterator ghg = baseTechIn->mGhgs.begin(); ghg != baseTechIn->mGhgs.end(); ++ghg ) {
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
    for( vector<IOutput*>::iterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ) {
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
        else if( GHGFactory::isGHGNode( nodeName ) ){
            parseContainerNode( curr, mGhgs, mGhgNameMap, GHGFactory::create( nodeName ).release() );
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
    
    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
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

    for( unsigned int iter = 0; iter < mOutputs.size(); iter++ ){
        mOutputs[ iter ]->toDebugXML( period, out, tabs );
    }
    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->toDebugXML( period, out, tabs );
    }
    expenditures[ period ].toDebugXML( period, out, tabs );

    toDebugXMLDerived( period, out, tabs );

    XMLWriteClosingTag( getXMLName(), out, tabs );
}

//! Complete the initialization of the BaseTechnology object.
void BaseTechnology::completeInit( const string& aRegionName,
                                   const string& aSectorName,
                                   const string& aSubsectorName )
{
    const Modeltime* modeltime = scenario->getModeltime();
    const int initialYear = max( modeltime->getStartYear(), year );
    const int initialPeriod = modeltime->getyr_to_per( initialYear );

    for( InputIterator anInput = input.begin(); anInput != input.end(); ++anInput ) {
         // This does not appear to do anything, might not work if it did due to later copying.
        (*anInput)->completeInit( aRegionName, aSectorName, aSubsectorName, name, 0, 0 );
    }

    // Check if CO2 is missing. 
    if( !util::hasValue( mGhgNameMap, CO2Emissions::getXMLNameStatic() ) ){
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        mGhgs.push_back( new CO2Emissions() ); // at least CO2 must be present
        mGhgNameMap[ "CO2" ] = static_cast<int>( mGhgs.size() ) - 1;
    }
    for( GHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        // (*ghg)->completeInit();
    }

    // Create the primary output for this technology. All technologies will have
    // a primary output. Always insert the primary output at position 0.
    SGMOutput* primaryOutput;
    
    // outputs before the start year need to get their conversionFactor now,
    // outputs in the initial year will have it's conversionFactor calculated in
    // the base year's initCalc and set it then
    if( initialYear == year ) {
        primaryOutput = new SGMOutput( aSectorName );
    } else {
        double conversionFactor = FunctionUtils::getMarketConversionFactor( aRegionName, aSectorName );
        double co2Coef = FunctionUtils::getCO2Coef( aRegionName, aSectorName, 0 );
        primaryOutput = new SGMOutput( aSectorName, conversionFactor, co2Coef );
    }
    mOutputs.insert( mOutputs.begin(), primaryOutput );    
        
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->completeInit( aSectorName, 0, 0, true );
    }
    initProdDmdFn();
}

void BaseTechnology::initCalc( const MoreSectorInfo* aMoreSectorInfo, const string& aRegionName,
                               const string& aSectorName, NationalAccount& nationalAccount,
                               const Demographic* aDemographics, const double aCapitalStock, const int aPeriod )
{
    for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
        (*ghg)->initCalc( aRegionName, 0, aPeriod );
    }

    for( InputIterator i = input.begin(); i != input.end(); ++i ){
        // TODO: Fix the is new vintage parameter here to be right.
        //( *i )->initCalc( aRegionName, aSectorName, isTrade(), true, aPeriod );
    }
    
    // Initialize the inputs.
    for( InputIterator curr = input.begin(); curr != input.end(); ++curr ){
        // TODO: Fix the is new vintage parameter here to be right.
        (*curr)->initCalc( aRegionName, aSectorName, true, isTrade(), aPeriod );
    }
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->initCalc( aRegionName, aSectorName, aPeriod );
    }
}

/*! \brief Clear out empty inputs.
* \details Loop through the set of inputs for a technology and remove all inputs
*          which have a currency demand of zero. Resets the name to number map
*          to contain the correct mappings.
*/
void BaseTechnology::removeEmptyInputs(){

    const Modeltime* modeltime = scenario->getModeltime();
    const int initialYear = max( modeltime->getStartYear(), year );
    const int initialPeriod = modeltime->getyr_to_per( initialYear );

    for( InputIterator currInput = input.begin(); currInput != input.end(); ++currInput ){
        if( (*currInput)->getCurrencyDemand( initialPeriod ) == 0 ){
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

//! Initialize prodDmdFn by choosing the appropriate type
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
    return mOutputs[ 0 ]->getCurrencyOutput( aPeriod );
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
        // fails without using inputName in argument, don't know the cause
        double tempPricePaid = 0;
        if( input[ i ]->hasTypeFlag( IInput::CAPITAL ) ){
            tempPricePaid = input[ i ]->getPrice( aRegionName, aPeriod ) + input[i]->getPriceAdjustment();
        }
        // This is not right because capital and numeraire are not mutually exclusive.
        else if( input[ i ]->hasTypeFlag( IInput::NUMERAIRE ) ){
            tempPricePaid = 1;
        }
        else {
            // Calculate GHG taxes.
            double carbonTax = 0;
            for( CGHGIterator ghg = mGhgs.begin(); ghg != mGhgs.end(); ++ghg ){
                carbonTax += (*ghg)->getGHGValue( input[ i ], aRegionName, aSectorName, aPeriod );
            }
            tempPricePaid = ( ( input[ i ]->getPrice( aRegionName, aPeriod ) + 
                ( transportationAdder * transportationMult ) ) * proportionalTax + additiveTax + carbonTax ) * 
                input[i]->getPriceAdjustment();
        }
        input[i]->setPricePaid( tempPricePaid, aPeriod );
    }
}

void BaseTechnology::updateMarketplace( const string& sectorName, const string& regionName, const int period ) {
    Marketplace* marketplace = scenario->getMarketplace();
    double totalDemand = 0;
    for( InputIterator curr = input.begin(); curr != input.end(); ++curr ) {
        double tempDemand = (*curr)->getCurrencyDemand( period );
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
    aFile << "Total Consumption" << ',' << mOutputs[ 0 ]->getCurrencyOutput( period ) << endl << endl;
}

void BaseTechnology::accept( IVisitor* aVisitor, const int aPeriod ) const
{
    aVisitor->startVisitBaseTechnology( this, aPeriod );
    for( CInputIterator cInput = input.begin(); cInput != input.end(); ++cInput ) {
        (*cInput)->accept( aVisitor, aPeriod );
    }

    if( aPeriod == -1 ) {
        int currPeriod = 0;
        for( vector<Expenditure>::const_iterator cExpenditure = expenditures.begin(); cExpenditure !=
            expenditures.end(); ++cExpenditure )
        {
            (*cExpenditure).accept( aVisitor, currPeriod );
            ++currPeriod;
        }
    } else {
        expenditures[ aPeriod].accept( aVisitor, aPeriod );
    }
    for( vector<AGHG*>::const_iterator cGHG = mGhgs.begin(); cGHG != mGhgs.end(); ++cGHG ) {
        (*cGHG)->accept( aVisitor, aPeriod );
    }

    aVisitor->endVisitBaseTechnology( this, aPeriod );
}

const string BaseTechnology::getIdentifier() const {
    return createIdentifier( name, year );
}

const string BaseTechnology::createIdentifier( const string& aName, int aYear ){
    return aName + util::toString( aYear );
}
