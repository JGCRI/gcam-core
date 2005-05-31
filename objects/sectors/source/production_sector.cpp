/*! 
* \file production_sector.cpp
* \ingroup Objects
* \brief Sector class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"
#include "sectors/include/sector.h"
#include "sectors/include/subsector.h"
#include "sectors/include/production_sector.h"
#include "sectors/include/more_sector_info.h"
#include "containers/include/scenario.h"
#include "investment/include/iinvestor.h"
// Need a factory method.
#include "investment/include/accelerator.h"
#include "investment/include/market_based_investment.h"
#include "investment/include/investment_utils.h"
#include "marketplace/include/marketplace.h"
#include "marketplace/include/imarket_type.h"
#include "reporting/include/output_container.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

//! Constructor.
ProductionSector::ProductionSector ( const string& aRegionName ) : Sector ( aRegionName ) {
    mIsFixedPrice = false;
    mIsEnergyGood = false;
    mIsPrimaryEnergyGood = false;
    mIsSecondaryEnergyGood = false;
}

//! Default destructor
ProductionSector::~ProductionSector() {
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Sonny Kim, Josh Lurz
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool ProductionSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // Note: This doesn't handle add-ons here. Difficult part is if a different type of 
    // sector investment is requested in an add on.
    if( nodeName == Accelerator::getXMLNameStatic() ){
        mInvestor.reset( new Accelerator() );
        mInvestor->XMLParse( curr );
    }
    else if( nodeName == MarketBasedInvestor::getXMLNameStatic() ){
        mInvestor.reset( new MarketBasedInvestor() );
        mInvestor->XMLParse( curr );
    }
    // Note: This behavior is either on or off, not by period currently.
    else if( nodeName == "FixedPricePath" ){
        mIsFixedPrice = XMLHelper<bool>::getValue( curr );
	} 
    else if( nodeName == "ghgEmissCoef" ){
		ghgEmissCoefMap[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
    } 
    else if( nodeName == "IsEnergyGood" ){
        mIsEnergyGood = XMLHelper<bool>::getValue( curr );
    }
    else if( nodeName == "IsPrimaryEnergyGood" ){
        mIsPrimaryEnergyGood = XMLHelper<bool>::getValue( curr );
    }
    else if( nodeName == "IsSecondaryEnergyGood" ){
        mIsSecondaryEnergyGood = XMLHelper<bool>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param node pointer to the current node in the XML input tree
* \todo Remove this function
*/
bool ProductionSector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    // do nothing
    return false;
}

void ProductionSector::toInputXMLDerived( std::ostream& out, Tabs* tabs ) const {
    if( mInvestor.get() ){
        mInvestor->toInputXML( out, tabs );
    }
    XMLWriteElementCheckDefault( mIsFixedPrice, "FixedPricePath", out, tabs );
    XMLWriteElementCheckDefault( mIsEnergyGood, "IsEnergyGood", out, tabs );

    for( map<string, double>::const_iterator coef = ghgEmissCoefMap.begin(); coef != ghgEmissCoefMap.end(); ++coef ){
        XMLWriteElement( coef->second, "ghgEmissCoef", out, tabs, 0, coef->first );
    }
}

//! Write out debugging information.
void ProductionSector::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    if( mInvestor.get() ){
        mInvestor->toDebugXML( period, out, tabs );
    }
    XMLWriteElement( mIsFixedPrice, "FixedPricePath", out, tabs, false );
    XMLWriteElement( mIsEnergyGood, "IsEnergyGood", out, tabs );

    for( map<string, double>::const_iterator coef = ghgEmissCoefMap.begin(); coef != ghgEmissCoefMap.end(); ++coef ){
        XMLWriteElement( coef->second, "ghgEmissCoef", out, tabs, 0, coef->first );
    }
}

//! Complete the initialization of this object.
void ProductionSector::completeInit(){
    // Call parent class complete init.
    Sector::completeInit();
    // Initialize the investment object to the default if one has not been read in.
    if( !mInvestor.get() ){
        cout << "Warning: Creating default investment type for sector " << name << "." << endl;
        mInvestor.reset( new Accelerator() );
    }

    mInvestor->completeInit( regionName, name );
}

void ProductionSector::setMarket() {	
    Marketplace* marketplace = scenario->getMarketplace();
    // name is Sector name (name of good supplied or demanded)
    // market is the name of the regional market from the input file (i.e., global, region, regional group, etc.)
    if( marketplace->createMarket( regionName, market, name, IMarketType::NORMAL ) ) {
        marketplace->setPriceVector( name, regionName, sectorprice );
        for( int period = 0; period < scenario->getModeltime()->getmaxper(); ++period ){
            // MarketInfo needs to be set in period 0, but the market should never be set to solve 
            // in period zero. 
            if( !mIsFixedPrice ){
                if( period > 0 ){
			        marketplace->setMarketToSolve( name, regionName, period );
                }
		    }
            else {
                marketplace->setMarketInfo( name, regionName, period, "IsFixedPrice", 1.0 );
            }
        }
   
    }
}

void ProductionSector::initCalc( const int period, const MarketInfo* aMarketInfo, NationalAccount& nationalAccount,
                                 Demographic* aDemographics ) {
	calcPriceReceived( period );

    // Setup the market information on the sector.
    Marketplace* marketplace = scenario->getMarketplace();
    
    // Shouldn't all this be in complete init?
    // Set whether it is an energy or material good. 
    marketplace->setMarketInfo( name, regionName, 0, "IsEnergyGood", mIsEnergyGood ? 1 : 0 );
    marketplace->setMarketInfo( name, regionName, 0, "IsPrimaryEnergyGood", mIsPrimaryEnergyGood ? 1 : 0 );
    marketplace->setMarketInfo( name, regionName, 0, "IsSecondaryEnergyGood", mIsSecondaryEnergyGood ? 1 : 0 );
    
    // Set the energy to physical conversion factor if the MoreSectorInfo
    // specifies one.
    if( moreSectorInfo.get() ){
        double newConversionFactor = moreSectorInfo->getValue( MoreSectorInfo::ENERGY_CURRENCY_CONVERSION );
        marketplace->setMarketInfo( name, regionName, 0, "ConversionFactor", newConversionFactor );
    }

    // add ghg gass coefficients to the market info for this sector
    for( map<string,double>::iterator i = ghgEmissCoefMap.begin(); i != ghgEmissCoefMap.end(); ++i ){
        marketplace->setMarketInfo( name, regionName, 0, i->first + "coefficient", i->second );
    }
    
    // The ITC is being read in at the sector level but set to the national level?
	if( moreSectorInfo.get() ){
		nationalAccount.setAccount( NationalAccount::INVESTMENT_TAX_CREDIT, 
			moreSectorInfo->getValue( MoreSectorInfo::INVEST_TAX_CREDIT_RATE ) );
	}

	Sector::initCalc( period, aMarketInfo, nationalAccount, aDemographics );
}

/*! \brief returns Sector output.
*
* Returns the total amount of the ProductionSector. 
*
* \author Sonny Kim
* \param period Model period
* \todo make year 1975 regular model year so that logic below can be removed
* \return total output
*/
double ProductionSector::getOutput( const int aPeriod ) const {
    double output = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        double subsecOutput = subsec[ i ]->getOutput( aPeriod );
        // error check.
        if ( !util::isValidNumber( subsecOutput ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Output for subsector " << subsec[ i ]->getName() << " in Sector " << name 
                    << " in region " << regionName <<" is not valid." << endl;
            continue;
        }
        output += subsecOutput;
    }
    return output;
}
	//! Operate the capital of the sector.
void ProductionSector::operate( NationalAccount& aNationalAccount, const Demographic* aDemographic,
                                const int aPeriod )
{
	calcPriceReceived( aPeriod );
    operateOldCapital( aDemographic, aNationalAccount, aPeriod );
    calcInvestment( aDemographic, aNationalAccount, aPeriod );
    operateNewCapital( aDemographic, aNationalAccount, aPeriod );
}

//! Calculate new investment.
// precondition: old vintages have calculated their output.
void ProductionSector::calcInvestment( const Demographic* aDemographic,
                                       NationalAccount& aNationalAccount,
                                       const int period )
{
    // Calculate and distribute investment to the subsectors of this sector.
    vector<IInvestable*> investableSubsecs = InvestmentUtils::convertToInvestables( subsec );
    mInvestor->calcAndDistributeInvestment( investableSubsecs,
                                            aNationalAccount,
                                            aDemographic,
                                            period );
}
    

//! Operate old capital.
void ProductionSector::operateOldCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount,
                                          const int period )
{
    for( CSubsectorIterator currSub = subsec.begin(); currSub != subsec.end(); ++currSub ){
        // flag tells the subsector only to operate old capital.
        (*currSub)->operate( aNationalAccount, aDemographic, moreSectorInfo.get(), false, period );
    }
}

//! Operate new capital.
void ProductionSector::operateNewCapital( const Demographic* aDemographic, NationalAccount& aNationalAccount,
                                          const int period )
{
    for( CSubsectorIterator currSub = subsec.begin(); currSub != subsec.end(); ++currSub ){
        // flag tells the subsector to operate new capital.
        (*currSub)->operate( aNationalAccount, aDemographic, moreSectorInfo.get(), true, period );
    }
}
/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, Sonny Kim
* \return The constant XML_NAME.
*/
const std::string& ProductionSector::getXMLName() const {
	return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, Sonny Kim
* \return The constant XML_NAME as a static.
*/
const std::string& ProductionSector::getXMLNameStatic() {
    const static string XML_NAME = "productionSector";
	return XML_NAME;
}

//! Calculate price received
void ProductionSector::calcPriceReceived( const int period ){

    Marketplace* marketplace = scenario->getMarketplace();
	// set price received in market info
	double priceReceived = ( marketplace->getPrice( name, regionName, period ) + 
		( moreSectorInfo->getValue( MoreSectorInfo::TRANSPORTATION_COST )
		* moreSectorInfo->getValue( MoreSectorInfo::TRAN_COST_MULT ) ) )
		/ ( 1 + moreSectorInfo->getValue( MoreSectorInfo::IND_BUS_TAX_RATE ) );
	// set price received in market info
	marketplace->setMarketInfo( name, regionName, period, "priceReceived", priceReceived );
}

//! Update an OutputContainer for reporting.
void ProductionSector::updateOutputContainer( OutputContainer* aOutputContainer, const int aPeriod ) const {
    // Update the base class
    Sector::updateOutputContainer( aOutputContainer, aPeriod );
    // Update the output container for the derived class.
    aOutputContainer->updateProductionSector( this, aPeriod );
}
