/*! 
* \file technology.cpp
* \ingroup Objects
* \brief Technology class source file.
* \author Sonny Kim
*/
// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <xercesc/dom/DOMNamedNodeMap.hpp>

// User headers
#include "technologies/include/technology.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/gdp.h"
#include "util/base/include/configuration.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/dependency_finder.h"
#include "technologies/include/icapture_component.h"
#include "technologies/include/capture_component_factory.h"
#include "technologies/include/ishutdown_decider.h"
#include "technologies/include/shutdown_decider_factory.h"
#include "util/base/include/ivisitor.h"
#include "containers/include/iinfo.h"

#include "technologies/include/ioutput.h"
#include "technologies/include/primary_output.h"
#include "technologies/include/output_factory.h"

#include "technologies/include/ical_data.h"
#include "technologies/include/iproduction_state.h"
#include "technologies/include/production_state_factory.h"
#include "technologies/include/marginal_profit_calculator.h"
#include "technologies/include/generic_technology_info.h"
#include "technologies/include/global_technology.h"
#include "technologies/include/global_technology_database.h"
#include "emissions/include/ghg_factory.h"
#include "emissions/include/co2_emissions.h"

// TODO: Factory for cal data objects.
#include "technologies/include/cal_data_input.h"
#include "technologies/include/cal_data_output.h"
#include "technologies/include/cal_data_output_percap.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef vector<IOutput*>::iterator OutputIterator;
typedef vector<IOutput*>::const_iterator COutputIterator;

typedef vector<AGHG*>::iterator GHGIterator;
typedef vector<AGHG*>::const_iterator CGHGIterator;

typedef vector<IProductionState*>::iterator ProductionStateIterator;
typedef vector<IProductionState*>::const_iterator CProductionStateIterator;

typedef vector<IShutdownDecider*>::iterator ShutdownDeciderIterator;
typedef vector<IShutdownDecider*>::const_iterator CShutdownDeciderIterator;

/*! 
 * \brief Constructor.
 * \param aName Technology name.
 * \param aYear Technology year.
 */
Technology::Technology( const string& aName, const int aYear ): mName( aName ), year ( aYear ){
    init();
}

//! Destructor
Technology::~Technology() {
    clear();
}

//! Copy constructor
Technology::Technology( const Technology& aTech ):mName( aTech.mName ), year( aTech.year ) {
    init();
    copy( aTech );
}

//! Assignment operator.
Technology& Technology::operator = ( const Technology& techIn ) {
    if( this != &techIn ) { // check for self assignment 
        clear();
        init();
        copy( techIn );
    }
    return *this;
}

//! Helper copy function to avoid replicating code.
void Technology::copy( const Technology& techIn ) {
    mName = techIn.mName;
    mLifetimeYears = techIn.mLifetimeYears;
    mGetGlobalTech = techIn.mGetGlobalTech;

    year = techIn.year;
    shrwts = techIn.shrwts;
    pMultiplier = techIn.pMultiplier;
    lexp = techIn.lexp;

    mInput = techIn.mInput;
    mCosts = techIn.mCosts;
    mFixedOutput = techIn.mFixedOutput;

    ghgNameMap = techIn.ghgNameMap;
    mShutdownDecidersMap = techIn.mShutdownDecidersMap;

    if( techIn.mCaptureComponent.get() ){
        mCaptureComponent.reset( techIn.mCaptureComponent->clone() );
    }
    for (CGHGIterator iter = techIn.ghg.begin(); iter != techIn.ghg.end(); ++iter) {
        ghg.push_back( (*iter)->clone() );
    }
    for ( CShutdownDeciderIterator iter = techIn.mShutdownDeciders.begin();
        iter != techIn.mShutdownDeciders.end(); ++iter)
    {
        mShutdownDeciders.push_back( (*iter)->clone() );
    }

    // all cloning should have been done before completeInit
    // because during completeInit GlobalTechnologies are fetched
    // and they cannot be cloned.
    if( techIn.mTechData.get() ) {
        mTechData.reset( techIn.mTechData->clone() );
    } else {
        mTechData.reset();
    }
    
    // Clone the existing cal data object if there is one.
    // TODO: This may correct given the usage of clone in technology to copy forward.
    mCalValue.reset( techIn.mCalValue.get() ? techIn.mCalValue->clone() : 0 );
    for (CGHGIterator iter = techIn.ghg.begin(); iter != techIn.ghg.end(); ++iter) {
        ghg.push_back( (*iter)->clone() );
    }
    for ( COutputIterator iter = techIn.mOutputs.begin(); iter != techIn.mOutputs.end(); ++iter) {
        mOutputs.push_back( (*iter)->clone() );
    }
}

//! Clear member variables.
void Technology::clear(){
    // Delete the GHGs and shutdown deciders.
    for( GHGIterator iter = ghg.begin(); iter != ghg.end(); ++iter ) {
        delete *iter;
    }
    for( ShutdownDeciderIterator iter = mShutdownDeciders.begin(); iter != mShutdownDeciders.end(); ++iter ) {
        delete *iter;
    }
    for( ProductionStateIterator iter = mProductionState.begin(); iter != mProductionState.end(); ++iter ) {
        delete *iter;
    }
    for( OutputIterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ) {
        delete *iter;
    }
}

//! Initialize elemental data members.
void Technology::init(){
    // This will be reinitialized in completeInit once the technologies start
    // year is known.
    mLifetimeYears = -1;

    const Modeltime* modeltime = scenario->getModeltime();
    mInput.resize( modeltime->getmaxper(), 0.0 );
    mCosts.resize( modeltime->getmaxper(), -1.0 );
    mProductionState.resize( modeltime->getmaxper() );
    shrwts = 1;
    pMultiplier = 1;
    lexp = getLogitExpDefault(); 
    mFixedOutput = IProductionState::fixedOutputDefault();
    mGetGlobalTech = false;
}

/*! \brief initialize Technology with xml data
*
* \author Josh Lurz
* \param node current node
*/
void Technology::XMLParse( const DOMNode* node ) {  
    /*! \pre Assume we are passed a valid node. */
    assert( node );
    
    const DOMNodeList* nodeList = node->getChildNodes();
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ) {
        const DOMNode* curr = nodeList->item( i );
        if( curr->getNodeType() != DOMNode::ELEMENT_NODE ) {
            continue;
        }
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );        
        if( nodeName == "lifetime" ){
            mLifetimeYears = XMLHelper<int>::getValue( curr );
        }
        else if( nodeName == "fuelname" ){
            createTechData();
            mTechData->setFuelName( XMLHelper<string>::getValue( curr ) );
        }
        else if( nodeName == "sharewt" ){
            shrwts = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fuelprefElasticity" ){
            createTechData();  
            mTechData->setFuelPrefElasticity( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "efficiency" ){
            createTechData();
            mTechData->setEfficiency( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "nonenergycost" ){
            createTechData();
            mTechData->setNonEnergyCost( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "pMultiplier" ){
           pMultiplier = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fMultiplier" ){
            createTechData();
            mTechData->setFMultiplier( XMLHelper<double>::getValue( curr ) );
        }
        else if( nodeName == "logitexp" ){
            lexp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fixedOutput" ){
            mFixedOutput = XMLHelper<double>::getValue( curr );
        }
        else if( CaptureComponentFactory::isOfType( nodeName ) ){
            // Check if a new capture component needs to be created because
            // there is not currently one or the current type does not match the
            // new type.
            if( !mCaptureComponent.get() || !mCaptureComponent->isSameType( nodeName ) ){
                mCaptureComponent = CaptureComponentFactory::create( nodeName );
            }
            mCaptureComponent->XMLParse( curr );
        }
        else if( ShutdownDeciderFactory::isOfType( nodeName ) ){
            parseContainerNode( curr, mShutdownDeciders, mShutdownDecidersMap,
                                ShutdownDeciderFactory::create( nodeName ).release() );
        }
        else if( nodeName == CalDataInput::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataInput );
        }
        else if( nodeName == CalDataOutput::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataOutput );
        }
        else if( nodeName == CalDataOutputPercap::getXMLNameStatic() ){
            parseSingleNode( curr, mCalValue, new CalDataOutputPercap );
        }
        else if( GHGFactory::isGHGNode( nodeName ) ){
            parseContainerNode( curr, ghg, ghgNameMap, GHGFactory::create( nodeName ).release() );
        }
        else if( OutputFactory::isOfType( nodeName ) ){
            parseContainerNode( curr, mOutputs, OutputFactory::create( nodeName ).release() );
        }
        else if( nodeName == GlobalTechnology::getXMLNameStatic() ) {
            mGetGlobalTech = true;
        }
        else if( nodeName == "keyword" ){
            DOMNamedNodeMap* keywordAttributes = curr->getAttributes();
            for( unsigned int attrNum = 0; attrNum < keywordAttributes->getLength(); ++attrNum ) {
                DOMNode* attrTemp = keywordAttributes->item( attrNum );
                mKeywordMap[ XMLHelper<string>::safeTranscode( attrTemp->getNodeName() ) ] = 
                    XMLHelper<string>::safeTranscode( attrTemp->getNodeValue() );
            }
        }
        // parse derived classes
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName
                    << " found while parsing " << getXMLName1D() << "." << endl;
        }
    }
}

/*! \brief Creates a generic technology info
 *  \details Will create and set a new generic technology
 *           info if mTechData has not already been set.
 *           Also will reset the flag to get a global technology
 *           to false as the generic technology is overriding it.
 *  \author Pralit Patel
 */
void Technology::createTechData() {
    if( !mTechData.get() ) {
        mTechData.reset( new GenericTechnologyInfo( mName ) );
    }
    mGetGlobalTech = false;
}

/*!
* \brief Complete the initialization of the technology.
* \note This routine is only called once per model run
* \param aSectorName Sector name, also the name of the product.
* \param aDepDefinder Regional dependency finder.
* \param aSubsectorInfo Subsector information object.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \author Josh Lurz
* \warning Markets are not necessarily set when completeInit is called
*/
void Technology::completeInit( const string& aRegionName,
                               const string& aSectorName,
                               DependencyFinder* aDepFinder,
                               const IInfo* aSubsectorInfo,
                               ILandAllocator* aLandAllocator,
                               const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Check for an unset or invalid year.
    if( year == 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Technology " << mName << " in sector " << aSectorName
            << " has an invalid year attribute." << endl;
    }

    // Default the lifetime to be the time step beginning when the Technology is
    // created.
    const Modeltime* modeltime = scenario->getModeltime();
    if( mLifetimeYears == -1 ){
        mLifetimeYears = modeltime->gettimestep( modeltime->getyr_to_per( year ) );
    }


    if( mGetGlobalTech && aGlobalTechDB ) {
        mTechData = aGlobalTechDB->getTechnology( mName, year );
    }
    if( !mTechData.get() ) {
        // create one so that it can have default values
        mTechData.reset( new GenericTechnologyInfo( mName ) );
    }
    mTechData->completeInit();

    if( !util::hasValue( ghgNameMap, CO2Emissions::getXMLNameStatic() ) ) {
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        AGHG* CO2 = new CO2Emissions(); // at least CO2 must be present
        ghg.push_back( CO2 );
        ghgNameMap[ CO2Emissions::getXMLNameStatic() ] = static_cast<int>( ghg.size() ) - 1;
    }

    // Create the primary output for this technology. All technologies will have
    // a primary output. Always insert the primary output at position 0.
    mOutputs.insert( mOutputs.begin(), new PrimaryOutput( aSectorName ) );

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        // TODO: Change this when dependencies are determined by period.
        mOutputs[ i ]->completeInit( aSectorName, aDepFinder, !hasNoInputOrOutput( 0 ) );
    }

    // Add the input dependency to the dependency finder if there is one. There
    // will not be one if this is a demand Technology.
    if( aDepFinder ){
        // Don't add dependency if technology does not ever function. If this is
        // the case, the technology can never have an effect on the markets.
        // This is necessary for export sectors to operate correctly, but will
        // be true in general.
        // TODO: Change this when dependencies are determined by period.
        if ( !hasNoInputOrOutput( 0 ) ) { // note the NOT operator
            aDepFinder->addDependency( aSectorName, mTechData->getFuelName() );
        }
    }

    // Clear share weights for fixed output technologies.
    if( mFixedOutput != IProductionState::fixedOutputDefault() ){
        shrwts = 0;
    }

    // Calibrating to zero does not work correctly.
    if( mCalValue.get() ){
        bool calibrationValid = false;
        // TODO: This code is excessively complex to handle the different permutations
        // of calibration and fixed output. What if fixed output were its own technology?
        // Check for attempts to calibrate fixed output.
        if( mFixedOutput != IProductionState::fixedOutputDefault() ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Cannot calibrate a fixed output Technology. Turning off calibration." << endl;
        }
        // If the shareweights are zero we are implicitly calibrating to zero.
        else if( ( shrwts < util::getSmallNumber() ) 
                && ( mCalValue->getCalInput( getEfficiency( 0 ) ) > 0 ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Ignoring calibration input for Technology " << mName
                    << " due to a zero shareweight." << endl;
        }
        else if( mCalValue->getCalInput( getEfficiency( 0 ) ) < 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Negative calibration value for technology " << mName
                    << ". Calibration removed." << endl;
        }
        // TODO: Calibrating to zero does not work correctly so reset the share weights
        // to zero and remove the calibration input. This could be improved.
        else if( mCalValue->getCalInput( getEfficiency( 0 ) ) < util::getSmallNumber() ){
            shrwts = 0;
        }
        else {
            calibrationValid = true;
        }

        // If the calibration setup was not valid remove the input.
        if( !calibrationValid ){
            mCalValue.reset( 0 );
        }
    }

    // Accidentally missing CO2 is very easy to do, and would cause big
    // problems. Add it automatically if it does not exist.
    if( !util::hasValue( ghgNameMap, CO2Emissions::getXMLNameStatic() ) ){
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        ghg.push_back( new CO2Emissions() ); // at least CO2 must be present
        ghgNameMap[ "CO2" ] = static_cast<int>( ghg.size() ) - 1;
    }

    // Initialize the capture component.
    if( mCaptureComponent.get() ){
        mCaptureComponent->completeInit( aRegionName, aSectorName, aDepFinder );
    }

    // Set the production state for the initial periods. Later periods will be
    // set in postCalc. The production state represents how the technology
    // decides to produce output.
    setProductionState( 0 );
}

//! write object to xml output stream
void Technology::toInputXML( ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLNameStatic2D(), out, tabs, "", year );
    // write the xml for the class members.
    XMLWriteElementCheckDefault( shrwts, "sharewt", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( mLifetimeYears, "lifetime", out, tabs, -1 );
    
    if ( mCalValue.get() ){
        mCalValue->toInputXML( out, tabs );
    }
    
    // don't print GlobalTechs in toInputXML, they will be printed in the
    // GlobalTechnologyDatabase
    if( !mGetGlobalTech ) {
        mTechData->toInputXML( out, tabs );
    }
    else {
        XMLWriteElement<string>( "", GlobalTechnology::getXMLNameStatic(), out, tabs );
    }

    XMLWriteElementCheckDefault( pMultiplier, "pMultiplier", out, tabs, 1.0 );
    XMLWriteElementCheckDefault( lexp, "logitexp", out, tabs,  getLogitExpDefault() );
    XMLWriteElementCheckDefault( mFixedOutput, "fixedOutput", out, tabs, IProductionState::fixedOutputDefault() );
    
    if( mCaptureComponent.get() ){
        mCaptureComponent->toInputXML( out, tabs );
    }
    
    for( CShutdownDeciderIterator i = mShutdownDeciders.begin();
         i != mShutdownDeciders.end(); ++i )
    {
        ( *i )->toInputXML( out, tabs );
    }

    for( COutputIterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ){
        ( *iter )->toInputXML( out, tabs );
    }
    for( CGHGIterator iter = ghg.begin(); iter != ghg.end(); ++iter ){
        ( *iter )->toInputXML( out, tabs );
    }

    // finished writing xml for the class members.
    toInputXMLDerived( out, tabs );
    XMLWriteClosingTag( getXMLNameStatic2D(), out, tabs );
}

//! write object to xml debugging output stream
void Technology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    // Only output technologies that are operating.
    if( !mProductionState[ period ]->isOperating() ){
        return;
    }

    XMLWriteOpeningTag( getXMLName1D(), out, tabs, mName, year );
    // write the xml for the class members.
    
    XMLWriteElement( shrwts, "sharewt", out, tabs );

    XMLWriteElement( mFixedOutput, "fixedOutput", out, tabs );
    XMLWriteElement( mLifetimeYears, "lifetime", out, tabs );
    if ( mCalValue.get() ) {
        mCalValue->toDebugXML( out, tabs );
    }

    mTechData->toDebugXML( period, out, tabs );

    XMLWriteElement( pMultiplier, "pMultiplier", out, tabs );
    XMLWriteElement( lexp, "logitexp", out, tabs );
    XMLWriteElement( mInput[ period ], "input", out, tabs );

    if( mCaptureComponent.get() ){
        mCaptureComponent->toDebugXML( period, out, tabs );
    }
    for( CShutdownDeciderIterator i = mShutdownDeciders.begin(); i != mShutdownDeciders.end(); ++i ){
        ( *i )->toDebugXML( period, out, tabs );
    }
    mProductionState[ period ]->toDebugXML( period, out, tabs );

    for( COutputIterator iter = mOutputs.begin(); iter != mOutputs.end(); ++iter ){
        ( *iter )->toDebugXML( period, out, tabs );
    }

    // write our ghg object, vector is of number of gases
    for( CGHGIterator i = ghg.begin(); i != ghg.end(); i++ ){
        ( *i )->toDebugXML( period, out, tabs );
    }

    // finished writing xml for the class members.
    toDebugXMLDerived( period, out, tabs );
    XMLWriteClosingTag( getXMLName1D(), out, tabs );
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& Technology::getXMLNameStatic2D() {
    const static string XML_NAME_2D = "period";
    return XML_NAME_2D;
}

/*! \brief Perform initializations that only need to be done once per period.
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aSubsectorInfo Parent information container.
* \param aDemographics Regional demographics container.
* \param aPeriod Model period.
*/
void Technology::initCalc( const string& aRegionName,
                           const string& aSectorName,
                           const IInfo* aSubsectorInfo,
                           const Demographic* aDemographics,
                           const int aPeriod )
{
    // initialize calDataobjects for the period.
    if ( mCalValue.get() ){
        mCalValue->initCalc( aDemographics, aPeriod );
    }

    const string& fuelName = mTechData->getFuelName();
    if( mCaptureComponent.get() ){
        mCaptureComponent->initCalc( aRegionName, aSectorName, fuelName, aPeriod );
    }

    // Check that the market for this technology is setup correctly.
    if( !fuelName.empty() && fuelName != "renewable" && fuelName != "none" &&
        scenario->getMarketplace()->getPrice( fuelName, aRegionName, aPeriod, false )
        == Marketplace::NO_MARKET_PRICE )
    {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Fuel " << fuelName << " for technology " << mName << " does not exist." << endl;
    }
    
    for( unsigned int i = 0; i < ghg.size(); i++ ){
        ghg[i]->initCalc( aRegionName, fuelName, aSubsectorInfo, aPeriod );
    }

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->initCalc( aRegionName, aPeriod );
    }
}

/*!
 * \brief Initializes the production state for the period.
 * \details Sets up the production state in the given period. Responsibility for
 *          selecting the production state is delegated to the
 *          ProductionStateFactory.
 * \param aPeriod Model period.
 */
void Technology::setProductionState( const int aPeriod ){
    // Check that the state for this period has not already been initialized. An
    // assertion here usually means initCalc was called twice for a single
    // period.

    // PolicyTargetRunner crashes on this in debug mode -- evidently objects are initialized more than once
    // So comment out for debugging.
    assert( !mProductionState[ aPeriod ] );
    
    const Modeltime* modeltime = scenario->getModeltime();
    double initialOutput =
        mOutputs[ 0 ]->getPhysicalOutput( modeltime->getyr_to_per( year ) );
    
    mProductionState[ aPeriod ] =
        ProductionStateFactory::create( year, mLifetimeYears, mFixedOutput,
                                        initialOutput, aPeriod ).release();
}

/*!
 * \brief Calculates all technology benefits and costs not accounted for by the
 *        primary output.
 * \details Technologies may contain greenhouse gases and secondary output,
 *          which incur both costs and benefits to the technology. Costs can be
 *          incurred if the emissions are taxed, or if the secondary output has
 *          a cost. Benefits may accrue if a the emissions are negative or if
 *          the secondary output is has a positive value.
 * \author Sonny Kim, Josh Lurz
 * \param aRegionName The region containing this technology.
 * \param aPeriod The period to calculate this value for.
 * \return Total secondary value.
 */
double Technology::calcSecondaryValue( const string& aRegionName, const int aPeriod ) const {
    double totalValue = 0;
    // Add all costs from the GHGs.
    for( unsigned int i = 0; i < ghg.size(); ++i ){
        totalValue -= ghg[i]->getGHGValue( aRegionName,
                                           mTechData->getFuelName(),
                                           mOutputs,
                                           getEfficiency( aPeriod ),
                                           mCaptureComponent.get(),
                                           aPeriod );
    }

    // Add all values from the outputs. The primary output is included in this
    // loop but will have a value of zero.
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        totalValue += mOutputs[ i ]->getValue( aRegionName, mCaptureComponent.get(), aPeriod );
    }
    return totalValue;
}

/*!
* \brief Perform calculations that need to be done after the solution is found
*        for the period.
* \param aRegionName Region name.
* \param aPeriod Model period that has solved.
*/
void Technology::postCalc( const string& aRegionName, const int aPeriod ) {
    for( unsigned int i = 0; i < mOutputs.size(); ++i ) {
        mOutputs[ i ]->postCalc( aRegionName, aPeriod );
    }
    // Set the production state for the next period.
    if( aPeriod + 1 < scenario->getModeltime()->getmaxper() ){
        setProductionState( aPeriod + 1 );
    }
}

/*! \brief This function calculates the sum of the Carbon Values for all GHG's
*          in this Technology.
* \details The function first checks if a carbon tax exists for the Technology,
*          and if it does loops through all GHGs to calculate a sum carbon
*          value. The GHG function which it calls, getGHGValue() calculates the
*          carbon equivalent of all GHG's contained in this Technology. The
*          totalGHGCost attribute of the Technology is then set to this new
*          value.
* \author Sonny Kim, Josh Lurz
* \param aRegionName The region containing this Technology.
* \param aSectorName The sector containing this Technology.
* \param aPeriod The period to calculate this value for.
* \return The total emissions and storage cost of all ghgs.
*/
double Technology::getTotalGHGCost( const string& aRegionName,
                                    const string& aSectorName,
                                    const int aPeriod ) const
{
    double totalGHGCost = 0;
    // totalGHGCost and carbontax must be in same unit as fuel price
    for( unsigned int i = 0; i < ghg.size(); i++ ){
        totalGHGCost += ghg[i]->getGHGValue( aRegionName,
                                             mTechData->getFuelName(),
                                             mOutputs,
                                             getEfficiency( aPeriod ),
                                             mCaptureComponent.get(),
                                             aPeriod );
    }
    return totalGHGCost;
}

/*!
* \brief Calculate unnormalized technology unnormalized shares.
* \author Sonny Kim, Steve Smith
* \param aRegionName Region name.
* \param aSectorName Sector name, also the name of the product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \todo Check to see if power function for trivial values really wastes time
* \return The Technology share.
*/
double Technology::calcShare( const string& aRegionName,
                              const string& aSectorName,
                              const GDP* aGDP,
                              const int aPeriod ) const
{
    // A Technology which is not operating does not have a share.
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return 0;
    }

    // Vintages and fixed output technologies should never have a share.
    if( !mProductionState[ aPeriod ]->isNewInvestment() ||
        mFixedOutput != IProductionState::fixedOutputDefault() ){
        return 0;
    }

    // Calculate the new vintage share.
    assert( getCost( aPeriod ) > 0 );
    double share = shrwts * pow( getCost( aPeriod ), lexp );
    
    // This is rarely used, so probably worth it to not to waste cycles on the power function. sjs
    if ( mTechData->getFuelPrefElasticity() != 0 ) {
        double scaledGdpPerCapita = aGDP->getBestScaledGDPperCap( aPeriod );
        share *= pow( scaledGdpPerCapita, mTechData->getFuelPrefElasticity() );
    }

    assert( share >= 0 && util::isValidNumber( share ) );
    return share;
}

/*! \brief Return true if technology is fixed for no output or input
* 
* returns true if this technology is set to never produce output or input
* At present, this can only be guaranteed by assigning a fixedOutput value of zero.
*
* \author Steve Smith
* \return Returns whether this technology will always have no output or input
*/
bool Technology::hasNoInputOrOutput( const int aPeriod ) const {
    // Technology has zero fixed output if fixed output was read-in as zero.
    return( util::isEqual( mFixedOutput, 0.0 ) );
}

/*! \brief Return fixed Technology output
* \details Returns the current value of fixed output. This may differ from the
*          variable fixedOutput due to scale down if demand is less than the
*          value of fixedOutput.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \author Steve Smith
* \param aPeriod model period
* \return Value of fixed output for this Technology
*/
double Technology::getFixedOutput( const string& aRegionName,
                                   const string& aSectorName, 
                                   const int aPeriod ) const
{
    // Check that a state has been created for the period.
    assert( mProductionState[ aPeriod ] );

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc( this );
    return mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                        aSectorName,
                                                        0, // No variable output.
                                                        &marginalProfitCalc,
                                                        1, // Not shutting down any fixed output using
                                                           // the scale factor.
                                                        mShutdownDeciders,
                                                        aPeriod );
}

/*! \brief Return fixed Technology input
* \details Returns the current value of fixed input. This may differ from the
*          variable fixedOutput/eff due to scale down if demand is less than the
*          value of fixed Output.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aPeriod model period
* \return value of fixed input for this Technology
* \author Steve Smith
*/
double Technology::getFixedInput( const string& aRegionName,
                                  const string& aSectorName,
                                  const int aPeriod ) const
{
    // TODO: This function is very fragile.

    // Efficiency should be positive because invalid efficiencies were
    // already corrected. 
    assert( getEfficiency( aPeriod ) > 0 );
    double currFixedOutput = getFixedOutput( aRegionName, aSectorName, aPeriod );
    // TODO: Fixed output of zero? What does this want to do with vintage output?
    // This is a hack to work around assumptions about a fixed output of zero.
    if( currFixedOutput > 0
        || util::isEqual( mFixedOutput, 0.0 )
        || util::isEqual( shrwts, 0.0 )
        || mTechData->getFuelName()== "renewable" 
        || !mProductionState[ aPeriod ]->isNewInvestment()
        || !mProductionState[ aPeriod ]->isOperating() )
    {
        return currFixedOutput / getEfficiency( aPeriod );
    }
    return -1;
}

/*
 * \brief Return the amount of input required to produce a specified amount of output.
 * \param aRequiredOutput The required amount of output.
 * \param aPeriod Model period.
 * \return The amount of input required for the output.
 */
double Technology::getRequiredInputForOutput( double aRequiredOutput,
                                              const int aPeriod ) const
{
    // Efficiency should be positive because invalid efficiencies were
    // already corrected. 
    assert( getEfficiency( aPeriod ) > 0 );
    return aRequiredOutput / getEfficiency( aPeriod );
}

/*! \brief Calculates fuel input and Technology output.
* \details Adds demands for fuels and ghg emissions to markets in the
*          marketplace.
* \param aRegionName name of the region
* \param aSectorName name of the product for this sector
* \param aVariableDemand Total variable demand for this subsector.
* \param aFixedOutputScaleFactor Scale factor by which to reduce production when
*        fixed output is greater than demand.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
*/
void Technology::production( const string& aRegionName,
                             const string& aSectorName,
                             double aVariableDemand,
                             double aFixedOutputScaleFactor,
                             const GDP* aGDP,
                             const int aPeriod )
{
    // Can't have a scale factor and positive demand.
    assert( aFixedOutputScaleFactor == 1 || aVariableDemand == 0 );

    // Can't have negative variable demand.
    assert( aVariableDemand >= 0 && util::isValidNumber( aVariableDemand ) );

    // Check for positive variable demand and positive fixed output.
    assert( mFixedOutput == IProductionState::fixedOutputDefault() || util::isEqual( aVariableDemand, 0.0 ) );

    // Check that a state has been created for the period.
    assert( mProductionState[ aPeriod ] );

    // Construct a marginal profit calculator. This allows the calculation of 
    // marginal profits to be lazy.
    MarginalProfitCalculator marginalProfitCalc( this );

    // Use the production state to determine output.
    double primaryOutput =
        mProductionState[ aPeriod ]->calcProduction( aRegionName,
                                                     aSectorName,
                                                     aVariableDemand,
                                                     &marginalProfitCalc,
                                                     aFixedOutputScaleFactor,
                                                     mShutdownDeciders,
                                                     aPeriod );

    // Calculate input demand.
    // Efficiency should be positive because invalid efficiencies were
    // already corrected. 
    assert( getEfficiency( aPeriod ) > 0 );
    mInput[ aPeriod ] = primaryOutput / getEfficiency( aPeriod );

    Marketplace* marketplace = scenario->getMarketplace();
    // set demand for fuel in marketplace
    
    const string& fuelName = mTechData->getFuelName();
    if( ( fuelName != "renewable" ) && ( fuelName != "none" ) && mInput[ aPeriod ] > util::getSmallNumber() ){ 
        marketplace->addToDemand( fuelName, aRegionName, mInput[ aPeriod ], aPeriod );
    }
    // Set the supply of the good to the marketplace.
    calcEmissionsAndOutputs( aRegionName, mInput[ aPeriod ], primaryOutput, aGDP, aPeriod );
}

/*!
 * \brief Calculate the emissions, primary and secondary outputs for the
 *        Technology.
 * \details Determines the output levels and emissions for the Technology once
 *          the primary output and input quantities are known. Emissions and
 *          outputs are added to the marketplace by the Output and GHG objects.
 * \param aRegionName Region name.
 * \param aInput Input quantity.
 * \param aPrimaryOutput Primary output quantity.
 * \param aGDP Regional GDP container.
 * \param aPeriod Period.
 */
void Technology::calcEmissionsAndOutputs( const string& aRegionName,
                                          const double aInput,
                                          const double aPrimaryOutput,
                                          const GDP* aGDP,
                                          const int aPeriod )
{
    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->setPhysicalOutput( aPrimaryOutput, aRegionName,
                                          mCaptureComponent.get(), aPeriod );
    }

    // calculate emissions for each gas after setting input and output amounts
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        ghg[ i ]->calcEmission( aRegionName, mTechData->getFuelName(), mInput[ aPeriod ], mOutputs,
                                aGDP, mCaptureComponent.get(), aPeriod );
    }
}

/*! \brief Adjusts Technology share weights to be consistent with calibration value.
* This is done only if there is more than one Technology
* Calibration is, therefore, performed as part of the iteration process. 
* Since this can change derivatives, best to turn calibration off when using N-R solver.
*
* This routine adjusts echnology share weights so that relative shares are correct for each subsector.
* Note that all calibration values are scaled (up or down) according to total sectorDemand 
* -- getting the overall scale correct is the job of the TFE calibration
*
* \author Steve Smith
* \param aTechnologyDemand Demand for the technology's product.
* \param aRegionName Region name.
* \param aSubsectorInfo Subsector information object.
* \param aPeriod Model period.
*/
void Technology::adjustForCalibration( double aTechnologyDemand,
                                       const string& aRegionName,
                                       const IInfo* aSubsectorInfo,
                                       const int aPeriod )
{
    // Variable demand must be positive.
    assert( aTechnologyDemand >= 0 );

    // Don't try and adjust the Technology if it isn't available.
    if( !isCalibrating( aPeriod ) ){
        return;
    }

    // total calibrated outputs for this subsector
    double calOutput = getCalibrationOutput( aPeriod );

    // Given the technology is available for calibration it must
    // have a positive calibration value.
    assert( calOutput >= 0 );

    // Adjust share weights
    if ( aTechnologyDemand > 0 ) {
        double shareScaleValue = calOutput / aTechnologyDemand;
        shrwts *= shareScaleValue;
    }

    // Share weight must be positive after adjustment.
    assert( shrwts >= 0 );

    // Report if share weight gets extremely large
    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    if ( debugChecking && shrwts > 1e6 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Large share weight in calibration for Technology: " << mName << endl;
    }
}

//! calculate GHG emissions from Technology use
/* \brief Get a map containing emissions by gas from the Technology.
* \param aGoodName Name of the sector.
* \param aPeriod Period for which to get emissions.
* \return A map of gas name to emissions. There are more gas names than ghgs.
*/
const map<string, double> Technology::getEmissions( const string& aGoodName, const int aPeriod ) const {
    map<string, double> emissions;
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        // emissions by gas name only
        emissions[ghg[i]->getName()] = ghg[i]->getEmission( aPeriod );
        // emissions by gas and fuel names combined
        // used to calculate emissions by fuel
        emissions[ghg[i]->getName() + mTechData->getFuelName()] = ghg[i]->getEmission( aPeriod );
        // add sequestered amount to emissions map used to calculate emissions
        // by fuel if there are sequestered emissions.
        if( mCaptureComponent.get() ){
            emissions[ ghg[i]->getName() + "sequestGeologic" ] = mCaptureComponent->
                getSequesteredAmount( ghg[ i ]->getName(),
                true, aPeriod );
            emissions[ ghg[i]->getName() + "sequestNonEngy" ] = mCaptureComponent->
                getSequesteredAmount( ghg[ i ]->getName(),
                false, aPeriod );
        }
    }

    return emissions;
}

/* \brief Get a map containing emissions by fuel from the Technology.
* \param aGoodName Name of the sector.
* \param aPeriod Period for which to get emissions.
* \return A map of fuel name to emissions.
*/
const map<string, double> Technology::getEmissionsByFuel( const string& aGoodName, const int aPeriod ) const {
    map<string, double> emissionsByFuel;
    for ( unsigned int i = 0; i < ghg.size(); ++i ) {
        emissionsByFuel[ghg[i]->getName()] = ghg[i]->getEmissFuel( aPeriod );
        // This really should include the GHG name as well.
        emissionsByFuel[mTechData->getFuelName()] = ghg[i]->getEmissFuel( aPeriod );
    }
    return emissionsByFuel;
}

/*! \brief Returns technology name
*
* \author Sonny Kim
* \return sector name as a string
*/
const string& Technology::getName() const {
    return mName;
}

/*! \brief Returns name of the fuel consumed by this Technology
*
* \author Sonny Kim
* \return fuel name as a string
*/
const string& Technology::getFuelName() const {
    return mTechData->getFuelName();
}

/*! \brief Returns the ratio of output to input for this Technology
*
* \author Sonny Kim
* \param aPeriod Model period.
* \return efficiency (out/In) of this Technology
*/
double Technology::getEfficiency( const int aPeriod ) const {
    // calculate effective efficiency. If there is a sequestration device applied use it to
    // calculate the efficiency, otherwise use the base efficiency.
    double baseEfficiency = mTechData->getEfficiency();
    return mCaptureComponent.get() ? mCaptureComponent->getEffectiveEfficiency( baseEfficiency, aPeriod ) 
                                    : baseEfficiency;
}

/*! \brief Return fuel intensity (input over output) for this Technology
*
* \author Sonny Kim
* \todo Need to implement method of adding appropriate units (btu/kwh; gallons/mile, etc.)
*/
double Technology::getIntensity( const int aPeriod ) const {
    // Efficiency should be positive because invalid efficiencies were
    // already corrected.
    assert( getEfficiency( aPeriod ) > 0 );
    return 1 / getEfficiency( aPeriod );
}

/*! \brief returns share weight for this Technology
*
* \author Steve Smith
* \return share weight
*/
double Technology::getShareWeight() const {
    return shrwts;
}

/*! \brief scale share weight for this Technology
*
* \author Steve Smith
* \param scaleValue multiplicative scaling factor for share weight
*/
void Technology::scaleShareWeight( double scaleValue ) {
    shrwts *= scaleValue;
}

/*! \brief scale share weight for this Technology
*
* \author Steve Smith
* \param shareWeightValue new value for share weight
*/
void Technology::setShareWeight( double shareWeightValue ) {
    shrwts = shareWeightValue;
}

//! returns true if all output is either fixed or calibrated
bool Technology::isOutputFixed( const int aPeriod ) const {
    // In the initial investment period, output is fixed under the following conditions.
    if( mProductionState[ aPeriod ]->isNewInvestment() ){
        return ( ( mCalValue.get() )
               || ( mFixedOutput != IProductionState::fixedOutputDefault() ) 
               || ( shrwts == 0 ) || mTechData->getFuelName() == "renewable" );
    }
    // Otherwise output is always fixed.
    return true;
}

/*! \brief Returns whether the Technology share weight should be adjusted by the
*          calibration routine.
* \details A technology should be calibrated if it is a new vintage with a
*          read-in calibration input or output, is not a fixed output, and has a
*          non-zero share weight.
* \author Steve Smith
* \param aPeriod Model period.
* \return Whether the Technology should be calibrated.
*/
bool Technology::isCalibrating( const int aPeriod ) const {
    return ( ( shrwts > 0 )
             && mProductionState[ aPeriod ]->isNewInvestment() 
             && ( mCalValue.get() )
             && ( mFixedOutput == IProductionState::fixedOutputDefault() ) );
}

//! return fuel input for Technology
double Technology::getInput( const int aPeriod ) const {
    return mInput[ aPeriod ];
}

/*! \brief Return the Technology's output for a given period.
* \details TODO
* \param aPeriod The period for which to get output.
* \return The output for the period.
*/
double Technology::getOutput( const int aPeriod ) const {
    // Primary output is at position zero.
    return mOutputs[ 0 ]->getPhysicalOutput( aPeriod );
}

/*! \brief Return Technology fuel cost.
* \param aRegionName The region containing the Technology.
* \param aSectorName The sector containing the Technology.
* \param aPeriod Period in which to calculate the fuel cost.
* \return A calculate fuel cost for the Technology.
*/
double Technology::getFuelCost( const string& aRegionName, const string& aSectorName, const int aPeriod ) const {
    Marketplace* marketplace = scenario->getMarketplace();
     // code special case where there is no fuel input. sjs
     // used now to drive non-CO2 GHGs
    double fuelprice;
    const string& fuelName = mTechData->getFuelName();
    if ( fuelName == "none" || fuelName == "renewable" ) {
        fuelprice = 0;
    }
    else {
        fuelprice = marketplace->getPrice( fuelName, aRegionName, aPeriod );
    } 

    // The market price of the fuel must be valid.
    if ( fuelprice == Marketplace::NO_MARKET_PRICE ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Requested fuel >" << fuelName << "< with no price in technology " << mName 
            << " in sector " << aSectorName << " in region " << aRegionName << "." << endl;
        // set fuelprice to a valid, although arbitrary, number
        fuelprice = util::getLargeNumber();
    }
    // fMultiplier and pMultiplier are initialized to 1 for those not read in
    return ( fuelprice * mTechData->getFMultiplier() ) / getEfficiency( aPeriod );
}

//! return technology calibration value
double Technology::getCalibrationInput( const int aPeriod ) const {
    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        if( mCalValue.get() ){
            return mCalValue->getCalInput( getEfficiency( aPeriod ) );
        }
        if( shrwts < util::getTinyNumber() &&
            mFixedOutput == IProductionState::fixedOutputDefault() ){
            return 0;
        }
    }
    return -1;
}

//! scale technology calibration or fixed values
void Technology::scaleCalibrationInput( const double aScaleFactor ) {
    if ( mCalValue.get() ){
        mCalValue->scaleValue( aScaleFactor );
    }
}

/*! \brief Calculate Technology fuel cost and total cost.
* \details This calculates the cost (per unit output) of this specific
*          Technology. The cost includes fuel cost, carbon value, and non-fuel
*          costs. Conversion efficiency, and optional fuel cost and total price
*          multipliers are used if specified.
* \author Sonny Kim, Steve Smith
* \param aRegionName Region name.
* \param aSectorName SectorName
*/
void Technology::calcCost( const string& aRegionName, const string& aSectorName, const int aPeriod ) {
    // If it is an existing stock or is a fixed output technology it has no
    // marginal cost.
    if( !mProductionState[ aPeriod ]->isNewInvestment() || mFixedOutput != IProductionState::fixedOutputDefault() ){
        mCosts[ aPeriod ] = 0;
    }
    else {
        double techCost = ( getFuelCost( aRegionName, aSectorName, aPeriod )
                            + getNonEnergyCost( aPeriod ) ) * pMultiplier
                            - calcSecondaryValue( aRegionName, aPeriod );

        assert( util::isValidNumber( techCost ) );

        // techcost can drift below zero in disequilibrium.
        mCosts[ aPeriod ] = max( techCost, util::getSmallNumber() );
    }
}

/*!
* \brief Get the total cost of the technology for a period.
* \details Returns the previously calculated cost for a period.
* \pre calcCost has been called for the iteration.
* \param aPeriod Model period.
* \return The total Technology cost.
*/
double Technology::getCost( const int aPeriod ) const {
    // Check that the cost has been calculated for the period. This could still
    // be a stale cost however if the cost has not been calculated for the
    // iteration.
    assert( mCosts[ aPeriod ] != -1 );
    return mCosts[ aPeriod ];
}

//! return technology calibration value
double Technology::getCalibrationOutput( const int aPeriod ) const {
    // If there is fixed output there is not calibrated output.
    assert( ! ( mFixedOutput != IProductionState::fixedOutputDefault() && mCalValue.get() ) );

    if( mProductionState[ aPeriod ]->isNewInvestment() ) {
        if( mCalValue.get() ){
            return mCalValue->getCalOutput( getEfficiency( aPeriod ) );
        }
        if( shrwts < util::getTinyNumber() && mFixedOutput == IProductionState::fixedOutputDefault() ){
            return 0;
        }
    }
    return -1;
}

/*! \brief Return the non-energy cost of the Technology
* \param aPeriod Model period.
*/
double Technology::getNonEnergyCost( const int aPeriod ) const {
    // If there is a sequestration device use it to calculate the total
    // non-energy cost, otherwise use the base non energy cost.
    double baseNonEnergyCost = mTechData->getNonEnergyCost();
    return mCaptureComponent.get() ? mCaptureComponent->getTotalNonEnergyCost( mTechData->getEfficiency(),
                                                                               baseNonEnergyCost,
                                                                               aPeriod ) 
                                      : baseNonEnergyCost;
}

/*! \brief Return a vector listing the names of all the GHGs within the Technology.
* \details This function returns all GHG names the Technology contains. It does 
* this by searching the underlying ghgNameMap.
* \author Josh Lurz
* \return A vector of GHG names contained in the Technology.
*/
const vector<string> Technology::getGHGNames() const {
    return util::getKeys( ghgNameMap );
}

/*! \brief returns the number of ghg objects.
*
* Calcuation is done using length of GHG string to be consistant with use of ghg names to access GHG information.
*
*
* \author Steve Smith
*/
int Technology::getNumbGHGs()  const {
    vector<string> ghgNames = getGHGNames();
    return static_cast<int>( ghgNames.size() ); 
}


/*! \brief Copies parameters across periods for a specific GHG 
* \param prevGHG Pointer to the previous GHG object that needs to be passed to the corresponding object this period.
* \warning Assumes there is only one GHG object with any given name
*/
void Technology::copyGHGParameters( const AGHG* prevGHG ) {
    const int ghgIndex = util::searchForValue( ghgNameMap, prevGHG->getName() );

    if ( prevGHG ) {
        ghg[ ghgIndex ]->copyGHGParameters( prevGHG );
    }
     
}

/*! \brief Returns the pointer to a specific GHG 
* \param aGHGName Name of GHG 
*/
const AGHG* Technology::getGHGPointer( const string& aGHGName ) const {
    const int ghgIndex = util::searchForValue( ghgNameMap, aGHGName );

    return ghg[ ghgIndex ];
     
}

//! return value for ghg
double Technology::getEmissionsByGas( const string& aGasName, const int aPeriod ) const {
    for( unsigned int i = 0; i < ghg.size(); ++i ){
        if( ghg[ i ]->getName() == aGasName ){
            return ghg[ i ]->getEmission( aPeriod );
        }
    }
    return 0;
}

//! Set the technology year.
void Technology::setYear( const int aYear ) {
    // This is called through parsing, so report an error to the user.
    if( aYear <= 0 ){
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid year passed to set year for technology " << mName << "." << endl;
    }
    else {
        year = aYear;
    }
}

/*! \brief Check for fixed demands and set values to the counter.
* If the output of this Technology is fixed then set that value to the
* appropriate marketplace counter. If it is not, then reset counter.
*
* \author Steve Smith
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aPeriod Model period.
*/
void Technology::tabulateFixedDemands( const string& aRegionName,
                                       const string& aSectorName,
                                       const int aPeriod )
{
    const double MARKET_NOT_FIXED = -1;

    // Non-operating technologies have no fixed demands.
    if( !mProductionState[ aPeriod ]->isOperating() ){
        return;
    }

    Marketplace* marketplace = scenario->getMarketplace();
    IInfo* fuelInfo = marketplace->getMarketInfo( mTechData->getFuelName(), aRegionName, aPeriod, false );
    if( fuelInfo ){
        if( isOutputFixed( aPeriod ) ) {

            double existingDemand = fuelInfo->getDouble( "calDemand", false );

            double fixedInput = getCalibrationInput( aPeriod );
            // this sector does not have fixed input.
            if( fixedInput == -1 ){
                double currFixedInput = getFixedInput( aRegionName, aSectorName, aPeriod );
                if( currFixedInput >= 0 ){
                    fixedInput = currFixedInput;
                }
            }
            // Make sure we aren't setting a negative demand into the marketplace.
            assert( fixedInput >= 0 );

            // set demand for fuel in marketInfo counter
            fuelInfo->setDouble( "calDemand", max( existingDemand, 0.0 ) + fixedInput );        
        }
        else {
            // If not fixed, then set to -1 to indicate a demand that is not
            // completely fixed.
            fuelInfo->setDouble( "calDemand", MARKET_NOT_FIXED );
        }
    }
}

/*! \brief Test to see if calibration worked for this Technology.
* \author Josh Lurz
* \param aPeriod The model period.
* \param aCalAccuracy Accuracy (fraction) to check if calibrations are within.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aSubsectorName Subsector name.
* \param aPrintWarnings Whether to print a warning.
* \return Whether calibration was successful.
*/
bool Technology::isAllCalibrated( const int aPeriod,
                                  double aCalAccuracy,
                                  const string& aRegionName, 
                                  const string& aSectorName,
                                  const string& aSubsectorName,
                                  const bool aPrintWarnings ) const
{
    // Check that the period is the new vintage period.
    if( !mProductionState[ aPeriod ]->isNewInvestment() ){
        return true;
    }

    // If the subsector is calibrated at the subsector level, check that it was successful.
    const double calOutput = getCalibrationOutput( aPeriod );
    if ( calOutput != -1 ) {
        const double output = getOutput( aPeriod );
        double relativeDiff;
        if( calOutput > 0 ){
            relativeDiff = fabs( output - calOutput ) / calOutput;
        }
        else {
            // Use absolute accuracy since the calibrated output level is zero.
            relativeDiff = fabs( output - calOutput );
        }
        if( relativeDiff > aCalAccuracy ){
            if( aPrintWarnings ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Calibration failed by " << relativeDiff * 100 << " percent for Technology "
                        << mName << " in subsector " << aSubsectorName << " in sector " << aSectorName
                        << " in region " << aRegionName << " with output " << output
                        << " and calibration value " << calOutput << "." << endl;
            }
            return false;
        }
    }
    // Calibration at the Technology level was successful.
    return true;
}

/*! \brief Return the default Technology logit exponential.
* \return The default logit exponential.
*/
double Technology::getLogitExpDefault(){
    return -6;
}

/*! \brief Return the marginal revenue for this Technology's output.
* \details The marginal revenue for the Technology is defined as the market
*          price for the good in the given period divided by the price
*          multiplier.
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aPeriod Model period.
* \return The marginal revenue.
*/
double Technology::getMarginalRevenue( const string& aRegionName,
                                       const string& aSectorName,
                                       const int aPeriod ) const
{
    // Price multiplier must be positive.
    assert( pMultiplier > 0 );

    // TODO: Change to true when below is fixed.
    double marginalRevenue = scenario->getMarketplace()->getPrice( aSectorName, aRegionName,
                                                                   aPeriod, false );

    // Demand sectors won't have markets so the price could be wrong here. This
    // will be fixed by splitting demand and supply sectors.
    // TODO: This will prevent vintaging from working for end use sectors.
    // assert( price != Marketplace::NO_MARKET_PRICE );
    if( marginalRevenue == Marketplace::NO_MARKET_PRICE ){
        return 0;
    }
    
    // Adjust for the price multiplier.
    marginalRevenue /= pMultiplier;

    // Add any value or costs of secondary good.
    marginalRevenue += calcSecondaryValue( aRegionName, aPeriod );

    return marginalRevenue;
}

/*! \brief Update an output container with information from a technology for a
*          given period.
* \param aOutputContainer The output container to update.
* \param aPeriod The period for which to update.
*/
void Technology::accept( IVisitor* aVisitor, const int aPeriod ) const {
    aVisitor->startVisitTechnology( this, aPeriod );

    for( unsigned int i = 0; i < mOutputs.size(); ++i ){
        mOutputs[ i ]->accept( aVisitor, aPeriod );
    }

    for( unsigned int i = 0; i < ghg.size(); ++i ){
        ghg[ i ]->accept( aVisitor, aPeriod );
    }
    aVisitor->endVisitTechnology( this, aPeriod );
}
