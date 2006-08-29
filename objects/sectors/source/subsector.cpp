/*! 
* \file subsector.cpp
* \ingroup Objects
* \brief Subsector class source file.
* \author Sonny Kim, Josh Lurz
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <vector>
#include <algorithm>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "util/base/include/configuration.h"
#include "sectors/include/subsector.h"
#include "technologies/include/technology.h"
#include "technologies/include/itechnology.h"
#include "containers/include/scenario.h"
#include "sectors/include/sector.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "containers/include/gdp.h"
#include "containers/include/info_factory.h"
#include "containers/include/iinfo.h"
#include "technologies/include/base_technology.h"
#include "consumers/include/consumer.h"
#include "consumers/include/household_consumer.h"
#include "consumers/include/govt_consumer.h"
#include "consumers/include/trade_consumer.h"
#include "consumers/include/invest_consumer.h"
#include "technologies/include/production_technology.h"
#include "util/base/include/ivisitor.h"
#include "technologies/include/technology_type.h"
#include "investment/include/idistributor.h"
#include "investment/include/iexpected_profit_calculator.h"
#include "investment/include/investment_utils.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string Subsector::XML_NAME = "subsector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, etc.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
const double LOGIT_EXP_DEFAULT = -3;

Subsector::Subsector( const string aRegionName, const string aSectorName ):
regionName( aRegionName ),
sectorName( aSectorName ){
    basesharewt = 0;

    // resize vectors.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    capLimit.resize( maxper, 1.0 );
    shrwts.resize( maxper, 1.0 ); // default 1.0, for sectors with one tech.
    lexp.resize( maxper, LOGIT_EXP_DEFAULT );
    share.resize(maxper); // subsector shares
    subsectorprice.resize(maxper); // subsector price for all periods
    fuelprice.resize(maxper); // subsector fuel price for all periods
    summary.resize(maxper); // object containing summaries
    fuelPrefElasticity.resize( maxper );
    summary.resize( maxper );
    calibrationStatus.resize( maxper, false );
    fixedShare.resize( maxper );
    capLimited.resize( maxper, false );
    scaleYear = modeltime->getEndYear(); // default year to scale share weight to after calibration
    techScaleYear = modeltime->getEndYear(); // default year to scale share weight to after calibration
    mInvestments.resize( maxper );
    mFixedInvestments.resize( maxper, -1 );
}

/*! \brief Default destructor.
*
* deletes all technology objects associated  with this sector.
*
* \author Josh Lurz
*/
Subsector::~Subsector() {
    clear();
}

//! Deallocate the subsector memory.
void Subsector::clear(){
    for ( vector< vector< ITechnology* > >::iterator outerIter = techs.begin(); outerIter != techs.end(); outerIter++ ) {
        for( vector< ITechnology* >::iterator innerIter = outerIter->begin(); innerIter != outerIter->end(); innerIter++ ) {
            delete *innerIter;
        }
    }
    for( BaseTechIterator delTech = baseTechs.begin(); delTech != baseTechs.end(); ++delTech ){
        delete *delTech;
    }
    for( map<string, TechnologyType*>::iterator techType = mTechTypes.begin(); techType != mTechTypes.end();
        ++techType )
    {
        delete techType->second;
    }
}

/*! \brief Returns sector name
*
* \author Sonny Kim
* \return sector name as a string
*/
const string Subsector::getName() const {
    return name;
}

//! Initialize Subsector with xml data
void Subsector::XMLParse( const DOMNode* node ) {   

    /*! \pre Make sure we were passed a valid node. */
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttr( node, "name" );

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();

    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "capacitylimit" ){
            XMLHelper<double>::insertValueIntoVector( curr, capLimit, modeltime );
        }
        else if( nodeName == "sharewt" ){
            XMLHelper<double>::insertValueIntoVector( curr, shrwts, modeltime );
        }
        else if( nodeName == "logitexp" ){
            XMLHelper<double>::insertValueIntoVector( curr, lexp, modeltime );
        }

        else if( nodeName == "fuelprefElasticity" ){
            XMLHelper<double>::insertValueIntoVector( curr, fuelPrefElasticity, modeltime );  
        }

        // basesharewt is not a vector but a single value
        else if( nodeName == "basesharewt" ){
            basesharewt = XMLHelper<double>::getValue( curr );
            share[0] = basesharewt;
        }
        else if( nodeName == "scaleYear" ){
            scaleYear = XMLHelper<int>::getValue( curr );
        }
        // Fixed investment
        else if( nodeName == "FixedInvestment" ){
            XMLHelper<double>::insertValueIntoVector( curr, mFixedInvestments, scenario->getModeltime() );
        }
        // household consumer object for final demands
        else if( nodeName == HouseholdConsumer::getXMLNameStatic() ) {
            parseBaseTechHelper( curr, new HouseholdConsumer() );
        }
        // government consumer object for final demands
        else if( nodeName == GovtConsumer::getXMLNameStatic() ) {
            parseBaseTechHelper( curr, new GovtConsumer() );
        }
        // Trade consumer object for final demands
        else if( nodeName == TradeConsumer::getXMLNameStatic() ) {
            parseBaseTechHelper( curr, new TradeConsumer() );
        }
        // government consumer object for final demands
        else if( nodeName == InvestConsumer::getXMLNameStatic() ) {
            parseBaseTechHelper( curr, new InvestConsumer() );
        }
        // production technology object for production sectors
        else if( nodeName == ProductionTechnology::getXMLNameStatic() ) {
            parseBaseTechHelper( curr, new ProductionTechnology() );
        }
        else if( nodeName == "techScaleYear" ){
            techScaleYear = XMLHelper<int>::getValue( curr );
        }
        else if( isNameOfChild( nodeName ) ){
            typedef vector<vector<ITechnology*> >::iterator TechVecIterator;

            const string techName = XMLHelper<string>::getAttr( curr, "name" );
            if( name.empty() ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Ignoring technology set because it does not have a name." << endl;
                continue;
            }

            // Search the vectors of technologies for a vector with a technology
            // with the given name. This would be greatly helped by a wrapper.
            TechVecIterator techPosition = techs.begin();
            for( ; techPosition != techs.end(); ++techPosition )
            {
                // Check if this is the correct technology vector.
                if( findTechName( *techPosition ) == techName ){
                    break;
                }
            }

            if( techPosition != techs.end() ) {
                // technology already exists.
                // Check if we should delete. This is a hack.
                if( XMLHelper<bool>::getAttr( curr, "delete" ) ){
                    // Deallocate memory.
                    for( vector<ITechnology*>::iterator iter = techPosition->begin();
                         iter != techPosition->end(); ++iter )
                    {
                        delete *iter;
                    }

                    // Wipe the vector.
                    techs.erase( techPosition );
                } // end hack.
                else {
                    DOMNodeList* childNodeList = curr->getChildNodes();

                    // loop through technologies children.
                    for( unsigned int j = 0; j < childNodeList->getLength(); j++ ){
                        DOMNode* currChild = childNodeList->item( j );
                        string childNodeName = XMLHelper<void>::safeTranscode( currChild->getNodeName() );

                        if( childNodeName == "#text" ){
                            continue;
                        }
                        else if( childNodeName == technology::getXMLNameStatic2D() ){
                            int thisPeriod = XMLHelper<void>::getNodePeriod( currChild, modeltime );
                            // While the vector for this technology has already
                            // been created, this particular time period may not
                            // have been initialized. Create the technology for
                            // the given year if it does not exist.
                            if( !(*techPosition)[ thisPeriod ] ){
                                int techYear = modeltime->getper_to_yr( thisPeriod );
                                (*techPosition)[ thisPeriod ] = createChild( nodeName, techName,
                                                                             techYear );
                            }
                            (*techPosition)[ thisPeriod ]->XMLParse( currChild );
                        }
                    }
                }
            }
            else if( XMLHelper<bool>::getAttr( curr, "nocreate" ) ){
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "Not creating technology " << techName
                    << " in subsector " << name << " because nocreate flag is set." << endl;
            }
            else {
                // technology does not exist, create a new vector of techs.

                DOMNodeList* childNodeList = curr->getChildNodes();
                vector<ITechnology*> techVec( modeltime->getmaxper() );

                // loop through technologies children.
                for( unsigned int j = 0; j < childNodeList->getLength(); j++ ){
                    DOMNode* currChild = childNodeList->item( j );
                    const string childNodeName = XMLHelper<void>::safeTranscode( currChild->getNodeName() );

                    if( childNodeName == "#text" ){
                        continue;
                    }

                    // 2nd dimension of the tech XML is "period". This is the
                    // same for all derived technologies.
                    else if( childNodeName == technology::getXMLNameStatic2D() ){
                        int thisPeriod = XMLHelper<void>::getNodePeriod( currChild, modeltime );
                        int currYear = modeltime->getper_to_yr( thisPeriod );
                        auto_ptr<ITechnology> tempTech( createChild( nodeName, techName, currYear ) );
                        tempTech->XMLParse( currChild );


                        // Check that a technology does not already exist.
                        if( techVec[ thisPeriod ] ){
                            ILogger& mainLog = ILogger::getLogger( "main_log" );
                            mainLog.setLevel( ILogger::DEBUG );
                            mainLog << "Removing duplicate technology " << techVec[ thisPeriod ]->getName() 
                                << " in subsector " << name << " in sector " << sectorName << "." << endl;
                            delete techVec[ thisPeriod ];
                        }

                        techVec[ thisPeriod ] = tempTech.release();

                        // copy technology object for one period to all the periods
                        if ( XMLHelper<bool>::getAttr( currChild, "fillout" ) ) {
                            // will not do if period is already last period or maxperiod
                            for ( int i = thisPeriod + 1; i < modeltime->getmaxper(); i++ ) {
                                // Check that a technology does not already exist.
                                if( techVec[ i ] ){
                                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                                    mainLog.setLevel( ILogger::DEBUG );
                                    mainLog << "Removing duplicate technology " << techVec[ i ]->getName() 
                                        << " in subsector " << name << " in sector " << sectorName << "." << endl;
                                    delete techVec[ i ];
                                }
                                techVec[ i ] = techVec[ thisPeriod ]->clone();
                                techVec[ i ]->setYear( modeltime->getper_to_yr( i ) );
                            } // end for
                        } // end if fillout
                    } // end else if
                } // end for
                techs.push_back( techVec );
            }
        }
        // parsed derived classes
        else if( XMLDerivedClassParse( nodeName, curr ) ){
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLName() << endl;
        }
    }
}

//! Virtual function which specifies the XML name of the children of this class, the type of technology.
bool Subsector::isNameOfChild( const string& nodename ) const {
    return nodename == technology::getXMLNameStatic1D();
}

/*!
 * \brief Derived helper function to generate a child element or construct the
 *        appropriate technology.
 * \param aTechType The name of the XML node, which is the type of the
 *        technology.
 * \param aTechName The name of the new technology.
 * \param aYear The year of the new technology.
 * \pre isNameOfChild returned that the type could be created.
 * \author Steve Smith
 * \return A newly created technology of the specified type.
 */
ITechnology* Subsector::createChild( const string& aTechType,
                                    const string& aTechName,
                                    const int aTechYear ) const
{
    return new technology( aTechName, aTechYear );
}

//! Helper function which parses any type of base technology correctly.
void Subsector::parseBaseTechHelper( const DOMNode* aCurr, BaseTechnology* aNewTech ){
    // Ensure a valid technology was passed.
    assert( aNewTech );

    // Use an auto_ptr to take responsibility for the memory.
    auto_ptr<BaseTechnology> newTech( aNewTech );
    
    // Check if the base technology already exists.
    const string id = BaseTechnology::createIdentifier( XMLHelper<string>::getAttr( aCurr, "name" ),
                      XMLHelper<int>::getAttr( aCurr, "year" ) );

    map<string,int>::const_iterator baseTechMapIter = baseTechNameMap.find( id );
    if( baseTechMapIter != baseTechNameMap.end() ) { 
        // already exists, so tell the existing one to parse
        baseTechs[ baseTechMapIter->second ]->XMLParse( aCurr );
    }
    else { 
        // doesn't exist so use the new passed in base technology type.
        newTech->XMLParse( aCurr );

        // Add the new technology to the vector and the map.
        baseTechs.push_back( newTech.release() ); // Releases ownership of the memory.
        baseTechNameMap[ baseTechs.back()->getIdentifier() ] = static_cast<int>( baseTechs.size() ) - 1;

        // the technology type may not exist yet.
        map<string,TechnologyType*>::iterator typePos = mTechTypes.find( baseTechs.back()->getName() );
        if( typePos == mTechTypes.end() ){
            // create the tech type, set the iterator to the new item.
            // Insert returns the pair of the iterator position the item was inserted in and whether 
            // the item was inserted, so set the iterator to the first spot in the pair.
            typePos = mTechTypes.insert( make_pair( baseTechs.back()->getName(), new TechnologyType ) ).first;
        }
        typePos->second->addVintage( baseTechs.back() );

        // Set the technology type helper object to the technology. This may be moved to the constructor
        // or removed if technology type is made to inherit from IInvestable.
        baseTechs.back()->setTypeHelper( typePos->second );
    }
}

//! Parses any input variables specific to derived classes
bool Subsector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
    return false;
}

/*! \brief Complete the initialization
*
* This routine is only called once per model run
* \param aSectorInfo The parent sector info object.
* \param aDependencyFinder The regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \author Josh Lurz
* \warning markets are not necesarilly set when completeInit is called
*/
void Subsector::completeInit( const IInfo* aSectorInfo,
                              DependencyFinder* aDependencyFinder,
                              ILandAllocator* aLandAllocator,
                              const GlobalTechnologyDatabase* aGlobalTechDB)
{
    mSubsectorInfo.reset( InfoFactory::constructInfo( aSectorInfo ) );
    
    for( unsigned int i = 0; i < baseTechs.size(); i++) {
        baseTechs[i]->completeInit( regionName );
    }

    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int j = 0; j < baseTechs.size(); ++j ){
        if( baseTechs[ j ]->getYear() == modeltime->getper_to_yr( 0 ) ) {
            baseTechs[ j ]->removeEmptyInputs();
        }
    }

    typedef vector<vector<ITechnology*> >::iterator TechVecIterator;
    for ( TechVecIterator techIter = techs.begin(); techIter != techs.end(); ++techIter ) {
        bool isInvalid = initializeTechVector( *techIter, sectorName, aDependencyFinder,
                                               mSubsectorInfo.get(), aLandAllocator, aGlobalTechDB );
        // Erase the entire vector if the technologies were invalid.
        if( isInvalid ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Removing a technology " << findTechName( *techIter ) << " from subsector " << name 
                    << " in sector " << sectorName << " because all periods were not filled out." << endl;

            // Adjust the iterator back one position before deleting. This is
            // because a deleted iterator cannot be incremented.
            techs.erase( techIter-- );
        }
    }
}

/*! \brief Completes the initialization of a vector of technologies.
* \details Static function which completes the initialization of a vector of
*          technologies. This will first ensure that a technology has been
*          created for each model period. The function will then call
*          completeInit on each technology.
* \param aTechVector Vector of technologies to initialize.
* \param aSectorName Sector name.
* \param aDependencyFinder Regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \return Whether the vector should be removed.
*/
bool Subsector::initializeTechVector( vector<ITechnology*>& aTechVector,
                                     const string& aSectorName,
                                     DependencyFinder* aDependencyFinder,
                                     const IInfo* aSubsecInfo,
                                     ILandAllocator* aLandAllocator,
                                     const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // First check that the entire vector is filled out with technologies.
    // Each vector is initialized to the number of periods the model will
    // run, so this search checks whether each position is initialized. This
    // must be done at this point instead of during parsing because the
    // complete vector could be initialized through multiple files.
    typedef vector<ITechnology*>::iterator TechIterator;
    bool isEmptyPeriod = false;
    for( TechIterator tech = aTechVector.begin(); tech != aTechVector.end(); ++tech ) {
        if( !*tech ){
            isEmptyPeriod = true;
        }
    }

    // Instruct the calling function to remove this vector.
    if( isEmptyPeriod ){
        return true;
    }

    // Complete the initialization of a valid vector.
    for( TechIterator tech = aTechVector.begin(); tech != aTechVector.end(); ++tech ) {
        ( *tech )->completeInit( aSectorName, aDependencyFinder, aSubsecInfo, aLandAllocator, aGlobalTechDB );
    }
    return false;
}

/*! \brief Find a valid technology name from a vector of technologies.
* \param aTechVector A vector of technologies.
* \return A valid technology name, the empty string if one was not found.
*/
const string Subsector::findTechName( const vector<ITechnology*>& aTechVector ){
    // This will store the value of a technology name if it is
    // found. Initialized to empty so that if no technology is
    // found, the name will be empty.
    string currTechName;

    // Search for an initialized technology to get a name.
    for( vector<ITechnology*>::const_iterator singleTech = aTechVector.begin();
        singleTech != aTechVector.end(); ++singleTech )
    {
        if( *singleTech ){
            currTechName = (*singleTech)->getName();
            break;
        }
    }
    return currTechName;
}

//! Output the Subsector member variables in XML format.
void Subsector::toInputXML( ostream& out, Tabs* tabs ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // write the xml for the class members.
    XMLWriteVector( capLimit, "capacitylimit", out, tabs, modeltime, 1.0 );

    XMLWriteElementCheckDefault( scaleYear, "scaleYear", out, tabs, modeltime->getEndYear() );
    XMLWriteElementCheckDefault( techScaleYear, "techScaleYear", out, tabs, modeltime->getEndYear() );
    
    XMLWriteVector( shrwts, "sharewt", out, tabs, modeltime, 1.0 );
    
    XMLWriteVector( lexp, "logitexp", out, tabs, modeltime, LOGIT_EXP_DEFAULT );
    
    XMLWriteVector( fuelPrefElasticity, "fuelprefElasticity", out, tabs, modeltime, 0.0 );
    

    XMLWriteElementCheckDefault( basesharewt, "basesharewt", out, tabs, 0.0, modeltime->getStartYear() );
    toInputXMLDerived( out, tabs );

    for ( unsigned int i = 0; i < baseTechs.size(); i++ ){
        baseTechs[i]->toInputXML( out, tabs );
    }
    
    XMLWriteVector( mFixedInvestments, "FixedInvestment", out, tabs, modeltime, -1.0 );

    // write out the technology objects.
    for( vector< vector< ITechnology* > >::const_iterator j = techs.begin(); j != techs.end(); j++ ){
        
        // If we have an empty vector this won't work, but that should never happen.
        assert( j->begin() != j->end() );
        const ITechnology* firstTech = *( j->begin() ); // Get pointer to first element in row. 
        XMLWriteOpeningTag( firstTech->getXMLName1D(), out, tabs, firstTech->getName() );
        
        for( vector<ITechnology*>::const_iterator k = j->begin(); k != j->end(); k++ ){
            ( *k )->toInputXML( out, tabs );
        }
        
        XMLWriteClosingTag( firstTech->getXMLName1D(), out, tabs );
    }
    
    // finished writing xml for the class members.
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
*/
void Subsector::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    XMLWriteOpeningTag( getXMLName(), out, tabs, name );
    
    // Write the data for the current period within the vector.
    XMLWriteElement( capLimit[ period ], "capLimit", out, tabs );
    XMLWriteElement( shrwts[ period ], "sharewt", out, tabs );
    XMLWriteElement( scaleYear, "scaleYear", out, tabs );
    XMLWriteElement( techScaleYear, "techScaleYear", out, tabs );
    XMLWriteElement( lexp[ period ], "lexp", out, tabs );
    XMLWriteElement( fuelPrefElasticity[ period ], "fuelprefElasticity", out, tabs );
    XMLWriteElement( share[ period ], "share", out, tabs );
    XMLWriteElement( basesharewt, "basesharewt", out, tabs );
    XMLWriteElement( getInput( period ), "input", out, tabs );
    XMLWriteElement( subsectorprice[ period ], "subsectorprice", out, tabs );
    XMLWriteElement( getOutput( period ), "output", out, tabs );
    XMLWriteElement( getTotalCarbonTaxPaid( period ), "carbontaxpaid", out, tabs );
    XMLWriteElement( mInvestments[ period ], "investment", out, tabs );
    XMLWriteElement( mFixedInvestments[ period ], "FixedInvestment", out, tabs );

    toDebugXMLDerived( period, out, tabs );
    // Write out the summary object.
    // summary[ period ].toDebugXML( period, out );
    // write out the technology objects.

    for ( unsigned int j = 0; j < baseTechs.size(); j++ ) {
        // This isn't right, techs with years other the current year could change output.
        if (baseTechs[j]->getYear() == scenario->getModeltime()->getper_to_yr( period ) ) {
            baseTechs[j]->toDebugXML( period, out, tabs );
        }
    }
    
    for( unsigned int j = 0; j < techs.size(); ++j ){
        techs[ j ][ period ]->toDebugXML( period, out, tabs );
    }
    
    // write out the hydrotech. Not yet implemented
    // hydro[ period ].toDebugXML( period, out );
    
    // finished writing xml for the class members.
    
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Subsector::getXMLName() const {
    return XML_NAME;
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
const std::string& Subsector::getXMLNameStatic() {
    return XML_NAME;
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calcuations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existance of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aMoreSectorInfo SGM sector info object.
* \param aPeriod Model period
*/
void Subsector::initCalc( NationalAccount& aNationalAccount,
                          const Demographic* aDemographics,
                          const MoreSectorInfo* aMoreSectorInfo,
                          const int aPeriod )
{
    // Set any fixed demands
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        techs[i][ aPeriod ]->initCalc( regionName, sectorName, mSubsectorInfo.get(),
                                       aDemographics, aPeriod );
    }

    // Initialize the baseTechs. This might be better as a loop over tech types. 
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int j = 0; j < baseTechs.size(); j++ ){
        if( aPeriod == 0 && baseTechs[ j ]->getYear() == modeltime->getper_to_yr( 0 ) ){
            double totalCapital = mTechTypes[ baseTechs[ j ]->getName() ]->getTotalCapitalStock( baseTechs[ j ]->getYear() );
            baseTechs[ j ]->initCalc( aMoreSectorInfo, regionName, sectorName, aNationalAccount,
                                      aDemographics, totalCapital, aPeriod );
        
            // copy base year tech to old vintages
            mTechTypes[ baseTechs[ j ]->getName() ]->initializeTechsFromBase( baseTechs[ j ]->getYear() );
        }
        // If the current tech is from the previous period, initialize the current tech with its parameters.
        else if ( aPeriod > 0 && baseTechs[ j ]->getYear() == modeltime->getper_to_yr( aPeriod - 1 ) ) {
            BaseTechnology* newTech = mTechTypes[ baseTechs[ j ]->getName() ]->initOrCreateTech( modeltime->getper_to_yr( aPeriod ), baseTechs[ j ]->getYear() );
            // Check if initOrCreate created a technology which needs to be added to the base tech vector and map.
            if( newTech ){
                // If the tech already existed, it will get initCalc called on it later in this loop. 
                baseTechs.push_back( newTech );
                baseTechNameMap[ baseTechs.back()->getName() + util::toString( baseTechs.back()->getYear() ) ] = static_cast<int>( baseTechs.size() ) - 1;
            }
        } 
    }

    if(aPeriod > 0){
        for( unsigned int j = 0; j < baseTechs.size(); j++ ){
            if( baseTechs[ j ]->getYear() <= modeltime->getper_to_yr( aPeriod ) ){
                baseTechs[ j ]->initCalc( aMoreSectorInfo, regionName, sectorName, aNationalAccount, aDemographics, 0, aPeriod );
            }
        }
    }

    setCalibrationStatus( aPeriod );
    interpolateShareWeights( aPeriod ); 
    fixedShare[ aPeriod ] = 0;
    
    // Prevent pathological situation where share is zero where a fixed capacity is present.
    // This can happen at begining of an initialization. Share will be set properly within secotr::calcShare 
    if ( ( getFixedOutput( aPeriod) > 0 ) && ( fixedShare[ aPeriod ] == 0 ) ) {
       fixedShare[ aPeriod ] = 0.1;
    }
   
    // Prevent pathological situation where a calibration value is present but a capacity limit is imposed. This will not work correctly.
    if ( ( getTotalCalOutputs( aPeriod ) > 0 ) && ( capLimit[ aPeriod ] < 1 ) ) {
       capLimit[ aPeriod ] = 1.0;
    }

   // Pass forward any emissions information
    for ( unsigned int i= 0; i< techs.size() && aPeriod > 0 && aPeriod < modeltime->getmaxper() ; i++ ) {
        std::vector<std::string> ghgNames;
        ghgNames = techs[i][aPeriod]->getGHGNames();
        
        int numberOfGHGs =  techs[ i ][ aPeriod ]->getNumbGHGs();

        if ( numberOfGHGs != techs[i][ aPeriod - 1 ]->getNumbGHGs() && aPeriod > 1 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << name << " Number of GHG objects changed in period " << aPeriod;
            mainLog << " to " << numberOfGHGs <<", tech: ";
            mainLog << techs[i][ aPeriod ]->getName();
            mainLog << ", sub-s: "<< name << ", sect: " << sectorName << ", region: " << regionName << endl;
        }
        // If number of GHG's decreased, then copy GHG objects
        if ( numberOfGHGs < techs[i][ aPeriod - 1 ]->getNumbGHGs() ) {
            // Not sure if to impliment this or not
        }
        
        // New method
        if ( aPeriod > 1 ) { // Note the hard coded base period
         for ( int j=0 ; j<numberOfGHGs; j++ ) {
            techs[i][ aPeriod ]->copyGHGParameters( techs[i][ aPeriod - 1]->getGHGPointer( ghgNames[j]  ) );
         } // End For
        }
      
    } // End For
}

/*! \brief check for fixed demands and set values to counter
*
* Routine flows down to technology and sets fixed demands to the appropriate marketplace to be counted
*
* \author Steve Smith
* \param period Model period
*/
void Subsector::tabulateFixedDemands( const int period, const IInfo* aSectorInfo ) {
    for( unsigned int i = 0; i < techs.size(); ++i ){        
        techs[i][ period ]->tabulateFixedDemands( regionName, period, mSubsectorInfo.get() );
   }
}

/*! \brief Computes weighted cost of all technologies in Subsector.
*
* Called from calcShare after technology shares are determined. Calculates share-weighted total price (subsectorprice) and cost of fuel (). 
*
* Price function separated to allow different weighting for Subsector price
* changed to void return maw
*
* \author Sonny Kim, Marshall Wise
* \param regionName region name
* \param period Model period
*/
void Subsector::calcPrice( const int period ) {
    subsectorprice[period] = 0; // initialize to 0 for summing
    fuelprice[period] = 0; // initialize to 0 for summing

    for ( unsigned int i = 0; i < techs.size(); ++i) {
        // calculate weighted average price for Subsector
        subsectorprice[period] += techs[i][period]->getShare()*
            techs[i][period]->getTechcost();
        // calculate weighted average price of fuel only
        // technology shares are based on total cost
        fuelprice[period] += techs[i][period]->getShare()*
            techs[i][period]->getFuelcost();
    }
    
}

/*! \brief returns the sector price.
*
* Returns the weighted price from sectorprice variable. See also price method.
*
* \author Sonny Kim
* \param period Model period
*/
double Subsector::getPrice( const int period ) const {
    return subsectorprice[ period ];
}

/*! \brief Returns calibration status.
*
* Since this information in needed often, this is stored in a variable. 
* Can be set just once, since this never changes during an interation.
* See setCalibrationStatus
*
* \author Steve Smith
* \param period Model period
* \pre must be set with setCalibrationStatus
* \return Boolean that is true if sub-sector is calibrated
*/
bool Subsector::getCalibrationStatus( const int period ) const {
    return calibrationStatus[ period ];
}

/*! \brief Sets the calibrationStatus variable to true if this Subsector, or underlying technologies, are calibrated.
*
* If either the Subsector output, or the output of all the technologies under this Subsector (not including those with zero output) are calibrated, then the calibrationStatus for the sector is set to true.
*
* \author Steve Smith
* \param period Model period
*/
void Subsector::setCalibrationStatus( const int period ) {
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
            calibrationStatus[ period ] = true;
            return;
        }
    }
}

/*! \brief returns Subsector capacity limit.
*
* The capacity limit is in terms of the sector share.
*
* \author Steve Smith
* \param period Model period
* \return Capacity limit for this sub-sector
*/
double Subsector::getCapacityLimit( const int period ) const {
    return capLimit[ period ];
}

/*! \brief sets flag for Subsector capacity limit status.
*
* capLimited is true when the sector has pegged at its capacity limit
*
* \author Steve Smith
* \param value This variable should be renamed and documented.
* \param period Model period
*/
void Subsector::setCapLimitStatus( const bool value, const int period ) {
   capLimited[ period ] = value;
}

/*! \brief returns Subsector capacity limit status.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return Boolean capacity limit status
*/
bool Subsector::getCapLimitStatus( const int period ) const {
    return capLimited[ period ];
}

/*! \brief returns Subsector fuel price.
*
* Status is true when the sector has pegged at its capacity limit for this iteration
*
* \author Steve Smith
* \param period Model period
* \return fuel price
*/
double Subsector::getfuelprice(int period) const
{
    return fuelprice[period];
}

/*! \brief returns Subsector fuel price times share
*
* Returns the share-weighted fuel price, which is later summed to get the sector-weighted fuel price (or cost)
*
* \author Sonny Kim
* \param period Model period
* \return share-weighted fuel price
*/
double Subsector::getwtfuelprice(int period) const
{
    double tempShare;
    // base year share
    if (period == 0) {
        tempShare = share[period]; 
    }
    // lagged one period
    else {
        tempShare = share[period-1];
    }
    return tempShare*fuelprice[period];
}

/*! \brief calculate technology shares within Subsector
*
* Calls technology objects to first calculate cost, then their share. Follos this by normalizing shares. 
*
* \author Marshall Weise, Josh Lurz
* \param regionName region name
* \param period model period
* \warning technologies can not independently have fixed outputs at this point
*/
void Subsector::calcTechShares( const GDP* gdp, const int period ) {
    double sum = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // calculate technology cost
        techs[i][period]->calcCost( regionName, sectorName, period );
        // determine shares based on technology costs
        techs[i][period]->calcShare( regionName, sectorName, gdp, period );
        
        /*! \invariant Technology shares must always be valid. */
        assert( util::isValidNumber( techs[ i ][ period ]->getShare() ) );

        // Sum the technology shares.
        sum += techs[i][period]->getShare();
    }
    // normalize technology shares to total one. Technology shares may also sum
    // to zero if there is no output from the subsector.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[i][period]->normShare(sum);
        /*! \invariant Technology shares must be valid after normalization. */
        assert( util::isValidNumber( techs[ i ][ period ]->getShare() ) );
    }
}   

/*! \brief calculate Subsector unnormalized shares
*
* Calculates the un-normalized share for this sector. 
* Also claculates the sector aggregate price (or cost)
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param period model period
* \param gdp gdp object
* \warning technologies can not independently have fixed outputs
* \warning there is no difference between demand and supply technologies. Control behavior with value of parameter fuelPrefElasticity
*/
void Subsector::calcShare(const int period, const GDP* gdp ) {
    // call function to compute technology shares
    calcTechShares( gdp, period );
    // calculate and return Subsector share; uses above price function
    // calcPrice() uses normalized technology shares calculated above

    // compute Subsector weighted average price of technologies
    calcPrice( period );

    // Calculate the subsector share based on its price.
    if( subsectorprice[ period ] > 0 ){
        double scaledGdpPerCapita = gdp->getBestScaledGDPperCap( period );
        share[ period ] = shrwts[ period ] * pow( subsectorprice[ period ], lexp[ period ] )
                                           * pow( scaledGdpPerCapita, fuelPrefElasticity[ period ] );
    }
    else {
        share[ period ] = 0;
    }

    // Check for invalid shares.
    if( share[ period ] < 0 || !util::isValidNumber( share[ period ] ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid share for " << name << " in " << regionName 
                << " of " << shrwts[ period ] << endl;
    }   
}

/*! \brief normalizes Subsector shares
*
* \author Sonny Kim, Josh Lurz
* \param sum sum of sector shares
* \param period model period
* \warning sum must be correct sum of shares
* \pre calc shares must be called first
*/
void Subsector::normShare( const double sum, const int period ) {
    if( sum > 0 ){ // this could overflow.
        setShare( share[ period ] / sum, period );
    }
    else {
        setShare( 0, period );
    }
    /*! \post Shares must be zero or greater and valid after normalization */
    assert( share[ period ] >= 0 && util::isValidNumber( share[ period ] ) );
}

/*!
* \brief normalizes shares to 100% subject to capacity limit.
*
* Used by sector::calcShare() to re-normalize shares, adjusting for capacity limits.
*
* Note that a multiplier is passed, not a divisor. The appropriate multiplier must be calculated by the calling routine.
*
* Sub-sectors that are not subject to a capacity limit get multiplied by mult.
* Capacity limited subsectors are set to their capacity limit.
*
* \author Steve Smith
* \warning The routine assumes that shares are already normalized.
* \param multiplier Multiplier by which to scale shares of all non-capacity limited sub-sectors
* \param period Model period
*/
void Subsector::limitShares( const double multiplier, const int period ) {
   if ( multiplier == 0 ) {
      share[period] = 0;
   }
   else {   
      double capLimitValue = capLimitTransform( capLimit[period], share[period] );
      if ( share[period] >= capLimitValue ) {
         // Only adjust if not already capacity limited
         // need this because can't transform more than once, see capLimitTransform
         if ( !capLimited[ period ] ) {
            setShare( capLimitValue, period );
            setCapLimitStatus( true, period ); // set status to true
         }
      } 
      else {
         if ( fixedShare[ period ] == 0 ) { // don't change if fixed
            setShare( share[period] * multiplier, period );
         }
      }
   }
}

/*! \brief Transform share to smoothly implement capacity limit.
*
* Function transforms the original share value into one that smoothly approaches the capacity limit.
* Returns the original orgShare when share << capLimit and returns capLimit when orgShare is large by using a logistic transformation.
* 
*
* \author Steve Smith
* \param capLimit capacity limit (share)
* \param orgShare original share for sector
* \return transformed share value
*/
 double Subsector::capLimitTransform( double capLimit, double orgShare ) {
   const double SMALL_NUM = util::getSmallNumber();
   const double exponentValue =  2;
   const double mult =  1.4;
   double newShare = capLimit ;

   if ( capLimit < ( 1 - SMALL_NUM ) ) {
      double factor = exp( pow( mult * orgShare/capLimit , exponentValue ) );
      newShare = orgShare * factor/( 1 + ( orgShare/capLimit ) * factor);
   }
   return newShare;
}

/*! \brief Return the total exogenously fixed technology output for this sector.
*
* \author Steve Smith
* \param period model period
* \pre calc shares must be called first
*/
double Subsector::getFixedOutput( const int period ) const {
    double fixedOutput = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        fixedOutput += techs[i][period]->getFixedOutput();
    }
    return fixedOutput;
}

/*! \brief Return the share from this sub-sector that is fixed supply
* \details Enables communication of fixed share to other classes. 
* This is necessary since, while the amount of fixed supply is available (via
* getFixedOutput), the total output of a sector is not always known. So this
* function enables the amount of fixed supply in terms of the sector share to be
* communicated.
* \author Steve Smith
* \param period Model period
*/
double Subsector::getFixedShare( const int period ) const {
    return fixedShare[ period ];
}

/*! \brief Save the share from this sub-sector that is fixed supply
* \details Enables communication of fixed share to other classes.
* \author Steve Smith
* \sa getFixedShare
* \param period Model period
* \param share sector share that is fixed supply
*/
void Subsector::setFixedShare( const int period, const double share ) {
    // option to turn this off during calibration
    // This does not work correctly, shares will not sum to one. -JPL
    // if ( world->getCalibrationSetting() ) {
        fixedShare[ period ] = share;
        if ( share > 1 ) {
            cerr << "Share set to value > 1. Value = " << share << endl;
        }
    // }
}

/*! \brief Set the share from this sub-sector to that saved for fixed supply
* \details This function changes the share to the share previously saved for the
*          fixed supply. This is done instead of using a function to directly
*          set the share in general. Doing this allows the price and calibration
*          routines to operate with an appropriate share.
* \author Steve Smith
* \param period Model period
*/
void Subsector::setShareToFixedValue( const int period ) {
   setShare( fixedShare[ period ], period );
}

/*! \brief Reset fixed supply for each technology
* Reset fixed supply to read-in value. This is needed in case the fixed supply had been downscaled to match demand.
* This is done instead of using a function to directly set the share in general. 
* Doing this allows the price and calibration routines to operate with an appropriate share.
*
*\author Steve Smith
*\param period Model period
*/
void Subsector::resetFixedOutput( const int period ) {
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[ i ][period]->resetFixedOutput(period); // eliminate any previous down-scaleing
    }
}

/*! \brief Scale down fixed supply
* This is use dif the total fixed production is greater than the actual demand. See scalefixedOutput.
*
* \author Steve Smith
* \param period Model period
* \param scaleRatio multiplicative scale factor by which to scale fixed supply
*/
void Subsector::scaleFixedOutput( const double scaleRatio, const int period ) {
    // scale fixed technology output down
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[ i ][ period ]->scaleFixedOutput( scaleRatio );
    }
    setFixedShare( period, fixedShare[ period ] * scaleRatio ); 
}

/*! \brief Consistently adjust share weights for previous period after calibration 
* If the sector share weight in the previous period was changed due to calibration, 
* then adjust next few shares so that there is not a big jump in share weights.
*
* Can turn this feature off by setting the scaleYear before the calibration year (e.g., 1975, or even zero)
*
* If scaleYear is set to be the calibration year then shareweights are kept constant
*
* \author Steve Smith
* \param period Model period
* \warning Share weights must be scaled (from sector) before this is called.
*/
void Subsector::interpolateShareWeights( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // if previous period was calibrated, then adjust future share weights
    // Only scale shareweights if after 1990 (don't like hard coded year, but need general solution to base year issue)
    if ( ( period > modeltime->getyr_to_per( 1990 ) ) && calibrationStatus[ period - 1 ] && Configuration::getInstance()->getBool( "CalibrationActive" ) ) {

        int endPeriod = 0;
        if ( scaleYear >= modeltime->getStartYear() ) {
            endPeriod = modeltime->getyr_to_per( scaleYear );
        }
        if  ( endPeriod >= ( period - 1) ) {
            // If begining share weight is zero, then it wasn't changed by calibration so do not scale
             if ( shrwts[ period - 1 ] > 0 ) {
                shareWeightLinearInterpFn( period - 1, endPeriod );
            }
        }
        
        adjustTechnologyShareWeights( period );
    }
}

/*! \brief Wrapper method for calls to normalize and/or interpolate technology shareweights  
*
* \author Steve Smith
* \param period Model period
*/
void Subsector::adjustTechnologyShareWeights( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();

    if ( techs.size() > 1 ) {
        // First renormalize share weights
        normalizeTechShareWeights( period - 1 );
    }

    // Linearlly interpolate technology shareweights
    techShareWeightLinearInterpFn( period - 1, modeltime->getyr_to_per( techScaleYear ) );
}

/*! \brief Linearly interpolate share weights between specified endpoints 
* Utility function to linearly scale share weights between two specified points.
*
* \author Steve Smith
* \param beginPeriod Period in which to begin the interpolation.
* \param endPeriod Period in which to end the interpolation.
*/
void Subsector::shareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    double shareIncrement = 0;

    int loopPeriod = endPeriod;
    if ( endPeriod > beginPeriod ) {
        shareIncrement = ( shrwts[ endPeriod ] - shrwts[ beginPeriod ] ) / ( endPeriod - beginPeriod );
    } 
    else if ( endPeriod == beginPeriod ) {
        // If end period equals the begining period then this is a flag to keep the weights the same, so make increment zero
        // and loop over rest of periods
        loopPeriod = modeltime->getmaxper();  
        shareIncrement = 0;
    }

    for ( int period = beginPeriod + 1; period < loopPeriod; period++ ) {
        shrwts[ period ] = shrwts[ period - 1 ] + shareIncrement;
    }

    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
    calibrationLog.setLevel( ILogger::DEBUG );
    calibrationLog << "Shareweights interpolated with increment " << shareIncrement 
                   << " for subsector " << name << " in sector " << sectorName << " in region " << regionName << endl;
}

/*! \brief Linearly interpolate technology share weights between specified endpoints 
* Utility function to linearly scale technology share weights between two specified points.
*
* \author Steve Smith
* \param beginPeriod Period in which to begin the interpolation.
* \param endPeriod Period in which to end the interpolation.
* \bug shareIncrement is never initialized if endPeriod < beginPeriod
*/
void Subsector::techShareWeightLinearInterpFn( const int beginPeriod,  const int endPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();
    double shareIncrement = 0;

    for( unsigned int i = 0; i < techs.size(); ++i ){
        double beginingShareWeight = techs[ i ][ beginPeriod ]->getShareWeight();

        // If begining share weight is zero, then it wasn't changed by calibration so do not scale
        if ( beginingShareWeight > 0 ) {
            if ( endPeriod > beginPeriod ) {
                shareIncrement = ( techs[ i ][ endPeriod ]->getShareWeight() - beginingShareWeight );
                shareIncrement /= endPeriod - beginPeriod;
            }

            int loopPeriod = endPeriod;
            // If end period equals the begining period then this is a flag to keep the weights the same, so loop over rest of periods
            if ( endPeriod == beginPeriod ) {
                loopPeriod = modeltime->getmaxper();  
                shareIncrement = 0;
            }

            for ( int period = beginPeriod + 1; period < loopPeriod; period++ ) {
                techs[ i ][ period ]->setShareWeight( techs[ i ][ period - 1 ]->getShareWeight() + shareIncrement );
            }
            ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
            calibrationLog.setLevel( ILogger::DEBUG );
            calibrationLog << "Shareweights interpolated for technologies in subsector " << name << " in sector " << sectorName << " in region " << regionName << endl;
        }
    }
}

/*! \brief Scales technology share weights so that they equal number of subsectors.
*
* This is needed so that 1) share weights can be easily interpreted (> 1 means favored) and so that
* future share weights can be consistently applied relative to calibrated years. This is particularly useful
* when new technologies are added. They can always be added with share weights relative to one no matter how
* many other technologies are present.
*
* \author Steve Smith
* \param period Model period
* \warning The routine assumes that all tech outputs are calibrated.
*/
void Subsector::normalizeTechShareWeights( const int period ) {
    
    double shareWeightTotal = 0;
    int numberNonzeroTechs = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        double techShareWeight = techs[ i ][ period ]->getShareWeight();
        shareWeightTotal += techShareWeight;
        if ( techShareWeight > 0 ) {
            numberNonzeroTechs += 1;
        }
    }

    if ( shareWeightTotal < util::getTinyNumber() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "ERROR: in subsector " << name << " (" << regionName << ") Shareweights sum to zero." << endl;
    } 
    else {
        for( unsigned int i = 0; i < techs.size(); ++i ){
             techs[ i ][ period ]->scaleShareWeight( numberNonzeroTechs / shareWeightTotal );
        }
        ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
        calibrationLog.setLevel( ILogger::DEBUG );
        calibrationLog << "Shareweights normalized for technologies in subsector " << name << " in sector " << sectorName << " in region " << regionName << endl;
    }
}

//! Adjusts shares to be consistant with any fixed production 
/*! This routine does two things. 

If this sub-sector has a fixed supply, it sets the share to be consistant with the fixed supply
If this sub-sector does not have a fixed supply, it adjusts the share to be consistant with all the fixed supplies of all other sub-sectors (totalfixedOutput)

\param dmd total demand for all sectors
\param shareRatio amount variable shares need to be adjusted to be consistant with fixed supply
\param totalfixedOutput total fixed supply from all sub-sectors
\param period model period
*/
void Subsector::adjShares( const double demand, double shareRatio, 
                          const double totalfixedOutput, const int period ) {
    double sumSubsectfixedOutput = 0; // total Subsector fixed supply
    double fixedOutput = 0; // fixed supply for each technology
    double varShareTot = 0; // sum of shares without fixed supply
    double subsecdmd; // Subsector demand adjusted with new shares

    // add up the fixed supply and share of non-fixed supply
    for( unsigned int i = 0; i < techs.size(); ++i ){
        fixedOutput = techs[i][period]->getFixedOutput();
        sumSubsectfixedOutput += fixedOutput;
        if (fixedOutput == 0) { 
           varShareTot += techs[i][period]->getShare();
        }
    }
    
    // Adjust the share for this Subsector
    // This makes the implicit assumption that the Subsector is either all
    // fixed production or all variable. Would need to amend the logic below
    // to take care of other cases.
    
    // totalfixedOutput is the sector total
    if(totalfixedOutput > 0) {
        if (sumSubsectfixedOutput > 0) {    // This Subsector has a fixed supply
            if ( demand > 0 ) {
                setShare( sumSubsectfixedOutput/demand, period ); 
            }
            else { // no fixed share if no demand
                share[period] = 0; 
            }
        }
        else {  // This Subsector does not have fixed supply 
            if ( demand > 0 ) {
                setShare( share[period] * shareRatio, period ); 
            }
            else {
                share[period] = 0; 
            }  
        } 
    }
    
    // then adjust technology shares to be consistent
    subsecdmd = share[period]*demand; // share is Subsector level
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // adjust tech shares 
        techs[ i ][period]->adjShares(subsecdmd, sumSubsectfixedOutput, varShareTot, period);
    }
    
}

/*! \brief The demand passed to this function is shared out at the technology
*          level.
* Demand from the "dmd" parameter (could be energy or energy service) is passed
* to technologies.
*  This is then shared out at the technology level. See also sector::setOutput.
*
* \author Sonny Kim, Josh Lurz
* \param aDemand Total demand for this product.
* \param aGDP Regional GDP container.
* \param aPeriod Model period
*/
void Subsector::setOutput( const double aDemand,
                           const GDP* aGDP,
                           const int aPeriod )
{
    assert( util::isValidNumber( aDemand ) && aDemand >= 0 );

    // note that output is in service unit when called from demand sectors
    // multiply dmd by Subsector share go get the total demand to be supplied by this Subsector
    double subsecdmd = share[ aPeriod ]* aDemand; 
    
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        // calculate technology output and fuel input from Subsector output
        techs[ i ][ aPeriod ]->production( regionName, sectorName, subsecdmd,
                                           aGDP, aPeriod );
    }
}

/*! \brief Adjusts share weights and Subsector demand to be consistent with calibration value.
* Calibration is performed by scaling share weights to be consistent with the calibration value. 
* Calibration is, therefore, performed as part of the iteration process. 
* Since this can change derivatives, best to turn calibration off when using N-R solver.
*
* This routine adjusts subsector shareweights so that relative shares are correct for each subsector.
* Note that all calibration values are scaled (up or down) according to total sectorDemand 
* -- getting the overall scale correct is the job of the TFE calibration
*
* Routine takes into account fixed supply, which is assumed to take precedence over calibration values
* Note that this routine doesn't notice if the calibration is at the technology or sub-sector level, 
* this is taken care of by routine getTotalCalOutputs.
*
* Routine also calls adjustment to scale technology share weights if necessary.
*
* \author Steve Smith
* \param sectorDemand total demand for this sector
* \param totalfixedOutput total amount of fixed supply for this sector
* \param totalCalOutputs total amount of calibrated outputs for this sector
* \param allFixedOutput flag if all outputs from this sector are calibrated
* \param period Model period
* \warning If calvalue is larger than sector demand nothing is done
* \warning The value of subsecdmd is changed (for sub-sector output calibration)
*/
void Subsector::adjustForCalibration( double sectorDemand, double totalfixedOutput, double totalCalOutputs, const bool allFixedOutput, const int period ) {
   double shareScaleValue = 0;
   double availableDemand;
   double subSectorDemand;

   // total calibrated outputs for this sub-sector
   double calOutputSubsect = getTotalCalOutputs( period );

    // make sure share weights aren't zero or else cann't calibrate
    if ( shrwts[ period ]  == 0 && ( calOutputSubsect > 0 ) ) {
        shrwts[ period ]  = 1;
    }
   
   // Determine available demand that can be shared out (subtract sub-sectors with fixed supply)
   availableDemand = sectorDemand - totalfixedOutput;
   if ( availableDemand < 0 ) {
      availableDemand = 0;
   }
   
   // Next block adjusts calibration values if total cal + fixed demands for this sector 
   // are different from total sector demand passed in.   
   // Do this in all cases, unless calvalues < available demand and all outputs are NOT fixed
   // (if all outputs are not fixed, then the sectors that are not fixed can take up the remaining demand)
   if ( !( ( totalCalOutputs < availableDemand ) && !allFixedOutput ) ) {
        calOutputSubsect = calOutputSubsect * ( availableDemand  / totalCalOutputs );
   }
   
   // Adjust share weights
   subSectorDemand = share[ period ] * sectorDemand;
   if ( subSectorDemand > 0 ) {
      shareScaleValue = calOutputSubsect / subSectorDemand;
      shrwts[ period ]  = shrwts[ period ] * shareScaleValue;
   }

   // Check to make sure share weights are not less than zero (and reset if they are)
   if( shrwts[ period ] < 0 ) {
       ILogger& mainLog = ILogger::getLogger( "main_log" );
       mainLog.setLevel( ILogger::WARNING );
       mainLog << "Share Weight is less than zero in Subsector " << name << ". Resetting to one." << endl;
       shrwts[ period ] = 1;
   }

    // Check for unreasonable shareweights.
    if ( shrwts[period]  > 1e4 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Huge shareweight for sector " << sectorName << ", sub-sector " << name  
                << " in region " << regionName << " of " << shrwts[period] << endl;
    }
    
   int numberTechs = getNumberAvailTechs( period );
   // Now calibrate technology shares if necessary
   if ( numberTechs > 1 ) {
       for( unsigned int i = 0; i < techs.size(); ++i ){
           // adjust tech shares 
           if ( techs[ i ][period]->techAvailable( ) ) {
               techs[ i ][period]->adjustForCalibration( calOutputSubsect, regionName,
                   mSubsectorInfo.get(), period );
           }
       }
   }
}
  
/*! \brief returns the total of technologies available this period
*
* Technologies are available if they exist and shareweights are not zero
*
* \author Steve Smith
* \param period Model period
* \return Number of available technologies
*/
int Subsector::getNumberAvailTechs( const int period ) const {
    int numberAvailable = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
      // calculate technology output and fuel input from Subsector output
      if ( techs[i][period]->techAvailable( ) ) {
         numberAvailable++;
      }
   }
   return numberAvailable;
}

/*! \brief returns the total calibrated output from this sector.
*
* Routine adds up calibrated values from both the sub-sector and (if not calibrated at Subsector), technology levels.
* This returns only calibrated outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
double Subsector::getTotalCalOutputs( const int period ) const {
    double sumCalValues = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
            if ( techs[ i ][ period ]->getCalibrationOutput( period ) < 0 ) {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::DEBUG );
                mainLog << "calibration < 0 for tech " << techs[ i ][ period ]->getName() 
                    << " in Subsector " << name << endl;
            }
            sumCalValues += techs[ i ][ period ]->getCalibrationOutput( period );
        }
    }
    return sumCalValues;
}


/*! \brief returns the total calibrated or fixed input from this sector for the specified good.
*
* Routine adds up calibrated or fixed input values from all technologies.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \param bothVals optional parameter. It true (default) both calibration and fixed values are returned, if false only calInputs
* \return Total calibrated input for this Subsector
*/
double Subsector::getCalAndFixedInputs( const int period, const std::string& goodName, const bool bothVals ) const {
    double sumCalInputValues = 0;

    for ( unsigned int i= 0; i< techs.size(); i++ ) {
        if ( techHasInput( techs[ i ][ period ], goodName ) || ( goodName == "allInputs" ) ) {
            if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
                sumCalInputValues += techs[ i ][ period ]->getCalibrationInput( period );
            } 
            else if ( techs[ i ][ period ]->outputFixed( ) && bothVals ) {
                sumCalInputValues += techs[ i ][ period ]->getFixedInput( period );
            }
        }
    }
    return sumCalInputValues;
}

/*! \brief returns the total calibrated or fixed input from this sector for the specified good.
*
* Routine adds up calibrated or fixed input values from all technologies.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \param bothVals optional parameter. It true (default) both calibration and fixed values are returned, if false only calInputs
* \return Total calibrated input for this Subsector
*/
double Subsector::getCalAndFixedOutputs( const int period, const std::string& goodName, const bool bothVals ) const {
    double sumCalOutputValues = 0;

    for ( unsigned int i= 0; i< techs.size(); i++ ) {
        if ( techHasInput( techs[ i ][ period ], goodName ) || ( goodName == "allInputs" ) ) {
            if ( techs[ i ][ period ]->getCalibrationStatus( ) ) {
                sumCalOutputValues += techs[ i ][ period ]->getCalibrationOutput( period );
            } 
            else if ( techs[ i ][ period ]->outputFixed( ) && bothVals ) {
                sumCalOutputValues += techs[ i ][ period ]->getFixedOutput( );
            }
        }
    }
    return sumCalOutputValues;
}

/*! \brief Adds the input value needed to produce the required output to the marketplace calDemand value
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to determine the inputs for.
* \param requiredOutput Amount of output to produce
* \return Whether any input was changed.
*/
bool Subsector::setImpliedFixedInput( const int period, const std::string& goodName, const double requiredOutput ) {


    IInfo* marketInfo = scenario->getMarketplace()->getMarketInfo( goodName, regionName, period, false );

    // Certain inputs may not exist.
    if( !marketInfo ){
        return false;
    }
    
    bool inputWasChanged = false;
    for ( unsigned int i= 0; i< techs.size(); i++ ) {
        if ( techHasInput( techs[ i ][ period ], goodName ) ) {
            double inputValue = techs[ i ][ period ]->getInputRequiredForOutput( requiredOutput, period );
            if ( !inputWasChanged ) {
                inputWasChanged = true;
                double existingMarketDemand = max( marketInfo->getDouble( "calDemand", true ), 0.0 );
                marketInfo->setDouble( "calDemand", existingMarketDemand + inputValue );
            } 
            else {
                ILogger& mainLog = ILogger::getLogger( "main_log" );
                mainLog.setLevel( ILogger::WARNING );
                mainLog << "More than one technology input would have been changed" 
                    << " in sub-sector " << name << " in sector " << sectorName
                    << " in region " << regionName << endl; 
            }
        }
    }
    return inputWasChanged;
}

/*! \brief returns true if inputs are all fixed for this subsector and input good
*
* Note that if the good is not used, then true is returned.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value "allInputs" then returns all inputs.
* \return boolean true if inputs of specified good are fixed
*/
bool Subsector::inputsAllFixed( const int period, const std::string& goodName ) const {

    // test for each method of fixing output, if none of these are true then demand is not all fixed
    for ( unsigned int i= 0; i< techs.size(); i++ ) {
        if ( techHasInput( techs[ i ][ period ], goodName ) || ( goodName == "allInputs" ) ) {
            if ( ( techs[ i ][ period ]->getCalibrationStatus( ) ) ) {
                continue;
            } 
            else if ( techs[ i ][ period ]->outputFixed( ) != 0 || shrwts[ period ] == 0  ) {
                continue;
            } 
            else {
                return false;
            }
        }
    }

    return true;
}

/*! \brief checks to see if technology demands the specified good
*
* \author Steve Smith
* \warning This routine depends on technologies being named for their fuel type or if fuelname is equal to the good. 
* This works currently for electricity, but will not for other techs. Need to impliment a more robust method of checking calibrations.
* \param goodName market good to check for
* \param pointer to technology to consider
* \return True if the specified technology has goodname as input
* \todo Need a more robust way of doing this check (requires a more fundamental change to the way calibrated inputs and outputs are found)
*/
bool Subsector::techHasInput( const ITechnology* thisTech, const std::string& goodName ) const {
    return ( thisTech->getFuelName() == goodName );
}

/*! \brief Scales calibrated values for the specified good.
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
* \param scaleValue multipliciative scaler for calibrated values 
* \return Total calibrated input for this Subsector
*/
void Subsector::scaleCalibratedValues( const int period, const std::string& goodName, const double scaleValue ) {
    for ( unsigned int i= 0; i< techs.size(); i++ ) {
        if ( techHasInput( techs[ i ][ period ], goodName ) ) {
            techs[ i ][ period ]->scaleCalibrationInput( scaleValue );
        }
   }
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this Subsector or all technologies in this sub-sector returns true.
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
bool Subsector::allOutputFixed( const int period ) const {
    // If there is no shareweight for this subsector than it cannot produce any
    // output, and so the output must be fixed.
    if( util::isEqual( shrwts[ period ], 0.0 ) ){
        return true;
    }

    // if not fixed at sub-sector level, then check at the technology level
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if ( !( techs[ i ][ period ]->outputFixed() ) ) {
            return false;
        }
    }
    return true;
}

/*! \brief scale calibration values.
*
* Scale calibration values in each technology by specified amount. 
*
* \author Steve Smith
* \param period Model period
* \param scaleFactor Multiplicitive scale factor for each calibration value
*/
void Subsector::scaleCalibrationInput( const int period, const double scaleFactor ) {
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[ i ][ period ]->scaleCalibrationInput( scaleFactor );
    }
}

/*! \brief returns share weight for this Subsector
*
* Needed so that share weights can be scaled by sector
*
* \author Steve Smith
* \param period Model period
* \return share weight
*/
double Subsector::getShareWeight( const int period ) const {
    return shrwts[ period ];
}

/*! \brief Scales share weight for this Subsector
*
* \author Steve Smith
* \param period Model period
* \param scaleValue Multipliciatve scale factor for shareweight
*/
void Subsector::scaleShareWeight( const double scaleValue, const int period ) {
    if ( scaleValue != 0 ) {
        shrwts[ period ] *= scaleValue;
    }
}

/*! \brief returns share for this Subsector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \pre calcShare
* \return share value
*/
double Subsector::getShare( const int period ) const {
    /*! \post Shares should be zero or greater and valid. */
    assert( share[ period ] >= 0 && util::isValidNumber( share[ period ] ) );
    return share[period];
}

/*! \brief set share for this Subsector with normalization check
*
* Use this function to set the share at any time where shares are supposed to be normalized
*
* \author Steve Smith
* \param shareVal Value to which to set the share.
* \param period Model period
*/
void Subsector::setShare( const double shareVal, const int period ) {
    /*! \pre The share value should be valid. */
    assert( util::isValidNumber( shareVal ) );
    
    /*! \pre The new share value should be less than or equal to one. */
    if ( shareVal > ( 1 + util::getVerySmallNumber() ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Setting share to value greater than one. New share value " << shareVal << endl;
    }
    share[ period ] = shareVal;
}

//! write Subsector output to database
void Subsector::csvOutputFile() const {
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    fileoutput3( regionName,sectorName,name," ","production","EJ",temp);
    // Subsector price
    fileoutput3( regionName,sectorName,name," ","price","$/GJ(ser)",subsectorprice);
    // Subsector carbon taxes paid
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getTotalCarbonTaxPaid( m );
    }
    fileoutput3( regionName, sectorName, name, " ", "C tax paid", "Mil90$", temp );

    for ( int m= 0;m<maxper;m++){
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    fileoutput3( regionName,sectorName,name," ","CO2 emiss","MTC",temp);

    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // sjs -- bad coding here, hard-wired period. But is difficult to do something different with current output structure. This is just for csv file.
        int numberOfGHGs =  techs[ i ][ 2 ]->getNumbGHGs();
        vector<string> ghgNames;
        ghgNames = techs[i][ 2 ]->getGHGNames();        
        for ( int ghgN = 0; ghgN <= ( numberOfGHGs - 1 ); ghgN++ ) {
            if ( ghgNames[ ghgN ] != "CO2" ) {
                for ( int m= 0;m<maxper;m++) {
                    temp[m] = techs[i][ m ]->get_emissmap_second( ghgNames[ ghgN ] );
                }
                fileoutput3( regionName,sectorName,name,techs[i][ 2 ]->getName(), ghgNames[ ghgN ] + " emiss","Tg",temp);
            }
        }

        // output or demand for each technology
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput( m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"production","EJ",temp);
        // technology share
        if( techs.size() > 1 ) {
            for ( int m= 0;m<maxper;m++) {
                temp[m] = techs[i][m]->getShare();
            }
            fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"tech share","%",temp);
        }
        // technology cost
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost();
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"price","$/GJ",temp);
        
        // ghg tax paid
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbonTaxPaid( regionName, m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"C tax paid","90Mil$",temp);
        // technology fuel input
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getInput();
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"fuel consump","EJ",temp);
        // technology efficiency
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEfficiency( m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"efficiency","%",temp);
        // technology non-energy cost
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNonEnergyCost( m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"non-energy cost","$/GJ",temp);
        // technology CO2 emission
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2");
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"CO2 emiss","MTC",temp);
        // technology indirect CO2 emission
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->get_emissmap_second("CO2ind");
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"CO2 emiss(ind)","MTC",temp);
    }
    
    csvDerivedClassOutput();
}

//! Outputs any variables specific to derived classes
void Subsector::csvDerivedClassOutput() const {
    // do nothing
}

/*! \brief Write supply sector MiniCAM style Subsector output to database.
*
* Writes outputs with titles and units appropriate to supply sectors.
*
* \author Sonny Kim
*/
void Subsector::MCoutputSupplySector() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double CVRT_90 = 2.212; //  convert '75 price to '90 price
    vector<double> temp(maxper);
    
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    dboutput4(regionName,"Secondary Energy Prod",sectorName,name,"EJ", temp );
    // Subsector price
    dboutput4(regionName,"Price",sectorName,name,"75$/GJ",subsectorprice);
    // for electricity sector only
    if (sectorName == "electricity") {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = subsectorprice[m] * CVRT_90 * 0.36;
        }
        dboutput4(regionName,"Price",sectorName+" C/kWh",name,"90C/kWh",temp);
    }
    
    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // technology non-energy cost
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getNonEnergyCost( m );
        }
        dboutput4( regionName, "Price NE Cost", sectorName, techs[i][ 0 ]->getName(), "75$/GJ", temp );
        // secondary energy and price output by tech
        // output or demand for each technology
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput( m );
        }
        dboutput4( regionName, "Secondary Energy Prod", sectorName + "_tech", techs[i][ 0 ]->getName(), "EJ", temp );
        // technology cost
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTechcost() * CVRT_90;
        }
        dboutput4( regionName, "Price", sectorName + "_tech", techs[i][ 0 ]->getName(), "90$/GJ", temp );
    }
}

/*! \brief Write demand sector MiniCAM style Subsector output to database.
*
* Writes outputs with titles and units appropriate to demand sectors.
* Part B is for demand sector, titles and units are different from Part A
*
* \author Sonny Kim
*/
void Subsector::MCoutputDemandSector() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    dboutput4(regionName,"End-Use Service",sectorName+" by Subsec",name,"Ser Unit",temp);
    dboutput4(regionName,"End-Use Service",sectorName+" "+name,"zTotal","Ser Unit",temp);
    // Subsector price
    dboutput4(regionName,"Price",sectorName,name+" Tot Cost","75$/Ser",subsectorprice);
    
    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if( techs.size() > 1 ) {  // write out if more than one technology
            // output or demand for each technology
            for ( int m= 0;m<maxper;m++) {
                temp[m] = techs[i][m]->getOutput( m );
            }
            dboutput4(regionName,"End-Use Service",sectorName+" "+name,techs[i][ 0 ]->getName(),"Ser Unit",temp);
            // total technology cost
            for ( int m= 0;m<maxper;m++) {
                temp[m] = techs[i][m]->getTechcost();
            }
            dboutput4(regionName,"Price",sectorName+" "+name,techs[i][ 0 ]->getName(),"75$/Ser",temp);
           // technology fuel cost
            for ( int m= 0;m<maxper;m++) {
                temp[m] = techs[i][m]->getFuelcost();
            }
            dboutput4( regionName,"Price",sectorName+" "+name+" Fuel Cost",techs[i][ 0 ]->getName(),"75$/Ser",temp);
            // technology non-energy cost
            for ( int m= 0;m<maxper;m++) {
                temp[m] = techs[i][m]->getNonEnergyCost( m );
            }
            dboutput4( regionName, "Price", sectorName + " " + name + " NE Cost", techs[i][ 0 ]->getName(), "75$/Ser", temp );
        }
    }
}

/*! \brief Write common MiniCAM style Subsector output to database.
*
* Writes outputs that are common to both supply and demand sectors.
*
* \author Sonny Kim
*/
void Subsector::MCoutputAllSectors() const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // Subsector carbon taxes paid
    // Subsector carbon taxes paid
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getTotalCarbonTaxPaid( m );
    }
    dboutput4( regionName, "General", "CarbonTaxPaid by subsec", sectorName + name, "$", temp );
    // Subsector share 
    dboutput4(regionName,"Subsec Share",sectorName,name,"100%",share);
    // Subsector emissions for all greenhouse gases
    // fuel consumption by Subsector
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getInput( m );
    }
    dboutput4( regionName, "Fuel Consumption", sectorName + " by Subsec", name, "EJ", temp );
    
    // subsector CO2 emission. How is this different then below?
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        for ( int m = 0; m < maxper; m++ ) {
            // this gives Subsector total CO2 emissions
            // get CO2 emissions for each technology
            temp[m] += techs[i][m]->get_emissmap_second("CO2");
        }
    }
    dboutput4( regionName, "CO2 Emiss", sectorName, name, "MTC", temp );

    typedef map<string,double>::const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gas names for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for ( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4( regionName, "Emissions",  "Subsec-" + sectorName + "_" + name, gmap->first, "Tg", temp );
    }

    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        const string subsecTechName = name + techs[i][ 0 ]->getName();
        // technology indirect CO2 emission
        for( int m= 0;m<maxper;m++) {
            temp[m] = summary[m].get_emindmap_second("CO2");
        }
        dboutput4(regionName,"CO2 Emiss(ind)",sectorName, subsecTechName,"MTC",temp);
        // technology share
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getShare();
        }
        dboutput4(regionName,"Tech Share",sectorName, subsecTechName,"%",temp);

        // ghg tax and storage cost applied to technology if any
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getTotalGHGCost( regionName, m );
        }
        dboutput4(regionName,"Total GHG Cost",sectorName, subsecTechName,"$/gj",temp);

        // ghg tax paid
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCarbonTaxPaid( regionName, m );
        }
        dboutput4(regionName,"C Tax Paid",sectorName, subsecTechName,"90Mil$",temp);

        // technology fuel input
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getInput();
        }
        if( techs[i][0]->getFuelName() == "" ) {
            dboutput4( regionName,"Fuel Consumption", sectorName + " by Technology " + subsecTechName, "No Fuelname", "EJ", temp );
        } 
        else {
            dboutput4( regionName,"Fuel Consumption", sectorName + " by Technology " + subsecTechName, techs[i][0]->getFuelName(), "EJ", temp );
        }

        // for 1 or more technologies
        // technology efficiency
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEfficiency( m );
        }
        dboutput4(regionName,"Tech Efficiency",sectorName, subsecTechName,"%",temp);
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getIntensity(m);
        }
        dboutput4(regionName,"Tech Intensity", sectorName, subsecTechName,"In/Out",temp);
    }

    MCDerivedClassOutput( );

}

//! Outputs any variables specific to derived classes
void Subsector::MCDerivedClassOutput( ) const {
    // do nothing
}

//! calculate GHG emissions from annual production of each technology
void Subsector::emission( const int period ){
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    summary[period].clearemiss(); // clear emissions map
    summary[period].clearemfuelmap(); // clear emissions map
    
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[i][period]->calcEmission( sectorName, period );
        summary[period].updateemiss( techs[i][period]->getemissmap() );
        summary[period].updateemfuelmap( techs[i][period]->getemfuelmap() );
    }
}

//! calculate indirect GHG emissions from annual production of each technology
void Subsector::indemission( const int period, const vector<Emcoef_ind>& emcoef_ind ) {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    summary[period].clearemindmap(); // clear emissions map
    for( unsigned int i = 0; i < techs.size(); ++i ){
        techs[i][period]->indemission( emcoef_ind );
        summary[period].updateemindmap(techs[i][period]->getemindmap());
    }
}


/*! \brief returns (energy) input to sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector input
*/
double Subsector::getInput( const int period ) const {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    double inputSum = 0;
    for ( unsigned int i= 0; i < techs.size(); i++ ) {
        inputSum += techs[i][period]->getInput();
    }
    return inputSum;
}

/*! \brief returns Subsector output
*
* output summed every time to ensure consistency
* this is never called for demand sectors!
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return sector output
*/
double Subsector::getOutput( const int period ) const {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    double outputSum = 0;
    for ( unsigned int i= 0;i< techs.size(); i++ ) {
        outputSum += techs[i][period]->getOutput( period );
    }

    // Add on the base techs output too.
    for( CBaseTechIterator currTech = baseTechs.begin(); currTech != baseTechs.end(); ++currTech ){
        outputSum += (*currTech)->getOutput( period );
    }
    /*! \post Total subsector output is positive. */
    assert( outputSum >= 0 );
    return outputSum;
}

/*! \brief Return the total annual investment in all technologies for a given period.
* \param aPeriod Period in which to determine total investmetn.
* \return Total investment for a given period.
* \author Josh Lurz
*/
double Subsector::getAnnualInvestment( const int aPeriod ) const {
    double totalInvestment = 0;
    for( CBaseTechIterator tech = baseTechs.begin(); tech != baseTechs.end(); ++tech ){
        totalInvestment += (*tech)->getAnnualInvestment( aPeriod );
    }
    return totalInvestment;
}

/*! \brief Distribute new investment determined by the SectorInvestment object.
* \param aDistributor An object which contains the algorithm for distributing
*        investment.
* \param aNationalAccount National account object needed to calculate share.
* \param aExpProfitRateCalc An object which contains the algorithm for
*        calculating expected profits.
* \param aRegionName The name of the region containing this subsector.
* \param aSectorName The name of the sector containing this subsector.
* \param aNewInvestment The new subsector investment to be distributed.
* \param aPeriod The period in which to add investment. 
* \return The actual amount of annual investment distributed.
*/
double Subsector::distributeInvestment( const IDistributor* aDistributor,
                                        NationalAccount& aNationalAccount,
                                        const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                        const string& aRegionName,
                                        const string& aSectorName,
                                        const double aNewInvestment,
                                        const int aPeriod )
{
    // If investment is fixed used that instead of the passed in investment.
    double actInvestment = aNewInvestment;
    if( mFixedInvestments[ aPeriod ] != -1 ){
        // Check that zero investment was passed in for this case.
        if( aNewInvestment > 0 ){
            cout << "Warning: Passed in positive investment to a fixed investment subsector." << endl;
        }
        actInvestment = mFixedInvestments[ aPeriod ];
    }

    // Set the investment amount for the subsector to the quantity actually distributed.
    vector<IInvestable*> investmentTechs = InvestmentUtils::convertToInvestables( baseTechs );
    mInvestments[ aPeriod ] = aDistributor->distribute( aExpProfitRateCalc,
                                                        investmentTechs,
                                                        aNationalAccount,
                                                        aRegionName,
                                                        aSectorName,
                                                        actInvestment,
                                                        aPeriod );

    // Check that the full amount of investment was distributed.
    if( !util::isEqual( mInvestments[ aPeriod ], actInvestment ) ){
        cout << "Warning: " << fabs( mInvestments[ aPeriod ] - actInvestment ) << " difference between "
             << " requested and actual investment in " << aSectorName << " in " << aRegionName << endl;
    }
    // Return the actual amount of investment that occurred.
    return mInvestments[ aPeriod ];
}

/*! \brief returns total Subsector carbon taxes paid
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return total carbon taxes paid by this sub-sector
*/
double Subsector::getTotalCarbonTaxPaid( const int period ) const {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    double sum = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        sum += techs[ i ][ period ]->getCarbonTaxPaid( regionName, period );
    }
    return sum;
}

/*! \brief returns gets fuel consumption map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \pre updateSummary
* \return fuel consumption map
*/
map<string, double> Subsector::getfuelcons( const int period ) const {
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    
    return summary[period].getfuelcons();
}

/*! \brief returns GHG emissions map for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return GHG emissions map
*/
map<string, double> Subsector::getemission( const int period ) const {
    return summary[ period ].getemission();
}

/*! \brief returns map of GHG emissions by fuel for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return map of GHG emissions by fuel
*/
map<string, double> Subsector::getemfuelmap( const int period ) const {
    return summary[ period ].getemfuelmap();
}

/*! \brief returns map of indirect GHG emissions for this sub-sector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return map of indirect GHG emissions
*/
map<string, double> Subsector::getemindmap( const int period ) const {
    return summary[ period ].getemindmap();
}

/*! \brief update summaries for reporting
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
*/
void Subsector::updateSummary( const int period ) {
    // clears Subsector fuel consumption map
    summary[period].clearfuelcons();
    for( unsigned int i = 0; i < techs.size(); ++i ){
        summary[period].initfuelcons( techs[i][0]->getFuelName(), techs[i][period]->getInput() );
    }
}

/*! \brief Return the  expected profit rate.
* \param aNationalAccount The regional accounting object.
* \param aRegionName The name of the region containing this subsector.
* \param aSectorName The name of the sector containing this subsector.
* \param aExpProfitRateCalc The calculator of expected profit rates.
* \param aInvestmentLogitExp The investment logit exponential.
* \param aIsShareCalc Whether this expected profit rate is being used to
*        calculate shares. Not great.
* \param aPeriod The period for which to get the expected profit rate.
* \return The expected profit rate for the subsector.
* \author Josh Lurz
*/
double Subsector::getExpectedProfitRate( const NationalAccount& aNationalAccount,
                                         const string& aRegionName,
                                         const string& aSectorName,
                                         const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                         const double aInvestmentLogitExp,
                                         const bool aIsShareCalc,
                                         const int aPeriod ) const
{   
    assert( aExpProfitRateCalc );

    // Check for fixed investment.
    if( mFixedInvestments[ aPeriod ] != -1 ){
        return 0;
    }
    // Use the passed in expected profit calculator to determine the rate.
    const vector<IInvestable*> investables = InvestmentUtils::convertToInvestables( baseTechs );
    return aExpProfitRateCalc->calcSectorExpectedProfitRate( investables,
                                                             aNationalAccount,
                                                             aRegionName,
                                                             aSectorName,
                                                             aInvestmentLogitExp, 
                                                             aIsShareCalc, 
                                                             aPeriod );
}

/*! \brief Get the capital output ratio.
* \param aRegionName The name of the region containing this subsector.
* \param aSectorName The name of the sector containing this subsector.
* \param aPeriod The period.
* \return The capital output ratio.
* \author Josh Lurz
*/
double Subsector::getCapitalOutputRatio( const IDistributor* aDistributor,
                                         const IExpectedProfitRateCalculator* aExpProfitRateCalc,
                                         const NationalAccount& aNationalAccount,
                                         const string& aRegionName,
                                         const string& aSectorName, 
                                         const int aPeriod ) const
{
    assert( aDistributor );
    // Use the passed in investment distributor to calculate the share weighted average
    // capital to output ratio for the subsector.
    const vector<IInvestable*> investables = InvestmentUtils::convertToInvestables( baseTechs );
    const double capOutputRatio = aDistributor->calcCapitalOutputRatio( investables, aExpProfitRateCalc,
                                                                        aNationalAccount,
                                                                        aRegionName, aSectorName,
                                                                        aPeriod );
    assert( capOutputRatio >= 0 );
    return capOutputRatio;
}
/*! \brief Operate the capital in the base technologies for this subsector. 
* \author Josh Lurz
* \param aMode Whether or not to operate all capital.
* \param aPeriod Period to operate in.
*/
void Subsector::operate( NationalAccount& aNationalAccount, const Demographic* aDemographic, const MoreSectorInfo* aMoreSectorInfo, const bool isNewVintageMode, const int aPeriod ){
    const Modeltime* modeltime = scenario->getModeltime();
    typedef vector<BaseTechnology*>::iterator BaseTechIterator;
    for( BaseTechIterator currTech = baseTechs.begin(); currTech != baseTechs.end(); ++currTech ){
        (*currTech)->operate( aNationalAccount, aDemographic, aMoreSectorInfo, regionName, sectorName, isNewVintageMode, aPeriod );
    }
}

/*! \brief Initialize the marketplaces in the base year to get initial demands from each technology
 * \author Pralit Patel
 * \param period The period will most likely be the base period
 */
void Subsector::updateMarketplace( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int j = 0; j < baseTechs.size(); j++ ) {
        if( baseTechs[ j ]->getYear() == modeltime->getper_to_yr( period ) ){ 
            baseTechs[ j ]->updateMarketplace( sectorName, regionName, period );
        }
    }
}

/*! \brief Function to finalize objects after a period is solved.
* \details This function is used to calculate and store variables which are only needed after the current
* period is complete. 
* \param aPeriod The period to finalize.
* \todo Finish this function.
* \author Josh Lurz
*/
void Subsector::finalizePeriod( const int aPeriod ){
    // Finalize base technologies.
    for( BaseTechIterator baseTech = baseTechs.begin(); baseTech != baseTechs.end(); ++baseTech ){
        (*baseTech)->finalizePeriod( regionName, sectorName, aPeriod );
    }
}

/*! \brief For outputing SGM data to a flat csv File
 * \author Pralit Patel
 * \param period The period which we are outputing for
 */
void Subsector::csvSGMOutputFile( ostream& aFile, const int period ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int j = 0; j < baseTechs.size(); j++ ) {
        if( baseTechs[ j ]->getYear() <= modeltime->getper_to_yr( period ) ){ 
            baseTechs[ j ]->csvSGMOutputFile( aFile, period );
        }
    }
}

/*! \brief Update an output container with information for the subsector.
* \param outputContainer Container to update.
* \param period Period in which to update.
*/
void Subsector::accept( IVisitor* aVisitor, const int period ) const {
    aVisitor->startVisitSubsector( this, period );
    const Modeltime* modeltime = scenario->getModeltime();
    if( period == -1 ){
        // Output all techs.
        for( unsigned int j = 0; j < baseTechs.size(); j++ ) {
            baseTechs[ j ]->accept( aVisitor, period );
        }
    }
    else {
        for( unsigned int j = 0; j < baseTechs.size(); j++ ) {
            if( baseTechs[ j ]->getYear() <= modeltime->getper_to_yr( period ) ){ // should be unneeded.
                baseTechs[ j ]->accept( aVisitor, period );
            }
        }
    }
    // If the period is -1 this means to update output containers for all periods.
    if( period == -1 ){
        for( unsigned int i = 0; i < techs.size(); ++i ){
            for( unsigned int j = 0; j < techs[ i ].size(); ++j ){
                techs[ i ][ j ]->accept( aVisitor, j );
            }
        }
    }
    else {
        for( unsigned int i = 0; i < techs.size(); ++i ){
            techs[ i ][ period ]->accept( aVisitor, period );
        }
    }
    aVisitor->endVisitSubsector( this, period );
}

/*! \brief Return fixed investment.
* \param aPeriod Period to return fixed investment for.
* \return Subsector level fixed investment.
* \todo Can't have a zero investment subsector.
*/
double Subsector::getFixedInvestment( const int aPeriod ) const {
    // Return amount of subsector fixed investment.
    if( mFixedInvestments[ aPeriod ] != -1 ){
        return mFixedInvestments[ aPeriod ];
    }
    // Sum any fixed investment at the vintage level.
    const vector<IInvestable*> investables = InvestmentUtils::convertToInvestables( baseTechs );
    double totalFixedTechInvestment = InvestmentUtils::sumFixedInvestment( investables, aPeriod );
    
    /*! \post Fixed investment must be positive */
    assert( totalFixedTechInvestment >= 0 );
    return totalFixedTechInvestment;
}
