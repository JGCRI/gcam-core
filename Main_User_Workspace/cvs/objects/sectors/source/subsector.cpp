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
#include "technologies/include/building_generic_dmd_technology.h"
#include "technologies/include/intermittent_technology.h"
#include "technologies/include/wind_technology.h"
#include "technologies/include/solar_technology.h"
#include "technologies/include/itechnology.h"
#include "containers/include/scenario.h"
#include "util/base/include/model_time.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
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
#include "technologies/include/default_technology.h"
#include "technologies/include/nuke_fuel_technology.h"
#include "sectors/include/sector_utils.h"
#include "investment/include/investment_utils.h"
#include "reporting/include/indirect_emissions_calculator.h"

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

Subsector::Subsector( const string& aRegionName, const string& aSectorName ):
regionName( aRegionName ),
sectorName( aSectorName ){
    // resize vectors.
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    shrwts.resize( maxper, 1.0 ); // default 1.0, for sectors with one tech.
    lexp.resize( maxper, LOGIT_EXP_DEFAULT );
    summary.resize(maxper); // object containing summaries
    fuelPrefElasticity.resize( maxper );
    summary.resize( maxper );
    scaleYear = modeltime->getEndYear(); // default year to scale share weight to after calibration
    mTechScaleYear = modeltime->getEndYear(); // default year to scale share weight to after calibration
    mInvestments.resize( maxper );
    mFixedInvestments.resize( maxper, -1 );
}

/*! \brief Default destructor.
*
* deletes all Technology objects associated  with this sector.
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
const string& Subsector::getName() const {
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
        else if( nodeName == "sharewt" ){
            XMLHelper<double>::insertValueIntoVector( curr, shrwts, modeltime );
        }
        else if( nodeName == "logitexp" ){
            XMLHelper<double>::insertValueIntoVector( curr, lexp, modeltime );
        }

        else if( nodeName == "fuelprefElasticity" ){
            XMLHelper<double>::insertValueIntoVector( curr, fuelPrefElasticity, modeltime );  
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
            mTechScaleYear = XMLHelper<int>::getValue( curr );
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
                        else if( childNodeName == Technology::getXMLNameStatic2D() ){
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
                // Technology does not exist, create a new vector of techs.

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
                    else if( childNodeName == Technology::getXMLNameStatic2D() ){
                        int thisPeriod = XMLHelper<void>::getNodePeriod( currChild, modeltime );
                        int currYear = modeltime->getper_to_yr( thisPeriod );
                        auto_ptr<ITechnology> tempTech( createChild( nodeName, techName, currYear ) );
                        tempTech->XMLParse( currChild );

                        // Check that a Technology does not already exist.
                        if( techVec[ thisPeriod ] ){
                            ILogger& mainLog = ILogger::getLogger( "main_log" );
                            mainLog.setLevel( ILogger::DEBUG );
                            mainLog << "Removing duplicate Technology " << techVec[ thisPeriod ]->getName() 
                                << " in subsector " << name << " in sector " << sectorName << "." << endl;
                            delete techVec[ thisPeriod ];
                        }

                        techVec[ thisPeriod ] = tempTech.release();

                        // copy Technology object for one period to all the periods
                        if ( XMLHelper<bool>::getAttr( currChild, "fillout" ) ) {
                            // will not do if period is already last period or maxperiod
                            for ( int i = thisPeriod + 1; i < modeltime->getmaxper(); i++ ) {
                                // Check that a Technology does not already exist.
                                if( techVec[ i ] ){
                                    ILogger& mainLog = ILogger::getLogger( "main_log" );
                                    mainLog.setLevel( ILogger::DEBUG );
                                    mainLog << "Removing duplicate Technology " << techVec[ i ]->getName() 
                                        << " in subsector " << name << " in sector " << sectorName << "." << endl;
                                    delete techVec[ i ];
                                }
                                techVec[ i ] = techVec[ thisPeriod ]->clone();
                                techVec[ i ]->setYear( modeltime->getper_to_yr( i ) );
                            } // end for
                        } // end if fillout
                    }
                    else {
                        ILogger& mainLog = ILogger::getLogger( "main_log" );
                        mainLog.setLevel( ILogger::WARNING );
                        mainLog << "Unknown element " << childNodeName
                                << " encountered while parsing " << nodeName
                                << endl;
                    }
                } // end for
                techs.push_back( techVec );
            }
        }
        // parsed derived classes
        else if( !XMLDerivedClassParse( nodeName, curr ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLName() << endl;
        }
    }
}

//! Virtual function which specifies the XML name of the children of this class, the type of Technology.
bool Subsector::isNameOfChild( const string& aTechnologyType ) const {
    return ( aTechnologyType == DefaultTechnology::getXMLNameStatic1D() ||
             aTechnologyType == BuildingGenericDmdTechnology::getXMLNameStatic1D() ||
             aTechnologyType == IntermittentTechnology::getXMLNameStatic1D() ||
             aTechnologyType == WindTechnology::getXMLNameStatic1D() ||
             aTechnologyType == SolarTechnology::getXMLNameStatic1D() ||
             aTechnologyType == NukeFuelTechnology::getXMLNameStatic1D() );
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
    /*! \pre Tech type should be known. */
    assert( isNameOfChild( aTechType ) );

    if( aTechType == DefaultTechnology::getXMLNameStatic1D() ){
        return new DefaultTechnology( aTechName, aTechYear );
    }
    if( aTechType == BuildingGenericDmdTechnology::getXMLNameStatic1D() ){
        return new BuildingGenericDmdTechnology( aTechName, aTechYear );
    }
    if( aTechType == IntermittentTechnology::getXMLNameStatic1D() ){
        return new IntermittentTechnology( aTechName, aTechYear );
    }
    if( aTechType == WindTechnology::getXMLNameStatic1D() ){
        return new WindTechnology( aTechName, aTechYear );
    }
    if( aTechType == SolarTechnology::getXMLNameStatic1D() ){
        return new SolarTechnology( aTechName, aTechYear );
    }
    if( aTechType == NukeFuelTechnology::getXMLNameStatic1D() ){
        return new NukeFuelTechnology( aTechName, aTechYear );
    }

    /*! \invariant createChild should never be called without first checking
    *              isNameOfChild so this operation should never fail. 
    */
    assert( false );

    // Avoid a compiler warning.
    return 0;
}

//! Helper function which parses any type of base Technology correctly.
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
        // doesn't exist so use the new passed in base Technology type.
        newTech->XMLParse( aCurr );

        // Add the new Technology to the vector and the map.
        baseTechs.push_back( newTech.release() ); // Releases ownership of the memory.
        baseTechNameMap[ baseTechs.back()->getIdentifier() ] = static_cast<int>( baseTechs.size() ) - 1;

        // the Technology type may not exist yet.
        map<string,TechnologyType*>::iterator typePos = mTechTypes.find( baseTechs.back()->getName() );
        if( typePos == mTechTypes.end() ){
            // create the tech type, set the iterator to the new item.
            // Insert returns the pair of the iterator position the item was inserted in and whether 
            // the item was inserted, so set the iterator to the first spot in the pair.
            typePos = mTechTypes.insert( make_pair( baseTechs.back()->getName(), new TechnologyType ) ).first;
        }
        typePos->second->addVintage( baseTechs.back() );

        // Set the Technology type helper object to the Technology. This may be moved to the constructor
        // or removed if Technology type is made to inherit from IInvestable.
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
* \warning markets are not necessarily set when completeInit is called
*/
void Subsector::completeInit( const IInfo* aSectorInfo,
                              DependencyFinder* aDependencyFinder,
                              ILandAllocator* aLandAllocator,
                              const GlobalTechnologyDatabase* aGlobalTechDB)
{
    mSubsectorInfo.reset( InfoFactory::constructInfo( aSectorInfo, regionName + "-" + sectorName + "-" + name ) );
    
    for( unsigned int i = 0; i < baseTechs.size(); i++) {
        baseTechs[i]->completeInit( regionName, sectorName, name );
    }

    for( unsigned int j = 0; j < baseTechs.size(); ++j ){
        //if( baseTechs[ j ]->getYear() == modeltime->getper_to_yr( 0 ) ) {
            baseTechs[ j ]->removeEmptyInputs();
        //}
    }

    typedef vector<vector<ITechnology*> >::iterator TechVecIterator;
    for ( TechVecIterator techIter = techs.begin(); techIter != techs.end(); ++techIter ) {
        bool isInvalid = initializeTechVector( *techIter, regionName, sectorName, name,
                                               aDependencyFinder,
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
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aDependencyFinder Regional dependency finder.
* \param aLandAllocator Regional land allocator.
* \param aGlobalTechDB Global Technology database.
* \return Whether the vector should be removed.
*/
bool Subsector::initializeTechVector( vector<ITechnology*>& aTechVector,
                                      const string& aRegionName,
                                      const string& aSectorName,
                                      const string& aSubsectorName,
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
        ( *tech )->completeInit( aRegionName, aSectorName, aSubsectorName, aDependencyFinder,
                                 aSubsecInfo, aLandAllocator, aGlobalTechDB );
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

    XMLWriteElementCheckDefault( scaleYear, "scaleYear", out, tabs, modeltime->getEndYear() );
    XMLWriteElementCheckDefault( mTechScaleYear, "techScaleYear", out, tabs, modeltime->getEndYear() );
    

    XMLWriteVector( shrwts, "sharewt", out, tabs, modeltime, 1.0 );

    XMLWriteVector( lexp, "logitexp", out, tabs, modeltime, LOGIT_EXP_DEFAULT );
    
    XMLWriteVector( fuelPrefElasticity, "fuelprefElasticity", out, tabs, modeltime, 0.0 );

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
    XMLWriteElement( shrwts[ period ], "sharewt", out, tabs );
    XMLWriteElement( scaleYear, "scaleYear", out, tabs );
    XMLWriteElement( mTechScaleYear, "techScaleYear", out, tabs );
    XMLWriteElement( lexp[ period ], "lexp", out, tabs );
    XMLWriteElement( fuelPrefElasticity[ period ], "fuelprefElasticity", out, tabs );
    XMLWriteElement( getEnergyInput( period ), "input", out, tabs );
    XMLWriteElement( getOutput( period ), "output", out, tabs );
    XMLWriteElement( getTotalCalOutputs( period ), "total-cal-outputs", out, tabs );
    XMLWriteElement( mInvestments[ period ], "investment", out, tabs );
    XMLWriteElement( mFixedInvestments[ period ], "FixedInvestment", out, tabs );
    XMLWriteElement( getCalibrationStatus( period ), "calibration-status", out, tabs );

    toDebugXMLDerived( period, out, tabs );
    // Write out the summary object.
    // summary[ period ].toDebugXML( period, out );
    // write out the Technology objects.

    for ( unsigned int j = 0; j < baseTechs.size(); j++ ) {
        // This isn't right, techs with years other the current year could change output.
        if (baseTechs[j]->getYear() == scenario->getModeltime()->getper_to_yr( period ) ) {
            baseTechs[j]->toDebugXML( period, out, tabs );
        }
    }
    
    for( unsigned int j = 0; j < techs.size(); ++j ){
        for( unsigned int per = 0; per < techs[ j ].size(); ++per ){
            techs[ j ][ per ]->toDebugXML( period, out, tabs );
        }
    }
    
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
const string& Subsector::getXMLName() const {
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
const string& Subsector::getXMLNameStatic() {
    return XML_NAME;
}

/*!
* \brief Perform any initializations needed for each period.
* \details Perform any initializations or calculations that only need to be done
*          once per period (instead of every iteration) should be placed in this
*          function.
* \warning The ghg part of this routine assumes the existence of technologies in
*          the previous and future periods
* \author Steve Smith, Sonny Kim
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics container.
* \param aMoreSectorInfo SGM sector info object.
* \param aPeriod Model period
*/
void Subsector::initCalc( NationalAccount* aNationalAccount,
                          const Demographic* aDemographics,
                          const MoreSectorInfo* aMoreSectorInfo,
                          const int aPeriod )
{
    // Initialize all technologies.
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        // Initialize the previous period info as having no input set and
        // cumulative Hicks neutral, energy of 1.
        PreviousPeriodInfo prevPeriodInfo = { 0, 1 };
        // Warning: aPeriod is the current model period and not the technology vintage.
        // Currently calls initCalc on all vintages past and future.
        // TODO: Should not call initialization for all future technology vintages beyond the
        // current period but correction causing error (SHK).
        for( unsigned int vintage = 0; vintage < techs[ i ].size(); ++vintage ){
            techs[ i ][ vintage ]->initCalc( regionName, sectorName, mSubsectorInfo.get(),
                                       aDemographics, prevPeriodInfo, aPeriod );
        }
    }

    // Initialize the baseTechs. This might be better as a loop over tech types. 
    const Modeltime* modeltime = scenario->getModeltime();
    for( unsigned int j = 0; j < baseTechs.size(); j++ ){
        if( aPeriod == 0 && baseTechs[ j ]->getYear() == modeltime->getper_to_yr( 0 ) ){
            double totalCapital = mTechTypes[ baseTechs[ j ]->getName() ]->getTotalCapitalStock( baseTechs[ j ]->getYear() );
            baseTechs[ j ]->initCalc( aMoreSectorInfo, regionName, sectorName, *aNationalAccount,
                                      aDemographics, totalCapital, aPeriod );
        
            // copy base year tech to old vintages
            mTechTypes[ baseTechs[ j ]->getName() ]->initializeTechsFromBase( baseTechs[ j ]->getYear() );
        }
        // If the current tech is from the previous period, initialize the current tech with its parameters.
        else if ( aPeriod > 0 && baseTechs[ j ]->getYear() == modeltime->getper_to_yr( aPeriod - 1 ) ) {
            BaseTechnology* newTech = mTechTypes[ baseTechs[ j ]->getName() ]->initOrCreateTech( modeltime->getper_to_yr( aPeriod ), baseTechs[ j ]->getYear() );
            // Check if initOrCreate created a Technology which needs to be added to the base tech vector and map.
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
                baseTechs[ j ]->initCalc( aMoreSectorInfo, regionName, sectorName, *aNationalAccount, aDemographics, 0, aPeriod );
            }
        }
    }

    // If calibration is active, reinitialize share weights.
    if( Configuration::getInstance()->getBool( "CalibrationActive" ) ){
        // Check for zero shareweight for subsector with calibration values
        if( getTotalCalOutputs( aPeriod ) > util::getSmallNumber() 
            && shrwts[ aPeriod ] == 0 ) 
        {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "Resetting zero shareweight for Subsector " << getName()
                << " in sector " << sectorName << " in region " << regionName
                << " since calibration values are present." << endl;
            shrwts[ aPeriod ] = 1.0;
        }
        // For subsectors with only fixed output technologies.
        if( containsOnlyFixedOutputTechnologies( aPeriod ) ){
            // Reset share weight for all periods to 0 to indicate that it does not have an 
            // impact on the results.
            shrwts[ aPeriod ] = 0;
        }
        // Reinitialize share weights to 1 for competing subsector with non-zero read-in share weight
        // for calibration periods only.
        else if( shrwts[ aPeriod ] != 0 && aPeriod <= modeltime->getFinalCalibrationPeriod() ){
            // Reinitialize to 1 to remove bias, calculate new share weights and
            // normalize in postCalc to anchor to dominant subsector.
            shrwts[ aPeriod ] = 1.0;
        }
    }

    // Interpolate subsector and technology share weights from calibrated value to exogenously
    // specified terminal value if applicable.
    interpolateShareWeights( aPeriod );

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
        // This would allow user to input a GHG object only in one period
        if ( numberOfGHGs < techs[i][ aPeriod - 1 ]->getNumbGHGs() ) {
            // TODO
        }
        
        // New method
        if ( aPeriod > 1 ) { // Note the hard coded base period
         for ( int j=0 ; j<numberOfGHGs; j++ ) {
            techs[i][ aPeriod ]->copyGHGParameters( techs[i][ aPeriod - 1]->getGHGPointer( ghgNames[j]  ) );
         } // End For
        }
      
    } // End For
}

/*! \brief Returns the subsector price.
* \details Calculates and returns share-weighted total price (subsectorprice)
*          and cost of fuel (fuelprice). 
* \author Sonny Kim
* \param aGDP Regional GDP object.
* \param aPeriod Model period
*/
double Subsector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    double subsectorPrice = 0; // initialize to 0 for summing
    const vector<double> techShares = calcTechShares( aGDP, aPeriod );
    for ( unsigned int i = 0; i < techs.size(); ++i ) {
        // Technologies with zero share cannot affect the marginal price.
        if( techShares[ i ] > util::getSmallNumber() ){
            double currCost = techs[i][aPeriod]->getCost( aPeriod );
            // calculate weighted average price for Subsector
            if( currCost >= util::getSmallNumber() ) {
                subsectorPrice += techShares[ i ] * currCost;
            }
            // We want to allow regional and delivered biomass prices to be negative
            else if ( ( sectorName == "regional biomass"  || sectorName == "delivered biomass" ) ){
                subsectorPrice += techShares[ i ] * currCost;
            }

        }
    }

    // We want to allow regional and delivered biomass prices to be negative
    if ( ( sectorName == "regional biomass"  || sectorName == "delivered biomass" ) 
        && ( subsectorPrice < 0 ) ){
        return subsectorPrice;
    }


    // Check for the condition where all technologies were fixed.
    return ( subsectorPrice >= util::getSmallNumber() ) ? subsectorPrice : -1;
}

/*! \brief Returns whether the subsector should be calibrated.
* \details If either the Subsector output, or the output of all the technologies
*          under this Subsector (not including those with zero output) are
*          calibrated, then the Subsector should calibrate.
* \author Steve Smith
* \param aPeriod Model period
*/
bool Subsector::getCalibrationStatus( const int aPeriod ) const {
    
    // Check all the technologies for the period.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // Check whether there is any calibration input, not one for a specific fuel.
        if ( techs[ i ][ aPeriod ]->hasCalibratedValue( aPeriod) ) {
            return true;
        }
    }
    return false;
}


/*! \brief returns Subsector fuel price times share
* \details Returns the share-weighted fuel price, which is later summed to get
*          the sector-weighted fuel price.
* \author Sonny Kim
* \param aGDP GDP container.
* \param aPeriod Model period.
* \return share-weighted fuel price
*/
double Subsector::getAverageFuelPrice( const GDP* aGDP, const int aPeriod ) const {
    // Determine the average fuel price.
    double fuelPrice = 0;

    // The base period is not solved so the current shares can be calculated and
    // used. In future periods the previous period's shares must be used as the
    // current period's are unknown.
    const int sharePeriod = ( aPeriod == 0 ) ? aPeriod : aPeriod - 1;

    const vector<double> techShares = calcTechShares( aGDP, sharePeriod );
    for ( unsigned int i = 0; i < techs.size(); ++i) {
        // calculate weighted average price of fuel only
        // Technology shares are based on total cost
        fuelPrice += techShares[ i ] * techs[i][ aPeriod ]->getEnergyCost( regionName, sectorName, aPeriod );
    }
    /*! \post Fuel price must be positive. */
    assert( fuelPrice >= 0 );
    return fuelPrice;
}

/*! \brief calculate Technology shares within Subsector
*
* Calls Technology objects to first calculate cost, then their share. Follows this by normalizing shares. 
*
* \author Marshall Wise, Josh Lurz
* \param regionName region name
* \param period model period
* \return A vector of technology shares.
*/
const vector<double> Subsector::calcTechShares( const GDP* aGDP, const int aPeriod ) const {
    vector<double> techShares( techs.size() );
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // determine shares based on Technology costs
        techShares[ i ] = techs[i][aPeriod]->calcShare( regionName, sectorName, aGDP, aPeriod );

        // Check that Technology shares are valid.
        assert( util::isValidNumber( techShares[ i ] ) );
    }
    // Normalize technology shares.
    SectorUtils::normalizeShares( techShares );
    return techShares;
}

/*!
* \brief Calculate the cost of the Subsector.
* \details Instructs all technologies to calculate their costs. The subsector
*          can calculate it's costs dynamically once all Technologies have
*          calculated their costs, so the Subsector cost is not stored.
* \param aPeriod Model period.
*/
void Subsector::calcCost( const int aPeriod ){
    // Instruct all technologies up to and including the current period to
    // calculate their costs. Future Technologies cannot have a cost as they do
    // not yet exist.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int j = 0; j <= aPeriod; ++j ){
            techs[ i ][ j ]->calcCost( regionName, sectorName, aPeriod );
        }
    }
}

/*! \brief calculate Subsector unnormalized shares
* \details Calculates the unnormalized share for this sector. Also calculates
*          the sector aggregate price (or cost)
* \author Sonny Kim, Josh Lurz
* \param period model period
* \param gdp gdp object
* \warning There is no difference between demand and supply technologies.
*          Control behavior with value of parameter fuelPrefElasticity
* \return The subsector share.
*/
double Subsector::calcShare(const int period, const GDP* gdp ) const {
    double subsectorPrice = getPrice( gdp, period );

    if( subsectorPrice >= util::getSmallNumber() ){
        double scaledGdpPerCapita = gdp->getBestScaledGDPperCap( period );
        // fuelPrefElasticity = 0 for Supply Sector and has no impact
        // fuelPrefElasticity is for Demand Sector only
        // TODO: make this explicit, shk 12/15/05
        // TODO: fuelPrefElasticity should be eliminated now that the
        // Demand Sector no longer exists.  SHK 7/16/07
        double share = shrwts[period] * pow( subsectorPrice, lexp[ period ] )
                        * pow( scaledGdpPerCapita, fuelPrefElasticity[ period ] );
        /*! \post Share is zero or positive. */
        // Check for invalid shares.
        if( share < -util::getSmallNumber() || !util::isValidNumber( share ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Invalid share for " << name << " in " << regionName 
                << " of " << share << endl;
        }
        return share;
    }

    // We want to allow regional and delivered biomass prices to be negative
    if( subsectorPrice < 0 && ( sectorName == "regional biomass"  || sectorName == "delivered biomass" ) ) {
        // This assumes that there is only one subsector in regional biomass
        return 1;
    }

    return 0;
}

/*! \brief Return the total exogenously fixed Technology output for this sector.
* \author Steve Smith
* \param period model period
*/
double Subsector::getFixedOutput( const int aPeriod ) const {
    double fixedOutput = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int per = 0; per <= aPeriod; ++per ){
            double currFixedOutput = techs[i][per]->getFixedOutput( regionName, sectorName, false, "", aPeriod );
            /*! \invariant Fixed output for each Technology must be -1 or
            *              positive. 
            */
            assert( fixedOutput == -1 || fixedOutput >= 0 );
            if( currFixedOutput > 0 ){
                fixedOutput += currFixedOutput;
            }
        }
    }
    /*! \post Fixed output total must be positive. */
    assert( fixedOutput >= 0 );
    return fixedOutput;
}

/*! \brief Consistently adjust share weights for previous period after calibration 
* If the sector share weight in the previous period was changed due to calibration, 
* then adjust next few shares so that there is not a big jump in share weights.
*
* Can turn this feature off by setting the scaleYear before the calibration year (e.g., 1975, or even zero)
*
* If scaleYear is set to be the calibration year then share weights are kept constant
*
* \author Steve Smith
* \param period Model period
* \warning Share weights must be scaled (from sector) before this is called.
*/
void Subsector::interpolateShareWeights( const int aPeriod ) {
    const Modeltime* modeltime = scenario->getModeltime();

    // if previous period was calibrated, then adjust future share weights Only
    // scale share weights if after the final calibration year.
    if ( ( aPeriod > modeltime->getFinalCalibrationPeriod() ) 
        && getCalibrationStatus( aPeriod - 1 )
        && Configuration::getInstance()->getBool( "CalibrationActive" ) )
    {
        int endPeriod = 0;
        if ( scaleYear >= modeltime->getStartYear() ) {
            endPeriod = modeltime->getyr_to_per( scaleYear );
        }
        if  ( endPeriod >= ( aPeriod - 1 ) ) {
            ILogger& mainLog = ILogger::getLogger( "calibration_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Interpolating share weights for subsector " << name
                << " in sector " << sectorName << " from starting period of "
                << aPeriod - 1 << " with scale year " << scaleYear
                << " and starting value " << shrwts[ aPeriod - 1 ] << "." << endl;
                // This is wrong, the period before could have calibrated to zero.
                if ( shrwts[ aPeriod - 1 ] > 0 ) {
                    shareWeightLinearInterpFn( aPeriod - 1, endPeriod );
                }
        }
        // Interpolate technology share weights
        techShareWeightLinearInterpFn( aPeriod - 1, modeltime->getyr_to_per( mTechScaleYear ) );
    }
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
        // If end period equals the beginning period then this is a flag to keep the weights the same, so make increment zero
        // and loop over rest of periods
        loopPeriod = modeltime->getmaxper();  
        shareIncrement = 0;
    }

    for ( int period = beginPeriod + 1; period < loopPeriod; period++ ) {
        shrwts[ period ] = shrwts[ period - 1 ] + shareIncrement;
    }

    ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
    calibrationLog.setLevel( ILogger::DEBUG );
    calibrationLog << "Share weights interpolated with increment " << shareIncrement 
                   << " for subsector " << name << " in sector " << sectorName << " in region " << regionName << endl;
}

/*! \brief Linearly interpolate Technology share weights between specified endpoints 
* Utility function to linearly scale Technology share weights between two specified points.
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

/*! \brief Scales Technology share weights so that the dominant technology is anchored to 1
* and the others are relative to the dominant technology.
*
* This is needed so that share weights can be easily interpreted and so that
* future share weights can be consistently applied relative to the dominant technology.
* Technology share weight greater than 1 implies bias towards the technology even though
* it is not the dominant technology. The call to normalizeShareWeights() must occur after the
* period has been solved and technology outputs are known.  
*
* \author Steve Smith, Marshall Wise, Kate Calvin, Sonny Kim
* \param period Model period
* \warning The routine assumes that all technology outputs are calibrated.
*/
void Subsector::normalizeTechShareWeights( const int aPeriod ) {
        
    // Dominant technology gets a share weight of one.
    double maxShareWeight = 0.0;
    double maxOutput = 0.0;
    const int NewVintage = aPeriod;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // Fixed output technologies are not included in the normalization.
        if( !techs[ i ][ NewVintage ]->isFixedOutputTechnology( aPeriod ) ){
            double techShareWeight = techs[ i ][ NewVintage ]->getShareWeight();
            if ( techs[ i ][ NewVintage ]->getOutput( aPeriod ) > maxOutput ) {
                maxShareWeight = techShareWeight;
                maxOutput = techs[ i ][ NewVintage ]->getOutput( aPeriod );
            }
        }
    }

    if ( maxShareWeight < util::getTinyNumber() ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::DEBUG );
        //mainLog << "Max shareweight is zero in subsector " << name << " in region " << regionName << "." << endl;
    } 
    else {
        for( unsigned int i = 0; i < techs.size(); ++i ){
            // Fixed output technologies are not included in the normalization.
            if( !techs[ i ][ NewVintage ]->isFixedOutputTechnology( aPeriod ) ){
                techs[ i ][ NewVintage ]->scaleShareWeight( 1 / maxShareWeight );
            }
        }
        ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
        calibrationLog.setLevel( ILogger::DEBUG );
        calibrationLog << "Shareweights normalized for technologies in subsector "
            << name << " in sector " << sectorName << " in region " << regionName << endl;
    }
}

/*! \brief The demand passed to this function is shared out at the Technology
*          level.
* \details Variable demand (could be energy or energy service) is passed to
*          technologies and then shared out at the Technology level.
* \author Sonny Kim, Josh Lurz
* \param aSubsectorVariableDemand Total variable demand for this subsector.
* \param aFixedOutputScaleFactor Scale factor to scale down fixed output
*        technologies.
* \param aPeriod Model period
* \param aGDP Regional GDP container.
*/
void Subsector::setOutput( const double aSubsectorVariableDemand, 
                           const double aFixedOutputScaleFactor,
                           const GDP* aGDP,
                           const int aPeriod )
{
    assert( util::isValidNumber( aSubsectorVariableDemand ) && aSubsectorVariableDemand >= 0 );
    
    // Calculate the technology shares.
    const vector<double> shares = calcTechShares( aGDP, aPeriod );
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        // Loop over periods.
        for( int j = 0; j <= aPeriod; ++j ){
            // Only pass variable output to current vintage.
            if( j == aPeriod ){
                // calculate Technology output and fuel input from Subsector output
                techs[i][j]->production( regionName, sectorName, aSubsectorVariableDemand * shares[ i ],
                                         aFixedOutputScaleFactor, aGDP, aPeriod );
            }
            else {
                // calculate Technology output and fuel input for past vintages
                techs[i][j]->production( regionName, sectorName, 0,
                                         aFixedOutputScaleFactor, aGDP, aPeriod );
            }
        }
    }
}

/*! \brief Test to see if calibration worked for this subsector
* \author Josh Lurz
* \param aPeriod The model period.
* \param aCalAccuracy Accuracy (fraction) to check if calibrations are within.
* \param aPrintWarnings Whether to print a warning.
* \return Whether calibration was successful.
*/
bool Subsector::isAllCalibrated( const int aPeriod, double aCalAccuracy, const bool aPrintWarnings ) const {
    // Check if each technology is calibrated.
    bool isAllCalibrated = true;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        bool isThisTechCalibrated = techs[ i ][ aPeriod ]->isAllCalibrated( aPeriod, aCalAccuracy,
                                               regionName, sectorName, name, aPrintWarnings );
 
        // if this (or any) technology not calibrated, indicate that subsector is not calibrated
        if (!isThisTechCalibrated) {
                isAllCalibrated = false;
        }
    }

    // if all technologies are calibrated, return true for the subsector, otherwise return false
    return isAllCalibrated;

}

/*! \brief returns the total calibrated output from this sector.
*
* Routine adds up calibrated values from both the sub-sector and (if not
* calibrated at Subsector), Technology levels. This returns only calibrated
* outputs, not values otherwise fixed (as fixed or zero share weights)
*
* \author Steve Smith
* \param period Model period
* \return Total calibrated output for this Subsector
*/
double Subsector::getTotalCalOutputs( const int period ) const {
    double sumCalValues = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        double currCalOutput = techs[ i ][ period ]->getCalibrationOutput( false, "", period );
        if( currCalOutput > 0 ){
            sumCalValues += currCalOutput;
        }
    }
    /*! \post Total calibrated output is greater than or equal to zero. */
    assert( sumCalValues >= 0 );

    return sumCalValues;
}

/*! \brief returns true if all output is either fixed or calibrated.
*
* If output is is calibrated, fixed, or share weight is zero for this Subsector or all technologies in this subsector returns true.
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

    // if not fixed at sub-sector level, then check at the Technology level
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if ( !techs[ i ][ period ]->isOutputFixed( false, "", period ) ) {
            return false;
        }
    }
    return true;
}

/*!\brief Returns a boolean for whether the subsector contains only fixed output technologies
* or at least one technology that competes on the margin.
*\author Sonny Kim
*\return Boolean for determining whether subsector contains only fixed output technologies.
*/
bool Subsector::containsOnlyFixedOutputTechnologies( const int aPeriod ) const {
    // Returns true if all technologies in the subsector has fixed exogenous outputs
    // for new investments.
    const int NewVintage = aPeriod;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // If at least one technology does not have fixed output return false.
        // Do not consider a technology that has a zero share weight
        if ( techs[ i ][ NewVintage ]->getShareWeight() != 0 
            && !techs[ i ][ NewVintage ]->isFixedOutputTechnology( aPeriod ) ) {
            return false;
        }
    }
    // Otherwise subsector contains only fixed output technologies.
    return true;
}

/*! \brief returns true if inputs are all fixed for this subsector and input
*          good
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for. If equal to the value
*        "allInputs" then returns all inputs.
* \return boolean true if inputs of specified good are fixed
*/
bool Subsector::inputsAllFixed( const int aPeriod, const string& goodName ) const {
    if( shrwts[ aPeriod ] == 0 ){
        return true;
    }

    // test for each method of fixing output, if none of these are true then
    // demand is not all fixed
    bool hasRequiredInput = ( goodName != "allInputs" );
    for ( unsigned int i=0; i< techs.size(); i++ ) {
        if ( !techs[ i ][ aPeriod ]->isOutputFixed( hasRequiredInput,
                                                   hasRequiredInput ? goodName : "", aPeriod ) ) {
            return false;
        }
    }
    return true;
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
    /*! \post Shareweight is valid and greater than or equal to zero. */
    assert( util::isValidNumber( shrwts[ period ] ) && shrwts[ period ] >= 0 );
    return shrwts[ period ];
}

/*! \brief Scales share weight for this Subsector
*
* \author Steve Smith
* \param period Model period
* \param scaleValue Multipliciatve scale factor for shareweight
*/
void Subsector::scaleShareWeight( const double scaleValue, const int period ) {
    /*! \pre Scale value must be greater than or equal to zero. */
    assert( scaleValue >= 0 );

    if ( scaleValue != 0 ) {
        shrwts[ period ] *= scaleValue;
    }
}

//! write Subsector output to database
// TODO: Fix up this output to handle multiple vintages correctly.
void Subsector::csvOutputFile( const GDP* aGDP, 
                               const IndirectEmissionsCalculator* aIndirectEmissCalc ) const {

    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const string& outputUnit = mSubsectorInfo->getString( "output-unit", true );
    const string& priceUnit = mSubsectorInfo->getString( "price-unit", true );
    vector<double> temp(maxper);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    fileoutput3( regionName,sectorName,name," ","production",outputUnit,temp);
    // Subsector price
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getPrice( aGDP, m );
    }
    fileoutput3( regionName,sectorName,name," ","price",priceUnit,temp);

    for ( int m= 0;m<maxper;m++){
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    fileoutput3( regionName,sectorName,name," ","CO2 emiss","MTC",temp);

    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        // sjs -- bad coding here, hard-wired period. But is difficult to do
        // something different with current output structure. This is just for
        // csv file. This should just use the emissions map.
        vector<string> ghgNames;
        ghgNames = techs[i][ 2 ]->getGHGNames();        
        for ( unsigned int ghgN =0; ghgN < ghgNames.size(); ghgN++ ) {
            if ( ghgNames[ ghgN ] != "CO2" ) {
                for ( int m=0;m<maxper;m++) {
                    temp[m] = techs[i][ m ]->getEmissionsByGas( ghgNames[ ghgN ], m );
                }
                fileoutput3( regionName,sectorName,name,techs[i][ 2 ]->getName(), ghgNames[ ghgN ] + " emiss","Tg",temp);
            }
        }

        // output or demand for each technology
        for ( int m= 0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput( m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"production",outputUnit,temp);
        // Technology share
        if( techs.size() > 1 ) {
            for ( int m=0;m<maxper;m++) {
                double subsecOutput = getOutput( m );
                if( subsecOutput > 0 ){
                    temp[m] = techs[i][m]->getOutput( m ) / subsecOutput;
                }
                else {
                    temp[ m ] = 0;
                }
            }
            fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"tech share","%",temp);
        }
        // Technology cost
        for ( int m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCost( m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"price",priceUnit,temp);

        // Technology CO2 emission
        for ( int m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getEmissionsByGas("CO2", m );
        }
        fileoutput3( regionName,sectorName,name,techs[i][ 0 ]->getName(),"CO2 emiss","MTC",temp);
    }
}

/*! \brief Write supply sector MiniCAM style Subsector output to database.
*
* Writes outputs with titles and units appropriate to supply sectors.
*
* \author Sonny Kim
*/
void Subsector::MCoutputSupplySector( const GDP* aGDP ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const double CVRT_90 = 2.212; //  convert '75 price to '90 price
    const string& outputUnit = mSubsectorInfo->getString( "output-unit", true );
    const string& priceUnit = mSubsectorInfo->getString( "price-unit", true );
    vector<double> temp(maxper);
    
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    dboutput4(regionName,"Secondary Energy Prod",sectorName,name,outputUnit,temp);
    // Subsector price
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getPrice( aGDP, m );
    }
    dboutput4(regionName,"Price",sectorName,name,priceUnit,temp);
    // for electricity sector only
    if (sectorName == "electricity") {
        for ( int m=0;m<maxper;m++) {
            temp[m] = getPrice( aGDP, m ) * CVRT_90 * 0.36;
        }
        dboutput4(regionName,"Price",sectorName+" C/kWh",name,"90C/kWh",temp);
    }
    
    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){

        // secondary energy and price output by tech
        // output or demand for each Technology
        for ( int m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getOutput( m );
        }

        dboutput4( regionName, "Secondary Energy Prod", sectorName + "_tech-new-investment", 
            techs[i][ 0 ]->getName(), outputUnit, temp );
        
        // Output for all vintages.
        for ( int m=0; m < maxper;m++) {
            temp[ m ] = 0;
            // Only sum output to the current period.
            for( int j = 0; j <= m; ++j ){
                temp[m] += techs[i][j]->getOutput( m );
            }
        }
        dboutput4( regionName, "Secondary Energy Prod", sectorName + "_tech-total", techs[i][ 0 ]->getName(), outputUnit, temp );
        // Technology cost
        for ( int m=0;m<maxper;m++) {
            temp[m] = techs[i][m]->getCost( m ) * CVRT_90;
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
void Subsector::MCoutputDemandSector( const GDP* aGDP ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const string& outputUnit = mSubsectorInfo->getString( "output-unit", true );
    const string& priceUnit = mSubsectorInfo->getString( "price-unit", true );
    vector<double> temp(maxper);
    
    // total Subsector output
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    dboutput4(regionName,"End-Use Service",sectorName+" by Subsec",name,outputUnit,temp);
    dboutput4(regionName,"End-Use Service",sectorName+" "+name,"zTotal",outputUnit,temp);
    // Subsector price
    for( int m = 0; m < maxper; m++ ){
        temp[ m ] = getPrice( aGDP, m );
    }
    dboutput4(regionName,"Price",sectorName,name+" Tot Cost",priceUnit,temp);
    
    // do for all technologies in the Subsector
    for( unsigned int i = 0; i < techs.size(); ++i ){
        if( techs.size() > 1 ) {  // write out if more than one Technology
            // output or demand for each Technology
            for ( int m=0;m<maxper;m++) {
                temp[ m ] = 0;
                for( unsigned int j = 0; j < techs[ i ].size(); ++j ){
                    temp[m] += techs[i][j]->getOutput( m );
                }
            }
            dboutput4(regionName,"End-Use Service",sectorName+" "+name,techs[i][ 0 ]->getName(),outputUnit,temp);
            // total Technology cost
            for ( int m=0;m<maxper;m++) {
                temp[m] = techs[i][m]->getCost( m );
            }

            dboutput4(regionName,"Price",sectorName+" "+name,techs[i][ 0 ]->getName(),priceUnit,temp);
        }
    }
}

/*! \brief Write common MiniCAM style Subsector output to database.
*
* Writes outputs that are common to both supply and demand sectors.
*
* \author Sonny Kim
*/
void Subsector::MCoutputAllSectors( const GDP* aGDP,
                                    const IndirectEmissionsCalculator* aIndirectEmissCalc, 
                                    const vector<double> aSectorOutput ) const {
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    const string outputUnit = mSubsectorInfo->getString( "output-unit", true );
    const string inputUnit = mSubsectorInfo->getString( "input-unit", true );
    const string priceUnit = mSubsectorInfo->getString( "price-unit", true );
    vector<double> temp(maxper);
    
    // Subsector share
    for( int m = 0; m < maxper; m++ ){
        if( aSectorOutput[ m ] > 0 ){
            temp[ m ] = getOutput( m ) / aSectorOutput[ m ];
        }
        else {
            temp[ m ] = 0;
        }
    }
    dboutput4(regionName,"Subsec Share",sectorName,name,"100%", temp );
    dboutput4(regionName,"Subsec Share Wts",sectorName,name,"NoUnits", shrwts );
    // Technology share weight by Subsector-technology for each sector
    for ( unsigned int i = 0; i < techs.size(); ++i ){
        for ( unsigned int m = 0; m <  techs[ i ].size(); ++m ) {
            temp[m] = techs[i][m]->getShareWeight();
        }
        dboutput4( regionName, "Tech Share Wts", sectorName, name+"-"+techs[i][0]->getName(),
            "NoUnits", temp );
    }

    // Subsector emissions for all greenhouse gases

    // subsector CO2 emission. How is this different then below?
    for ( int m = 0; m < maxper; m++ ) {
        temp[ m ] = 0;
        for ( unsigned int i = 0; i < techs.size(); ++i ){
            for ( unsigned int j = 0; j <  techs[ i ].size(); ++j ) {
                // this gives Subsector total CO2 emissions
                // get CO2 emissions for each Technology
                temp[m] += techs[i][j]->getEmissionsByGas("CO2", m );
            }
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
        dboutput4(regionName,"CO2 Emiss(ind)",sectorName, subsecTechName,"MTC",temp);

        // Technology share
        for ( int m = 0; m < maxper; m++) {
            temp[ m ] = 0;
            double subsecOutput = getOutput( m );
            if( subsecOutput > 0 ){
                // sums all periods
                // does this exclude non operating vintages?
                for( unsigned int j = 0; j < techs[ i ].size(); ++j ){
                    temp[m] += techs[i][j]->getOutput( m ) / subsecOutput;
                }
            }
        }
        dboutput4(regionName,"Total Tech Share",sectorName, subsecTechName,"%",temp);

        // New technology share
        for ( int m = 0; m < maxper; m++) {
            temp[ m ] = 0;
            double subsecOutput = getOutput( m );
            if( subsecOutput > 0 ){
                temp[m] = techs[i][m]->getOutput( m ) / subsecOutput;
            }
        }
        dboutput4(regionName,"Tech Share (New)",sectorName, subsecTechName,"%",temp);

        // New technology share of investment.
        for ( int m=0;m<maxper;m++) {
            const vector<double> shares = calcTechShares( aGDP, m );
            temp[m] = shares[ i ];
        }

        dboutput4(regionName,"Tech Inv Share",sectorName, subsecTechName,"%",temp);

        // Old technology share
        for ( int m = 0; m < maxper; m++) {
            temp[ m ] = 0;
            double subsecOutput = getOutput( m );
            if( subsecOutput > 0 ){
                for( int j = 0; j < m; ++j ){
                    // does this exclude non-operating vintages
                    temp[m] += techs[i][j]->getOutput( m ) / subsecOutput;
                }
            }
        }
        dboutput4(regionName,"Tech Share (Old)",sectorName, subsecTechName,"%",temp);

        // New technology share of investment.
        for ( int m = 0; m < maxper; m++) {
            const vector<double> shares = calcTechShares( aGDP, m );
            temp[m] = shares[ i ];
        }
        dboutput4(regionName,"Tech Invest Share",sectorName, subsecTechName,"%",temp);

        // ghg tax and storage cost applied to Technology if any
        for ( int m = 0; m < maxper; m++) {
            temp[m] = techs[i][m]->getTotalGHGCost( regionName, sectorName, m );
        }
        dboutput4( regionName, "Total GHG Cost", sectorName, subsecTechName, priceUnit, temp);
    }
                                    }

//! calculate GHG emissions from annual production of each Technology
void Subsector::emission( const int period ){
    /*! \pre period is less than max period. */
    assert( period < scenario->getModeltime()->getmaxper() );
    summary[period].clearemiss(); // clear emissions map
    summary[period].clearemfuelmap(); // clear emissions map
    
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int j = 0; j <= period; ++j ){
            summary[period].updateemiss( techs[i][j]->getEmissions( sectorName, period ) );
            summary[period].updateemfuelmap( techs[i][j]->getEmissionsByFuel( sectorName, period ) );
        }
    }
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
    for ( unsigned int i = 0; i < techs.size(); i++ ) {
        // Only sum output up to the current period. Future Technologies cannot
        // have output.
        for( int j = 0; j <= period; ++j ){
            outputSum += techs[i][j]->getOutput( period );
        }
    }

    // Add on the base techs output too.
    for( CBaseTechIterator currTech = baseTechs.begin(); currTech != baseTechs.end(); ++currTech ){
        outputSum += (*currTech)->getOutput( period );
    }
    /*! \post Total subsector output is positive. */
    assert( outputSum >= 0 );
    return outputSum;
}

/*!
 * \brief Get the energy input for the Subsector.
 * \param aPeriod Period.
 * \return Total energy input.
 */
double Subsector::getEnergyInput( const int aPeriod ) const {
    double totalEnergy = 0;
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int j = 0; j <= aPeriod; ++j ){
            totalEnergy += techs[ i ][ j ]->getEnergyInput( aPeriod );
        }
    }
    return totalEnergy;
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

/*! \brief returns gets fuel consumption map for this subsector
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

/*! \brief returns GHG emissions map for this subsector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return GHG emissions map
*/
map<string, double> Subsector::getemission( const int period ) const {
    return summary[ period ].getemission();
}

/*! \brief returns map of GHG emissions by fuel for this subsector
*
* \author Sonny Kim, Josh Lurz
* \param period Model period
* \return map of GHG emissions by fuel
*/
map<string, double> Subsector::getemfuelmap( const int period ) const {
    return summary[ period ].getemfuelmap();
}

/*! \brief update summaries for reporting
*
* \author Sonny Kim, Josh Lurz
* \param aPrimaryFuelList List of primary fuels.
* \param period Model period
*/
void Subsector::updateSummary( const list<string>& aPrimaryFuelList,
                               const int period )
{
    // clears Subsector fuel consumption map
    summary[period].clearfuelcons();
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( int j = 0; j <= period; ++j ){
            summary[period].updatefuelcons( aPrimaryFuelList, techs[ i ][ j ]->getFuelMap( period ) );
        }
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
    typedef vector<BaseTechnology*>::iterator BaseTechIterator;
    for( BaseTechIterator currTech = baseTechs.begin(); currTech != baseTechs.end(); ++currTech ){
        (*currTech)->operate( aNationalAccount, aDemographic, aMoreSectorInfo, regionName, sectorName, isNewVintageMode, aPeriod );
    }
}

/*! \brief Initialize the marketplaces in the base year to get initial demands from each Technology
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
* \author Josh Lurz, Sonny Kim
*/
void Subsector::postCalc( const int aPeriod ){
    // Finalize base technologies.
    for( BaseTechIterator baseTech = baseTechs.begin(); baseTech != baseTechs.end(); ++baseTech ){
        (*baseTech)->postCalc( regionName, sectorName, aPeriod );
    }

    // Finalize all technologies in all periods.
    for( unsigned int i = 0; i < techs.size(); ++i ){
        for( unsigned int j = 0; j < techs[ i ].size(); ++j ){
            techs[ i ][ j ]->postCalc( regionName, aPeriod );
        }
    }

    // Do only when calibration is on and for calibration periods only.
    // Normalize technology share weights after model has solved and technology outputs
    // are known.
    if( Configuration::getInstance()->getBool( "CalibrationActive" ) &&
        aPeriod <= scenario->getModeltime()->getFinalCalibrationPeriod() ){
            normalizeTechShareWeights( aPeriod );
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
            // Loop through all periods. techs[i].size() happens to be max period.
            for( int per = 0; per < modeltime->getmaxper(); ++per ){
                // For each and every vintaged technology, use the last period for
                // the accept() arguement so that children of technology for all periods
                // can be visited.
                techs[ i ][ per ]->accept( aVisitor, modeltime->getmaxper() - 1 );
            }
        }
    }
    else {
        for( unsigned int i = 0; i < techs.size(); ++i ){
            for( int j = 0; j <= period; ++j ){
                techs[ i ][ j ]->accept( aVisitor, period );
            }
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
