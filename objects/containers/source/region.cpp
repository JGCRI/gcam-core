/*! 
* \file region.cpp
* \ingroup CIAM
* \brief The Region class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>
#include <algorithm>

#include "containers/include/region.h"
#include "containers/include/gdp.h"
#include "util/base/include/summary.h"
#include "sectors/include/sector.h"
#include "sectors/include/supply_sector.h"
#include "sectors/include/demand_sector.h"
#include "sectors/include/tran_sector.h"
#include "resources/include/resource.h"
#include "sectors/include/ag_sector.h"
#include "demographics/include/population.h"
#include "emissions/include/ghg_policy.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "emissions/include/indirect_emiss_coef.h"
#include "containers/include/world.h"
#include "util/base/include/model_time.h" 
#include "marketplace/include/marketplace.h"
#include "util/base/include/configuration.h"
#include "util/base/include/util.h"
#include "util/logger/include/logger.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/xy_data_point.h"
#include "util/curves/include/point_set.h"
#include "util/curves/include/explicit_point_set.h"

using namespace std;
using namespace xercesc;

extern ofstream logfile;

extern Scenario* scenario;
// static initialize.
const string Region::XML_NAME = "region";

//! Default constructor
Region::Region() {
    // Resize all vectors to maximum period
    const int maxper = scenario->getModeltime()->getmaxper();
    TFEcalb.resize( maxper ); // Total Final Energy calibration value
    priceSer.resize( maxper ); // aggregate price for demand services
    carbonTaxPaid.resize( maxper ); // total regional carbon taxes paid
    summary.resize( maxper ); // summary object for reporting
    calibrationGDPs.resize( maxper ); // GDPs for calibration
}

//! Default destructor destroys sector, demsector, Resource, agSector, and population objects.
Region::~Region() {
    clear();
}

//! Clear member variables and initialize elemental members.
void Region::clear(){
    for ( SupplySectorIterator secIter = supplySector.begin(); secIter != supplySector.end(); secIter++ ) {
        delete *secIter;
    }

    for ( DemandSectorIterator demIter = demandSector.begin(); demIter != demandSector.end(); demIter++ ) {
        delete *demIter;
    }

    for ( ResourceIterator rescIter = resources.begin(); rescIter != resources.end(); rescIter++ ) {
        delete *rescIter;
    }
}

/*! Return the region name.
* \return The string name of the region is returned.
*/
string Region::getName() const {
    return name;
}

/*! 
* \brief Sets the data members from the XML input.  This function parses all XML data from Region down to the lowest set of objects.
*  As the XML data is parsed, new objects are continually added to the object container using the push_back routine.
*
* \param node XML DOM node of the region
* \todo Change the diagnosic "assert( node );" to fail with a more informative error (file, previous node?, location?)
*/
void Region::XMLParse( const DOMNode* node ){
    // make sure we were passed a valid node.
    assert( node );

    // get the name attribute.
    name = XMLHelper<string>::getAttrString( node, "name" );

#if ( _DEBUG )
    cout << "Region name set as " << name << endl;
#endif

    // get all child nodes.
    DOMNodeList* nodeList = node->getChildNodes();
    string nodeName;

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
        else if( nodeName == "PrimaryFuelCO2Coef" ) {
            primaryFuelCO2Coef[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "CarbonTaxFuelCoef" ) {
            carbonTaxFuelCoef[ XMLHelper<string>::getAttrString( curr, "name" ) ] = XMLHelper<double>::getValue( curr );
        }
		else if( nodeName == Population::getXMLNameStatic() ){
            if( !population.get() ){
                population.reset( new Population() );
            }
            population->XMLParse( curr ); // only one demographics object.
        }
		else if( nodeName == GDP::getXMLNameStatic() ){
            if( !gdp.get() ){
                gdp.reset( new GDP() );
            }
            gdp->XMLParse( curr );
        }
		else if( nodeName == DepletableResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new DepletableResource() );
        }
		else if( nodeName == FixedResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new FixedResource() );
        }
		else if( nodeName == RenewableResource::getXMLNameStatic() ){
            parseContainerNode( curr, resources, resourceNameMap, new RenewableResource() );
        }
		else if( nodeName == SupplySector::getXMLNameStatic() ){
            parseContainerNode( curr, supplySector, supplySectorNameMap, new SupplySector( name ) );
        }
		else if( nodeName == DemandSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new DemandSector( name ) );
        }
        // transportation sector is contained in demandSector
		else if( nodeName == TranSector::getXMLNameStatic() ){
            parseContainerNode( curr, demandSector, demandSectorNameMap, new TranSector( name ) );
        } 
        else if( nodeName == AgSector::getXMLNameStatic() ) {
            if( Configuration::getInstance()->getBool( "agSectorActive" ) ){
                if( !agSector.get() ){
                    agSector.reset( new AgSector() );
                }
                agSector->XMLParse( curr );
            }
        }
        else if( nodeName == GHGPolicy::getXMLNameStatic() ){
            parseContainerNode( curr, ghgMarket, ghgMarketNameMap, new GHGPolicy() );
        }
        // regional economic data
        else if( nodeName == "calibrationdata" ){
            // get all child nodes.
            DOMNodeList* nodeListChild = curr->getChildNodes();
            // loop through the child nodes.
            string nodeNameChild;
            for( unsigned int j = 0; j < nodeListChild->getLength(); j++ ){
                DOMNode* currChild = nodeListChild->item( j );
                nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );
                const Modeltime* modeltime = scenario->getModeltime();

                if( nodeNameChild == "#text" ) {
                    continue;
                }
                else if( nodeNameChild == "GDPcal" ) { // TODO: MOVE TO GDP
                    XMLHelper<double>::insertValueIntoVector( currChild, calibrationGDPs, modeltime );
                }

                else if(nodeNameChild == "TFEcalb") {
                    XMLHelper<double>::insertValueIntoVector( currChild, TFEcalb, modeltime );
                }
                else {
                    cout << "Unrecognized text string: " << nodeNameChild << " found while parsing region->calibrationdata." << endl;
                }
            }
        }
         // A list representing the correct order in which to calculate the sectors. 
        else if( nodeName == "SectorOrderList" ){
            // get all child nodes.
            string nodeNameChild;
            DOMNodeList* nodeListChild = curr->getChildNodes();
            // loop through the child nodes.
            for( unsigned int j = 0; j < nodeListChild->getLength(); j++ ){
                DOMNode* currChild = nodeListChild->item( j );
                nodeNameChild = XMLHelper<string>::safeTranscode( currChild->getNodeName() );

                if( nodeNameChild == "#text" ) {
                    continue;
                }
                else if( nodeNameChild == "SectorName" ){
                    sectorOrderList.push_back( XMLHelper<string>::getAttrString( currChild, "name" ) );
                }
                else {
                    cout << "Unrecognized text string: " << nodeNameChild << " found while parsing region->SectorOrderList." << endl;
                }
            }
        }
        else {
            cout << "Unrecognized text string: " << nodeName << " found while parsing region." << endl;
        }
    }
}

/*! Complete the initialization.  Get the size of vectors, initialize AGLU, create all markets, call complete initialization 
*  functions for nested objects, update the fuel map, and find simultaneities.
* \todo I think since there is one indirect ghg object for each sector, it might be better in sector. This may require deriving supply sector.
*/
void Region::completeInit() {
    Configuration* conf = Configuration::getInstance();
    
    // Initialize the GDP
    gdp->initData( population.get() );
    
    for( SupplySectorIterator supplySectorIter = supplySector.begin(); supplySectorIter != supplySector.end(); ++supplySectorIter ) {
        ( *supplySectorIter )->completeInit();
        (*supplySectorIter)->setMarket();
        Emcoef_ind temp( ( *supplySectorIter )->getName() );
        emcoefInd.push_back( temp );
    }

    if( conf->getBool( "agSectorActive" ) ){
        agSector->setGNP( calcFutureGDP() );
        agSector->setPop( population->getTotalPopVec() );
        agSector->setMarket( name );
    }
    for( ResourceIterator resourceIter = resources.begin(); resourceIter != resources.end(); ++resourceIter ) {
        (*resourceIter)->completeInit();
        (*resourceIter)->setMarket( name );
    }

    for( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ) {
        (*currSector)->completeInit();
    }

    for( GHGPolicyIterator ghgPolicy = ghgMarket.begin(); ghgPolicy != ghgMarket.end(); ++ghgPolicy ){
        (*ghgPolicy)->setMarket( name );
    }

    /*
    if( conf->getBool( "CalibrationActive" ) ){
        gdp->setupCalibrationMarkets( name );
    }
    */
    // Find simuls.
    updateSummary( 0 );	// Dummy call to final supply to setup fuel map
    findSimul( 0 );

    // Set sector ordering via read-in list (sectorOrderList).
    if( !sectorOrderList.empty() ){
        reorderSectors( sectorOrderList );
    }
    else { // Otherwise use the built-in sort routine.
        // Setup each sector for sorting. 
        for( SupplySectorIterator iter = supplySector.begin(); iter!= supplySector.end(); ++iter ){
            (*iter)->setupForSort( this );
        }
        sortSectorsByDependency();
    }
}

/*! \brief Initialize the calibration markets. */
void Region::setupCalibrationMarkets() {
    if( Configuration::getInstance()->getBool( "CalibrationActive" ) ){
        gdp->setupCalibrationMarkets( name );
    }
}

/*! \brief Reorder the sectors based on a read in list of sector names. 
* \details This function is used to reorder a list of sectors based on a user-supplied
* ordering. In the partial-equilibrium model, the ordering of the supply sectors is critical
* to the correct solving of the model. This method allows the user to order the list of 
* sectors in a specific way. This function handles errors as follows: 
* <ul><li>If a read-in sector is not specified in the list, the function will issue 
* a warning and remove the sector. </li>
* <li>If a sector name in the ordering list is not an existing sector in the model, 
* a warning will be issued and the sector will be skipped. </li></ul>
* \param orderList A list of sector names in the order in which the sectors should be put. 
* \return Whether the reordering was completed without any errors. 
*/
bool Region::reorderSectors( const vector<string>& orderList ){
    bool success = true;
    // Create temporary copy of the sectors and the sector name map.
    vector<SupplySector*> originalOrder = supplySector;
    map<string,int> originalNameMap = supplySectorNameMap;
    
    // Clear the list of sectors and sectorNames. 
    supplySector.clear();
    supplySectorNameMap.clear();

    // Loop through the sector order vector. 
    typedef vector<string>::const_iterator NameIterator;
    typedef map<string,int>::iterator NameMapIterator;

    for( NameIterator currSectorName = orderList.begin(); currSectorName != orderList.end(); ++currSectorName ){
        NameMapIterator origSectorPosition = originalNameMap.find( *currSectorName );

        // Check if the sector name in the sector orderling list exists currently.
        if( origSectorPosition != originalNameMap.end() ){
            // Assign the sector.
            supplySector.push_back( originalOrder[ origSectorPosition->second ] );
            supplySectorNameMap[ *currSectorName ] = static_cast<int>( supplySector.size() - 1 );

            // Remove the original sector mapping. This will allow us to clean up sectors that were 
            // not assigned a new ordering. This also is more efficient as there are less entries to search.
            originalNameMap.erase( origSectorPosition );
        }
        else {
            success = false;
            cout << "Error: " << *currSectorName << " is not the name of an existing sector. " << endl;
            cout << "It will not be included in the sector ordering." << endl;
        } // end else
    } // end for.

    // Check if there are any unassigned sectors and remove them.
    for( NameMapIterator currSecName = originalNameMap.begin(); currSecName != originalNameMap.end(); ++currSecName ){
        success = false;
        cout << "Error: " << currSecName->first << " was not assigned a position in the explicit sector ordering list." << endl;
        cout << "This sector will be removed from the model." << endl;
        // This sector is not in the new list, so free its memory. 
        delete originalOrder[ currSecName->second ];
    }
    return success;
}

/*! \brief Sort the sectors based on their dependencies defined as sectors that have output
* which the current sector requires as input. 
* \details This function is used to reorder the sectors dynamically based on their 
* input dependencies. Simultinaties are ignored as the ordering is already resolved.
* In the partial-equilibrium model, the ordering of the supply sectors is critical
* to the correct solving of the model.
* \pre setupForSort() must have been called on all sectors so that the comparison operator
* can work.
* \return Whether the sorting was completed without any errors.
* \todo Currently this routine may be able to go into an infinite loop for certain unsortable sector orderings.
* It would be possible to design a check which searched for input loops that would cause this. 
*/
bool Region::sortSectorsByDependency() {
    Sector::DependencyOrdering orderingOperator;
    bool success = true;

    // Loop through each position in the vector except the last.
    for( SupplySectorIterator outerPosition = supplySector.begin(); outerPosition != supplySector.end() - 1; ++outerPosition ){
        // Compare this position with every other. 
        for( SupplySectorIterator innerPosition = outerPosition + 1; innerPosition != supplySector.end(); ++innerPosition ){
            // Check if the outer position sector depends on the inner position sector, and so should 
            // be after the innerposition sector.
            if( orderingOperator( *innerPosition, *outerPosition ) ){
                // Create a temporary copy of the pointer to the sector which will be moved.
                SupplySector* sectorToMove = *innerPosition;

                // Remove the vector spot for the innerPosition
                supplySector.erase( innerPosition );

                // Add the value that was at the innerPosition iterator before the outerPosition in the vector.
                // Reset outerPosition to this value.
                outerPosition = supplySector.insert( outerPosition, sectorToMove );

                // Set the innerposition iterator, skipping over the sector we just 
                // moved in front of. This might be able to be optimized by 
                // moving the iterator further. 
                innerPosition = outerPosition + 2;
            }
        }
    }

    // Perform an extra looping over all sectors to check that all dependency ordering is correct.
    if( Configuration::getInstance()->getBool( "debugChecking", false ) ){
        // C++ Note: This && operator ensures that if success was set to false previously in this
        // function, that it will retain its false value. 
        success = success && isRegionOrderedCorrectly();
    }

    // Reset the map. Could this be a function somewhere? xml_helper or util?
    supplySectorNameMap.clear();
    for( unsigned int i = 0; i < supplySector.size(); ++i ){
        supplySectorNameMap[ supplySector[ i ]->getName() ] = i;
    }

    // Return success code. 
    return success;
}
/*! \brief Function to check whether the sectors are properly ordered.
* \details This function loops through the sectors comparing a sector 
* to all sectors which follow it in the ordering. If a sector is before
* a sector it depends on, as defined as the other sector takes the current sector
* as an input, then the function will report an error and set the return code to false.
* \pre setupForSort() must have been called on all sectors so that the comparison operator
* can work.
* \return Whether the sectors are properly ordered.
*/
bool Region::isRegionOrderedCorrectly() const { 
    // Declare an instance of the sort operator. 
    Sector::DependencyOrdering orderingOperator;
    
    // Boolean marking whether the sectors are correctly ordered. 
    bool isOrderedCorrectly = true;

    // Loop through all sectors except the last one as there is nothing to compare the last to.
    for( CSupplySectorIterator outerPosition = supplySector.begin(); outerPosition != supplySector.end() - 1; ++outerPosition ){
        // Compare the sector to all other following sectors.
        for( CSupplySectorIterator innerPosition = outerPosition + 1; innerPosition != supplySector.end(); ++innerPosition ){
            // Check if the outer position sector depends on the inner position sector, and so should 
            // be before the outer position sector.
            if( orderingOperator( *innerPosition, *outerPosition ) ){
                // The sectors are ordered incorrectly. Do not early return so that more than one
                // error statement can be printed, one for each bad ordering. 
                isOrderedCorrectly = false;
                // Add log out here.
                cout << "Error: " << ( *innerPosition )->getName() << " should be before " << ( *outerPosition )->getName() << endl;
            } // end if
        } // end inner for loop
    } // end outer for loop
    return isOrderedCorrectly;
}

/*! 
* \brief Write datamembers to datastream in XML format. Calls XMLWriteElement function from the XMLHelper class for the actual writing.
* \param out Output file in XML format.
* \note 
* \ref faqitem1 
*/
void Region::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, tabs, 0, coefPriIter->first );
    }
    // write the xml for the class members.
    // write out the single population object.
	if( population.get() ){ // Check if population object exists
		population->toInputXML( out, tabs );
	}
	if( gdp.get() ){ // Check if gdp object exists
		gdp->toInputXML( out, tabs );
	}
    // write out the resources objects.
    for( CResourceIterator i = resources.begin(); i != resources.end(); i++ ){
        ( *i )->toInputXML( out, tabs );
    }

    // write out supply sector objects.
    for( CSupplySectorIterator j = supplySector.begin(); j != supplySector.end(); j++ ){
        ( *j )->toInputXML( out, tabs );
    }

    // write out demand sector objects.
    for( CDemandSectorIterator k = demandSector.begin(); k != demandSector.end(); k++ ){
        ( *k )->toInputXML( out, tabs );
    }

    if( agSector.get() ){
        agSector->toInputXML( out, tabs );
    }
	else {
		tabs->writeTabs( out );
		out << "<agsector/>" << endl;
	}
    // write out ghgMarket objects.
    for( CGHGPolicyIterator l = ghgMarket.begin(); l != ghgMarket.end(); l++ ){
        ( *l )->toInputXML( out, tabs );
    }
    
    // Note: The count function is an STL algorithm that counts the number of times a value occurs
    // within the a range of a container. The first two arguments to the function are the range of the 
    // container to search, the third is the value to search for.
    if( ( count( calibrationGDPs.begin(), calibrationGDPs.end(), 0 ) != static_cast<int>( calibrationGDPs.size() ) ) 
        || ( count( TFEcalb.begin(), TFEcalb.end(), 0 ) != static_cast<int>( TFEcalb.size() ) ) ){ // makes sure tags aren't printed if no real data

            // Write out regional economic data
            XMLWriteOpeningTag( "calibrationdata", out, tabs ); 

            // write out calibration GDP
            const Modeltime* modeltime = scenario->getModeltime();
            for( unsigned int m = 0; m < calibrationGDPs.size(); m++ ){
                XMLWriteElementCheckDefault( calibrationGDPs[ m ], "GDPcal", out, tabs, 0.0, modeltime->getper_to_yr( m ) );
            }

            // write out TFE calibration values
            for( unsigned int m = 0; m < TFEcalb.size(); m++ ) {
                XMLWriteElementCheckDefault( TFEcalb[ m ],"TFEcalb", out, tabs, 0.0, modeltime->getper_to_yr( m ) );
            }
            XMLWriteClosingTag( "calibrationdata", out, tabs );
            // End write out regional economic data
        } // close calibration IF
        // Write out the sector ordering.
        if( !sectorOrderList.empty() ){
            XMLWriteOpeningTag( "SectorOrderList", out, tabs );
            for( unsigned int m = 0; m < sectorOrderList.size(); m++ ){
                XMLWriteElement( "", "SectorName", out, tabs, 0, sectorOrderList[ m ] );
            }
            XMLWriteClosingTag( "SectorOrderList", out, tabs );
        }

        // finished writing xml for the class members.
        XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Write datamembers to datastream in XML format for debugging purposes.  
* Calls XMLWriteElement function from the XMLHelper class for the actual writing.
*  Calls debug functions in other contained objects. 
*
* \param period Model time period
* \param out Output file for debugging purposes in XML format
*
*/
void Region::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
	XMLWriteOpeningTag ( getXMLName(), out, tabs, name );

    // write out basic datamembers
    XMLWriteElement( static_cast<unsigned int>( ghgMarket.size() ), "noGhg", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( resources.size() ), "numResources", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( supplySector.size() ), "noSSec", out, tabs );
    XMLWriteElement( static_cast<unsigned int>( demandSector.size() ), "noDSec", out, tabs );
    XMLWriteElement( calibrationGDPs[ period ], "calibrationGDPs", out, tabs );
    XMLWriteElement( priceSer[ period ], "priceSer", out, tabs );
    XMLWriteElement( carbonTaxPaid[ period ], "carbonTaxPaid", out, tabs );

    // Write out the Co2 Coefficients. 
    for( map<string,double>::const_iterator coefAllIter = primaryFuelCO2Coef.begin(); coefAllIter != primaryFuelCO2Coef.end(); coefAllIter++ ) {
        XMLWriteElement( coefAllIter->second, "PrimaryFuelCO2Coef", out, tabs, 0, coefAllIter->first );
    }

    for( map<string,double>::const_iterator coefPriIter = carbonTaxFuelCoef.begin(); coefPriIter != carbonTaxFuelCoef.end(); coefPriIter++ ) {
        XMLWriteElement( coefPriIter->second, "CarbonTaxFuelCoef", out, tabs, 0, coefPriIter->first );
    }
    // write the xml for the class members.
    // write out the single population object.
    if( population.get() ){
        population->toDebugXML( period, out, tabs );
    }
    if( gdp.get() ){
        gdp->toDebugXML( period, out, tabs );
    }
    // write out the resources objects.
    for( CResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->toDebugXML( period, out, tabs );
    }

    // write out supply sector objects.
    for( CSupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->toDebugXML( period, out, tabs );
    }

    // write out demand sector objects.
    for( CDemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->toDebugXML( period, out, tabs );
    }

    // Write out the single agSector object.
    // agSector->toDebugXML( period, out );

    // write out ghgMarket objects.
    for( CGHGPolicyIterator currPolicy = ghgMarket.begin(); currPolicy != ghgMarket.end(); ++currPolicy ){
        (*currPolicy)->toDebugXML( period, out, tabs );
    }

    // Write out summary object.
    //summary[ period ].toDebugXML( period, out ); // is this vector by period?
    // Write out the sector ordering.
    XMLWriteOpeningTag( "SectorOrderList", out, tabs );
    for( unsigned int m = 0; m < sectorOrderList.size(); m++ ){
        XMLWriteElement( "", "SectorName", out, tabs, 0, sectorOrderList[ m ] );
    }
    XMLWriteClosingTag( "SectorOrderList", out, tabs );
	// Finished writing xml for the class members.

	XMLWriteClosingTag( getXMLName(), out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& Region::getXMLName() const {
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
const std::string& Region::getXMLNameStatic() {
	return XML_NAME;
}

//! Calculate the region.
void Region::calc( const int period, const bool doCalibrations ) {
    // Store configuration variables locally as statics.
    static const Configuration* conf = Configuration::getInstance();
    static const bool agSectorActive = conf->getBool( "agSectorActive" );
    static const bool calibrationActive = conf->getBool( "CalibrationActive" );
    
    // Write back calibrated values to the member variables.
    // These are still trial values.
    if( calibrationActive ) {
       gdp->writeBackCalibratedValues( name, period );
    }
    // calculate regional GDP
    calcGDP( period );
    // set regional GHG constraint to market supply
    setGhgSupply( period );
    // determine supply of primary resources
    calcResourceSupply( period );
    // determine prices of refined fuels and electricity
    calcFinalSupplyPrice( period );
    // calculate enduse service price
    calcEndUsePrice( period );
    // adjust GDP for energy cost changes
    adjustGDP( period );

    // determine end-use demand for energy and other goods
    calcEndUseDemand( period );

    // determine supply of final energy and other goods based on demand
    setFinalSupply( period );

    if( agSectorActive ){
        calcAgSector( period );
    }

    // Perform calibrations
    if( calibrationActive ) {
        calibrateRegion( doCalibrations, period );
    }
}

/*! Run the agLu Model and determine CO2 emitted.
* \param period Model time period
*/
void Region::calcAgSector( const int period ) {
    agSector->runModel( period, name );
    agSector->carbLand( period, name );
}

/*! \brief Set regional ghg constraint from input data to market supply.
*
* \param period Model time period
*/
void Region::setGhgSupply( const int period ) {
    for ( unsigned int i = 0; i < ghgMarket.size(); i++ ) {
        ghgMarket[i]->addGHGSupply( name, period );
    }
}

/*! Calculates annual supply of primay resources.
*
* \param period Model time period
*/
void Region::calcResourceSupply( const int period ){
    for( ResourceIterator currResource = resources.begin(); currResource != resources.end(); ++currResource ){
        (*currResource)->calcSupply( name, gdp.get(), period );
    }
}

/*! Calculate prices of refined fuels and electricity.
*
* \param period Model time period
*/
void Region::calcFinalSupplyPrice( const int period ) {
    for( SupplySectorIterator currSupply = supplySector.begin(); currSupply != supplySector.end(); ++currSupply ){
        (*currSupply)->calcFinalSupplyPrice( gdp.get(), period );
    }
}

/*! Calculates supply of final energy and other goods.
*
* \param period Model time period
*/
void Region::setFinalSupply( const int period ) {
    // loop through all sectors in reverse once to get total output.
    typedef  vector<SupplySector*>::reverse_iterator ReverseSupplySectorIterator;
    for ( ReverseSupplySectorIterator currSupply = supplySector.rbegin(); currSupply != supplySector.rend(); ++currSupply ) {
        (*currSupply)->supply( period, gdp.get() );
    }

    // loop through supply sectors and assign supplies to marketplace.
    for( SupplySectorIterator currSupply = supplySector.begin(); currSupply != supplySector.end(); ++currSupply ){
        (*currSupply)->setFinalSupply( period );
    }
}

/*! Calculate initial gdp value (without feedbacks) 
*
* \param period Model time period
*/
void Region::calcGDP( const int period ) {
	 gdp->initialGDPcalc( period, population->getTotal( period ) );
}

/*! Calculate forward-looking gdp (without feedbacks) for AgLU use
* It is necessary to have a gdp without feedbacks so that all values are known and AgLU can calibrate
*
* This routine runs through each period and calculates a series of gdp values
* without use of the energy price feedback.
*
* \author Steve Smith, Josh Lurz
* \param period Model time period
* \warning this will interfere with the normal gdp calculation if this is used after model calc starts
* \todo check to see if this works with AgLU. Not sure about conversions.
*/
const vector<double> Region::calcFutureGDP() const {
	const Modeltime* modeltime = scenario->getModeltime();
	vector<double> gdps;
	gdps.resize( modeltime->getmaxper() );
   
	for ( int period = 0; period < modeltime->getmaxper(); period++ ) {
		gdp->initialGDPcalc( period, population->getTotal( period ) );
		gdps[ period ] = gdp->getApproxScaledGDPperCap( period );
	}
	return gdps;
}

/*! Calculate demand sector aggregate price.
*
* \param period Model time period
*/
void Region::calcEndUsePrice( const int period ) {

    priceSer[ period ] = 0;

    for ( DemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ) {
        (*currDemSector)->calcShare( period, gdp.get() );		

        // calculate service price for each demand sector

        // calculate aggregate service price for region
        priceSer[ period ] += (*currDemSector)->getOutput( 0 ) * (*currDemSector)->getPrice( period );

        // calculate service price elasticity for each demand sector
        // or use read in value, temporary code
        // Note: If this is set to false the model solves better generally. -JPL
        bool useReadinData = true;
        // do nothing if false
        if (!useReadinData) {
            (*currDemSector)->calc_pElasticity( period );
        } 
    }
}

/*! Adjust regional gdp for energy.
*
* \param period Model time period
* \todo Move this calculation down to GDP
*/
void Region::adjustGDP( const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();

	double tempratio = 1;
	if ( period > modeltime->getyr_to_per(1990) ) {
		tempratio = priceSer[period]/priceSer[period-1];
	}

	 gdp->adjustGDP( period, tempratio );
}

//! Do regional calibration
/*! Must be done after demands are calculated. 
Two levels of calibration are possible. 
First at the sector or technology level (via. calibrateSector method),
or, at the level of total final energy demand (via calibrateTFE)
*
* \param doCalibrations Boolean for running or not running calibration routine
* \param period Model time period
*/
void Region::calibrateRegion( const bool doCalibrations, const int period ) {
    // Do subsector and technology level energy calibration
    // can only turn off calibrations that do not involve markets
    if ( doCalibrations ) {
        // Calibrate demand sectors
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            demandSector[ i ]->calibrateSector( period );
        }

        // Calibrate supply sectors
        for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
            supplySector[ i ]->calibrateSector( period );
        }
        // Calibrate Regional TFE
        if ( !isDemandAllCalibrated( period ) ) {
            calibrateTFE( period );
        } else {
            // do nothing now. Need to make a variant of the total cal outputs function
            // so that can compare TFE with cal value 
        }
    }

    // Set up the GDP calibration. Need to do it each time b/c of nullsup call in marketplace.
    // Insert the newly calculated values into the calibration markets. 
    if( static_cast<int>( calibrationGDPs.size() ) > period && calibrationGDPs[ period ] > 0 ){ 
        const string goodName = "GDP";
        Marketplace* marketplace = scenario->getMarketplace();
        marketplace->addToDemand( goodName, name, calibrationGDPs[ period ], period );
        marketplace->addToSupply( goodName, name, gdp->getGDP( period ), period );
        if( period > 0 ){
            marketplace->setMarketToSolve( goodName, name );
        }
    }
}

/*! Returns true if all demand sectors are calibrated (or fixed)
*
* \param period Model time period
*/
bool Region::isDemandAllCalibrated( const int period ) const {
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        if ( !demandSector[ i ]->outputsAllFixed( period ) ) {
            return false;
        }
    }
    return true;
}

//! Calibrate total final energy Demand for this region.
/*! Adjusts AEEI in each demand sector until TFE is equal to the calibration value.
*/
void Region::calibrateTFE( const int period ) {
    // Calculate total final energy demand for all demand sectors
    double totalFinalEnergy = 0;
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        totalFinalEnergy += demandSector[ i ]->getInput( period );;
    }

    // Don't calibrate unless non zero value of TFE
    if ( TFEcalb[ period ]  > 0 ) {
        // Ratio of TFE in sector to cal value
        double scaleFactor = TFEcalb[ period ] / totalFinalEnergy;

        if ( totalFinalEnergy == 0 ) {
            cout << "ERROR: totalFinalEnergy = 0 in region " << name << endl;
        }

        //   cout << name << ":  TFE Calib: " << TFEcalb[ period ] << "; TFE: " << totalFinalEnergy << endl;

        // Scale each sector's output to approach calibration value
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
            if ( !demandSector[ i ]->outputsAllFixed( period ) ) {
                demandSector[ i ]->scaleOutput( period , scaleFactor );
            }
        }
    }
}

/*! \brief Perform checks on consistancy of the input data.
*
* At present this checks constancy of calibrated supply and demand sectors.
*
* \author Steve Smith
* \param period Model period
*/
void Region::checkData( const int period ) {
	 adjustCalibrations( period );	 
}

/*! \brief Call any initializations that are only done once per period.
*
* This simply calls the initcalc functions of the supply and demand sectors.
*
* \author Steve Smith
* \param period Model period
*/
void Region::initCalc( const int period ) 
{
    for( SupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->initCalc( period );
    }
    for ( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ) {
        (*currSector)->initCalc( period ); 
    }
    // Make sure TFE is same as calibrated values
    if ( isDemandAllCalibrated( period ) ) {
      double totalFinalEnergy = 0;
      for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        totalFinalEnergy += demandSector[ i ]->getFixedInputs( period, "allInputs" );
      }
      if ( TFEcalb[ period ] != 0 ) {
         double scaleFactor = totalFinalEnergy / TFEcalb[ period ];
         if ( abs( scaleFactor - 1 ) > util::getSmallNumber() ) {
            logfile << "TFE in region " << name << " scaled by: "<< scaleFactor << endl;
            TFEcalb[ period ] = totalFinalEnergy; 
         }
      }
    } 

}

/*! \brief Adjusts calibrated demands to be consistant with calibrated supply.
*
* For each supply sector that is completely calibrated, this routine adjusts calibrated demands, 
* if they are all calibrated, to exactly match supplies. 
* The adjustments are written to the log file.
*
* If a calibration value is missing for a subsector or technology then the calibration check will not run and warnings may appear
* 
* \author Steve Smith
* \param period Model period
* \warning the current version cheats and assumes sub-sector names are equal to the supply sector fuel names.
* \todo need to impliment "calonly" runmode for world.calc() so that can look at full chain of supplies and demands, working through the sector order.
*/
void Region::adjustCalibrations( const int period ) {
   Configuration* conf = Configuration::getInstance();
   bool debugChecking = conf->getBool( "debugChecking" );

    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
      string goodName = supplySector[ i ]->getName();
      if ( inputsAllFixed( period, goodName ) ) {
         
         // First find the total calibrated or fixed supply of this good         
         double calSupply = supplySector[ i ]->getCalOutput( period );  // total calibrated output
         calSupply += supplySector[ i ]->getFixedOutput( period );  // total fixed output
            
         // now find total fixed inputs demanded for this good
         double calDemand = getFixedDemand( period, goodName );
            
         // if calibrated output and demand are not equal, then scale demand so that they match
         if ( !util::isEqual( calSupply, calDemand ) && ( calDemand != 0 ) && supplySector[ i ]->outputsAllFixed( period ) ) {
            logfile << "Inputs and Outputs all fixed for " << goodName << endl;
            
            logfile << "Cal difference in region " << name << " sector: " << goodName;
            logfile << " Supply: " << calSupply << " S-D: " << calSupply-calDemand;
            logfile << " ("<<(calSupply-calDemand)*100/calSupply<<"%)"<<endl;

            // Get calibrated inputs, only scale those, not fixed demands (if any)
            double fixedCalInputs = 0;
            for ( unsigned int j = 0; j < demandSector.size(); j++ ) {
               fixedCalInputs += demandSector[ j ]->getFixedInputs( period, goodName, false ); 
            }
            
            double ScaleValue = 1 + ( calSupply-calDemand )/fixedCalInputs;
            for ( unsigned int j = 0; j < demandSector.size(); j++ ) {
               demandSector[ j ]->scaleCalibratedValues( period, goodName, ScaleValue ); 
            }
         } else {
            if ( calDemand != 0 ) {
               logfile << ", ****Outputs are NOT all fixed." << endl;
            } else {
               logfile << ", fixed demand is zero or indirect." << endl;
            }
         }
         
         calDemand = getFixedDemand( period, goodName ); // get new demand to check if ok
         // if calibrated demand is less than calibrated supply, even if supply is not all calibrated, issue warning
         // This would cause a problem if not all supply was calibrated, but what was calibrated was > calibrated demand
          // If debugchecking flag is on extra information is printed
         if ( ( calSupply - calDemand ) > util::getSmallNumber() ) {
            logfile << "WARNING: Calibrated Demand < Fixed Supply in Region " << name << " sector: " << goodName << endl;
            cout << "WARNING: Calibrated Demand < Fixed Supply in Region " << name << " sector: " << goodName << endl;
            if ( debugChecking ) {
               cout << "   supply all fixed values : " << supplySector[ i ]->outputsAllFixed( period ) << endl;
               cout << "   demand all fixed : " << inputsAllFixed( period, goodName ) << endl;
               cout << "   fixedDemandsTot: " << calDemand << "  "; calDemand = getFixedDemand( period, goodName, true ); cout << endl;
               cout << "   fixedSupplyTot: " << calSupply<< "  "; 
               calSupply = supplySector[ i ]->getFixedOutput( period , true ); cout << endl;
            }
         }
      } // if allfixed
      else {   // if not all fixed
         if ( supplySector[ i ]->outputsAllFixed( period ) ) {
         logfile << "Inputs NOT ALL fixed for " << goodName << ", but supplies ARE all fixed" << endl;
         }
      }
    } // for block
}

/*! \brief Returns true if all inputs for the selected good are fixed.
*
* Fixed inputs can be by either fixedCapacity, calibration, or zero share
*
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return inputs for
*/
bool Region::inputsAllFixed( const int period, const std::string& goodName ) const {
   
   for ( unsigned int j = 0; j < demandSector.size(); j++ ) {
      if ( !demandSector[ j ]->inputsAllFixed( period, goodName ) ) {
         return false; 
      }
   }
   return true;
}

/*! \brief Returns total fixed demand for the specified good
*
* Can be either calibrated demand or fixed input
*
* \author Steve Smith
* \param period Model period
* \param goodName market good to return demand for
* \param printValues Optional toggle to print out each value for debugging (default false)
*/
double Region::getFixedDemand( const int period, const std::string& goodName, bool printValues ) {
   double calDemand = 0;
   for ( unsigned int j = 0; j < demandSector.size(); j++ ) {
      calDemand += demandSector[ j ]->getFixedInputs( period, goodName ); 
      if ( printValues ) { cout << "dsec["<<j<<"] "<< demandSector[ j ]->getFixedInputs( period, goodName ) << ", "; }
   }
   return calDemand;
}

//! Calculate regional demand for energy and other goods for all sectors.
void Region::calcEndUseDemand( const int period ) {
    for ( DemandSectorIterator currDemSector = demandSector.begin(); currDemSector != demandSector.end(); ++currDemSector ){
        // calculate aggregate demand for end-use sector services
        // set fuel demand from aggregate demand for services
        (*currDemSector)->aggdemand( gdp.get(), period );
    }
    
}

//! Calculate regional emissions from resources.
void Region::calcEmissions( const int period ) {
    summary[period].clearemiss(); // clear emissions map

    // need to call emissions function but sum is not needed
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->emission(period);
        summary[period].updateemiss(supplySector[i]->getemission(period));
        emcoefInd[i].setemcoef(supplySector[i]->getemfuelmap(period), 
            supplySector[i]->getOutput(period));
    }
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->emission(period);
        summary[period].updateemiss(demandSector[i]->getemission(period));
    }
}

/*! \brief Calculate regional emissions by fuel for reporting
\warning This function assumes emission has already been called, as this function cannot clear the summary emissions.-JPL */
void Region::calcEmissFuel( const int period )
{
    map<string, double> fuelemiss; // tempory emissions by fuel
    const vector<string> primaryFuelList = scenario->getWorld()->getPrimaryFuelList();

    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        fuelemiss[ *fuelIter ] = summary[period].get_pemap_second( *fuelIter ) * primaryFuelCO2Coef[ *fuelIter ];
    }

    summary[period].updateemiss(fuelemiss); // add CO2 emissions by fuel
}

//! Calculate regional indirect emissions from intermediate and final demand sectors.
void Region::emissionInd( const int period ){
    for( SupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        (*currSector)->indemission( period, emcoefInd );
    }
    for( DemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        (*currSector)->indemission( period, emcoefInd );
    }
}

//! Calculate total carbon tax paid in the region by all supply and demand sectors.
void Region::calcTotalCarbonTaxPaid( const int period ) {
    carbonTaxPaid[ period ] = 0; // initialize total regional carbon taxes paid 
    // Loop through supply sectors.
    for( CSupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        carbonTaxPaid[ period ] += (*currSector)->getTotalCarbonTaxPaid( period );
    }
    // Loop through demand sectors.
    for( CDemandSectorIterator currSector = demandSector.begin(); currSector != demandSector.end(); ++currSector ){
        carbonTaxPaid[ period ] += (*currSector)->getTotalCarbonTaxPaid( period );
    }
}

//! Write all outputs to file.
void Region::csvOutputFile() const {
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write population results to database
    population->csvOutputFile( name );
    gdp->csvOutputFile( name );

    // regional total carbon taxes paid
    fileoutput3(name," "," "," ","C tax revenue","Mil90$",carbonTaxPaid);

    // write total emissions for region
    for (int m=0;m<maxper;m++)
        temp[m] = summary[m].get_emissmap_second("CO2");
    fileoutput3(name," "," "," ","CO2 emiss","MTC",temp);
    // write depletable resource results to file
    for ( unsigned int i = 0; i < resources.size(); i++ ) 
        resources[i]->csvOutputFile( name );
    // write supply sector results to file
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->csvOutputFile();
        supplySector[i]->subsec_outfile();
    }
    // write end-use sector demand results to file
    for ( unsigned int i = 0; i < demandSector.size(); i++ ){
        demandSector[i]->csvOutputFile();	
        demandSector[i]->subsec_outfile();
    }
}

//! Write MiniCAM style outputs to file.
void Region::dbOutput() const {
    const Modeltime* modeltime = scenario->getModeltime();
    int m=0;
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper),temptot(maxper);
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write population results to database
    population->dbOutput( name );
    gdp->dbOutput( name );

    // regional total carbon taxes paid
    dboutput4(name,"General","CarbonTax","revenue","90US$",carbonTaxPaid);

    // CO2 emissions by fuel
    const vector<string> primaryFuelList = scenario->getWorld()->getPrimaryFuelList();

    for( vector<string>::const_iterator fuelIter = primaryFuelList.begin(); fuelIter != primaryFuelList.end(); fuelIter++ ) {
        for (m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissfuelmap_second( *fuelIter );
            temptot[m] += temp[m];
        }
        dboutput4(name,"CO2 Emiss","by Fuel",*fuelIter,"MTC",temp);
    }
    // add amount of geologic sequestration to emissions by fuel
	// todo change hardcoded category name
    for (m=0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestGeologic" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","geologic sequestration","MTC",temp);

    // add amount of sequestration from non-energy use to emissions by fuel
	// todo change hardcoded category name
    for (m=0;m<maxper;m++) {
        // note the negative value for sequestered amount
        temp[m] = - summary[m].get_emissmap_second( "CO2sequestNonEngy" );
        temptot[m] += temp[m];
    }
    dboutput4(name,"CO2 Emiss","by Fuel","non-energy use","MTC",temp);

    // total emissions by sector for region
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    // CO2 emissions by fuel and sector totals use same value
    dboutput4(name,"CO2 Emiss","by Fuel","zTotal","MTC",temptot);
    dboutput4(name,"CO2 Emiss","by Sector","zTotal","MTC",temp);

    // regional emissions for all greenhouse gases
    typedef map<string,double>:: const_iterator CI;
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(name,"Emissions","by gas",gmap->first,"MTC",temp);
    }

    // regional total end-use service demand for all demand sectors
    for (m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getService( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector","zTotal","Ser Unit",temp);

    // regional total end-use service demand without Tech Change for all demand sectors
    for (m=0;m<maxper;m++) {
        temp[m] = 0; // initialize temp to 0 for each period
        for ( unsigned int i = 0; i < demandSector.size(); i++ ) { // sum for all period and demand sectors
            temp[m] += demandSector[i]->getServiceWoTC( m );
        }
    }
    dboutput4(name,"End-Use Service","by Sector w/o TC","zTotal","Ser Unit",temp);

    // regional fuel consumption (primary and secondary) by fuel type
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        dboutput4(name,"Fuel Consumption","by fuel",fmap->first,"EJ",temp);
    }

    /*	summary does not contain fuel consumption by sector	
    // regional total fuel consumption for all demand sectors
    for (m=0;m<maxper;m++) {
    temp[m] = 0; // initialize temp to 0 for each period
    for (i=0;i<noDSec;i++) { // sum for all period and demand sectors
    temp[m] += summary[m].get_fmap_second(demandSector[i]->getName());
    }
    }
    dboutput4(name,"Fuel Consumption","by End-Use Sector","zTotal","EJ",temp);
    */	
    // region primary energy consumption by fuel type
    map<string,double> tpemap = summary[0].getpecons();
    CI pmap;
    for (pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_pemap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Consumption by fuel",pmap->first,"EJ",temp);
    }

    // region primary energy trade by fuel type
    tpemap = summary[0].getpetrade();
    for (pmap=tpemap.begin(); pmap!=tpemap.end(); ++pmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_petrmap_second(pmap->first);
        }
        dboutput4(name,"Pri Energy","Trade by fuel",pmap->first,"EJ",temp);
    }

    // regional Pri Energy Production Total
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_peprodmap_second("zTotal");
    }
    dboutput4(name,"Pri Energy","Production by Sector","zTotal","EJ",temp);

    // write depletable resource results to database
    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        resources[i]->dbOutput( name );
    }
    // write supply sector results to database
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        supplySector[i]->dbOutput();
    }
    // write end-use sector demand results to database
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        demandSector[i]->dbOutput();
    }
}

//! Find out which markets have simultaneities 
/* Want to loop through each sector, then loop through each fuels that sector uses.
Then loop through each other sector that is also a fuel.
Then loop through the fuels in that sector to see if that sector uses
the first as a fuel.  */
void Region::findSimul(const int period) {
    Marketplace* marketplace = scenario->getMarketplace();
    string OuterSectorName;
    string InnerSectorName;
    string InnerFuelName;    
    map<string, double> fuelcons;  
    map<string, double> Innerfuelcons;  
    typedef map<string,double>:: const_iterator CI;
    CI fuelIterOne;
    CI fuelIterTwo;
    const bool WriteOut = false;

    // Loop through all supply sectors
    if (WriteOut) { cout << "Region: " << name << endl; }
    for ( unsigned int isec=0; isec<supplySector.size(); isec++ ) {				
        OuterSectorName = supplySector[isec]->getName();
        if (WriteOut) { cout << "Checking Sector: " << OuterSectorName << endl; }
        fuelcons = supplySector[isec]->getfuelcons(period);	// Get fuel consumption map for outer sector
        // Inner loop through all supply sectors
        for ( unsigned int jsec=0; jsec<supplySector.size(); jsec++ ) {
            InnerSectorName = supplySector[jsec]->getName();
            fuelIterOne=fuelcons.find(InnerSectorName);	// Search in outer sector for name of inner sector 
            // Check if the inner sector is a fuel used by the outer sector (and not same sector!)
            if ( ( jsec != isec ) && ( fuelIterOne!=fuelcons.end() ) ) {	
                Innerfuelcons = supplySector[jsec]->getfuelcons(period);	// Get map of fuels used in inner sector
                if (WriteOut) {  cout << " Against Sector: " << InnerSectorName << endl; }

                // Now loop through inner sector, checking to see if it uses the outer sector as an input
                for ( fuelIterTwo=Innerfuelcons.begin(); fuelIterTwo!=Innerfuelcons.end(); fuelIterTwo++ ) {
                    InnerFuelName = fuelIterTwo->first;
                    if(InnerFuelName == OuterSectorName) {
                        // Have found a simultaneity
                        supplySector[ isec ]->addSimul( supplySector[ jsec ]->getName() );
                        supplySector[ jsec ]->addSimul( supplySector[ isec ]->getName() );

                        marketplace->resetToPriceMarket( InnerFuelName, name );
                        marketplace->resetToPriceMarket(InnerSectorName, name);
                        if (WriteOut) { 
                            cout << "  ***Sector " << InnerSectorName << " uses " << InnerFuelName << endl; 
                        }
                    }
                    else {
                        if (WriteOut) { 
                            cout << "     Sector " << InnerSectorName << " also uses " << InnerFuelName << endl; 
                        }
                    }
                }
            }
        }
    }
}

//! Initialize the market prices for the agricultural products.
void Region::initializeAgMarketPrices( const vector<double>& pricesIn ) { 
    agSector->initMarketPrices( name, pricesIn );
}

//! update regional summaries for reporting
void Region::updateSummary( const int period ) { 
    summary[period].clearpeprod();
    summary[period].clearfuelcons();

    for ( unsigned int i = 0; i < resources.size(); i++ ) {
        summary[period].initpeprod(resources[i]->getName(),resources[i]->getAnnualProd(period));
    }
    for ( unsigned int i = 0; i < demandSector.size(); i++ ) {
        // call update for demand sector
        demandSector[i]->updateSummary( period );
        // update regional fuel consumption (primary and secondary) for demand sector
        summary[ period ].updatefuelcons( demandSector[ i ]->getfuelcons( period ) ); 
        summary[ period ].updateemfuelmap( demandSector[ i ]->getemfuelmap( period ) );
    }
    for ( unsigned int i = 0; i < supplySector.size(); i++ ) {
        // call update for supply sector
        supplySector[i]->updateSummary( period );
        // update regional fuel consumption (primary and secondary) for supply sector
        summary[period].updatefuelcons(supplySector[i]->getfuelcons(period)); 
        summary[ period ].updateemfuelmap( supplySector[ i ]->getemfuelmap( period ) );
    }
    // update primary energy trade from consumption and production amounts
    summary[period].updatepetrade(); 
}

/*! A function which print dependency graphs showing fuel usage by sector.
*
* This function prints the opening tag for the graph, calls Sector::addToDependencyGraph
* on all supply and demand sectors, and then prints the closing tag.
*
* \param outStream An output stream to write to which was previously created.
* \param period The period to print graphs for.
*/
void Region::printGraphs( ostream& outStream, const int period ) const {

    // Make sure the outputstream is open.
    assert( outStream );

    // Remove spaces from the region name.
    string tempName = name;
    util::replaceSpaces( tempName );

    // Print the graph header.
    outStream << "digraph " << tempName << " {" << endl;

    // Now iterate through sectors.

    // Loop through all resource sectors
    for ( unsigned int resourceIter = 0; resourceIter < resources.size(); resourceIter++ ) {				
        resources[ resourceIter ]->addToDependencyGraph( outStream, period );
    }

    // Loop through all supply sectors
    for ( unsigned int supplyIter = 0; supplyIter < supplySector.size(); supplyIter++ ) {				
        supplySector[ supplyIter ]->addToDependencyGraph( outStream, period );
    }

    // Loop through all demand sectors.
    for ( unsigned int demandIter = 0; demandIter < demandSector.size(); demandIter++ ) {				
        demandSector[ demandIter ]->addToDependencyGraph( outStream, period );
    }

    // Now close the graph
    outStream << "}" << endl << endl;
}

//! Return the primaryFuelCO2Coef for a specific  fuel.
double Region::getPrimaryFuelCO2Coef( const string& fuelName ) const {

    // Determine the correct fuel.
    double coef = 0;
    map<string,double>::const_iterator coefIter = primaryFuelCO2Coef.find( fuelName );
    if( coefIter != primaryFuelCO2Coef.end() ) {
        coef = coefIter->second;
    }

    return coef;
}

//! Return the carbonTaxCoef for a specific  fuel.
double Region::getCarbonTaxCoef( const string& fuelName ) const {

    // Determine the correct fuel.
    double coef = 0;
    map<string,double>::const_iterator coefIter = carbonTaxFuelCoef.find( fuelName );
    if( coefIter != carbonTaxFuelCoef.end() ) {
        coef = coefIter->second;
    }

    return coef;
}

//! Return the summary object for the given period.
/*! \todo This is a temporary fix to get the global CO2. This should be restructured.
* \param period Model period to return the summary for.
* \return The summary object.
*/
const Summary Region::getSummary( const int period ) const {
    return summary[ period ];
}

/*! \brief Return the dynamically determined input dependencies for a given sector.
*
* This function is a helper function to the recursive sector::getInputDependencies.
* It is required so that a sector can determine its full list of input dependencies,
* which in turn requires determining that for each of its input sectors.
*
* \author Josh Lurz
* \param sectorName Sector to find the full list of input dependencies for.
* \return The full list of input dependencies for the given sector
*/
vector<string> Region::getSectorDependencies( const string& sectorName ) const {

    // Setup the return vector.
    vector<string> retVector;

    // Find the correct sector.
    map<string,int>::const_iterator iter = supplySectorNameMap.find( sectorName );

    // If the sector exists returns the dependency list.
    if( iter != supplySectorNameMap.end() ) {
        retVector = supplySector[ iter->second ]->getInputDependencies( this );
    }

    // Return the resulting list.
    return retVector;
}

/*! \brief A function to print a csv file including the final sector ordering, and all sectors and their dependencies.
* 
* \author Josh Lurz
* \param logger The to which to print the dependencies. 
*/
void Region::printSectorDependencies( Logger* logger ) const {
    typedef CSupplySectorIterator CSupplySectorIterator;

    // Print the final ordering of the sectors within the region.
    LOG( logger, Logger::DEBUG_LEVEL ) << " Final Sector ordering for " << name << endl;
    for(  CSupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ){
        LOG( logger, Logger::DEBUG_LEVEL )<< ( *currSector )->getName() << endl;
    }
    LOG( logger, Logger::DEBUG_LEVEL ) << endl;

    // Print the sector dependencies for all sectors within this region.
    LOG( logger, Logger::DEBUG_LEVEL ) << name << ",Sector,Dependencies ->," << endl;
    for( CSupplySectorIterator currSector = supplySector.begin(); currSector != supplySector.end(); ++currSector ) {
        ( *currSector )->printSectorDependencies( logger );
    }
    LOG( logger, Logger::DEBUG_LEVEL ) << endl;
}

/*! \brief This function will set the tax policy with the given name to a fixed tax policy.
* \details This function searches for a GHGPolicy with the name policyName. If it finds it, it will
* reset it to a fixed tax policy using the taxes in the taxes vector. Otherwise, it will create a new
* fixed tax policy with policyName.
* \author Josh Lurz
* \param policyName The name of the GHGPolicy to convert to a fixed tax.
* \param marketName The name of the market the GHGPolicy applies to.
* \param taxes The taxes to use for the policy.
*/
void Region::setFixedTaxes( const std::string& policyName, const std::string& marketName, const vector<double>& taxes ){
    bool foundPolicy = false;

    for( int i = 0; i < static_cast<int>( ghgMarket.size() ); i++ ){
        if( ghgMarket[ i ]->getName() == policyName ){
            foundPolicy = true;
            ghgMarket[ i ]->changePolicyToFixedTax( name );
            ghgMarket[ i ]->setFixedTaxes( name, taxes );
            break;
        }
    }
    // Create a new policy since the policy did not exist.
    if( !foundPolicy ){
        GHGPolicy* policy = new GHGPolicy( policyName, "", marketName , true );
        policy->setFixedTaxes( name, taxes );
        policy->setMarket( name );
        ghgMarket.push_back( policy );
    }
}

/*! \brief A function to generate a ghg emissions quantity curve based on an already performed model run.
* \details This function used the information stored in it to create a curve, with each datapoint 
* containing a time period and an amount of gas emissions. These values are retrieved from the emissions.
* \note The user is responsible for deallocating the memory in the returned Curve.
* \author Josh Lurz
* \param The name of the ghg to create a curve for.
* \return A Curve object representing ghg emissions quantity by time period.
*/
const Curve* Region::getEmissionsQuantityCurve( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    const Modeltime* modeltime = scenario->getModeltime();
    
    ExplicitPointSet* emissionsPoints = new ExplicitPointSet();
    
    for( int i = 0; i < scenario->getModeltime()->getmaxper(); i++ ) {
        XYDataPoint* currPoint = new XYDataPoint( modeltime->getper_to_yr( i ), summary[ i ].get_emissmap_second( ghgName ) );
        emissionsPoints->addPoint( currPoint );
    }
    
    Curve* emissionsCurve = new PointSetCurve( emissionsPoints );
    emissionsCurve->setTitle( ghgName + " emissions curve" );
    emissionsCurve->setXAxisLabel( "year" );
    emissionsCurve->setYAxisLabel( "emissions quantity" );

    return emissionsCurve;
}

/*! \brief A function to generate a ghg emissions price curve based on an already performed model run.
* \details This function used the information stored in it to create a curve, with each datapoint 
* containing a time period and the price gas emissions. These values are retrieved from the marketplace. 
* \note The user is responsible for deallocating the memory in the returned Curve.
* \author Josh Lurz
* \param The name of the ghg to create a curve for.
* \param The region to use to determine the market.
* \return A Curve object representing the price of ghg emissions by time period. 
*/
const Curve* Region::getEmissionsPriceCurve( const string& ghgName ) const {
    /*! \pre The run has been completed. */
    const Modeltime* modeltime = scenario->getModeltime();
    const Marketplace* marketplace = scenario->getMarketplace();

    ExplicitPointSet* emissionsPoints = new ExplicitPointSet();
    
    for( int i = 0; i < modeltime->getmaxper(); i++ ) {
        XYDataPoint* currPoint = new XYDataPoint( modeltime->getper_to_yr( i ), marketplace->getPrice( ghgName, name, i ) );
        emissionsPoints->addPoint( currPoint );
    }
    
    Curve* emissionsCurve = new PointSetCurve( emissionsPoints );
    emissionsCurve->setTitle( ghgName + " emissions tax curve" );
    emissionsCurve->setXAxisLabel( "year" );
    emissionsCurve->setYAxisLabel( "emissions tax" );

    return emissionsCurve;
}
