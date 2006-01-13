/*! 
* \file total_sector_emissions.cpp
* \ingroup Objects
* \brief TotalSectorEmissions class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

#include "containers/include/iinfo.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"
#include "sectors/include/sector.h"
#include "emissions/include/total_sector_emissions.h"


using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor
*/
TotalSectorEmissions::TotalSectorEmissions(){
	mType = "none";
	mAggregateEmissions = 0;
	mApplicableYear = 0;
}

void TotalSectorEmissions::XMLParse( const DOMNode* aNode ){
	/*! \pre make sure we were passed a valid node. */
    assert( aNode );

    // get the name attribute.
    mName = XMLHelper<string>::getAttrString( aNode, "name" );

    // get all child nodes.
    const DOMNodeList* nodeList = aNode->getChildNodes();
    const Modeltime* modeltime = scenario->getModeltime();

    // loop through the child nodes.
    for( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        const DOMNode* curr = nodeList->item( i );
        const string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );

        if( nodeName == "#text" ) {
            continue;
        }
		else if( nodeName == "type" ){
            mType = XMLHelper<string>::getValue( curr );
        }
		else if( nodeName == "value" ){
            mAggregateEmissions = XMLHelper<double>::getValue( curr );
        }
		else if( nodeName == "year" ){
            mApplicableYear = XMLHelper<int>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Unrecognized text string: " << nodeName << " found while parsing "
                    << getXMLNameStatic() << "." << endl;
        }
    }
}

void TotalSectorEmissions::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElementCheckDefault( mType, "type", aOut, aTabs, string( "none" ) );
    XMLWriteElementCheckDefault( mAggregateEmissions, "value", aOut, aTabs );
    XMLWriteElementCheckDefault( mApplicableYear, "year", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

void TotalSectorEmissions::toDebugXML( const int aPeriod, ostream& aOut, Tabs* aTabs ) const {
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, mName );
    XMLWriteElement( mType, "type", aOut, aTabs );
    XMLWriteElement( mAggregateEmissions, "value", aOut, aTabs );
    XMLWriteElement( mApplicableYear, "year", aOut, aTabs );
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
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
const std::string& TotalSectorEmissions::getXMLNameStatic() {
    const static string XML_NAME = "TotalSectorEmissions";
	return XML_NAME;
}

/*! \brief Returns the name.
* \author James Blackwood
* \return The name.
*/
const string& TotalSectorEmissions::getName() const {
    return mName;
}

/*! \brief Returns prefix to use for storing aggregate emissions factor in info
*          object.
* \author Steve Smith
* \return The prefix string
*/
const string& TotalSectorEmissions::aggrEmissionsPrefix() {
    const static string AGGR_EM_PREFIX = "AggrEmissionFactor"; 
    return AGGR_EM_PREFIX;
}

/*! \brief Sets aggregate emissions factor into the region info object.
* \details Calculates the total calibrated output for sectors of the specified
*          type in the specified year from the list of sectors. This is used to
*          calculate an emissions coefficient for the gas given the total
*          emissions for the set of sectors of the specified type.
* \author James Blackwood
* \param aSectors Vector of supply sectors used to determine the total
*        calibrated output.
* \param aRegionInfo Region info object which will be updated to contain the
*        emissions coefficients.
* \param aPeriod Model period.
*/
void TotalSectorEmissions::setAggregateEmissionFactor( const std::vector<Sector*>& aSectors,
                                                       IInfo* aRegionInfo,
                                                       const int aPeriod ) const
{
    // Check if the object is applicable in the current period.
    const Modeltime* modeltime = scenario->getModeltime();
    if ( mApplicableYear <= 0 || modeltime->getyr_to_per( mApplicableYear ) != aPeriod ) {
        return;
    }

    typedef vector<Sector*>::const_iterator SectorIterator;

    double summedOutput = 0;
    
    // TODO: What if all sectors are not calibrated? This condition must at least be checked for.
    for( SectorIterator currSector = aSectors.begin(); currSector != aSectors.end(); ++currSector ){
        summedOutput += (*currSector)->getCalOutput( aPeriod, mType );
    }

    // TODO: What if summed output is zero?
    double emissionFactor = mAggregateEmissions / summedOutput;
    
    // Store aggregate emissions factor in region info object
    aRegionInfo->setDouble( aggrEmissionsPrefix() + mName, emissionFactor );
}
