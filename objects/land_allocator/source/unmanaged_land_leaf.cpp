/*! 
* \file unmanaged_land_leaf.cpp
* \ingroup Objects
* \brief UnmanagedLandLeaf class source file.
* \author James Blackwood
*/

#include "land_allocator/include/unmanaged_land_leaf.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "emissions/include/ghg_input.h"
#include "technologies/include/primary_output.h"
#include "emissions/include/unmanaged_carbon_calc.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief UnmanagedLandLeafault constructor.
* \author James Blackwood
*/
UnmanagedLandLeaf::UnmanagedLandLeaf(){
    const Modeltime* modeltime = scenario->getModeltime();
    int maxper = modeltime->getmaxper();
    baseIntrinsicRate.resize( maxper );
    baseLandAllocation.resize( maxper );
    historyYear = defaultHistoryYear();
}

//! Default destructor
UnmanagedLandLeaf::~UnmanagedLandLeaf() {
}

bool UnmanagedLandLeaf::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    if( nodeName == GhgInput::getXMLNameStatic() ){
        parseContainerNode( curr, mGHGs, new GhgInput() );
    }
    else if( nodeName == "historyYear" ) {
        historyYear = XMLHelper<int>::getValue( curr );
    }
    else if( nodeName == "intrinsicRate" ){
        XMLHelper<double>::insertValueIntoVector( curr, intrinsicRate, scenario->getModeltime() );
    }
    else if( nodeName == UnmanagedCarbonCalc::getXMLNameStatic() ) {
        parseSingleNode( curr, mCarbonContentCalc, new UnmanagedCarbonCalc );
    }
    else if( !LandLeaf::XMLDerivedClassParse( nodeName, curr ) ) {
        return false;
    }
    return true;
}

/*! 
* \brief Write datamembers specific to this class  to datastream in XML format. 
* \param out Output file in XML format.
* \param tabs Tabs object used to track the number of tabs to print.
* \ref faqitem1 
*/
void UnmanagedLandLeaf::toInputXML( ostream& out, Tabs* tabs ) const {
    XMLWriteOpeningTag ( getXMLName(), out, tabs, name );
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( intrinsicRate, "intrinsicRate", out, tabs, modeltime, 0.0 );

    XMLWriteElementCheckDefault( historyYear, "historyYear", out, tabs, defaultHistoryYear() );

    // Only write out land allocation values for historical years. Leave constant after last historical year
    vector <double> tempLandAllocation( modeltime->getmaxper() );
    int lastHistoryPeriod = 0;
    for( int per = 0; per < modeltime->getmaxper(); ++per ){
        if ( modeltime->getper_to_yr( per ) <= historyYear ) {
            tempLandAllocation[ per ] = landAllocation[ per ];
            lastHistoryPeriod = per;
        }
        else {
            tempLandAllocation[ per ] = landAllocation[ lastHistoryPeriod ];
        }
    }
    XMLWriteVector( tempLandAllocation, "landAllocation", out, tabs, modeltime, 0.0 );

    mCarbonContentCalc->toInputXML( out, tabs );

    for ( unsigned int j = 0; j < mGHGs.size(); j++ ) {
        mGHGs[ j ]->toInputXML( out, tabs );
    }
    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

void UnmanagedLandLeaf::toDebugXMLDerived( const int aPeriod, std::ostream& out, Tabs* tabs ) const {
    LandLeaf::toDebugXMLDerived( aPeriod, out, tabs );
    XMLWriteElement( baseIntrinsicRate[ aPeriod ], "baseIntrinsicRate", out, tabs );
    XMLWriteElement( baseLandAllocation[ aPeriod ], "baseLandAllocation", out, tabs );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author James Blackwood
* \return The constant XML_NAME.
*/
const string& UnmanagedLandLeaf::getXMLName() const {
    return getXMLNameStatic();
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& UnmanagedLandLeaf::getXMLNameStatic() {
    const static string XML_NAME = "UnmanagedLandLeaf";
    return XML_NAME;
}

/*! \brief Initialize the carbon cycle object.
* \details Instantiate a carbon cycle for an unmanaged land leaf.
*/
void UnmanagedLandLeaf::initCarbonCycle(){
    if( !mCarbonContentCalc.get() ){
        mCarbonContentCalc.reset( new UnmanagedCarbonCalc );
    }
}

/*! \brief Returns whether this is a production leaf.
* \details Returning true means that this leaf was set-up by a supply sector,
*          returning falses means this is an unmanaged land leaf that was
*          read-in from the land allocator.
* \return Whether this is a production leaf.
* \author Steve Smith
*/
bool UnmanagedLandLeaf::isProductionLeaf() const {
    return false;
}

/*! \brief Sets land allocation of unmanged land leaf
* 
* See LandAllocatorNode::setUnmanagedLandAllocation

* \param landAllocationIn Total land to be allocated to this unmanaged land leaf
* \param period Period index
* \author Steve Smith
*/
void UnmanagedLandLeaf::setUnmanagedLandAllocation( const string& aRegionName,
                                                    const double aLandAllocation,
                                                    const int aPeriod )
{
    landAllocation[ aPeriod ] = baseLandAllocation[ aPeriod ] =  aLandAllocation;  
    baseIntrinsicRate[ aPeriod ] =  intrinsicRate[ aPeriod ];
}

/*! \brief Adjust land values for unmanaged land nodes as necessary
*
* Need this because unmanaged land nodes do not get their rates set by a supply sector
*
* \todo carbon value simply spread over 20 years, need to have a more grounded method for this
* \param aRegionName Region name.
* \param aPeriod Period index
* \author Steve Smith
*/
void UnmanagedLandLeaf::setUnmanagedLandValues( const string& aRegionName,
                                                const int aPeriod )
{
    intrinsicRate[ aPeriod ] = baseIntrinsicRate[ aPeriod ]
                              + getCarbonValue( aRegionName, aPeriod ) * 1000 / 20;   
}

/*! \brief This calculates a temporary share, later a normalized share is
*          assigned in the node version of this method.
* \param sigmaAbove the sigma value from the node above this level.
* \param aTotalLandAllocated Total base unmanaged land allocation for this
*        level.
* \author James Blackwood, Steve Smith
*/
void UnmanagedLandLeaf::calcLandShares( const string& aRegionName,
                                        const double aSigmaAbove,
                                        const double aTotalLandAllocated,
                                        const int aPeriod )
{
    LandLeaf::calcLandShares( aRegionName, aSigmaAbove, 0, aPeriod );

    // This is an unmanaged land leaf, so adjust the share proportional to the
    // base land allocated to this leaf (relative to the total land in this
    // unmanaged land node)
    if ( aTotalLandAllocated > 0 ) {
        share[ aPeriod ] *= baseLandAllocation[ aPeriod ] / aTotalLandAllocated; 
    }
}

/*! \brief returns the baseLandAllocation of this landType.
*
* \author Steve Smith
* \return the baseLandAllocation of this landType
*/
double UnmanagedLandLeaf::getBaseLandAllocation ( int period ) {
    return baseLandAllocation[ period ];
}

void UnmanagedLandLeaf::calcEmission( const string& aRegionName,
                                      const GDP* aGDP, 
                                      const int aPeriod )
{    
    for ( unsigned int j = 0; j < mGHGs.size(); j++ ) {
        double input = landAllocation[ aPeriod ];
        
        // Create a temporary primary output. This is rather hackish to get
        // around the GHG interface being designed for Technologies.
        PrimaryOutput landOutput( name );
        vector<IOutput*> outputs;
        outputs.push_back( &landOutput ); 
        mGHGs[ j ]->calcEmission( aRegionName, name, input, outputs, aGDP, aPeriod );
    }
}

/*! \brief Check whether the calibrated observed yield is valid.
* \details Unmanaged land leaves do not have to have a calibrated observed
*          yield, so no checking is done.
* \param aPeriod Model period.
*/
void UnmanagedLandLeaf::checkCalObservedYield( const int aPeriod ) const {
}

/*! \brief Returns the default for the history year.
* \return Default for the history year.
*/
int UnmanagedLandLeaf::defaultHistoryYear(){
    return 1990;
}
