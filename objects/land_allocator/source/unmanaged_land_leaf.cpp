/*! 
 * \file unmanaged_land_leaf.cpp
 * \ingroup Objects
 * \brief UnmanagedLandLeaf class source file.
 * \author James Blackwood
 */

#include "util/base/include/definitions.h"
#include "land_allocator/include/unmanaged_land_leaf.h"
#include "land_allocator/include/land_use_history.h"
#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "containers/include/scenario.h"
#include "emissions/include/ghg_input.h"
#include "technologies/include/primary_output.h"
#include "emissions/include/unmanaged_carbon_calc.h"
#include "emissions/include/ghg.h"
#include "util/base/include/summary.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief UnmanagedLandLeafault constructor.
* \author James Blackwood
*/
UnmanagedLandLeaf::UnmanagedLandLeaf():
// Default the name to the empty string. It will be read in during XML parsing.
LandLeaf( "" )
{}

//! Default destructor
UnmanagedLandLeaf::~UnmanagedLandLeaf() {
}

bool UnmanagedLandLeaf::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    if( nodeName == GhgInput::getXMLNameStatic() ){
        parseContainerNode( curr, mGHGs, new GhgInput() );
    }
    else if( nodeName == "intrinsicRate" ){
        XMLHelper<double>::insertValueIntoVector( curr, mBaseIntrinsicRate, scenario->getModeltime() );
    }
    else if( nodeName == "landAllocation" ){
        XMLHelper<double>::insertValueIntoVector( curr, mLandAllocation, scenario->getModeltime() );
    }
    else if( nodeName == LandUseHistory::getXMLNameStatic() ){
        parseSingleNode( curr, mLandUseHistory, new LandUseHistory );
    }
    else if( nodeName == UnmanagedCarbonCalc::getXMLNameStatic() ) {
        parseSingleNode( curr, mCarbonContentCalc, new UnmanagedCarbonCalc );
    }
    else {
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
    XMLWriteOpeningTag ( getXMLName(), out, tabs, mName );
    const Modeltime* modeltime = scenario->getModeltime();
    XMLWriteVector( mBaseIntrinsicRate, "intrinsicRate", out, tabs, modeltime, 0.0 );
    XMLWriteVector( mLandAllocation, "landAllocation", out, tabs, scenario->getModeltime() );

    if( mLandUseHistory.get() ){
        mLandUseHistory->toInputXML( out, tabs );
    }

    mCarbonContentCalc->toInputXML( out, tabs );

    for ( unsigned int j = 0; j < mGHGs.size(); j++ ) {
        mGHGs[ j ]->toInputXML( out, tabs );
    }
    // finished writing xml for the class members.
    XMLWriteClosingTag( getXMLName(), out, tabs );
}

void UnmanagedLandLeaf::toDebugXMLDerived( const int aPeriod, ostream& out, Tabs* tabs ) const {
    LandLeaf::toDebugXMLDerived( aPeriod, out, tabs );
    XMLWriteElement( mBaseIntrinsicRate[ aPeriod ], "baseIntrinsicRate", out, tabs );
    XMLWriteElement( mBaseLandAllocation[ aPeriod ], "baseLandAllocation", out, tabs );

    // Don't write out land allocation because ALandAllocatorItem writes it.

    if( mLandUseHistory.get() ){
        mLandUseHistory->toDebugXML( aPeriod, out, tabs );
    }
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

void UnmanagedLandLeaf::initLandUseHistory( const LandUseHistory* aLandUseHistory,
                                            const int aPeriod )
{
    // Initialize the carbon calculator with the unmanaged land leaf history,
    // not the node history. The share is one because the history object
    // is for this leaf only.
    mCarbonContentCalc->initLandUseHistory( mLandUseHistory.get(), 1 );
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
    mLandAllocation[ aPeriod ] = mBaseLandAllocation[ aPeriod ] = aLandAllocation;
}

/*! \brief Adjust land values for unmanaged land nodes as necessary
*
* Need this because unmanaged land nodes do not get their rates set by a supply sector
*
* \param aRegionName Region name.
* \param aPeriod Period index
* \author Steve Smith
*/
void UnmanagedLandLeaf::setUnmanagedLandValues( const string& aRegionName,
                                                const int aPeriod )
{
    mIntrinsicRate[ aPeriod ] = mBaseIntrinsicRate[ aPeriod ]
                                + getCarbonValue( aRegionName, aPeriod );   
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
    if ( aTotalLandAllocated > util::getSmallNumber() ) {
        mShare[ aPeriod ] *= mBaseLandAllocation[ aPeriod ] / aTotalLandAllocated;
        assert( util::isValidNumber( mShare[ aPeriod ] ) );
    }
}

/*! \brief returns the baseLandAllocation of this landType.
*
* \author Steve Smith
* \return the baseLandAllocation of this landType
*/
double UnmanagedLandLeaf::getBaseLandAllocation( const int aPeriod ) const {
    return mBaseLandAllocation[ aPeriod ];
}

// TODO: This gets called too late if land GHGs are included in policies.
void UnmanagedLandLeaf::calcEmission( const string& aRegionName,
                                      const GDP* aGDP, 
                                      const int aPeriod )
{    
    for ( unsigned int j = 0; j < mGHGs.size(); j++ ) {
        double input = mLandAllocation[ aPeriod ];
        
        // Create a temporary primary output. This is rather hackish to get
        // around the GHG interface being designed for Technologies.
        PrimaryOutput landOutput( mName );
        landOutput.initCalc( aRegionName, aPeriod );
        landOutput.setPhysicalOutput( 0, aRegionName, aPeriod );
        vector<IOutput*> outputs;
        outputs.push_back( &landOutput ); 
        mGHGs[ j ]->calcEmission( aRegionName, mName, input, outputs, aGDP, aPeriod );
    }
}

/*! \brief Check whether the calibrated observed yield is valid.
* \details Unmanaged land leaves do not have to have a calibrated observed
*          yield, so no checking is done.
* \param aPeriod Model period.
*/
void UnmanagedLandLeaf::checkCalObservedYield( const int aPeriod ) const {
}

/*! \brief Update a visitor for a LandLeaf.
* \param aVisitor Visitor to update.
* \param aPeriod Period to update.
*/
void UnmanagedLandLeaf::accept( IVisitor* aVisitor, const int aPeriod ) const {
	aVisitor->startVisitUnmanagedLandLeaf( this, aPeriod );
    LandLeaf::accept( aVisitor, aPeriod );

    for( unsigned int i = 0; i < mGHGs.size(); ++i ){
        mGHGs[ i ]->accept( aVisitor, aPeriod );
    }
	aVisitor->endVisitUnmanagedLandLeaf( this, aPeriod );
}

void UnmanagedLandLeaf::updateSummary( Summary& aSummary, const int period ) {
    // Map each ghg emission to its corresponding value.
    map<string, double> emissMap;
    for ( unsigned i = 0; i < mGHGs.size(); i++ ) {
        emissMap[ mGHGs[ i ]->getName( ) ] = mGHGs[ i ]->getEmission( period ) ;
    }

    //update the summary object with this mapping
    aSummary.updateemiss( emissMap );
}

/*! \brief Write output to csv output file. 
*
*
* \author Steve Smith
*/
void UnmanagedLandLeaf::csvOutput( const string& aRegionName ) const {
     LandLeaf::csvOutput( aRegionName );
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    //print out each ghg emission
    for ( unsigned int i = 0; i < mGHGs.size(); i++ ) {
        for ( int j = 0; j < maxper; j++) {
            temp[j] = mGHGs[i]->getEmission( j );
        }
        fileoutput3(aRegionName, mName," "," ",mGHGs[i]->getName()+" emiss", mGHGs[i]->getUnit(),temp);
    }
}

void UnmanagedLandLeaf::dbOutput( const string& aRegionName ) const {
    LandLeaf::dbOutput( aRegionName );

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    //print out each ghg emission
    for ( unsigned int i = 0; i < mGHGs.size(); i++ ) {
        for ( int j = 0; j < maxper; j++) {
            temp[j] = mGHGs[i]->getEmission( j );
        }
        // TODO: Does this match emissions for Technology GHGs?
        dboutput4( aRegionName, "Land Allocation", mName, mGHGs[i]->getName()+" emiss", "MTC",temp);
    }
}

