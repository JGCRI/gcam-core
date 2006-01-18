/*! 
* \file land_allocator_leaf.cpp
* \ingroup CIAM
* \brief LandLeaf class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/definitions.h"
#include <string>
#include <vector>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "containers/include/scenario.h"
#include "util/base/include/configuration.h"
#include "emissions/include/ghg.h"
#include "land_allocator/include/land_leaf.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Constructor.
* \author James Blackwood
*/
LandLeaf::LandLeaf() {
    const Modeltime* modeltime = scenario->getModeltime();
    int maxper = modeltime->getmaxper();
    intrinsicYieldMode.resize( maxper );
    yield.resize( maxper, -1 );
    calObservedYield.resize( maxper );
    carbonValue.resize( maxper );
    agProdChange.resize( maxper, 1 );
    production.resize( maxper );
    carbAboveGround = 0;
}

//! Default destructor
LandLeaf::~LandLeaf() {
}

size_t LandLeaf::getNumChildren() const {
    return 0;
}

const ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) const {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

ALandAllocatorItem* LandLeaf::getChildAt( const size_t aIndex ) {
    /*! \invariant Leaves have no children so this should never be called. */
    assert( false );
    return 0;
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool LandLeaf::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();

    if( nodeName == "intrinsicRate" ) {
        XMLHelper<double>::insertValueIntoVector( curr, intrinsicRate, modeltime );
    }
    else if( nodeName == "landAllocation" ) {
        XMLHelper<double>::insertValueIntoVector( curr, landAllocation, modeltime );
    }
    else if( nodeName == "intrinsicYieldMode" ) {
        XMLHelper<double>::insertValueIntoVector( curr, intrinsicYieldMode, modeltime );
    } 
    else if( nodeName == "carbAboveGround" ) {
        carbAboveGround = XMLHelper<double>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& LandLeaf::getXMLName() const {
    return getXMLNameStatic();
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
const std::string& LandLeaf::getXMLNameStatic() {
    const static string XML_NAME = "LandAllocatorLeaf";
    return XML_NAME;
}

/*! \brief Complete the Initialization in the LandAllocator.
* \author James Blackwood
*/
void LandLeaf::completeInit( const string& aRegionName, const IInfo* aRegionInfo ) {
}

void LandLeaf::addLandUsage( const string& aLandType,
                             const string& aProductName,
                             const ILandAllocator::LandUsageType aType )
{
    // The leaf is the land usage so this should never be called.
    assert( false );
}

/*! \brief Returns whether this is a production leaf.
* \details Returning true means that this leaf was set-up by a supply sector,
*          returning falses means this is an unmanaged land leaf that was
*          read-in from the land allocator.
* \return Whether this is a production leaf.
* \author Steve Smith
*/
bool LandLeaf::isProductionLeaf() const {
    return true;
}

/*! \brief Set the initial shares for this land.
*
* Sets the share for this land in the period passed in, from the ratio of
* the the land allocated for this leaf over the land allocated for the type above.
*
* \author James Blackwood
*/
void LandLeaf::setInitShares( double landAllocationAbove, int period ) {
    if ( landAllocationAbove > 0 ) {
        share[ period ] = landAllocation[ period ] / landAllocationAbove;
    }
}

/*! \brief Calculate the Intrinsic Yield Mode
*
* Note: intrinsicYieldMode has the observedYield stored in it
* \todo If the above is true, then see if that can be made explicit
* \todo find better way of specifying share for the intrinsic yield calc for good with no initial share
* \author James Blackwood, Steve Smith
*/
void LandLeaf::setIntrinsicYieldMode( double intrinsicRateAbove, double sigmaAbove, int period ) {
    double tempShare = share[ period ];
    // If share is zero and have read in a calibrated yield (e.g. biomass or
    // other new crops) then use an arbitrary 0.25 share need to figure out what
    // to read in for this, or how to specify (specify comparable?)
    if ( share[ period ] < util::getSmallNumber() ) {
        tempShare = 0.25;
    }
    intrinsicRateAbove *= pow( tempShare, sigmaAbove );
    intrinsicYieldMode[ period ] = intrinsicRateAbove * calObservedYield[ period ];
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();    
    for ( int i = period + 1; i < maxper; i++ ) {
        intrinsicYieldMode[ i ] = intrinsicYieldMode[ period ];
    }

    checkCalObservedYield( period );
}

/*! \brief Check whether the calibrated observed yield is valid for this leaf type.
* \param aPeriod Model period.
*/
void LandLeaf::checkCalObservedYield( const int aPeriod ) const {
    if ( calObservedYield[ aPeriod ] == 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::NOTICE );
        mainLog << "calObservedYield is zero in " << name << " in "
                << name << " period " << aPeriod << endl; 
    }
}

/*! 
* \brief Write out input stream.
* \param out Output file in XML format.
* \param tabs Tabs object used to track the number of tabs to print.
*/
void LandLeaf::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
    // do nothing since leaves are created dynamicaly
}

void LandLeaf::toDebugXMLDerived( const int period, std::ostream& out, Tabs* tabs ) const {
    XMLWriteElement( isProductionLeaf(), "isProductionLeaf", out, tabs );
    XMLWriteElement( intrinsicYieldMode[ period ], "intrinsicYieldMode", out, tabs );
    XMLWriteElement( calObservedYield[ period ], "calObservedYield", out, tabs );
    XMLWriteElement( yield[ period ], "yield", out, tabs );
    XMLWriteElement( agProdChange[ period ], "agProdChange", out, tabs );
    XMLWriteElement( production[ period ], "production", out, tabs );
    XMLWriteElement( carbonValue[ period ], "carbonValue", out, tabs );
    XMLWriteElement( carbAboveGround, "carbAboveGround", out, tabs );
}

/*! \brief Overwrites the intrinsicRate for this leaf node.
* This works differnet in LandAllocatorNode
* \todo carbon value simply spread over 20 years, need to have a more grounded method for this
* \author James Blackwood, Steve Smith
*/
void LandLeaf::setIntrinsicRate( const string& aRegionName,
                                 const string& aLandType,
                                 const string& aProductName,
                                 const double aIntrinsicRate, 
                                 const int aPeriod )
{
    assert( aProductName == name );

    // intrinsicRateIn [$/GCal] * intrinsicYieldMode [GCal/kHa] = [$/kHa]
    //the intrinsicRate that is passed in is $/Gcal. 
    //It is multiplied by intrinsicYieldMode (GCal/kHa) to convert it to $/kHa.   

    // Add carbon value to intrinsic rate (optional)
    // TODO: Magic number!
    intrinsicRate[ aPeriod ] = aIntrinsicRate * intrinsicYieldMode[ aPeriod ] 
                               + getCarbonValue( aRegionName, aPeriod ) * 1000/20;
}

/*! \brief Calculates the carbon value per hectare for this land type.
* \author James Blackwood
*/
double LandLeaf::getCarbonValue( const string& aRegionName, const int aPeriod ) const {
    // TODO: This should be done through the input files.
    const static bool doCarbValue = Configuration::getInstance()->getBool( "landCarbValue" );
    if( doCarbValue ){
        const Marketplace* marketplace = scenario->getMarketplace();
        double carbonPrice = marketplace->getPrice( "CO2", aRegionName, aPeriod, false );
        if( carbonPrice != Marketplace::NO_MARKET_PRICE ){
            // With carbon content in Mg C/Ha == TC/Ha, to TC/Ha * $/TC = $/Ha.
            return carbAboveGround * carbonPrice;
        }
    }
    return 0;
}

/*! \brief Set land allocated for this land.
*
* Sets the land allocated for this land in through a passed in value.
*
* \author James Blackwood
*/
void LandLeaf::setCalLandAllocation( const string& aLandType,
                                              const string& aProductName,
                                              const double aCalLandUsed,
                                              const int aHarvestPeriod, 
                                              const int aCurrentPeriod )
{
    assert( aProductName == name );
    landAllocation[ aHarvestPeriod ] = aCalLandUsed;
}

/*! \brief Sets land allocation of unmanged land leaf
* \details Does nothing for normal production leafs
* \param aRegionName Region name.
* \param aLandAllocation Total land to be allocated to this unmanaged land leaf
* \param aPeriod Model period.
* \author Steve Smith
*/
void LandLeaf::setUnmanagedLandAllocation( const string& aRegionName,
                                           const double aLandAllocation,
                                           const int aPeriod )
{
}

/*! \brief Adjust land values for unmanaged land nodes as necessary
*
* Does nothing for normal production leafs
* \param aRegionName Region name.
* \param aPeriod Model period.
* \author Steve Smith
*/
void LandLeaf::setUnmanagedLandValues( const string& aRegionName, const int aPeriod ) {
}

/*! \brief Set calibrated observed yield for this land.
*
* Sets the calibrated observed yield for this land in through a passed in value.
*
* \author James Blackwood
*/
void LandLeaf::setCalObservedYield( const string& aLandType,
                                    const string& aProductName,
                                    const double aCalObservedYield,
                                    const int aPeriod )
{
    assert( aProductName == name );
    calObservedYield[ aPeriod ] = aCalObservedYield;
}

double LandLeaf::getCalAveObservedRateInternal( const string& aLandType,
                                                const int aPeriod,
                                                const double aSigma ) const
{
    // This should never be called for a land leaf.
    assert( false );
    return 0;
}

/*! \brief Applies a technology change for this land.
*
* Changes the agProdChange variable which will later be factored into the
* intrinsicYieldMode.
*
* \author James Blackwood
*/
void LandLeaf::applyAgProdChange( const string& aLandType,
                                  const string& aProductName,
                                  const double aAgProdChange,
                                  const int aPeriod )
{
    assert( aProductName == name );
    double previousAgProdChange = 1;
    if ( aPeriod > 0 ) {
        previousAgProdChange = agProdChange[ aPeriod - 1 ];
    }

    const Modeltime* modeltime = scenario->getModeltime();
    int timestep = modeltime->gettimestep( aPeriod );
    agProdChange[ aPeriod ] = previousAgProdChange * pow( 1 + aAgProdChange, timestep );
    intrinsicYieldMode[ aPeriod ] *= agProdChange[ aPeriod ];
}

/*! \brief This calculates a temporary share, later a normalized share is assigned in the node version of this method.
* Added capability to share out unmanaged land proportionately with the land allocated to that level originally.
* \param sigmaAbove the sigma value from the node above this level.
* \param totalBaseLandAllocation total base unmanaged land allocation for this level.
* \author James Blackwood, Steve Smith
*/
void LandLeaf::calcLandShares( const string& aRegionName,
                               const double aSigmaAbove,
                               const double aTotalLandAllocated,
                               const int aPeriod )
{
    if( intrinsicRate[ aPeriod ] == 0 ){
        share[ aPeriod ] = util::getVerySmallNumber();
    }
    else {
        if ( aSigmaAbove > 0 ) {
            share[ aPeriod ] = pow ( intrinsicRate[ aPeriod ], 1 / aSigmaAbove );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::NOTICE );
            mainLog << "sigmaAbove is less than or equal to zero in " 
                    << name << " period " << aPeriod << endl; 
        }
    }
}

/*! \brief Calculates the land allocated for this land.
*
* Uses the land allocated in the node above and the share from this land to calculate the landAllocation.
*
* \author James Blackwood
*/
void LandLeaf::calcLandAllocation ( double landAllocationAbove, int period ) {
    landAllocation[period] = landAllocationAbove * share[period];
}

void LandLeaf::calcYieldInternal( const string& aLandType,
                                           const string& aProductName,
                                           const double aProfitRate,
                                           const double aAvgIntrinsicRate,
                                           const int aPeriod )
{
    if ( aProfitRate >= util::getSmallNumber() ) {
        // AveRate $/kHa / ($/GCal) = GCal/kHa)
        yield[ aPeriod ] = aAvgIntrinsicRate / aProfitRate;
    }
    else {
        // Make yield zero so that there is no production at zero or negative
        // profit rates.
        yield[ aPeriod ] = 0;
    }
}

double LandLeaf::getYield ( const string& landType, const string& productName, const int period ) const {
    return yield[ period ];
}

/*! \brief Should never be called, because only a child can be added to a node, not a leaf.
*
* \author James Blackwood
*/
void LandLeaf::addChild( ALandAllocatorItem* child ) {
    assert( false );
}

/*! \brief The leaf version of getLandAllocation.
*
* \author James Blackwood
* \return the LandAllocation at this node, if the productName matches the name of this landType.
*/
double LandLeaf::getLandAllocation( const string& aProductName,
                                    const int aPeriod ) const
{
    if ( aProductName == name ) {
        return landAllocation[ aPeriod ];
    }
    return 0;
}

/*! \brief Get total land allocation.
*
* For leaves without land vintaging this is identical to getLandAllocation
*
* \author James Blackwood
* \return the LandAllocation at this node, if the productName matches the name of this landType.
*/
double LandLeaf::getTotalLandAllocation( const string& productName, int period ) {
    return getLandAllocation( productName, period );
}

/*! \brief Returns the baseLandAllocation of this leaf.
* \author Steve Smith
* \return the baseLandAllocation of this landType
*/
double LandLeaf::getBaseLandAllocation ( int period ) {
    return 0;
}

void LandLeaf::calcEmission( const string& aRegionName,
                             const GDP* aGDP, 
                             const int aPeriod ){
}

void LandLeaf::updateSummary( Summary& aSummary, const int period ) {
    //check that there are the same number of ghg's in each period
    for ( unsigned i = 1; i < mGHGs.size(); i++ ) {
        assert (mGHGs[ i ].size() == mGHGs[ i - 1 ].size());
    }

    //map each ghg emission to its corresponding value
    for ( unsigned i = 0; i < mGHGs[ period ].size(); i++ ) {
        emissmap[ mGHGs[ period ][ i ]->getName( ) ] = mGHGs[ period ][ i ]->getEmission( period ) ;
    }

    //update the summary object with this mapping
    aSummary.updateemiss( emissmap );
}

/*! \brief Write output to csv output file. 
*
*
* \author Steve Smith
*/
void LandLeaf::csvOutput( const string& aRegionName ) const {
     ALandAllocatorItem::csvOutput( aRegionName );
    
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);

    // write land allocations for region
    fileoutput3(aRegionName,name," "," ","Intr Rate","$/kHa",intrinsicRate);
    fileoutput3(aRegionName,name," "," ","Intr Yield Mode","GCal/kHa",intrinsicYieldMode);
    fileoutput3(aRegionName,name," "," ","calObsYield","GCal/kHa",calObservedYield);
    fileoutput3(aRegionName,name," "," ","carbonValue","000Ha",carbonValue);
    fileoutput3(aRegionName,name," "," ","Ag Productivity Change","none",agProdChange);

    //check that there are the same number of ghg's in each period.
    for ( unsigned int i = 1; i < mGHGs.size(); i++ ) {
        assert (mGHGs[ i ].size() == mGHGs[ i - 1 ].size());
    }

    //print out each ghg emission
    for ( unsigned int i = 0; i < mGHGs[0].size(); i++ ) {
        for ( int j = 0; j < maxper; j++) {
            temp[j] = mGHGs[j][i]->getEmission( j );
        }
        fileoutput3(aRegionName,name," "," ",mGHGs[0][i]->getName(), mGHGs[0][i]->getUnit(),temp);
    }
}

void LandLeaf::dbOutput( const string& aRegionName ) const {
    ALandAllocatorItem::dbOutput( aRegionName );

    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);

    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // write land allocations for region
    dboutput4(aRegionName,name," ","Intr Rate","$/kHa",intrinsicRate);
    dboutput4(aRegionName,name," ","Intr Yield Mode","GCal/kHa",intrinsicYieldMode);
    dboutput4(aRegionName,name," ","calObsYield","GCal/kHa",calObservedYield);
    dboutput4(aRegionName,name," ","carbonValue","000Ha",carbonValue);
    dboutput4(aRegionName,name," ","Ag Productivity Change","none",agProdChange);

    //check that there are the same number of ghg's in each period
    for ( unsigned int i = 1; i < mGHGs.size(); i++ ) {
        assert (mGHGs[ i ].size() == mGHGs[ i - 1 ].size());
    }

    //print out each ghg emission
    for ( unsigned int i = 0; i < mGHGs[0].size(); i++ ) {
        for ( int j = 0; j < maxper; j++) {
            temp[j] = mGHGs[j][i]->getEmission( j );
        }
        dboutput4( aRegionName,name," ",mGHGs[0][i]->getName(),mGHGs[0][i]->getUnit(),temp);
    }
}
