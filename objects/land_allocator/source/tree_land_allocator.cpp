/*! 
* \file tree_land_allocator.cpp
* \ingroup Objects
* \brief TreeLandAllocator class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include "util/base/include/xml_helper.h"

#include "land_allocator/include/tree_land_allocator.h"
#include "containers/include/scenario.h"
#include "containers/include/iinfo.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
TreeLandAllocator::TreeLandAllocator(){
}

//! Default destructor
TreeLandAllocator::~TreeLandAllocator() {
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author James Blackwood
* \return The constant XML_NAME.
*/
const std::string& TreeLandAllocator::getXMLName() const {
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
const std::string& TreeLandAllocator::getXMLNameStatic() {
	const static string XML_NAME = "LandAllocatorRoot";
	return XML_NAME;
}

void TreeLandAllocator::XMLParse( const DOMNode* aNode ){
    // Call the node XML parse.
    LandNode::XMLParse( aNode );
}

void TreeLandAllocator::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const {
    // Call the node toDebugXML
    LandNode::toDebugXML( aPeriod, aOut, aTabs );
}

void TreeLandAllocator::toInputXML( std::ostream& aOut, Tabs* aTabs ) const {
    // Call the node toInputXML
    LandNode::toInputXML( aOut, aTabs );
}

/*! \brief Write XML values specific to derived objects
*
* \author Steve Smith
*/
void TreeLandAllocator::toInputXMLDerived( ostream& out, Tabs* tabs ) const {
	const Modeltime* modeltime = scenario->getModeltime();

    XMLWriteVector( landAllocation, "landAllocation", out, tabs, modeltime, 0.0 );
}

/*! \brief Complete the Initialization in the LandAllocator.
* This is called for the root node only
* Calls 2 functions inside LandAllocator that set the initial shares, LandAllocation and calibrated observed yield.
* \author James Blackwood
*/
void TreeLandAllocator::completeInit( const string& aRegionName, 
                                      const IInfo* aRegionInfo )
{
    // Check that all model periods are equal, which is required for this land
    // allocator.
	const Modeltime* modeltime = scenario->getModeltime();
	for( int period = 1; period < modeltime->getmaxper(); ++period ) {
		if ( modeltime->gettimestep( period ) != modeltime->gettimestep( period - 1 ) ) {
			ILogger& mainLog = ILogger::getLogger( "main_log" );
			mainLog.setLevel( ILogger::WARNING );
			mainLog << "All time steps are not constant." << endl;
		}
	}

	// A check to verify that the rotation period is a multiple of the model's
    // timestep.
    const int rotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
	for ( int period = 0; period < modeltime->getmaxper(); ++period ) {
        if( rotationPeriod % modeltime->gettimestep( period ) != 0 ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::DEBUG );
            mainLog << "Rotation period is not evenly divisible by timestep in land allocator " 
                    << " in period " << period << "." << endl;
        }
	}

	for ( unsigned int i = 0; i < children.size(); i++ ) {
		children[ i ]->completeInit( name, aRegionInfo );
	}
	for( int period = 0; period < modeltime->getmaxper(); period++ ) {
		// Send any unmanaged land nodes the amount of land not already allocated
		double totalManagedLand = 0;
		// First determine total managed land allocated
		for ( unsigned int i = 0; i < children.size(); i++ ) {
			if ( children[ i ]->isProductionLeaf() ) {
				totalManagedLand += getTotalLandAllocation( children[ i ]->getName() , period );
			}
		}

		//check that the total calLandUsed is not greater than the total available
		if ( totalManagedLand >= landAllocation[ period ]) {
			ILogger& mainLog = ILogger::getLogger( "main_log" );
			mainLog.setLevel( ILogger::ERROR );
			if (totalManagedLand > landAllocation[ period ]) {
				mainLog << "The total managed land allocated is greater than the total land in " << name
                        << " in " << modeltime->getper_to_yr( period )
                        << " by " << totalManagedLand - landAllocation[ period ] 
                        << "(" << 100*( totalManagedLand - landAllocation[ period ] ) / landAllocation[ period ] << "%)" << endl;
			}
			else if (totalManagedLand == landAllocation[ period ]) {
				mainLog << "The total managed land is equal to the total land value in " << name << endl;
			}

			mainLog.setLevel( ILogger::WARNING );
			mainLog << "Total land value set to total managed land plus 20% " << endl;
			for ( unsigned int i = period; i < landAllocation.size(); i++ ) {
				landAllocation[ i ] = totalManagedLand * 1.2;
			}
		}

		// Now re-allocate unmanaged land. Read-in land allocations are used as
		// weights with total unmanaged land set to be equal to total land minus
        // land allocation.
		for ( unsigned int i = 0; i < children.size(); i++ ) {
			// This will not work for more than one unmanaged land node under the root
			double landToBePassed = landAllocation[ period ] - totalManagedLand;
			children[i]->setUnmanagedLandAllocation( aRegionName, landToBePassed, period );
		}
		setInitShares( 0, period );
		setIntrinsicYieldMode( 1, sigma, period );
	}
}

/*! \brief Returns the intrinsicRate.
*
* \author James Blackwood
* \return the intrinsicRate of this LandAllocator which should only be called by the root node.
*/
double TreeLandAllocator::getAvgIntrinsicRate( int period ) {
	return intrinsicRate[ period ];
}

void TreeLandAllocator::addLandUsage( const string& aLandType,
                                      const string& aProductName )
{
    LandNode::addLandUsage( aLandType, aProductName );
}

double TreeLandAllocator::getCalAveObservedRate( const string& aLandType, int aPeriod ) const {
    return getCalAveObservedRateInternal( aLandType, aPeriod, sigma );
}

double TreeLandAllocator::getLandAllocation( const string& aProductName,
                                             const int aPeriod ) const
{
    return LandNode::getLandAllocation( aProductName, aPeriod );
}

void TreeLandAllocator::applyAgProdChange( const string& aLandType,
                                           const string& aProductName,
                                           const double aAgProdChange,
                                           const int aPeriod )
{
    LandNode::applyAgProdChange( aLandType, aProductName, aAgProdChange, aPeriod );
}

void TreeLandAllocator::calcYield( const string& aLandType,
                                   const string& aProductName,
                                   const double aProfitRate,
                                   const int aHarvestPeriod,
                                   const int aCurrentPeriod )
{
    LandNode::calcYieldInternal( aLandType, aProductName, aProfitRate,
                                 intrinsicRate[ aCurrentPeriod ], aHarvestPeriod );
}

double TreeLandAllocator::getYield( const string& aLandType,
                                    const string& aProductName,
                                    const int aPeriod ) const
{
    return LandNode::getYield( aLandType, aProductName, aPeriod );
}

void TreeLandAllocator::setCalLandAllocation( const string& aLandType,
                                              const string& aProductName,
                                              const double aCalLandUsed,
                                              const int aHarvestPeriod, 
                                              const int aCurrentPeriod )
{
    LandNode::setCalLandAllocation( aLandType, aProductName, aCalLandUsed,
                                             aHarvestPeriod, aCurrentPeriod );
}

void TreeLandAllocator::setCalObservedYield( const string& aLandType,
                                             const string& aProductName,
                                             const double aCalObservedYield,
                                             const int aPeriod )
{
    LandNode::setCalObservedYield( aLandType, aProductName, aCalObservedYield, aPeriod );
}

void TreeLandAllocator::setIntrinsicRate( const string& aRegionName,
                                          const string& aLandType,
                                          const string& aProductName,
                                          const double aIntrinsicRate, 
                                          const int aPeriod )
{
    LandNode::setIntrinsicRate( aRegionName, aLandType, aProductName, aIntrinsicRate, aPeriod );
}

void TreeLandAllocator::setInitShares( double landAllocationAbove, int period ) {
	//Check that land allocations are valid
    double totalLandAllocated = 0;
	for ( unsigned int i = 0; i < children.size(); i++ ) {
		totalLandAllocated += children[ i ]->getTotalLandAllocation( children[ i ]->getName(), period );
	}
    
    if ( totalLandAllocated - landAllocation[ period ] > util::getSmallNumber() ) {
        const Modeltime* modeltime = scenario->getModeltime();
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Sum of land allocated is greater than the total land in " << name << " in "
                << modeltime->getper_to_yr( period ) << " by "
                << ( totalLandAllocated - landAllocation[ period ] ) / landAllocation[ period ] * 100
                << " %" << endl;
    }
    
	//Calculating the shares
	for ( unsigned int i = 0; i < children.size(); i++ ) {
		children[ i ]->setInitShares( landAllocation[ period ], period );
	}

	share[ period ] = 1;
}

void TreeLandAllocator::calcLandShares( const string& aRegionName,
                                        const double aSigmaAbove,
                                        const double aTotalLandAllocated,
                                        const int aPeriod )
{
	// First adjust value of unmanaged land nodes
	setUnmanagedLandValues( aRegionName, aPeriod );

	//Calculate the temporary unnormalized shares and sum them
	double unnormalizedSum = 0;
	// double excessShares = 0; (ignore this for now)
	double totalBaseLandAllocation = getBaseLandAllocation( aPeriod );
	for ( unsigned int i = 0; i < children.size(); i++ ) {
		// If this is managed land, or if unmanaged land and no base land allocation is specified then use standard method
		if ( isProductionLeaf() ||  totalBaseLandAllocation == 0 ) {
			children[ i ]->calcLandShares( aRegionName, sigma, 0, aPeriod );
		} else {
			// If this is unmanaged land then use initial land allocation to weight land use.
			children[ i ]->calcLandShares( aRegionName, sigma, totalBaseLandAllocation, aPeriod );
		}
        
        // Get temporary unnomalized share
		unnormalizedSum += children[ i ]->getShare( aPeriod );                  
	}

	//Normalizing the temporary unnormalized shares
	for ( unsigned int i = 0; i < children.size(); i++ ) {
		children[ i ]->normalizeLandAllocation( unnormalizedSum, aPeriod );
	}

	// The unnormalizedSum is actually the 1/sigma weighted intrinsic rates of the children
	// Therefore, the equation below gives the intrinsic rate of this node
	intrinsicRate[ aPeriod ] = pow( unnormalizedSum, sigma );
    
    // This is the root node so its share should be 100%   
	share[ aPeriod ] = 1;										 
}

/*! \brief Recursively calculates the landAllocation at each leaf and node using the shares.
* landAllocationAbove is passed the value of 0 at the root when this method is called,
*  so the value in landAllocation at the root will not be changed and be passed down recursively.
* \author Steve Smith, James Blackwood
*/
void TreeLandAllocator::calcLandAllocation( double landAllocationAbove, int period ) {
	for ( unsigned int i = 0; i < children.size(); i++ ) {
		children[ i ]->calcLandAllocation ( landAllocation[ period ], period );
	}
}

/*! \brief Calculate the land allocation of the entire nest.
* \param aRegionName Region name.
* \param aPeriod Model period.
*/
void TreeLandAllocator::calcFinalLandAllocation( const string& aRegionName, const int aPeriod ){
    calcLandShares( aRegionName, 0, 0, aPeriod );
    calcLandAllocation( 0, aPeriod );
}

void TreeLandAllocator::csvOutput( const string& aRegionName ) const {
    LandNode::csvOutput( aRegionName );
}


void TreeLandAllocator::dbOutput( const string& aRegionName ) const {
    LandNode::dbOutput( aRegionName );
}
	
void TreeLandAllocator::calcEmission( const string& aRegionName,
                                      const GDP* aGDP, 
                                      const int aPeriod )
{
    LandNode::calcEmission( aRegionName, aGDP, aPeriod );
}

void TreeLandAllocator::updateSummary( Summary& aSummary, const int aPeriod ){
    LandNode::updateSummary( aSummary, aPeriod );
}
