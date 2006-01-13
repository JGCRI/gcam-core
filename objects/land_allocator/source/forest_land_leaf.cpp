/*! 
* \file forest_land_leaf.cpp
* \ingroup CIAM
* \brief ForestLandLeaf class source file.
* \author James Blackwood
* \date $Date$
* \version $Revision$
*/

#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "land_allocator/include/forest_land_leaf.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

/*! \brief Default constructor.
* \author James Blackwood
*/
ForestLandLeaf::ForestLandLeaf(){
	const Modeltime* modeltime = scenario->getModeltime();
	int maxper = modeltime->getmaxper();
	rotationPeriod = 0;
	steps = -1;
	landToBeHarvested.resize( maxper );
}

//! Default destructor
ForestLandLeaf::~ForestLandLeaf() {
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author James Blackwood
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool ForestLandLeaf::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ){
    const Modeltime* modeltime = scenario->getModeltime();
	const int maxper = modeltime->getmaxper();

    // TODO: This isn't right as overrides won't work and it is parsing base
    // class items. This relies on XMLDerivedClassParse being called before
    // parsing the base class item.
	if( nodeName == "intrinsicRate" ) {
		intrinsicRate.clear();
		intrinsicRate.resize( maxper, XMLHelper<double>::getValue( curr ) );
    }
	else if( nodeName == "intrinsicYieldMode" ) {
		intrinsicYieldMode.clear();
		intrinsicYieldMode.resize( maxper, XMLHelper<double>::getValue( curr ) );
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
* \author James Blackwood
* \return The constant XML_NAME.
*/
const std::string& ForestLandLeaf::getXMLName() const {
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
const std::string& ForestLandLeaf::getXMLNameStatic() {
	const static string XML_NAME = "ForestLandAllocatorLeaf";
	return XML_NAME;
}

/*! \brief Complete the Initialization in the LandAllocator.
* rotationPeriod is passed through regionInfo which is then used to calculate the number of steps.
* It is assumed that time steps are constant for all periods.
* \author James Blackwood
*/
void ForestLandLeaf::completeInit( const string& aRegionName, 
                                   const IInfo* aRegionInfo )
{
	const Modeltime* modeltime = scenario->getModeltime();
	int maxper = modeltime->getmaxper();

	rotationPeriod = aRegionInfo->getInteger( "rotationPeriod", true );
	steps = rotationPeriod / modeltime->gettimestep(0);
    
    // reset yield and land variables to be larger
    yield.resize( maxper + steps );
    landToBeHarvested.resize( maxper + steps );
}

void ForestLandLeaf::calcLandAllocation( double aLandAllocationAbove, int aPeriod ){
    // Call standard land allocation.
    LandLeaf::calcLandAllocation( aLandAllocationAbove, aPeriod );

    // Calculate the land allocation for the harvest period defined by this
    // period and the constant rotation period. First calculate land already
    // allocated to be harvested in the future.
    double forestLandAside = 0;
    for( int i = aPeriod + 1; i < aPeriod + steps; ++i ){
        forestLandAside += landToBeHarvested[ i ];
    }
    
    // Calculate the amount of land to be harvested per year.
    const Modeltime* modeltime = scenario->getModeltime();
    double annualizedLand = landAllocation[ aPeriod ] / modeltime->gettimestep( aPeriod );

    // Store the land to be harvested in the future.
    landToBeHarvested[ aPeriod + steps ] = max( annualizedLand - forestLandAside, 0.0 );
}

/*! \brief Return annual amount of land allocated to forest production
* When harvestPeriod is equal to period + steps (i.e., for forests being planted now to be
* harvested later), the method returns the amount of total forest land allocation
* that is free for future forest production.  This is done by subtracting
* the land set aside for future forests from the total land allocated to forests.
* When harvestPeriod is less than period, this method will return the forest land that has been 
* set aside previously to be harvested in that specific harvestPeriod.
* The landAllocation variable is the total land allocated, while landToBeHarvested is an annual value.
* It is assumed that time steps are constant for all periods.
* \author James Blackwood, Steve Smith
* \return annual amount of land allocated to forest production
*/
double ForestLandLeaf::getLandAllocation( const string& aProductName,
                                                   const int aPeriod ) const
{
    if ( aProductName == name ) {
        return landToBeHarvested[ aPeriod ];
    }
    return 0;
}

/*! \brief Get total land allocation.
*
* Return total land allocated to forest in rotation 
* This is all land that has been committed to forestry over the rotation period
*
* \author Steve Smith
* \return the LandAllocation at this node
*/
double ForestLandLeaf::getTotalLandAllocation( const string& productName, int period ) {
	return LandLeaf::getLandAllocation( productName, period );
}

/*! \brief Set the amount of initial calibrated land allocated for this leaf.
*
* Sets the land allocated for this land in through a passed in value.
*
* \author James Blackwood
* \param calLandUsed annual amount of land harvested in harvestYear
* \param landType the landtype (node) where this land is located
* \param productName product produced by this land (name of this land leaf)
* \param harvestYear the year this product will be harvested
* \param currentYear the current year in which these values are being set
* \warning This method assumes that it will only be called once for each land type. 
* \todo Need to move summation to another method.
* \todo Need fix to reset land allocation when calibrations are overwritten.
*/
void ForestLandLeaf::setCalLandAllocation( const string& aLandType,
                                                    const string& aProductName,
                                                    const double aCalLandUsed,
                                                    const int aHarvestPeriod, 
                                                    const int aCurrentPeriod )
{
	const Modeltime* modeltime = scenario->getModeltime();
	assert( aProductName == name );
	landToBeHarvested[ aHarvestPeriod ] = aCalLandUsed;
 
   // Only add land to land allocation for future periods (current land is now free for reallocation)
	if ( aHarvestPeriod > aCurrentPeriod ) {
		landAllocation[ aCurrentPeriod ] += aCalLandUsed * modeltime->gettimestep( aCurrentPeriod );
	}
    //reset land allocation if current period. KLUDGE. NEED TO FIX THIS. sjsTEMP
    // Problem occurs, for example, when this routine is called in 1975 and this value is set for 1990. 
    // In 1990 this value needs to be reset to zero. The fix below relies on this routine being called in order.
    // Need a better solution.
    else {
        landAllocation[ aCurrentPeriod ] = 0;
    }
}

/*! \brief Set calibrated observed yield for this land.
*
* Sets the calibrated observed yield for this land in through a passed in value.
*
* \author James Blackwood
*/
void ForestLandLeaf::setCalObservedYield( const string& aLandType,
                                                   const string& aProductName,
                                                   const double aCalObservedYield,
                                                   const int aPeriod )
{
	assert( aProductName == name );
	calObservedYield[ aPeriod ] = yield[ aPeriod ] = aCalObservedYield;
}

void ForestLandLeaf::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    LandLeaf::toDebugXMLDerived( period, out, tabs );
    // Write out land to be harvested into the future as well as it may change.
    const Modeltime* modeltime = scenario->getModeltime();
    for( int i = period; i < modeltime->getmaxper(); ++i ){
        XMLWriteElement( landToBeHarvested[ i ], "landToBeHarvested", out, tabs, modeltime->getper_to_yr( i ) );
    }
}
