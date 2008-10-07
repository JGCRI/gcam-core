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
 * \file building_generic_dmd_technology.cpp
 * \ingroup Objects
 * \brief BuildingGenericDmdTechnology technology source file.
 * \author Steve Smith, Josh Lurz
 */

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>

#include "technologies/include/building_generic_dmd_technology.h"
#include "util/base/include/xml_helper.h"
#include "containers/include/scenario.h"
#include "functions/include/iinput.h"
#include "util/logger/include/ilogger.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"
#include "containers/include/info_factory.h"
#include "functions/include/function_manager.h"
#include "functions/include/function_utils.h"
#include "technologies/include/iproduction_state.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

typedef vector<IInput*>::iterator InputIterator;
typedef vector<IInput*>::const_iterator CInputIterator;

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for both read-in and output and can be easily
* changed. The "==" operator that is used when parsing, required this second
* function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const string& BuildingGenericDmdTechnology::getXMLNameStatic1D() {
	const static string XML_NAME1D = "building-demand-technology";
	return XML_NAME1D;
}

/*!
 * \brief Constructor
 * \param aName Technology name.
 * \param aYear Technology year.
 */
BuildingGenericDmdTechnology::BuildingGenericDmdTechnology( const string& aName, const int aYear )
: Technology( aName, aYear ){
}

/*!
 * \brief Destructor
 */
BuildingGenericDmdTechnology::~BuildingGenericDmdTechnology() {
}

/*!
 * \brief Copy constructor.
 * \param aTechnology Building demand technology to copy.
 */
BuildingGenericDmdTechnology::BuildingGenericDmdTechnology( const BuildingGenericDmdTechnology& aOther ):
Technology( aOther ){
    mAveInsulation = aOther.mAveInsulation;
    mFloorToSurfaceArea = aOther.mFloorToSurfaceArea;

    // TODO: Can't currently copy info objects. This should not be a problem as
    // copying is done before completeInit is called.
}

BuildingGenericDmdTechnology* BuildingGenericDmdTechnology::clone() const {
	return new BuildingGenericDmdTechnology( *this );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& BuildingGenericDmdTechnology::getXMLName1D() const {
	return getXMLNameStatic1D();
}

void BuildingGenericDmdTechnology::completeInit( const string& aRegionName,
                                                 const string& aSectorName,
                                                 const string& aSubsectorName,
                                                 DependencyFinder* aDepFinder,
                                                 const IInfo* aSubsectorInfo,
                                                 ILandAllocator* aLandAllocator,
                                                 const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // Construct the info object before calling Technology::completeInit so that
    // the info object can be used in the Technology.
    mInfo.reset( InfoFactory::constructInfo( aSubsectorInfo, aRegionName + "-" + aSectorName + "-" + 
        aSubsectorName + "-" + mName ) );
    
    if( util::isEqual( mAveInsulation, 0.0 ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Input variable aveInsulationis 0. Reset to 1." << endl;
        mAveInsulation = 1;
    }

    if( util::isEqual( mFloorToSurfaceArea, 0.0 ) ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Input variable floorToSurfaceArea is 0. Reset to 1." << endl;
        mFloorToSurfaceArea = 1;
    }
    
    // Add the average insulation value and floor to surface area ratio.
    mInfo->setDouble( "average-insulation", mAveInsulation );
    mInfo->setDouble( "floor-to-surface-area", mFloorToSurfaceArea );

    Technology::completeInit( aRegionName, aSectorName, aSubsectorName, 
                              aDepFinder, aSubsectorInfo, aLandAllocator,
                              aGlobalTechDB );
}

void BuildingGenericDmdTechnology::initCalc( const string& aRegionName,
                                             const string& aSectorName,
                                             const IInfo* aSubsectorInfo,
                                             const Demographic* aDemographics,
                                             PreviousPeriodInfo& aPrevPeriodInfo,
                                             const int aPeriod )
{
    Technology::initCalc( aRegionName, aSectorName, aSubsectorInfo,
                          aDemographics, aPrevPeriodInfo, aPeriod );

    // Check coefficients in the initial year of the technology.
    if( mProductionState[ aPeriod ]->isNewInvestment() ){
        checkCoefficients( aSectorName, aPeriod );
    }
}


void BuildingGenericDmdTechnology::postCalc( const string& aRegionName, const int aPeriod ) {
    Technology::postCalc( aRegionName, aPeriod );
}

bool BuildingGenericDmdTechnology::XMLDerivedClassParse( const string& aNodeName, const DOMNode* aCurr ) {
    if( aNodeName == "aveInsulation" ){
        mAveInsulation = XMLHelper<double>::getValue( aCurr );
    } 
    else if( aNodeName == "floorToSurfaceArea" ){
        mFloorToSurfaceArea = XMLHelper<double>::getValue( aCurr );
    }
    else {
        return false;
    }
    return true;
}

void BuildingGenericDmdTechnology::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    XMLWriteElementCheckDefault( mAveInsulation, "aveInsulation", out, tabs, 0.0 );
    XMLWriteElementCheckDefault( mFloorToSurfaceArea, "floorToSurfaceArea", out, tabs, 1.0 );
}	

void BuildingGenericDmdTechnology::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const { 
    XMLWriteElement( mAveInsulation, "aveInsulation", out, tabs );
    XMLWriteElement( mFloorToSurfaceArea, "floorToSurfaceArea", out, tabs );
}	

/*!
* \brief calculate technology unnormalized shares
* \details Building technologies are really just calculating demands for
*          specific services so shares are always 1. This ensures that sector
*          price is correctly calculated.
* \author Steve Smith
* \param aRegionName Region name.
* \param aSectorName Sector name.
* \param aGDP Regional GDP container.
* \param aPeriod Model period.
* \return The building demand technology share which is always one.
*/
double BuildingGenericDmdTechnology::calcShare( const string& aRegionName,
                                                const string& aSectorName,
                                                const GDP* aGDP,
                                                const int aPeriod ) const
{
    return Technology::calcShare( aRegionName, aSectorName, aGDP, aPeriod );
}

/*! \brief Adjusts technology parameters as necessary to be consistent with calibration value.
*
* For these demand "technologies" the unitDemand needs to be adjusted so that output
* is consistent with calibrated input demand. This version works for demands that do not take into account internal gains.
*
* \author Steve Smith
* \param aTechnologyDemand calibrated unit demand (demand per unit floorspace) for this subsector
* \param aRegionName regionName
* \param aSubSectorInfo Info object (not used for this class so name is left out) 
* \param aPeriod model period
*/
void BuildingGenericDmdTechnology::adjustForCalibration( const double aDemand,
                                                         const double aCalibratedDemand,
                                                         const bool aIsOnlyTechnology,
                                                         const string& aRegionName,
                                                         const string& aSectorName,
                                                         const IInfo* aSubsectorInfo,
                                                         const int aPeriod )
{
    // Only adjust for calibration in the new investment period.
    if( !mProductionState[ aPeriod ]->isNewInvestment() ){
        return;
    }
    

    // Default technology calibration is required for adjusting technology
    // shareweights so that the building shell received the correct amount of
    // demand.
    Technology::adjustForCalibration( aDemand,
                                      aCalibratedDemand,
                                      aIsOnlyTechnology,
                                      aRegionName,
                                      aSectorName,
                                      aSubsectorInfo,
                                      aPeriod );

    adjustCoefsForCalibration( aDemand, aRegionName, aPeriod );
}

void BuildingGenericDmdTechnology::adjustCoefficients( const string& aRegionName,
                                                       const string& aSectorName,
                                                       const int aPeriod )
{
    // Override this function to prevent the base technology from adjusting coefficients
    // based on a leontief assumption.
    // TODO: Find a cleaner way to do this.
}

void BuildingGenericDmdTechnology::production( const string& aRegionName,
                                               const string& aSectorName,
											   double aVariableDemand,
                                               double aFixedOutputScaleFactor,
											   const GDP* aGDP, const int aPeriod )
{
    Technology::production( aRegionName, aSectorName, aVariableDemand,
                            aFixedOutputScaleFactor, aGDP, aPeriod );
}

void BuildingGenericDmdTechnology::calcCost( const string& aRegionName,
                                             const string& aSectorName,
											 const int aPeriod )
{
	Technology::calcCost( aRegionName, aSectorName, aPeriod );
}

double BuildingGenericDmdTechnology::getTotalInputCost( const string& aRegionName,
                                                        const string& aSectorName,
														const int aPeriod ) const 
{
	return Technology::getTotalInputCost( aRegionName, aSectorName, aPeriod );
}

const IInfo* BuildingGenericDmdTechnology::getTechInfo() const {
    return mInfo.get();
}

const IFunction* BuildingGenericDmdTechnology::getProductionFunction() const {
    return FunctionManager::getFunction( "minicam-price-elasticity" );
}

/*!
 * \brief Adjust the input coefficients to scale them such that output will
 *        equal calibrated output.
 * \details Loops through the inputs and adjusts the coefficient of each energy
 *          input. The coefficient is adjusted so the input demand for the given
 *          output is equal to the calibrated demand.
 * \param aDemand Demand for the output of the technology.
 * \param aRegionName Name of the containing region.
 * \param aPeriod Model period.
 */
void BuildingGenericDmdTechnology::adjustCoefsForCalibration( const double aDemand,
                                                              const string& aRegionName,
                                                              const int aPeriod )
{
    int normPeriod = SectorUtils::getDemandNormPeriod( aPeriod );

    // Adjust coefficients of the inputs if the total calibrated output is
    // known.
    // TODO: Make this function agnostic to production function choice.
    for( InputIterator i = mInputs.begin(); i != mInputs.end(); ++i ){
        // Skip non-energy inputs.
        if( !(*i)->hasTypeFlag( IInput::ENERGY ) ){
            continue;
        }

        double calOutput = (*i)->getCalibrationQuantity( aPeriod );

        double priceRatio = SectorUtils::calcPriceRatio( aRegionName, (*i)->getName(),
                                                         normPeriod, aPeriod );
        double adjustedDemand = aDemand * pow( priceRatio, (*i)->getPriceElasticity() );
        // Calculate a coefficient that would cause actual demand to equal
        // calibrated demand.
        double coef = adjustedDemand > util::getSmallNumber() ? calOutput / adjustedDemand : 0;
        (*i)->setCoefficient( coef / mAlphaZero, aPeriod );

        // Double check things worked correctly.
        assert( util::isEqual( coef / mAlphaZero * aDemand * pow( priceRatio, ( *i )->getPriceElasticity() ),
                               calOutput ) );
    }
}

/*!
 * \brief Error check the input coefficients to ensure they have been
 *        initialized correctly.
 * \details Prints a warning if the input coefficients have not been
 *          initialized. The input is valid if:
 *              - It has a calibration value, which will cause the input to be
 *                adjusted
 *              - It read-in a coefficient adjustment
 *              - It copied a coefficient adjustment from the previous period.
 *              - It is a non-energy input.
 * \note The check for coefficient greater than one does not directly check for
 *       a non-unity coefficient adjustment but should be correct most of the
 *       time.
 * \param aSectorName Name of the containing sector.
 * \param aPeriod Model period.
 */
void BuildingGenericDmdTechnology::checkCoefficients( const string& aSectorName,
                                                      const int aPeriod ) const
{
    for( CInputIterator i = mInputs.begin(); i != mInputs.end(); ++i ){
        if( ( *i )->hasTypeFlag( IInput::ENERGY ) &&
            ( *i )->getCalibrationQuantity( aPeriod ) == -1 &&
            ( *i )->getCoefficient( aPeriod ) >= 1 )
        {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Input " << ( *i )->getName() << " in technology " << mName
                << " with year " << year << " in sector " << aSectorName
                << " has no calibration quantity and a coefficient greater than one."
                << endl;
        }
    }
}
