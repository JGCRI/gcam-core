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
* Copyright 2011 Battelle Memorial Institute.  All Rights Reserved.
* Distributed as open-source under the terms of the Educational Community 
* License version 2.0 (ECL 2.0). http://www.opensource.org/licenses/ecl2.php
* 
* For further details, see: http://www.globalchange.umd.edu/models/gcam/
*
*/


#include "util/base/include/definitions.h"
#include "containers/include/iinfo.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "technologies/include/residue_biomass_output.h"
#include "util/base/include/ivisitor.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/xml_parse_helper.h"
#include "util/base/include/TValidatorInfo.h"
#include "util/curves/include/point_set_curve.h"
#include "util/curves/include/curve.h"
#include "util/curves/include/explicit_point_set.h"
#include "util/curves/include/xy_data_point.h"
#include "land_allocator/include/aland_allocator_item.h"
#include "containers/include/market_dependency_finder.h"

#include <cstdio>

using namespace std;

extern Scenario* scenario;

ResidueBiomassOutput::ResidueBiomassOutput( const std::string& sectorName )
{
    mName = sectorName;
    mProductLeaf = 0;
    mHarvestIndex = 0;
    mErosCtrl = 0;
    mMassConversion = 0;
    mWaterContent = 0;
    mMassToEnergy = 0;
    mCostCurve = 0;
}

ResidueBiomassOutput::~ResidueBiomassOutput( ) 
{
    delete mCostCurve;
}

ResidueBiomassOutput* ResidueBiomassOutput::clone() const {
    ResidueBiomassOutput* clone = new ResidueBiomassOutput( mName );
    clone->copy( *this );
    return clone;
}

void ResidueBiomassOutput::copy( const ResidueBiomassOutput& aOther ) {
    mName = aOther.mName;
    mCachedCO2Coef = aOther.mCachedCO2Coef;
    mProductLeaf = aOther.mProductLeaf;
    mHarvestIndex = aOther.mHarvestIndex;
    mErosCtrl = aOther.mErosCtrl;
    mMassConversion = aOther.mMassConversion;
    mMassToEnergy = aOther.mMassToEnergy;
    mWaterContent = aOther.mWaterContent;
    
    delete mCostCurve;
    mCostCurve = aOther.mCostCurve ? aOther.mCostCurve->clone() : 0;
    
    // note results are not copied.
}

/*! \brief Get the XML name for reporting to XML file.
*
* This public function accesses the private constant string, XML_NAME. This way
* the tag is always consistent for reporting outputs and can be easily
* changed.
* \author Sonny Kim
* \return The constant XML_NAME.
*/
const string& ResidueBiomassOutput::getXMLReportingName() const{
    static const string XML_REPORTING_NAME = "output-residue-biomass";
    return XML_REPORTING_NAME;
}

const string& ResidueBiomassOutput::getXMLName() const{
    return getXMLNameStatic();
}

void ResidueBiomassOutput::accept( IVisitor* aVisitor, const int aPeriod ) const
{
   if ( aVisitor )
   {
      aVisitor->startVisitOutput( this, aPeriod );
      aVisitor->endVisitOutput( this, aPeriod );
   }
}

/*!
 * \brief Calculates the crop or forest residue bioenergy production
 * \details Calculates the potential bioenergy production which
 *          is equal to the energy content of the primary output of 
 *          the crop or forest less the biomass needed to prevent
 *          erosion. The fraction of the potential bioenergy that is
 *          actually produced is determined by the bioenergy price.
 *          Returns the residue bioenergy produced at the current
 *          market price and primary output quantity
 * \param aPrimaryOutput Primary Production for the Parent Technology
 * \param aRegionName Region
 * \param aCaptureComponent Emissions Capture Component for the Parent Technology
 * \param aPeriod Model Period
 * \return Biomass Production
 */
IOutput::OutputList ResidueBiomassOutput::calcPhysicalOutput( const double aPrimaryOutput,
                                                              const std::string& aRegionName,
                                                              const ICaptureComponent* aCaptureComponent,
                                                              const int aPeriod ) const 
{
    // create the list to return and default it to zero
    OutputList outputList;
    outputList.push_back( make_pair( mName, 0 ) );

    // If primary output is less than or equal to zero, then residue biomass production
    // is zero.
    if ( aPrimaryOutput <= 0 ) {
        return outputList;
    }

    const Marketplace* marketplace = scenario->getMarketplace();
    double price = marketplace->getPrice( getName(), aRegionName, aPeriod, true );

    // If there is no market price, return
    if ( price == Marketplace::NO_MARKET_PRICE ) {
        return outputList;
    }

    // If there is an erosion control parameter, then we need
    // the land area to calculate the biomass needed to prevent
    // erosion. ( mErosCtrl is measured in tonnes/kHa ).
    double landArea = 0;
    if ( mErosCtrl > 0 ) {
        landArea = mProductLeaf->getLandAllocation( "", aPeriod );
    }
   
    // Compute the amount of crop produced in tonnes
    // Primary Output is in billion m^3 or in ECal
    // mMassConversion is in tonnes/billion m^3 or tonnes/ECal
    double cropMass = aPrimaryOutput * mMassConversion;

    // Compute the above-ground biomass that is not used for food
    // production. Harvest index is the fraction of total biomass produced for food
    // Thus, the amount of biomass available for bioenergy production
    // is food production * [ ( 1 / harvest index ) - 1 ]
    double resMass;
    if ( mHarvestIndex > 0.0 ) {
        resMass = cropMass * ( std::pow( mHarvestIndex, double( -1 ) ) - double( 1 ) );
    }
    else {
        resMass = 0.0;
    }

    // Compute the mass of residue that are required to sustain
    // agriculture in terms of soil nutrients and erosion 
    // mErosCtrl is in tonnes/kHa, landArea is in kHa
    // So, meanErosCtrl is in tonnes
    double meanErosCtrl = landArea * mErosCtrl;

    // Compute the amount of residue biomass available to the energy sector
    // The amount available is the total biomass not produced for food
    // less the amount of biomass needed for erosion control
    // multiply by moisture content to get DRY residue available; residue from model is wet
    double resAvail = (1 - mWaterContent) * ( resMass - meanErosCtrl);

    // If there is no biomass available for production after
    // erosion control demands are met, then return. Residue
    // biomass production is zero.
    if ( resAvail <= 0 ) {
        return outputList;
    }

    // Compute energy content of the available biomass 
    // resAvail is measured in tonnes
    // mMassToEnergy is measured in EJ/tonne
    // maxBioEnergySupply is measured in EJ
    double maxBioEnergySupply = resAvail * mMassToEnergy;

    // Compute the fraction of the total possible supply that is
    // produced at the current biomass price 
    double fractProduced = mCostCurve->getY( price );

    // Compute the quantity of a crop residue biomass produced
    double resEnergy = maxBioEnergySupply * fractProduced;

    outputList.front().second = resEnergy;
    return outputList;
}

void ResidueBiomassOutput::completeInit( const std::string& aSectorName, const std::string& aRegionName,
                                         const IInfo* aTechInfo, const bool aIsTechOperating )
{
    #if !defined( _MSC_VER )
        using std::exit;
    #endif

    // If erosion control is positive, but a land allocator
    // has not been assigned, then print an error message
    // Erosion control equations need land allocations.
    if ( !mProductLeaf  && mErosCtrl > 0 ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid land allocator for " << getXMLNameStatic()
                << " in sector " << aSectorName << std::endl;
        exit( -1 );
    }

    // Validate input parameters
    typedef ObjECTS::TValidatorInfo<> validator_type;
    validator_type validator[] = {
                    validator_type( mCostCurve->getMaxY(), "max-harvest-fraction",
                                    mCostCurve->getMaxY() <= 1 ),
                    validator_type( mErosCtrl, "eros-ctrl", mErosCtrl >= 0 ),
                    validator_type( mHarvestIndex, "harvest-index", mHarvestIndex >= 0 ),
                    validator_type( mMassConversion, "mass-conversion", mMassConversion >= 0 ),
                    validator_type( mMassToEnergy, "mass-to-energy", mMassToEnergy >= 0 ) };
    unsigned short numParams = sizeof( validator ) / sizeof( validator[0] );
    std::string msg = ObjECTS::getInvalidNames( &validator[0], &validator[numParams] );

    // If input parameters are invalid, print an error message and exit
    if ( msg.length() ) {
        // Invalid input parameter
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::ERROR );
        mainLog << "Invalid input parameter(s) to " << getXMLNameStatic()
                << " in sector " << aSectorName << ": " << msg << std::endl;
        exit( -1 );
    }
    
    scenario->getMarketplace()->getDependencyFinder()->addDependency( aSectorName,
                                                                      aRegionName,
                                                                      getName(),
                                                                      aRegionName );
}

double ResidueBiomassOutput::getEmissionsPerOutput( const std::string& aGHGName, const int aPeriod ) const
{
    // Currently other GHGs do not use output emissions coefficients.
    assert( aGHGName == "CO2" );
    return mCachedCO2Coef;
}

double ResidueBiomassOutput::getPhysicalOutput( const int aPeriod ) const
{
    /*if ( !mPhysicalOutputs.size() ) {
        const Modeltime* modeltime = scenario->getModeltime();
        mPhysicalOutputs.resize( modeltime->getmaxper() );
    }*/

    return mPhysicalOutputs[ aPeriod ];
}

double ResidueBiomassOutput::getValue( const std::string& aRegionName,
                                       const ICaptureComponent* aCaptureComponent,
                                       const int aPeriod ) const
{
    // TODO: consider adding the residue biomass value to crop value
    // First, need to change residue biomass output as per unit of land
    // in order to prevent a simultaneity.  Then, we can include this value.
    return 0;
}

const std::string& ResidueBiomassOutput::getXMLNameStatic( void )
{
    static const std::string XMLName = "residue-biomass-production";
    return XMLName;
}

void ResidueBiomassOutput::initCalc( const std::string& aRegionName, const std::string& aSectorName,
                                     const int aPeriod )
{
    assert( scenario != 0 );
    const Marketplace* marketplace = scenario->getMarketplace();
    const IInfo* productInfo = marketplace->getMarketInfo( getName(), aRegionName, aPeriod, false );

    mCachedCO2Coef.set( productInfo ? productInfo->getDouble( "CO2Coef", false ) : 0 );
}

void ResidueBiomassOutput::postCalc( const std::string& aRegionName, const int aPeriod )
{
   // Not used
}

void ResidueBiomassOutput::scaleCoefficient( const double aScaler )
{
   // Not used
}

void ResidueBiomassOutput::sendLandAllocator( const ILandAllocator* aLandAllocator,
                                              const std::string& aName )
{
    // TODO: maybe the technology should just pass the product leaf
    mProductLeaf = const_cast<ILandAllocator*>( aLandAllocator )->findProductLeaf( aName );
}

void ResidueBiomassOutput::setPhysicalOutput( const double aPrimaryOutput, const std::string& aRegionName,
                                              ICaptureComponent* aCaptureComponent, const int aPeriod )
{
    // Set the physical output for the specified period
    // calcPhysicalOutput only returns a single value.
    OutputList outputList = calcPhysicalOutput( aPrimaryOutput, aRegionName, aCaptureComponent, aPeriod );
    mPhysicalOutputs[ aPeriod ].set( outputList.front().second );

    // Add output to the supply
    Marketplace* marketplace = scenario->getMarketplace();
    marketplace->addToSupply( getName(), aRegionName, mPhysicalOutputs[ aPeriod ],
            aPeriod, true );
}

void ResidueBiomassOutput::toDebugXML( const int aPeriod, std::ostream& aOut, Tabs* aTabs ) const
{
    XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs, getName() );
    if ( aPeriod < mPhysicalOutputs.size() ) {
        XMLWriteElement( mPhysicalOutputs[ aPeriod ], "output", aOut, aTabs );
    }
    XMLWriteElement( mErosCtrl, "eros-ctrl", aOut, aTabs );
    XMLWriteElement( mHarvestIndex, "harvest-index", aOut, aTabs );
    XMLWriteElement( mMassConversion, "mass-conversion", aOut, aTabs );
    XMLWriteElement( mMassToEnergy, "mass-to-energy", aOut, aTabs );
    XMLWriteElement( mWaterContent, "water-content", aOut, aTabs );
    
    
    const vector<pair<double,double> > pairs = mCostCurve->getSortedPairs();
    typedef vector<pair<double, double> >::const_iterator PairIterator;
    map<string, double> attrs;
    for( PairIterator currPair = pairs.begin(); currPair != pairs.end(); ++currPair ) {
        attrs[ "price" ] = currPair->first;
        XMLWriteElementWithAttributes( currPair->second, "fract-harvested", aOut, aTabs, attrs );
    }
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}

bool ResidueBiomassOutput::XMLParse( rapidxml::xml_node<char>* & aNode ) {
    string nodeName = XMLParseHelper::getNodeName(aNode);
    if(nodeName == "fract-harvested") {
        // we need some specialized behavior to read in the point set curve
        PointSet* curvePoints;
        if(!mCostCurve) {
            curvePoints = new ExplicitPointSet();
            mCostCurve = new PointSetCurve(curvePoints);
        }
        else {
            curvePoints = mCostCurve->getPointSet();
        }
        map<string, string> attrs = XMLParseHelper::getAllAttrs(aNode);
        double price = XMLParseHelper::getValue<double>( attrs["price"] );
        double fraction = XMLParseHelper::getValue<double>( aNode );
        XYDataPoint* currPoint = new XYDataPoint( price, fraction );
        curvePoints->addPoint( currPoint );
        return true;
    }
    else {
        // return false to indicate XMLParseHelper should try to parse aNode
        return false;
    }
}

string ResidueBiomassOutput::getOutputUnits( const string& aRegionName ) const {
    return scenario->getMarketplace()->getMarketInfo( getName(), aRegionName, 0, true )
        ->getString( "output-unit", false );
}

void ResidueBiomassOutput::doInterpolations( const int aYear, const int aPreviousYear,
                                             const int aNextYear, const IOutput* aPreviousOutput,
                                             const IOutput* aNextOutput )
{
    // TODO: what to interpolate?
}

// end of residue_biomass_output.cpp ***********************************


