/*! 
* \file demand_sector.cpp
* \ingroup Objects
* \brief DemandSector class source file.
* \author Sonny Kim
*/

#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cmath>
#include <xercesc/dom/DOMNode.hpp>
#include "util/base/include/xml_helper.h"

#include "sectors/include/demand_sector.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "util/base/include/summary.h"
#include "sectors/include/subsector.h"
#include "containers/include/scenario.h"
#include "containers/include/gdp.h"
#include "util/base/include/ivisitor.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
// static initialize.
const string DemandSector::XML_NAME = "demandsector";

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
DemandSector::DemandSector( const string aRegionName ): Sector( aRegionName ){
    perCapitaBased = false;
    pElasticityBase = 0;
    
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    service.resize( maxper );
    iElasticity.resize( maxper );
    pElasticity.resize( maxper );
    aeei.resize( maxper );
    techChangeCumm.resize( maxper, 1 );
}

//! Default destructor
DemandSector::~DemandSector() {
}

/*! \brief Override the calculation of the final supply price to do nothing currently.
* \details Does not do anything.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void DemandSector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
}

/*! \brief Override the setting of supply to not produce any output.
* \details Demand sectors to do not produce any output.
* \author Josh Lurz
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period.
*/
void DemandSector::supply( const GDP* aGDP, const int aPeriod ) {
}

/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class.
*
* \author Josh Lurz, Steve Smith
* \param nodeName The name of the curr node. 
* \param curr pointer to the current node in the XML input tree
*/
bool DemandSector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    
    const Modeltime* modeltime = scenario->getModeltime();
    
    if( nodeName == "priceelasticity" ) {
        XMLHelper<double>::insertValueIntoVector( curr, pElasticity, modeltime );
    }
    else if( nodeName == "serviceoutput" ){
        XMLHelper<double>::insertValueIntoVector( curr, service, modeltime );
    }
    else if( nodeName == "incomeelasticity" ){
        XMLHelper<double>::insertValueIntoVector( curr, iElasticity, modeltime );
    }
    else if( nodeName == "aeei" ) {
        XMLHelper<double>::insertValueIntoVector( curr, aeei, modeltime );
    }
    else if( nodeName == "perCapitaBased" ) {
        perCapitaBased = XMLHelper<bool>::getValue( curr );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void DemandSector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  
    const Modeltime* modeltime = scenario->getModeltime();
   
    // write the xml for the class members.
    XMLWriteElementCheckDefault( perCapitaBased, "perCapitaBased", out, tabs, false );

    for( unsigned int i = 0; i < pElasticity.size(); i++ ){
        XMLWriteElementCheckDefault( pElasticity[ i ], "priceelasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( int i = 0; modeltime->getper_to_yr( i ) <= 1990; i++ ){
        XMLWriteElementCheckDefault( service[ i ], "serviceoutput", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < iElasticity.size(); i++ ){
        XMLWriteElementCheckDefault( iElasticity[ i ], "incomeelasticity", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
    for( unsigned int i = 0; i < aeei.size(); i++ ){
        XMLWriteElementCheckDefault( aeei[ i ], "aeei", out, tabs, 0.0, modeltime->getper_to_yr( i ) );
    }
}   

//! Write object to debugging xml output stream.
void DemandSector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( pElasticityBase, "pElasticityBase", out, tabs );
    XMLWriteElementCheckDefault( perCapitaBased, "perCapitaBased", out, tabs, false );
    XMLWriteElement( techChangeCumm[ period ], "techChangeCumm", out, tabs );
    
    // Now write out own members.
    XMLWriteElement( getEnergyInput( period ), "TFE", out, tabs );
    XMLWriteElement( outputsAllFixed( period ), "OutputAllFixed", out, tabs );
    XMLWriteElement( service[ period ], "service", out, tabs );
    XMLWriteElement( getCalOutput( period ), "TotalCalOutput", out, tabs );
    XMLWriteElement( service[ period ] * techChangeCumm[ period ], "servicePreTechChange", out, tabs );
    XMLWriteElement( iElasticity[ period ], "iElasticity", out, tabs );
    XMLWriteElement( pElasticity[ period ], "pElasticity", out, tabs );
    XMLWriteElement( aeei[ period ], "aeei", out, tabs );
    
    // Write out the summary
    // summary[ period ].toDebugXML( period, out );
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overriden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const std::string& DemandSector::getXMLName() const {
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
const std::string& DemandSector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief Create new market for this Sector
*
* Normal demand sectors do not set any markets so this is blank.
*
* \author Steve Smith
*/
void DemandSector::setMarket() {    
}

//! Calibrate sector output
/* This performs supply sector technology and sub-sector output/input calibration. 
   Determines total amount of calibrated and fixed output and passes that down to the subsectors.
   
   The first part of this code is similar to that for the supply sectors. 
   The second portion is specific to demand sectors. 
   For demand sectors, the aggregate output is assured to be the same as the summed calibrated output from the technologies
   
   If all subsector demands are calibrated (or zero) then also adjusts AEEI in order to be 
   consistent with calibrated values.
* \author Steve Smith
* \param regionName region name
* \param period Model period
*/
void DemandSector::calibrateSector( const int period ) {
    double totalCalOutputs = getCalOutput( period );
    double marketDemand;
    
    if ( outputsAllFixed( period ) ) {
        marketDemand = getCalOutput( period ); // If all outputs are calibrated, then make demand equal to calibrated outputs for consistancy
    }
    else {
        marketDemand = getService( period ); // demand for the good produced by this sector
    }
    
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        if ( subsec[i]->getCalibrationStatus( period ) ) {
            subsec[i]->adjustForCalibration( marketDemand, 0, totalCalOutputs, outputsAllFixed( period ), period );
        }
    }

    // If outputs are all fixed then scale AEEI for aggregate demand function to match this output so that all parameters 
    // are consistent. This can happen many times during the calculation.
    if ( outputsAllFixed( period ) ) {
        if ( getService( period ) == 0 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Service less than or equal to zero in demand sector " << name << " in region " << regionName << endl;
        } 
        else {
            double scaleFactor = totalCalOutputs/getService( period );
            scaleOutput( period , scaleFactor );
        }
    }
}

/*! \brief Returns the weighted energy price.
* \details Returns the price of the sector weighted by the output in the base
*          period.
* \return The weighted energy price.
*/
double DemandSector::getWeightedEnergyPrice ( const int aPeriod ) const {
	return getOutput( 0 ) * getPrice( aPeriod );
}

//! Set output for Sector (ONLY USED FOR energy service demand at present).
/*! Demand from the "dmd" parameter (could be energy or energy service) is passed to subsectors.
This is then shared out at the technology level.
In the case of demand, what is passed here is the energy service demand. 
The technologies convert this to an energy demand.
The demand is then summed at the subsector level (subsec::sumOutput) then
later at the Sector level (in region via supplysector[j].sumOutput( per ))
to equal the total Sector output.
*
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param demand Demand to be passed to the subsectors. (Sonny check this)
* \param period Model period
* \param gdp GDP object uses to calculate various types of GDPs.
*/
void DemandSector::setOutput( const double demand, const GDP* gdp, const int period ) {
    assert( util::isValidNumber( demand ) && demand >= 0 );

    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        // set subsector output from Sector demand
        subsec[ i ]->setOutput( demand, gdp, period );
    }
}

//! scale output by changing value of some scale parameters (used for calibration)
/*! The scaleFactor is the amount by which the output needs to change. 
    The routine then calculates the necessary change in the AEEI. 
    
    \warning For derived demand sectors, some version of this routine needs to be included
    in order for the output of that sector to be able to be calibrated. sjs
* \author Steve Smith
* \param scaleFactor amount by which to scale output from this sector
* \param period Model period
* \todo need to generalize scaling and aggdemand function so that multiple periods can be calibrated if necessary
* \todo clean-up code below after merging (left for now to ease merge)
*/
void DemandSector::scaleOutput( int period, double scaleFactor ) {
    const Modeltime* modeltime = scenario->getModeltime();

    // The solution for the scaling factor for AEEI (Afact), is
    // SF = TC_0 * (1+AEII)^T / [ TC_0 * (1+Afact*AEII)^T ] = (1+AEII)^T /[(1+Afact*AEII)^T]
    // So Afact = [( (1+AEII)^T /SF )^(1/T)-1]/AEII
    // TC_0 = techChangeCumm[period-1] & SF = scaleFactor

    // If scale factor is significant then change AEEI if there is one
    if ( fabs( 1 - scaleFactor ) > 1e-6 && aeei[ period ] != 0 ) { 
        double aeeiSave = aeei[ period ];
        double timeStep = modeltime->gettimestep(period);
        double temp = pow( 1+aeei[period] , timeStep );
        double aeeiScale = ( pow( temp/ scaleFactor ,1/timeStep ) - 1) / aeei[ period ];  // amount to change AEEI

        aeei[ period ]  = aeei[ period ] * aeeiScale;

        ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
        calibrationLog.setLevel( ILogger::DEBUG );
        calibrationLog << "scaled demand sector output for " << regionName << ":" << name <<" by " << scaleFactor << " using AEEI." << endl;

        if ( !util::isValidNumber( aeei[ period ] ) ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << "Attempt to scale AEEI to an invalid value of "<< aeei[ period ] << " in sector "<< name <<" in region : " << regionName << endl;
            mainLog << "    AEEI not changed." << endl;
            aeei[ period ]  = aeeiSave;
        }
    }
    else if ( fabs( 1 - scaleFactor ) > 1e-6 && aeei[ period ] == 0 ) {
        // If AEEI is zero, then must scale base output instead.

        if( service[ 0 ] == 0 ) {
            mBaseOutput *= scaleFactor;
        }
        else {
            service[ 0 ] *= scaleFactor;
        }

        ILogger& calibrationLog = ILogger::getLogger( "calibration_log" );
        calibrationLog.setLevel( ILogger::DEBUG );
        calibrationLog << "Scaled demand sector output for " << regionName << " " << name
                       << " by " << scaleFactor << " using period 0 base output." << endl;
    }
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period
* (instead of every iteration) should be placed in this function.
*
* \author James Blackwood
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the DemandSector.
*/
void DemandSector::initCalc( NationalAccount& nationalAccount, const Demographic* aDemographics, const int aPeriod )
{
	Sector::initCalc( nationalAccount, aDemographics, aPeriod );
	
    // If no output has been specified for period zero (mBaseOutput), then can never calibrate.
    // So set mBaseOutput to calibrated value to provide non-zero value from which to scale.
    // Only do the initialization once
	if ( mBaseOutput == 0 && ( getCalOutput( aPeriod ) != 0 ) && ( getOutput( 0 ) == 0 ) ) {
		mBaseOutput = getCalOutput( aPeriod );
	}
}

/*! \brief Complete the initialization of a demand sector.
* \param aRegionInfo Regional information object.
* \param aDependencyFinder The region's dependency finder.
* \param aLandAllocator Regional land allocator.
*/
void DemandSector::completeInit( const IInfo* aRegionInfo,
                                 DependencyFinder* aDependencyFinder,
                                 ILandAllocator* aLandAllocator )
{
    Sector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator );
    pElasticityBase = pElasticity[ 0 ]; // Store the base year price elasticity.

    // Check to see if demand sector has the same name as a market.
    // TODO: This check relies on SupplySector completeInit occurring first.
    Marketplace* marketplace = scenario->getMarketplace();
    const Modeltime* modeltime = scenario->getModeltime();
    if( marketplace->getPrice(name, regionName, modeltime->getBasePeriod(), false ) != Marketplace::NO_MARKET_PRICE ) {
        ILogger& mainLog = ILogger::getLogger( "main_log" );
        mainLog.setLevel( ILogger::WARNING );
        mainLog << "Demand sector " << name << " in region " << regionName 
                << " appears to have the same name as an exiting market. This may cause an error. " << endl;
    }
}

/*! \brief Calculate end-use service price elasticity
*
*
* \author Sonny Kim
* \param period Model period
* \todo Sonny to add more to this description
*/
void DemandSector::calcPriceElasticity( int period ){
    if( period > 0 ){
        double sectorFuelCost = 0;
        for ( unsigned int i = 0; i < subsec.size(); ++i ) {
            sectorFuelCost += subsec[ i ]->getwtfuelprice( period );
        }
        double tmpPriceRatio = getPrice( period ) / sectorFuelCost;
        pElasticity[ period ] = pElasticityBase * tmpPriceRatio;
    }
}

/*! \brief Aggrgate sector energy service demand function
*
* Function calculates the aggregate demand for energy services and passes that down to the sub-sectors. 
* Demand is proportional to either GDP (to a power) or GDP per capita (to a power) times population.
*
* \author Sonny Kim
* \param gdp GDP object for calculating various types of gdps.
* \param period Model period
* \todo Sonny to add more to this description if necessary
* \pre Sector price attribute must have been previously calculated and set (via calcPrice)
*/
void DemandSector::aggdemand( const GDP* gdp, const int period ) {
    const Modeltime* modeltime = scenario->getModeltime();
    double serviceDemand;
    double pelasticity = pElasticity[period];
    const double base = getOutput(0);
    // demand for service
    if (period == 0) {
        serviceDemand = base; // base output is initialized by data
        techChangeCumm[period] = 1; // base year technical change
    }
    else {
        const int normPeriod = modeltime->getyr_to_per(1990);
        double priceRatio = 1;
        if( period > normPeriod ){
            assert( getPrice( normPeriod ) > 0 );
            priceRatio = getPrice( period ) / getPrice( normPeriod );
            assert( util::isValidNumber( priceRatio ) );
        }

        const int basePer = modeltime->getBasePeriod();
        double gdpRatio = gdp->getGDP( period ) / gdp->getGDP( basePer );
        // If perCapitaBased, service_demand = B * P^r * GDPperCap^r * Population.
        // All values are relative to the base year
        if ( perCapitaBased ) { // demand based on per capita GDP
            double scaledGDPperCap = gdp->getScaledGDPperCap( period );
            serviceDemand = base*pow(priceRatio,pelasticity)*pow(scaledGDPperCap,iElasticity[period]);
            // need to multiply above by population ratio (current population/base year
            // population).  This ratio provides the population ratio.
            serviceDemand *= gdpRatio/scaledGDPperCap;
        }
        // If not perCapitaBased, service_demand = B * P^r * GDP^r
        else { // demand based on scale of GDP    

            serviceDemand = base*pow( priceRatio, pelasticity )*pow( gdpRatio, iElasticity[period] );
        }

        // calculate cummulative technical change using AEEI, autonomous end-use energy intensity
        // it would be much faster to calculate this in initCalc if we knew aeei was not changing.
        techChangeCumm[period] = techChangeCumm[period-1]*pow(1+aeei[period],modeltime->gettimestep(period));
    }
    
    // demand sector output is total end-use sector demand for service
    // adjust demand using cummulative technical change
    assert( techChangeCumm[ period ] > 0 );
    assert( util::isValidNumber( serviceDemand ) && serviceDemand >= 0 );
    service[ period ] = serviceDemand / techChangeCumm[ period ];

    // sets subsector outputs, technology outputs, and market demands
    setOutput( service[ period ], gdp, period );
}

//! Write sector output to database.
void DemandSector::csvOutputFile() const {
    // function protocol
    void fileoutput3( string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total Sector output
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    fileoutput3( regionName, getName(), " ", " ", "production", "SerUnit", service );
    // total Sector eneryg input
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getInput( per );
    }
    fileoutput3( regionName, getName(), " ", " ", "consumption", "EJ", temp );
    // Sector price
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getPrice( per );
    }
    fileoutput3( regionName, getName(), " ", " ", "price", "$/Service", temp );
    // Sector carbon taxes paid
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getTotalCarbonTaxPaid( per );
    }
    fileoutput3( regionName, getName(), " ", " ", "C tax paid", "Mil90$", temp );
}

//! Write MiniCAM style demand sector output to database.
void DemandSector::dbOutput() const {
    const Modeltime* modeltime = scenario->getModeltime();
    int m;
    const int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    
    const string secname = Sector::getName();
    string str; // temporary string
    
    // total sector output
    dboutput4(regionName,"End-Use Service","by Sector",secname,"Ser Unit",service);
    dboutput4(regionName,"End-Use Service",secname,"zTotal","Ser Unit",service);
    dboutput4(regionName,"End-Use Service",secname+"_bySubsec","zTotal","Ser Unit",service);
    for ( int i = 0; i < maxper; i++ ) {
        temp[ i ] = service[ i ] * techChangeCumm[ i ];
    }
    dboutput4(regionName,"End-Use Service","by Sector w/o TC",secname,"Ser Unit", temp );

    // End-use service price elasticity
    dboutput4(regionName,"End-Use Service","Elasticity",secname + "_price"," ",pElasticity);
    // End-use service income elasticity
    dboutput4(regionName,"End-Use Service","Elasticity",secname + "_income"," ",iElasticity);
    
    // TFE for this demand sector
    for (m=0;m<maxper;m++) {
        temp[m] = getEnergyInput( m );
    }
    dboutput4(regionName,"Final Energy Cons",name,"zTotal","EJ",temp);

    // sector fuel consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tfuelmap = Sector::getfuelcons(m=0);
    // Write out total (zTotal) fuel consumption for each sector only.
    if( !tfuelmap.empty() ){
        CI fmap = --tfuelmap.end();
        for (m=0;m<maxper;m++) {
            temp[m] = Sector::getConsByFuel(m,fmap->first);
        }
        dboutput4(regionName,"Fuel Consumption",secname,fmap->first,"EJ",temp);
        dboutput4(regionName,"Fuel Consumption","by End-Use Sector",secname,"EJ",temp);
        // output for zTotal gets written for each demand sector and dataviewer sums it up
        dboutput4(regionName,"Fuel Consumption","by End-Use Sector","zTotal","EJ",temp);
    }
    
    // sector emissions for all greenhouse gases
    map<string,double> temissmap = summary[0].getemission(); // get gases for period 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(regionName,"Emissions","Sec-"+secname,gmap->first,"MTC",temp);
    }
    
    // CO2 emissions by sector
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    dboutput4(regionName,"CO2 Emiss","by Sector",secname,"MTC",temp);
    dboutput4(regionName,"CO2 Emiss",secname,"zTotal","MTC",temp);
    
    // CO2 indirect emissions by sector
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emindmap_second("CO2");
    }
    dboutput4(regionName,"CO2 Emiss(ind)",secname,"zTotal","MTC",temp);
    
    // sector price (not normalized)
    for (m=0;m<maxper;m++) {
        temp[m] = getPrice( m );
    }
    dboutput4(regionName,"Price",secname,"zSectorAvg","75$/Ser",temp);
    
    // sector price normalized to base price
    for (m=0;m<maxper;m++) {
        temp[m] = getPrice( m ) / getPrice( 0 );
    }
    dboutput4(regionName,"Price","by End-Use Sector",secname,"Norm75",temp);
    
    // sector carbon taxes paid
    for (m=0;m<maxper;m++) {
        temp[m] = Sector::getTotalCarbonTaxPaid(m);
    }
    dboutput4(regionName,"General","CarbonTaxPaid",secname,"$",temp);
    
    // do for all subsectors in the sector
    MCoutput_subsec();
}

//! Write out subsector results from demand Sector.
void DemandSector::MCoutput_subsec() const {
    // do for all subsectors in the Sector
    for ( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->MCoutputDemandSector();
        subsec[ i ]->MCoutputAllSectors();
    }
}

/*! \brief returns the demand sector service supplied.
*
*
* \author Sonny Kim
* \param period Model period
* \return amount of energy service supplied by this sector
*/
double DemandSector::getService( const int period ) const {
    return service[period];
}

/*! \brief Get the output for the demand sector, which is equivalent to the demand.
* \param aPeriod Period to get the output for.
* \return Output for the period.
* \authod Josh Lurz
*/
double DemandSector::getOutput( int aPeriod ) const {
    // In the base period return a read in output if there is none.
    if( aPeriod == 0 && service[ aPeriod ] == 0 ){
        return mBaseOutput;
    }
    return service[ aPeriod ];
}

/*! \brief Return the price of the DemandSector.
* \details The price of a DemandSector is the weighted average subsector price.
* \param aPeriod Model period.
* \return Price.
* \todo Remove this calculation once DemandSectors are rewritten.
*/
double DemandSector::getPrice( const int aPeriod ) const {
    return Sector::getPrice( aPeriod );
}

/*! \brief returns the demand sector service before tech change is applied.
*
* This is useful for debugging and output, but is not used by the model itself at this point
*
* \author Sonny Kim
* \param period Model period
* \return energy service demand before technological change is applied
*/
double DemandSector::getServiceWoTC( const int period ) const {
    return service[ period ] * techChangeCumm[ period ];
}

void DemandSector::accept( IVisitor* aVisitor, const int aPeriod ) const{
	aVisitor->startVisitDemandSector( this, aPeriod );
	Sector::accept( aVisitor, aPeriod );
	aVisitor->endVisitDemandSector( this, aPeriod );
}
