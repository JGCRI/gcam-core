/*! 
* \file technology.cpp
* \ingroup CIAM
* \brief technology class source file.
* \author Sonny Kim
* \date $Date$
* \version $Revision$
*/
// Standard Library headers
#include "util/base/include/definitions.h"
#include <string>
#include <iostream>
#include <cassert>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMNodeList.hpp>

// User headers
#include "technologies/include/technology.h"
#include "emissions/include/ghg.h"
#include "emissions/include/ghg_output.h"
#include "containers/include/scenario.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/marketplace.h"
#include "emissions/include/indirect_emiss_coef.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// Technology class method definition

//! Default constructor.
technology::technology() {
    initElementalMembers();
}

// ! Destructor
technology::~technology() {
    for( vector<Ghg*>::iterator iter = ghg.begin(); iter != ghg.end(); iter++ ) {
        delete *iter;
    }
}

technology::technology( const technology& techIn ) {
    copy( techIn );
}

//! Assignment operator.
technology& technology::operator =( const technology& techIn ) {
    
    if( this != &techIn ) { // check for self assignment 
        clear();
        copy( techIn );
    }
    return *this;
}

//! Helper copy function to avoid replicating code.
void technology::copy( const technology& techIn ) {
    year = techIn.year;
    shrwts = techIn.shrwts;
    eff = techIn.eff; 
    effBase = techIn.effBase; 
    effPenalty = techIn.effPenalty; 
    intensity = techIn.intensity; 
    necost = techIn.necost;
    neCostBase = techIn.neCostBase;
    neCostPenalty = techIn.neCostPenalty;
    fuelcost = techIn.fuelcost;
    techcost = techIn.techcost;
    tax = techIn.tax;
    fMultiplier = techIn.fMultiplier;
    pMultiplier = techIn.pMultiplier;
    carbontax = techIn.carbontax;
    carbontaxgj = techIn.carbontaxgj;
    carbontaxpaid = techIn.carbontaxpaid;
    lexp = techIn.lexp;
    share = techIn.share;
    input = techIn.input;
    output = techIn.output;
    techchange = techIn.techchange;
    fixedSupply = techIn.fixedSupply;
    fixedOutputVal = techIn.fixedOutputVal;
    name = techIn.name;
    unit = techIn.unit;
    fuelname = techIn.fuelname;
    doCalibration = techIn.doCalibration;
    calInputValue = techIn.calInputValue;
    emissmap = techIn.emissmap; 
    emfuelmap = techIn.emfuelmap; 
    emindmap = techIn.emindmap; 
    calInputValue = techIn.calInputValue;
    carbonValue = techIn.carbonValue;
    
    resource = techIn.resource;
    A = techIn.A;
    B = techIn.B;
    ghgNameMap = techIn.ghgNameMap; 
    
    for (vector<Ghg*>::const_iterator iter = techIn.ghg.begin(); iter != techIn.ghg.end(); iter++) {
        ghg.push_back( new Ghg( **iter ) );
    }
}


//! Clear member variables.
void technology::clear(){
    initElementalMembers();
    name = "";
    unit = "";
    fuelname = "";
    ghg.clear();
    emissmap.clear();
    emfuelmap.clear();
    emindmap.clear();
    ghgNameMap.clear();
    
    // Delete the GHGs to avoid a memory leak.
    for( vector<Ghg*>::iterator iter = ghg.begin(); iter != ghg.end(); iter++ ) {
        delete *iter;
    }
}

//! Initialize elemental data members.
void technology::initElementalMembers(){
    year = 0;
    shrwts = 1;
    eff = 1; 
    effBase = 1; 
    effPenalty = 0; 
    intensity = 1; 
    fuelcost = 0;
    necost = 0;
    neCostBase = 0;
    neCostPenalty = 0;
    techcost = 0;
    tax = 0;
    fMultiplier = 1; 
    pMultiplier = 1; 
    carbontax = 0;
    carbontaxgj = 0;
    carbontaxpaid = 0;
    lexp = -6; 
    share = 0;
    input = 0;
    output = 0;
    techchange = 0;
    A = 0;
    B = 0;
    resource = 0;
    fixedSupply = 0; // initialize to no fixed supply
    fixedOutputVal = 0;
    doCalibration = false;
    doCalOutput = false;
    calInputValue = 0;
    carbonValue = 0;
}

//! initialize technology with xml data
void technology::XMLParse( const DOMNode* node )
{	
    Ghg* tempGhg = 0;
    DOMNode* curr = 0;
    string nodeName;
    DOMNodeList* nodeList;
    
    /*! \pre Assume we are passed a valid node. */
    assert( node );
    
    nodeList = node->getChildNodes();
    
    for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ) {
        curr = nodeList->item( i );
        nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );		
        
        if( nodeName == "#text" ) {
            continue;
        }
        
        else if( nodeName == "name" ) {
            name = XMLHelper<string>::getValueString( curr );
            
#if( _DEBUG )
            // cout << "\t\t\tTechnology name set as " << name << endl;
#endif
        } 
        else if( nodeName == "year" ){
            year = XMLHelper<int>::getValue( curr );
        }
        else if( nodeName == "fuelname" ){
            fuelname = XMLHelper<string>::getValueString( curr );
        }
        else if( nodeName == "sharewt" ){
            shrwts = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "calInputValue" ){
            calInputValue = XMLHelper<double>::getValue( curr );
            doCalibration = true;
        }
        else if( nodeName == "calOutputValue" ){
            calOutputValue = XMLHelper<double>::getValue( curr );
            doCalibration = true;
            doCalOutput = true;
        }
        else if( nodeName == "efficiency" ){
            effBase = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "efficiencyPenalty" ){
            effPenalty = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "nonenergycost" ){
            neCostBase = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "neCostPenalty" ){
            neCostPenalty = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "tax" ){
            tax = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "pMultiplier" ){
            pMultiplier = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "fMultiplier" ){
            fMultiplier = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "logitexp" ){
            lexp = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "techchange" ){
            techchange = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "resource" ){
            resource = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "A" ){
            A = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "B" ){
            B = XMLHelper<double>::getValue( curr );
        }
        else if( nodeName == "GHG" ){
            parseContainerNode( curr, ghg, ghgNameMap, new Ghg() );
        }
        else if( nodeName == "GHG_OUTPUT" ){
            parseContainerNode( curr, ghg, ghgNameMap, new GhgOutput() );
        }
        // parse derived classes
        else {
            XMLDerivedClassParse( nodeName, curr );
        }
        
    }
}

//! Parses any input variables specific to derived classes
void technology::XMLDerivedClassParse( const string nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
    cout << "Unrecognized text string: " << nodeName << " found while parsing technology." << endl;
}


//! Complete initialization
void technology::completeInit() {
    if( ghg.empty() ) {
        // arguments: gas, unit, remove fraction, GWP, and emissions coefficient
        // for CO2 this emissions coefficient is not used
        Ghg* CO2 = new Ghg( "CO2", "MTC", 0, 1, 0 ); // at least CO2 must be present
        ghg.push_back( CO2 );
        ghgNameMap[ "CO2" ] = 0;
    }
	// calculate effective efficiency
	eff = effBase * (1 - effPenalty); // reduces efficiency by penalty
	necost = neCostBase * (1 + neCostPenalty); // increases cost by penalty
}


//! write object to xml output stream
void technology::toXML( ostream& out, Tabs* tabs ) const {
    
    tabs->writeTabs( out );
    out << "<period year=\"" << year << "\">" << endl;
    
    tabs->increaseIndent();
    
    // write the xml for the class members.
    
    XMLWriteElement( name, "name", out, tabs );
    XMLWriteElement( year, "year", out, tabs );
    
    XMLWriteElementCheckDefault( shrwts, "sharewt", out, tabs, 1 );
    
    if (doCalibration) {
        XMLWriteElement( calInputValue, "calInputValue", out, tabs );
    }
    
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElementCheckDefault( effBase, "efficiency", out, tabs, 1 );
    XMLWriteElementCheckDefault( effPenalty, "efficiencyPenalty", out, tabs, 0 );
    XMLWriteElementCheckDefault( neCostBase, "nonenergycost", out, tabs, 0 );
	XMLWriteElementCheckDefault( neCostPenalty, "neCostPenalty", out, tabs, 0 );
    XMLWriteElementCheckDefault( tax, "tax", out, tabs, 0 );
    XMLWriteElementCheckDefault( fMultiplier, "fMultiplier", out, tabs, 1 );
    XMLWriteElementCheckDefault( pMultiplier, "pMultiplier", out, tabs, 1 );
    XMLWriteElementCheckDefault( lexp, "logitexp", out, tabs, -6 );
    XMLWriteElementCheckDefault( techchange, "techchange", out, tabs, 0 );
    XMLWriteElementCheckDefault( resource, "resource", out, tabs, 0 );
    XMLWriteElementCheckDefault( A, "A", out, tabs, 0 );
    XMLWriteElementCheckDefault( B, "B", out, tabs, 0 );
    
    for( vector<Ghg*>::const_iterator ghgIter = ghg.begin(); ghgIter != ghg.end(); ghgIter++ ){
        ( *ghgIter )->toXML( out, tabs );
    }
    
    // finished writing xml for the class members.
    
    tabs->decreaseIndent();
    
    tabs->writeTabs( out );
    out << "</period>" << endl;
}

//! write object to xml debugging output stream
void technology::toDebugXML( const int period, ostream& out, Tabs* tabs ) const {
    
    tabs->writeTabs( out );
    out << "<technology name=\"" << name << "\" year=\"" << year << "\">" << endl;
    
    tabs->increaseIndent();
    // write the xml for the class members.
    
    XMLWriteElement( unit, "unit", out, tabs );
    XMLWriteElement( fuelname, "fuelname", out, tabs );
    XMLWriteElement( shrwts, "sharewt", out, tabs );
    if (doCalibration) {
        XMLWriteElement( calInputValue, "calInputValue", out, tabs );
    }
    XMLWriteElement( eff, "efficiencyEffective", out, tabs );
    XMLWriteElement( effBase, "efficiencyBase", out, tabs );
    XMLWriteElement( effPenalty, "efficiencyPenalty", out, tabs );
    XMLWriteElement( fuelcost, "fuelcost", out, tabs );
    XMLWriteElement( necost, "nonEnergyCostEffective", out, tabs );
    XMLWriteElement( neCostBase, "neCostBase", out, tabs );
    XMLWriteElement( neCostPenalty, "neCostPenalty", out, tabs );
    XMLWriteElement( tax, "tax", out, tabs );
    XMLWriteElement( fMultiplier, "fMultiplier", out, tabs );
    XMLWriteElement( pMultiplier, "pMultiplier", out, tabs );
    XMLWriteElement( carbontax, "carbontax", out, tabs );
    XMLWriteElement( carbontaxgj, "carbontaxgj", out, tabs );
    XMLWriteElement( carbontaxpaid, "carbontaxpaid", out, tabs );
    XMLWriteElement( lexp, "logitexp", out, tabs );
    XMLWriteElement( share, "share", out, tabs );
    XMLWriteElement( output, "output", out, tabs );
    XMLWriteElement( input, "input", out, tabs );
    XMLWriteElement( techchange, "techchange", out, tabs );
    XMLWriteElement( resource, "resource", out, tabs );
    XMLWriteElement( A, "A", out, tabs );
    XMLWriteElement( B, "B", out, tabs );

    // write our ghg object, vector is of number of gases
    for( vector<Ghg*>::const_iterator i = ghg.begin(); i != ghg.end(); i++ ){
        ( *i )->toDebugXML( period, out, tabs );
    }
    
    // finished writing xml for the class members.
    
    tabs->decreaseIndent();
    
    tabs->writeTabs( out );
    out << "</technology>" << endl;
}

//! Perform initializations that only need to be done once per period
void technology::initCalc( ) {    
   if ( doCalOutput ) {
      calInputValue = calOutputValue/eff;
      doCalibration = true;
   }
   
   if ( calInputValue < 0 ) {
      cerr << "Calibration value < 0 for tech " << name << ". Calibration removed" << endl;
       doCalibration = false;
   }
}

//! sets ghg tax to technologies
/*! does not get called if there are no markets for ghgs */
void technology::addGhgTax( const string ghgname, const string regionName, const string sectorName, const int per ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // returns coef for primary fuels only
    // carbontax has value for primary fuels only
    carbonValue = 0; // initialize
    carbontax = marketplace->getPrice(ghgname,regionName,per);
    // add to previous ghg tax if more than one ghg
    // carbonValue and carbontax must be in same unit as fuel price
    for(int i=0;i< static_cast<int>( ghg.size() );i++) {
        carbonValue += ghg[i]->getGHGValue( regionName, fuelname, sectorName, eff, per);
    }
    // need to add taxes from all ghgs
}

/*! \brief Calculate technology fuel cost and total cost.
*
* This caculates the cost (per unit output) of this specific technology. 
* The cost includes fuel cost, carbon value, and non-fuel costs.
* Conversion efficiency, and optional fuelcost and total price multipliers are used if specified.
*
* Special check for "none" fuel is implimented here.
*
* \warning Check for "none" fueltype is implimented here. Need to find a better way do do this and renewable fuels.
* \author Sonny Kim, Steve Smith
* \param period Model regionName
* \param period Model per
*/
void technology::calcCost( const string regionName, const int per ) 
{
    Marketplace* marketplace = scenario->getMarketplace();

	 // code specical case where there is no fuel input. sjs
	 // used now to drive non-CO2 GHGs
    double fuelprice;
	 if ( fuelname != "none" ) {
		fuelprice = marketplace->getPrice(fuelname,regionName,per);
    } else {
		fuelprice = 0;
	 }
	 
    // code for integrating technical change
    //techcost = fprice/eff/pow(1+techchange,modeltime->gettimestep(per)) + necost;
    // fMultiplier and pMultiplier are initialized to 1 for those not read in
    fuelcost = ( fuelprice * fMultiplier ) / eff;
    techcost = ( fuelcost + necost ) * pMultiplier;
	techcost += carbonValue;
    
    /* \post fuelcost and techcost are greater than or equal to 0. */
	if(fuelcost < 0){
		cout << "fuelcost < 0"<<endl;
		cout << regionName <<","<<name <<","<<fuelname<<","<<fuelprice<<","<<fMultiplier<<","<<carbonValue<<endl;
		cout << "CO2EmFactor  "<< marketplace->getMarketInfo(fuelname,regionName,per,"CO2EmFactor")<<endl;
	}
	else if(techcost <= 0){
		cout << "techcost <= 0"<<endl;
		cout << regionName <<","<<name <<","<<fuelname<<","<<fuelprice<<","<<fMultiplier<<","<<carbonValue<<endl;
		cout << "CO2EmFactor  "<< marketplace->getMarketInfo(fuelname,regionName,per,"CO2EmFactor")<<endl;
	}
	assert( fuelcost >= 0 );
    assert( techcost >= 0 );
	
}

/*! \brief calculate technology unnormalized shares
*
*
* \author Sonny Kim
* \param regionName region name
* \param per model period
*/
void technology::calcShare( const string regionName, const int per)
{
    share = shrwts * pow(techcost,lexp);
}

/*! \brief normalizes technology shares
*
* \author Sonny Kim
* \param sum sum of sector shares
* \warning sum must be correct sum of shares
* \pre calcShares must be called first
*/
void technology::normShare(double sum) 
{
    if (sum==0) {
        share = 0;
    }
    else {
        share /= sum;
    }
}

/*! \brief This function sets the value of fixed supply
* This needs to be called only once per period. Sets the amount of fixed supply to either the read-in value or the "MiniCAM-style" formula used for hydro. 
* 
* A == Minicam HYDRO(1,L)
* B == Minicam HYDRO(2,L) ??
* resource == Minicam HYDRO(3,L)
*
* \author Sonny Kim, Steve Smith
* \param per model period
*/
void technology::calcFixedSupply(int per)
{
    const Modeltime* modeltime = scenario->getModeltime();
    const string FIXED_TECH = "hydro";
    
    // MiniCAM style hydro specification
    if( name == FIXED_TECH ) {
        const int T = per * modeltime->gettimestep( per );
        // resource and logit function 
        const double fact = exp( A + B * T );
        output = fixedOutputVal = resource * fact / ( 1 + fact );
        fixedSupply = fixedOutputVal;
    }
    
    // Data-driven specification
    if ( fixedSupply > 0 ) {
        fixedOutputVal = fixedSupply;
    }
}


/*! \brief This function resets the value of fixed supply to the maximum value
* See calcFixedSupply
*
* \author Steve Smith
* \param per model period
*/
void technology::resetFixedSupply( int per ) {
    fixedOutputVal = fixedSupply;
}

/*! \brief Return fixed technology supply
* 
* returns the current value of fixedSupply. This may differ from the variable fixedSupply due to scale down if demand is less than the value of fixedSupply.
*
* \author Steve Smith
* \param per model period
* \return value of fixed output for this technology
*/
double technology::getFixedSupply() const {
    return fixedOutputVal;
}

/*! \brief Scale fixed technology supply
* 
* Scale down fixed supply. Used if total fixed production is greater than actual demand.
*
* \author Steve Smith
* \param scaleRatio multipliciative value to scale fixed supply
*/
void technology::scaleFixedSupply(const double scaleRatio)
{
  //  string FixedTech = "hydro";
    // dmd is total subsector demand
    if( fixedOutputVal != 0 ) {
        output *= scaleRatio;
        fixedOutputVal *= scaleRatio;
    }
}

/*! \brief Adjusts shares to be consistant with any fixed production
* 
* Adjust shares to account for fixed supply. Sets appropriate share for fixed supply and scales shares of other technologies appropriately.
*
* \author Steve Smith
* \param subsecdmd subsector demand
* \param subsecFixedSupply total amount of fixed supply in this subsector
* \param varShareTot Sum of shares that are not fixed
* \param per model period
* \warning This version may not work if more than one (or not all) technologies within each sector 
has a fixed supply
*/
void technology::adjShares(double subsecdmd, double subsecFixedSupply, double varShareTot, int per)
{
    double remainingDemand = 0;
    
    if(subsecFixedSupply > 0) {
        remainingDemand = subsecdmd - subsecFixedSupply;
        if (remainingDemand < 0) {
            remainingDemand = 0;
        }
        
        if ( fixedOutputVal > 0 ) {	// This tech has a fixed supply
            if (subsecdmd > 0) {
                share = fixedOutputVal/subsecdmd;
                // Set value of fixed supply
                if (fixedOutputVal > subsecdmd) {
                    fixedOutputVal = subsecFixedSupply; // downgrade output if > fixedsupply
                }  
            }
            else {
                share = 0;
            }
        }
        else {	// This tech does not have fixed supply
            if (subsecdmd > 0) {
                share = share * (remainingDemand/subsecdmd)/varShareTot;
            }
            else {
                // Maybe not correct, but if other params are zero then something else is wrong
                share = 0; 
            }
        }
    }    
}

//! Calculates fuel input and technology output.
/*! Adds demands for fuels and ghg emissions to markets in the marketplace
*/
void technology::production(const string& regionName,const string& prodName,
                            double dmd,const int per) {
    string hydro = "hydro";
    Marketplace* marketplace = scenario->getMarketplace();
    
    // dmd is total subsector demand
    if(name != hydro) {
        output = share * dmd; // use share to get output for each technology
    }
    else { // do for hydroelectricity
        //output = fixedOutputVal;
        output = fixedOutputVal = dmd;
    }
    
    // eliminated renewable branch for input calc, since code was the same. sjs
    // non renewable technologies previously had
    //input = output/eff/pow(1+techchange,timestep);
    input = output/eff;
	   
    if (input < 0) {
        cerr << "ERROR: Output value < 0 for technology " << name << endl;
    }
    
    // set demand for fuel in marketplace
    marketplace->addToDemand(fuelname,regionName,input,per);
    
    // total carbon taxes paid for reporting only
    // carbontax and carbontaxpaid is null for technologies that do not consume fossil fuels
    // input(EJ), carbontax(90$/GJ), carbontaxpaid(90$Mil)
    carbontaxpaid = input*carbonValue*1e+3;
    
    // calculate emissions for each gas after setting input and output amounts
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        ghg[i]->calcEmission(regionName, fuelname,input,prodName,output);
        // set emissions as demand side of gas market
        marketplace->addToDemand(ghg[i]->getName(),regionName,ghg[i]->getEmission(),per);		
        // set sequestered amount as demand side of carbon storage market
        marketplace->addToDemand("carbon storage",regionName,ghg[i]->getSequestAmountGeologic(),per);		
    }
}

//! calculate GHG emissions from technology use
void technology::calcEmission( const string prodname ) {
    // alternative ghg emissions calculation
    emissmap.clear(); // clear emissions map
    emfuelmap.clear(); // clear emissions map
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        // emissions by gas name only
        emissmap[ghg[i]->getName()] = ghg[i]->getEmission();
        // emissions by gas and fuel names combined
        // used to calculate emissions by fuel
        emissmap[ghg[i]->getName() + fuelname] = ghg[i]->getEmission();
        // add sequestered amount to emissions map
        // used to calculate emissions by fuel
        emissmap[ghg[i]->getName() + "sequestGeologic"] = ghg[i]->getSequestAmountGeologic();
        emissmap[ghg[i]->getName() + "sequestNonEngy"] = ghg[i]->getSequestAmountNonEngy();
        
        // emfuelmap[ghg[i]->getName()] = ghg[i]->getEmissFuel();
        // This really should include the GHG name as well.
        emfuelmap[fuelname] = ghg[i]->getEmissFuel();
    }
}

//! calculate indirect GHG emissions from technology use
void technology::indemission( const vector<Emcoef_ind>& emcoef_ind )
{
    emindmap.clear(); // clear emissions map
    for (int i=0; i< static_cast<int>( ghg.size() ); i++) {
        ghg[i]->calcIndirectEmission(input,fuelname, emcoef_ind );
        emindmap[ghg[i]->getName()] = ghg[i]->getEmissInd();
    }
}

/*! \brief Returns technology name
*
* \author Sonny Kim
* \return sector name as a string
*/
string technology::getName() const {
    return name;
}

/*! \brief Returns name of the fuel consumed by this technology
*
* \author Sonny Kim
* \return fuel name as a string
*/
string technology::getFName() const {
    return fuelname;
}

/*! \brief Returns the ratio of output to input for this technology
*
* \author Sonny Kim
* \return efficiency (out/In) of this technology
*/
double technology::getEff() const {
    return eff;
}

/*! \brief Return fuel intensity (input over output) for this technology
*
* \author Sonny Kim
* \return fuel intensity (input/output) of this technology
* \todo Need to impliment method of adding appropriate units (btu/kwh; gallons/mile, etc.)
*/
double technology::getIntensity(const int per) const {
    return intensity;
}

/*! \brief returns share for this technology
*
* \author Sonny Kim
* \pre calcShare
* \return share value
*/
double technology::getShare() const {
    return share;
}

/*! \brief Returns calibration status for this technoloy
*
* This is true if a calibration value has been read in for this technology.
* 
* \author Steve Smith
* \return Boolean that is true if technoloy is calibrated
*/
bool technology::getCalibrationStatus( ) const {
    return doCalibration;
}

//! returns true if all output is either fixed or calibrated
bool technology::ouputFixed( ) const {
    bool outputFixed = false;

   if ( doCalibration || ( fixedSupply != 0 ) ) {
      outputFixed = true;  // this sector has fixed output
   } 
   
   return outputFixed;
   
}

//! return fuel input for technology
double technology::getInput() const {
    return input;
}

//! return output of technology
double technology::getOutput() const {
    return output;
}

//! return technology fuel cost only
double technology::getFuelcost( ) const {
    return fuelcost;
}

//! return technology calibration value
double technology::getCalibrationInput( ) const {
    return calInputValue;
}

//! return technology calibration value
void technology::scaleCalibrationInput( const double scaleFactor ) {
    if ( scaleFactor != 0 ) {
        calInputValue = calInputValue / scaleFactor;
    }
}

//! return technology calibration value
double technology::getCalibrationOutput( ) const {
    return calInputValue * eff;
}

//! return the cost of technology
double technology::getTechcost() const {
    return techcost;
}

//! return the non-energy cost of the technology
double technology::getNecost() const {
    return necost;
}

//! return carbon taxes applied to technology
double technology::getCarbontax() const {
    // ($/TC)
    return carbontax;
}

//! return carbon taxes applied to technology
double technology::getCarbontaxgj() const {
    // ($/GJ)
    return carbontaxgj;
}

//! return any carbon tax and storage cost applied to technology
double technology::getCarbonValue() const {
    // (75$/GJ)
    return carbonValue;
}

//! return carbon taxes paid by technology
double technology::getCarbontaxpaid() const {
    return carbontaxpaid;
}

//! returns actual CO2 emissions from technology, alternate
double technology::getCO2()  const {
    return ghg[0]->getEmission(); // index 0 is for CO2
}

/*! \brief Return a vector listing the names of all the GHGs within the Technology.
* \detailed This function returns all GHG names the Technology contains. It does 
* this by searching the underlying ghgNameMap.
* \author Josh Lurz
* \return A vector of GHG names contained in the Technology.
*/
const vector<string> technology::getGHGNames() const {
    return util::getKeys( ghgNameMap );
}

/*! \brief Return a GHG emissions coefficient for a given GHG name.
* This function searches the mapping of GHG names to values and
* returns the emissions coefficient from the GHG with the given name,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \return The emissions coefficient of the GHG with ghgName, -1 if the GHG does not exist.
*/
double technology::getGHGEmissionCoef( const std::string& ghgName ) const {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
    double emissCoef;

    // Need to perform error checking b/c the searchForValue function will return 0 if the name
    // is not found or if the correct element is at position 1. This handles the first case. 
    if( ( ghgIndex == 0 ) && ( ( ghg.size() == 0 ) || ( ghg[ 0 ]->getName() != ghgName ) ) ){
        // A ghg with the passed in name does not exist.
        emissCoef = -1;
    }
    else {
        emissCoef = ghg[ ghgIndex ]->getEmissCoef();
    }
    return emissCoef;
}

/*! \brief sets the emissions coefficient for a GHG specified byname.
* This function searches the mapping of GHG names to values and
* returns the emissions coefficient from the GHG with the given name,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \return The emissions coefficient of the GHG with ghgName, -1 if the GHG does not exist.
*/
void technology::setGHGEmissionCoef( const std::string& ghgName, const double emissionsCoef ) {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );

    // Need to perform error checking b/c the searchForValue function will return 0 if the name
    // is not found or if the correct element is at position 1. This handles the first case. 
    if( ( ghgIndex == 0 ) && ( ( ghg.size() == 0 ) || ( ghg[ 0 ]->getName() != ghgName ) ) ){
        // A ghg with the passed in name does not exist.
    }
    else {
         ghg[ ghgIndex ]->setEmissCoef( emissionsCoef );
    }
}

/*! \brief Return the flag that tells if the GHG had an emissions coefficient read in
* This function searches the mapping of GHG names to values and
* returns the appropriate flag,
* or -1 if there is not a GHG with the given name.
* \todo Eliminate this function and getGHGNames when technology is converted to a multi-timeperiod structure.
* \param ghgName The name of a GHG to return the emissions coefficient for.
* \warning Assumes there is only one GHG object with any given name
* \warning No error checking is possible for this function. Error checking is done in function getGHGEmissionCoef.
* \return A boolean that indicates if the GHG with the given name had an emissions read-in.
*/
bool technology::getEmissionsInputStatus( const std::string& ghgName ) const {
    const int ghgIndex = util::searchForValue( ghgNameMap, ghgName );
	 
	 return ghg[ ghgIndex ]->getEmissionsInputStatus();
}

//! return map of all ghg emissions
map<string,double> technology::getemissmap() const {
    return emissmap;
}

//! return map of all ghg emissions
map<string,double> technology::getemfuelmap() const {
    return emfuelmap;
}

//! return map of all ghg emissions
map<string,double> technology::getemindmap() const {
    return emindmap;
}

//! return value for ghg
double technology::get_emissmap_second( const string& str) const {
    return ( emissmap.find( str ) )->second;
}

//! returns technology logit exponential
double technology::getlexp()  const {
    return lexp;
}

//! Set the technology year.
void technology::setYear( const int yearIn ) {
    year = yearIn;
}

/*! \brief returns the number of ghg objects.
*
* Calcuation is done using length of GHG string to be consistant with use of ghg names to access GHG information.
*
*
* \author Steve Smith
*/
int technology::getNumbGHGs()  const {
	std::vector<std::string> ghgNames = getGHGNames();

	return ghgNames.size();
}



//  ******* method definition for hydro_tech

//! Default constructor
hydro_tech::hydro_tech(): technology() {
    resource = 0;
    A = 0;
    B = 0;
}

//! Clear all member variables.
void hydro_tech::clear(){
    technology::clear();
    resource = 0;
    A = 0;
    B = 0;
}

//! calculates hydroelectricity output based on logit function
void hydro_tech::production(double dmd,int per) 
{
    int T = per*scenario->getModeltime()->gettimestep(per);
    double tempOutput;
    // resource and logit function 
    double fact = exp(A+B*T);
    tempOutput = resource*fact/(1+fact);
    tempOutput = min(tempOutput,fixedOutputVal);
    output = tempOutput;
    input = 0; // no fuel input for hydro
}
