/*! 
* \file Sector.cpp
* \ingroup CIAM
* \brief Sector class source file.
* \author Sonny Kim, Steve Smith, Josh Lurz
* \date $Date$
* \version $Revision$
*/

#include "Definitions.h"
#include <string>
#include <iostream>
#include <fstream>
#include <cassert>

// xml headers
#include "xmlHelper.h"
#include <xercesc/dom/DOM.hpp>
#include <iostream>
#include <iomanip>
#include <algorithm>

#include "sector.h"
#include "subsector.h"
#include "scenario.h"
#include "modeltime.h"
#include "Marketplace.h"
#include "Configuration.h"
#include "Summary.h"
#include "Emcoef_ind.h"
#include "World.h"
#include "Util.h"
#include "Region.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;
extern ofstream outfile, bugoutfile;

/*! \brief Default constructor.
*
* Constructor initializes member variables with default values, sets vector sizes, and sets value of debug flag.
*
* \author Sonny Kim, Steve Smith, Josh Lurz
*/
sector::sector() {
    initElementalMembers();
    Configuration* conf = Configuration::getInstance();
    debugChecking = conf->getBool( "debugChecking" );
    
    // resize vectors
    const Modeltime* modeltime = scenario->getModeltime();
    const int maxper = modeltime->getmaxper();
    
    sectorprice.resize( maxper );
    price_norm.resize( maxper ); // sector price normalized to base year
    pe_cons.resize( maxper ); // sectoral primary energy consumption
    input.resize( maxper ); // sector total energy consumption
    output.resize( maxper ); // total amount of final output from sector
    fixedOutput.resize( maxper );
    carbonTaxPaid.resize( maxper ); // total sector carbon taxes paid
    summary.resize( maxper ); // object containing summaries
    capLimitsPresent.resize( maxper, false ); // flag for presence of capacity limits
}

/*! \brief Default destructor.
*
* deletes all subsector objects associated  with this sector.
*
* \author ?????
*/
sector::~sector() {
    for( vector<subsector*>::iterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
        delete *subSecIter;
    }
}

//! Clear member variables
void sector::clear(){
    initElementalMembers();
    name = "";
    unit = "";
    market = "";
    subsec.clear();
    sectorprice.clear();
    price_norm.clear();
    pe_cons.clear();
    input.clear();
    output.clear();
    carbonTaxPaid.clear();
    summary.clear();
}

//! Initialize elemental data members.
void sector::initElementalMembers(){
    nosubsec = 0;
    tax = 0;
    prevVal = 0;
    prevPer = -1;
   anyFixedCapacity = false;
}

/*! \brief Returns sector name
*
* \author Sonny Kim
* \return sector name as a string
*/
string sector::getName() const
{
    return name;
}

/*! \brief Set data members from XML input
*
* \author Josh Lurz
* \param node pointer to the current node in the XML input tree
* \todo josh to add appropriate detailed comment here
*/
void sector::XMLParse( const DOMNode* node ){
   
   const Modeltime* modeltime = scenario->getModeltime();
   DOMNode* curr = 0;
   DOMNodeList* nodeList = 0;
   string nodeName;
   subsector* tempSubSector = 0;
   
   /*! \pre make sure we were passed a valid node. */
   assert( node );
   
   // get the name attribute.
   name = XMLHelper<string>::getAttrString( node, "name" );
   
   // get additional attributes for derived classes
   XMLDerivedClassParseAttr( node );
   
#if( _DEBUG )
   cout << "\tSector name set as " << name << endl;
#endif
   
   // get all child nodes.
   nodeList = node->getChildNodes();
   
   // loop through the child nodes.
   for( int i = 0; i < static_cast<int>( nodeList->getLength() ); i++ ){
      curr = nodeList->item( i );
      nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
      
      if( nodeName == "#text" ) {
         continue;
      }

      else if( nodeName == "market" ){
         market = XMLHelper<string>::getValueString( curr ); // only one market element.
      }
      else if( nodeName == "price" ){
         XMLHelper<double>::insertValueIntoVector( curr, sectorprice, modeltime );
      }
      else if( nodeName == "output" ) {
         XMLHelper<double>::insertValueIntoVector( curr, output, modeltime );
      }
      else if( nodeName == "unit" ) {
         unit = XMLHelper<string>::getValueString( curr );
      }
      else if( nodeName == "subsector" ){
         map<string,int>::const_iterator subSectorMapIter = subSectorNameMap.find( XMLHelper<string>::getAttrString( curr, "name" ) );
         if( subSectorMapIter != subSectorNameMap.end() ) {
            // subSector already exists.
            subsec[ subSectorMapIter->second ]->XMLParse( curr );
         }
         else {
            tempSubSector = new subsector();
            tempSubSector->XMLParse( curr );
            subsec.push_back( tempSubSector );
            subSectorNameMap[ tempSubSector->getName() ] = static_cast<int>( subsec.size() ) - 1;
         }	
      }
      
      else {
         XMLDerivedClassParse( nodeName, curr );
      }
   }
}

/*! \brief Complete the initialization
*
* \author Josh Lurz
* \todo josh to add appropriate detailed comment here
*/
void sector::completeInit() {
   
   nosubsec = static_cast<int>( subsec.size() );
   
   for( vector<subsector*>::iterator subSecIter = subsec.begin(); subSecIter != subsec.end(); subSecIter++ ) {
      ( *subSecIter )->completeInit();
   }
}

/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
void sector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
      cout << "Unrecognized text string: " << nodeName << " found while parsing Sector." << endl;
}


/*! \brief Parses any attributes specific to derived classes
*
* Method parses any input data attributes (not child nodes, see XMLDerivedClassParse) that are specific to any classes derived from this class. Since sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param node pointer to the current node in the XML input tree
*/
void sector::XMLDerivedClassParseAttr( const DOMNode* node ) {
    // do nothing
    // defining method here even though it does nothing so that we do not
    // create an abstract class.
}

/*! \brief Write object to xml output stream
*
* Method writes the contents of this object to the XML output stream.
*
* \author Josh Lurz
* \param out reference to the output stream
*/
void sector::toXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<supplysector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out );
    XMLWriteElement( unit, "unit", out );
    
    for( int i = 0; modeltime->getper_to_yr( i ) <= 1990; i++ ){
        XMLWriteElementCheckDefault( sectorprice[ i ], "price", out, 0, modeltime->getper_to_yr( i ) );
    }
    
    for( int j = 0; modeltime->getper_to_yr( j ) <= 1990; j++ ){
        XMLWriteElement( output[ j ], "output", out, modeltime->getper_to_yr( j ) );
    }

    // write out the subsector objects.
    for( vector<subsector*>::const_iterator k = subsec.begin(); k != subsec.end(); k++ ){
        ( *k )->toXML( out );
    }

    // write out variables for derived classes
    toXMLDerivedClass( out );
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</supplysector>" << endl;
}


/*! \brief Write output (selected output?) from this object to XML
*
* I don't know how this is different from the previous method.....
*
* \author Josh Lurz
* \param out reference to the output stream
* \todo josh to update documentation on this method ..
*/
void sector::toOutputXML( ostream& out ) const {
    const Modeltime* modeltime = scenario->getModeltime();
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<supplysector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out );
    XMLWriteElement( unit, "unit", out );
    
    for( int i = 0; i < static_cast<int>( sectorprice.size() ); i++ ){
        XMLWriteElement( sectorprice[ i ], "price", out, modeltime->getper_to_yr( i ) );
    }
    
    for( int j = 0; j < static_cast<int>( output.size() ); j++ ){
        XMLWriteElement( output[ j ], "output", out, modeltime->getper_to_yr( j ) );
    }
    
    // write out the subsector objects.
    for( vector<subsector*>::const_iterator k = subsec.begin(); k != subsec.end(); k++ ){
        ( *k )->toXML( out );
    }

    // write out variables for derived classes
    toXMLDerivedClass( out );
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</supplysector>" << endl;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML
*
* \author Steve Smith, Josh Lurz
* \param out reference to the output stream
*/
void sector::toXMLDerivedClass( ostream& out ) const {  
    // do nothing
}	


/*! \brief Write information useful for debugging to XML output stream
*
* Function writes market and other useful info to XML. Useful for debugging.
*
* \author Josh Lurz
* \param period model period
* \param out reference to the output stream
*/
void sector::toDebugXML( const int period, ostream& out ) const {
    
    // write the beginning tag.
    Tabs::writeTabs( out );
    out << "<supplysector name=\"" << name << "\">"<< endl;
    
    // increase the indent.
    Tabs::increaseIndent();
    
    // write the xml for the class members.
    // write out the market string.
    XMLWriteElement( market, "market", out );
    XMLWriteElement( unit, "unit", out );
    
    // Write out the data in the vectors for the current period.
    XMLWriteElement( sectorprice[ period ], "sectorprice", out );
    XMLWriteElement( pe_cons[ period ], "pe_cons", out );
    XMLWriteElement( input[ period ], "input", out );
    XMLWriteElement( output[ period ], "output", out );
    XMLWriteElement( carbonTaxPaid[ period ], "carbonTaxPaid", out );
    
    // Write out the summary
    // summary[ period ].toDebugXML( period, out );
    
    // write out the subsector objects.
    for( vector<subsector*>::const_iterator j = subsec.begin(); j != subsec.end(); j++ ){
        ( *j )->toDebugXML( period, out );
    }
    
    // finished writing xml for the class members.
    
    // decrease the indent.
    Tabs::decreaseIndent();
    
    // write the closing tag.
    Tabs::writeTabs( out );
    out << "</supplysector>" << endl;
}

/*! \brief Perform any initializations needed for each period.
*
* Any initializations or calcuations that only need to be done once per period (instead of every iteration) should be placed in this function.
*
* \author Steve Smith
* \param regionName region name
* \param per Model period
*/
void sector::initCalc( const string& regionName, const int per ) {
    double calOutput;
    double sectorOutput;
    
    // do any sub-sector initializations
    for ( int i=0; i<nosubsec; i++ ) {
        subsec[ i ]->setRegionName( regionName );
        subsec[ i ]->setSectorName( name );
        subsec[ i ]->initCalc( per );
    }
    
    // set flag if there are any fixed supplies
    if ( getFixedSupply( per ) > 0 ) {
      anyFixedCapacity = true;
    }
    
    // find out if this sector has any capacity limits
    capLimitsPresent[ per ] = capacityLimitsInSector( per );
    
    // check to see if previous period's calibrations were set ok
    // Not sure if this works. Didn't seem to for demand sector
    if ( per > 0 ) {
        for ( int i=0; i<nosubsec; i++ ) {
            if ( subsec[ i ]->getCalibrationStatus( per - 1 ) ) {
                calOutput = subsec[ i ]->getTotalCalOutputs( per - 1 );
                sectorOutput = subsec[ i ]->getFixedSupply( per - 1 );
                if ( calOutput < sectorOutput * 0.99999 ) {
                    cerr << "WARNING: calibrated output < sector output for " 
                        << name << " subSect " << subsec[ i ]->getName()
                        << " in region " << regionName << endl;
                }
            }
        }
    }
}

/*! \brief Create new market for this sector
*
* Sets up the appropriate market within the marketplace for this sector. Note that the type of market is NORMAL -- signifying that this market is a normal market that is solved (if necessary).
*
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param regionName region name
*/
void sector::setMarket( const string& regionName ) {
    
    Marketplace* marketplace = scenario->getMarketplace();
    // name is sector name (name of good supplied or demanded)
    // market is the name of the regional market from the input file (i.e., global, region, regional group, etc.)
    
    if( marketplace->createMarket( regionName, market, name, Marketplace::NORMAL ) ) {
        marketplace->setPriceVector( name, regionName, sectorprice );
    }
}

/*! \brief Pass along a fixed carbon price to subsectors
*
* Routine passes a fixed carbon price to each subsector as given through data read-in
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param tax the carbon prie for this region
* \param per model period
* \todo combine this with ghgtax by using a "fixedTax" tag in data input (see e-mail of 10/21/03)
*/
void sector::applycarbontax( const string& regionName, double tax, int per ) {
    int i=0;
    for ( i=0; i<nosubsec; i++ ) {
        subsec[ i ]->applycarbontax( regionName, tax, per );
    }
}

/*! \brief Passes ghg price (tax) to subsectors
*
* The GHG price is passed along to each subsector. This is the price as determined through the solution routine.
*
* \author Sonny Kim, Josh Lurz
* \param regionName region name
* \param ghgname name of the ghg to apply tax to
* \param per model period
*/
void sector::addghgtax( const string& ghgname, const string& regionName, const int per)
{
    for (int i=0;i<nosubsec;i++) {
        subsec[ i ]->addghgtax(ghgname,regionName,per);
    }
}

/*!
* \brief  Calculate subsector shares, adjusting for capacity limits.

* This routine calls subsector::calcShare for each subsector, which calculated an unnormalized share, and then calls normShare to normalize the shares for each subsector.

* The code below also takes into account sectors with fixed output. The sectors without fixed output are normalized to sum/(1-fixedSum) and the sectors with fixed output are reset to their fixed share. Note that the fixed share is an approximation, held over from the last iteration, of the actual share of any technology with a fixed output. 

* \param regionName Region name
* \param per model period
* \param gnpPerCap GDP per capita (scaled to base year)

* \author Sonny Kim, Steve Smith, Josh Lurz
* \todo add warnings to sub-sector and technology (?) that fixed capacity has to be >0
* \warning model with fixed capacity in sectors where demand is not a solved market may not solve
*/
void sector::calcShare( const string& regionName, const int per, const double gnpPerCap )
{
// Note that this solution for the fixed capacity share problem requires that 
// simultaneity be turned on. This would seem to be because the fixed share is lagged one period
// and can cause an oscillation. With the demand for this sector in the marketplace, however, the
// fixed capacity converges as the trial value for demand converges.

    int i=0;
    double sum = 0.0;
    double fixedSum = 0.0;
    
    // first loop through all subsectors to get the appropriate sums
    for ( i=0; i<nosubsec; i++ ) {
        // determine subsector shares based on technology shares
        subsec[ i ]->calcShare( regionName, per );
        sum += subsec[ i ]->getShare( per );
        
        // sum fixed capacity separately, but don't bother with the extra code if this sector has none
        if ( anyFixedCapacity) {
            double fixedShare = getFixedShare( regionName, i , per );
            if ( fixedShare > 0 ) {
               sum -= subsec[ i ]->getShare( per ); // if fixed, subtract fixed share from sum
               fixedSum += fixedShare; // keep track of total fixed shares
            }
        }

        // initialize cap limit status as false
        // (will be changed in adjSharesCapLimit if necessary)
        subsec[ i ]->setCapLimitStatus( false , per );
    }
    
    // Tkae care of case where fixed share is > 1
    double scaleFixedShare = 1.0;
    if ( fixedSum > 1.0 ) {
      scaleFixedShare = 1/fixedSum;
      fixedSum = 1.0;
    }
    
    // Now normalize shares
    for ( i=0; i<nosubsec; i++ ) {
                
        if ( subsec[ i ]->getFixedSupply( per ) == 0) {
            // normalize subsector shares that are not fixed
            if ( fixedSum < 1 ) {
               subsec[ i ]->normShare( sum/( 1 - fixedSum ) , per );	
            } else {
               subsec[ i ]->normShare( sum/util::getTinyNumber() , per );	// if all fixed supply, eliminate other shares
            }

        // reset share of sectors with fixed supply to their appropriate value
        } else {
          double fixedShare = getFixedShare( regionName, i , per ) * scaleFixedShare;
          double currentShare = subsec[ i ]->getFixedShare( per );
          subsec[ i ]->setShareToFixedValue( per );
          if ( currentShare > 0 ) { subsec[ i ]->scaleFixedSupply( fixedShare/currentShare, per ); }
          subsec[ i ]->setShareToFixedValue( per );
      }
    }
   fixedShareSavedVal = fixedSum; // save share value for debugging check

    // Now adjust for capacity limits
    // on 10/22/03 adding this check saves about 1/40 of the model run time.
    if ( capLimitsPresent[ per ] ) {
       adjSharesCapLimit( regionName , per );
    }
    
   // Check to make sure shares still equal 1
   if ( debugChecking ) {
      checkShareSum( regionName, per );
   }
        
}


/*!
* \brief Determine if any capacity limits are exceeded and adjust for shares if so.
*
* If a capacity limit comes into play the routine shifts the "excess" share (over the capacity limits) to the non-limited sectors. This routine loops several times in case this shift then causes another sector to exceed a capacity limit.
*
* Sectors that have fixed outputs are not adjusted.
* 
* The logic for the share adjustment is as follows:
*     Sum_notlim(Si) + Sum_limited(Si) + sumSharesOverLimit = 1
*     where:
*         Sum_notlim(Si)  = sum of shares that are not capacity limited
*         Sum_limited(Si) = sum of shares that are capacity limited
*         sumSharesOverLimit = portion of shares that were over capacity limits
*         
*     Need to solve for the amount to increase shares to account for capacity limits
*    if:
*         newSi = a * Si, then a few lines of algebra gives:
*         a = 1 + sumSharesOverLimit/Sum_notlim(Si)
*
* \author Steve Smith
* \warning The routine assumes that shares are already normalized.
* \param regionName region name
* \param per Model period
*/
void sector::adjSharesCapLimit( const string& regionName, const int per )
{
    const double SMALL_NUM = util::getSmallNumber();
    double tempCapacityLimit;
    double tempSubSectShare;
    double totalFixedShares = 0;
    bool capLimited = true; // true if any sub-sectors are over their capacity limit
    //bool capLimited = false; // set to false to turn cap limits off for testing
    int i=0;
    
    // check for capacity limits, repeating to take care of any knock-on effects. 
    // Do this a maximum of times equal to number of subsectors, 
    // which is the maximum number of times could possibly need to do this
    for (int n = 0;  n < nosubsec && capLimited; n++) {
        double sumSharesOverLimit = 0.0;		// portion of shares over cap limits
        double sumSharesNotLimited = 0.0;	// sum of shares not subject to cap limits
        capLimited = false;
        
        //  Check for capacity limits and calculate sums, looping through each subsector
        for ( i=0; i<nosubsec; i++ ) {
            double actualCapacityLimit = subsec[ i ]->getCapacityLimit( per ); // call once, store these locally
            tempSubSectShare = subsec[ i ]->getShare( per ) ;
            
            // if sector has been cap limited, then return limit, otherwise transform
            // this is needed because can only do the transform once
            if ( subsec[ i ]->getCapLimitStatus( per ) ) {
               tempCapacityLimit = subsec[ i ]->getShare( per );
            } else {
               tempCapacityLimit = subsector::capLimitTransform( actualCapacityLimit, tempSubSectShare );
            }
            
            // if there is a capacity limit and are over then set flag and count excess shares
            if ( tempSubSectShare - tempCapacityLimit > SMALL_NUM ) {
                capLimited = true;
                sumSharesOverLimit += tempSubSectShare - tempCapacityLimit;
                
    //           cout << "Cap limit changed from " << actualCapacityLimit << " to " << tempCapacityLimit;
    //  cout << " in sub-sector: " << subsec[ i ]->getName() << endl;

            }
            
            // also sum shares under limit (but not those just at their limits)
            if ( tempSubSectShare < tempCapacityLimit ) {
                sumSharesNotLimited += tempSubSectShare;
            }
            
            // But don't count shares that have fixed outputs. 
            // Sectors with fixed outputs are not adjusted in subsec::limitShares below.
            if ( subsec[ i ]->getFixedShare( per ) > 0 ) {
                sumSharesNotLimited -= tempSubSectShare;
                totalFixedShares += subsec[ i ]->getFixedShare( per );
            }
          
        } // end of loop over sub-sectors
        
        // re-normalize subsector shares if capacity limits have been exceeded
        // See comments above for derivation of multiplier
        if ( capLimited ) {
            if ( sumSharesNotLimited > 0 ) {
                for ( i=0; i<nosubsec; i++ ) {
                    double multiplier = 1 + sumSharesOverLimit/sumSharesNotLimited;
                    subsec[ i ]->limitShares( multiplier, per );
                }
            }
            else { // If there are no sectors without limits and there are still shares to be re-distributed
                if ( sumSharesOverLimit > 0 ) {
                    // if there is no shares left then too much was limited!
                    cerr << regionName << ": Insufficient capacity to meet demand in sector " << name << endl;
                }
            }
            
        }
        
    } // end for loop
    
    // if have exited and still capacity limited, then report error
    if ( capLimited ) {
        cerr << "Capacity limit not resolved in sector " << name << endl;
    }
}

/*! \brief Check that sum of shares is equal to one.
*
* Routine used for checking. Prints an error if shares do not sum to one. 
* Good to run if debugChecking flag is on.
*
* \author Steve Smith
* \param regionName region name
* \param per Model period
*/
void sector::checkShareSum( const string& regionName, int per ) {
    const double SMALL_NUM = util::getSmallNumber();
    double sumshares = 0;
    int i;
    
    for ( i=0; i<nosubsec; i++ ) {
        // Check the validity of shares.
       assert( util::isValidNumber( subsec[ i ]->getShare( per ) ) );
        
        sumshares += subsec[ i ]->getShare( per ) ;
    }
    if ( fabs(sumshares - 1) > SMALL_NUM ) {
        cerr << "ERROR: Shares do not sum to 1. Sum = " << sumshares << " in sector " << name;
        cerr << ", region: " << regionName << endl;
        cout << "Shares: ";
        for ( i=0; i<nosubsec; i++ ) {
         cout << subsec[ i ]->getShare( per ) << ", ";
       }
       cout << endl;
    }
}

/*! \brief Calculate weighted average price of subsectors.
*
* weighted price is put into sectorprice variable
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void sector::calcPrice( int per )
{
    sectorprice[ per ]=0.0;
    for (int i=0;i<nosubsec;i++) {	
        sectorprice[ per ] += subsec[ i ]->getShare( per ) * subsec[ i ]->getPrice( per );
    }
}

/*! \brief returns the sector price.
*
* Returns the weighted price. 
* Calcuation of price incorporated into call to ensure that this is calculated.
* 
*
* \author Sonny Kim
* \param per Model period
*/
double sector::getPrice( int per )
{
    calcPrice( per );
    return sectorprice[ per ];
}

/*! \brief Returns true if all sub-sector outputs are fixed or calibrated.
*
* Routine loops through all the subsectors in the current sector. If output is calibrate, assigned a fixed output, or set to zero (by setting the share weight to zero) then true is returned. If all ouptput is not fixed, then the sector has at least some capacity to respond to a change in prices.
*
* \author Steve Smith
* \param per Model period
* \return Boolean that is true if entire sector is calibrated or has fixed output
*/
bool sector::sectorAllCalibrated( int per ) {
    bool allCalibrated = true;

   if ( per < 0 ) {
      allCalibrated = false;
   } else {
      for ( int i=0; i<nosubsec; i++ ) {
         if ( !(subsec[ i ]->allOuputFixed( per )) ) {
            allCalibrated = false;
         }
      }
   }
    
   return allCalibrated;
}

/*! \brief Returns true if any sub-sectors have capacity limits.
*
* Routine checks to see if any capacity limits are present in the sub-sectors. Is used to avoid calling capacity limit calculation unnecessary. *
* \author Steve Smith
* \param per Model period
* \return Boolean that is true if there are any capacity limits present in any sub-sector
*/
bool sector::capacityLimitsInSector( int per ) {
    bool anyCapacityLimits = false;

   if ( per < 0 ) {
      anyCapacityLimits = false;
   } else {
      for ( int i=0; i<nosubsec && !anyCapacityLimits; i++ ) {
         if ( !( subsec[ i ]->getCapacityLimit( per ) == 1 ) ) {
            anyCapacityLimits = true;
         }
      }
   }
    
   return anyCapacityLimits;
}

//! Set output for sector (ONLY USED FOR energy service demand at present).
/*! Demand from the "dmd" parameter (could be energy or energy service) is passed to subsectors.
This is then shared out at the technology level.
In the case of demand, what is passed here is the energy service demand. 
The technologies convert this to an energy demand.
The demand is then summed at the subsector level (subsec::sumOutput) then
later at the sector level (in region via supplysector[j].sumOutput( per ))
to equal the total sector output.
*
* \author Sonny Kim, Josh Lurz, Steve Smith
* \param regionName Region the sector is contained in.
* \param dmd Demand to be passed to the subsectors. (Sonny check this)
* \param per Model period
*/
void sector::setoutput(const string& regionName, double dmd, int per)
{
    int i;
    carbonTaxPaid[ per ] = 0; // initialize carbon taxes paid
    
    for ( i=0; i<nosubsec; i++ ) {
        // set subsector output from sector demand
        subsec[ i ]->setoutput(regionName,name,dmd,per);
        carbonTaxPaid[ per ] += subsec[ i ]->getTotalCarbonTaxPaid( per );
    }
}

/*! \brief Return subsector fixed Supply.
*
* Returns the total amount of fixed supply from all subsectors and technologies.
*
* \author Steve Smith
* \param per Model period
* \return total fixed supply
*/
double sector::getFixedSupply( int per ) const {
    double totalFixedSupply = 0;
    for ( int i=0; i<nosubsec; i++ ) {
        totalFixedSupply += subsec[ i ]->getFixedSupply( per );
    }
    return totalFixedSupply;
}

/*! \brief Returns the share of fixed supply from the given subsector using a particular logic, depending on model setup.
*
* This function returns either the saved sub-sector share, or the share as derived from the marketplace demand, if available.
*
* \author Steve Smith
* \param regionName Region name
* \param sectorNum Sector number
* \param per Model period
* \return total fixed supply
* \warning Not sure how well using market demand will work if multiple sectors are adding demands. 
*/
double sector::getFixedShare( const std::string& regionName, const int sectorNum, const int per ) {
   Marketplace* marketplace = scenario->getMarketplace();
   World* world = scenario->getWorld();
   
   if ( sectorNum >= 0 && sectorNum < nosubsec ) {
      double fixedShare = subsec[ sectorNum ]->getFixedShare( per );
      if ( fixedShare > 0) {
         // if demand is available through marketplace then use this instead of lagged value
         double mktDmd = marketplace->getDemand( name, regionName, per );
         // set 1==2 to turn this off during N-R (do also in subsector) -- but doesn't seem to matter
         if ( mktDmd > 0 && ( world->getCalibrationSetting() || 1==1 ) ) {
            fixedShare = subsec[ sectorNum ]->getFixedSupply( per )/mktDmd;
         }
    //     fixedShare = 0.1;
      }
      return fixedShare;
   } else {
      cerr << "Illegal sector number: " << sectorNum << endl;
      return 0;
   }
}

/*! \brief Return subsector total calibrated outputs.
*
* Returns the total calibrated outputs from all subsectors and technologies. Note that any calibrated input values are converted to outputs and are included.
*
* \author Steve Smith
* \param per Model period
* \return total calibrated outputs
*/
double sector::getCalOutput( int per ) const {
    double totalCalOutput = 0;
    for ( int i=0; i<nosubsec; i++ ) {
        totalCalOutput += subsec[ i ]->getTotalCalOutputs( per );
    }
    return totalCalOutput;
}

/*! \brief Calibrate sector output.
*
* This performs supply sector technology and sub-sector output/input calibration. 
   Determines total amount of calibrated and fixed output and passes that down to the subsectors.
   
* Note that this routine only performs subsector and technology-level calibration. Total final energy calibration is done by Region::calibrateTFE and GDP calibration is set up in Region::calibrateRegion.
*
* \author Steve Smith
* \param regionName region name
* \param per Model period
*/
void sector::calibrateSector( const string& regionName, const int per )
{
    Marketplace* marketplace = scenario->getMarketplace();
    double totalFixedSupply;
    double totalCalOutputs;
    double mrkdmd;
    
    totalFixedSupply = getFixedSupply( per ); 
    mrkdmd = marketplace->getDemand( name, regionName, per ); // demand for the good produced by this sector
    totalCalOutputs = getCalOutput( per );
    
    for (int i=0; i<nosubsec; i++ ) {
        if ( subsec[ i ]->getCalibrationStatus( per ) ) {
            subsec[ i ]->adjustForCalibration( mrkdmd, totalFixedSupply, totalCalOutputs, per );
        }
    }
} 

/*! \brief Adjust shares to be consistant with fixed supply
*
* This routine determines the total amount of fixed supply in this sector and adjusts other shares to be consistant with the fixed supply.  If fixed supply exceeds demand then the fixed supply is reduced. An internal variable with the sector share of fixed supply for each sub-sector is set so that this information is available to other routines.

* \author Steve Smith
* \param mrkdmd demand for the good produced by this sector
* \param regionName region name
* \param per Model period
* \warning fixed supply must be > 0 (to obtain 0 supply, set share weight to zero)
*/
void sector::adjustForFixedSupply( const double mrkdmd, const string& regionName, const int per) {
    World* world = scenario->getWorld();
    int i;
    double totalFixedSupply = 0; 
    double variableShares = 0; // original sum of shares of non-fixed subsectors   
    double variableSharesNew = 0; // new sum of shares of non-fixed subsectors   
    double shareRatio;  // ratio for adjusting shares of non-fixed subsectors
        
    // set output from technologies that have fixed outputs such as hydro electricity

    // Determine total fixed production and total var shares
    // Need to change the exog_supply function once new, general fixed supply method is available
    totalFixedSupply = 0;
    for ( i=0; i<nosubsec; i++ ) {
        double fixedSupply = 0;
        subsec[ i ]->resetFixedSupply( per );
        fixedSupply = subsec[ i ]->getFixedSupply( per );
        
        // initialize property to zero every time just in case fixed share property changes 
        // (shouldn't at the moment, but that could allways change)
        subsec[ i ]->setFixedShare( per, 0 ); 
        
        // add up subsector shares without fixed output
        // sjs -- Tried treating capacity limited sub-sectors differently, here and in adjShares,
        //     -- but that didn't give capacity limits exactly.
        if ( fixedSupply == 0 ) { 
            variableShares += subsec[ i ]->getShare( per );
        } else {
           if ( mrkdmd != 0 ) {
              double shareVal = fixedSupply/mrkdmd;
              if ( shareVal > 1 ) { 
                 shareVal = 1; // Eliminates warning message since this conditionshould be fixed below
               } 
              subsec[ i ]->setFixedShare( per, shareVal ); // set fixed share property
           }
        }
        totalFixedSupply += fixedSupply;
   }
    
    // Scale down fixed output if its greater than actual demand
    if ( totalFixedSupply > mrkdmd ) {
        for ( i=0; i<nosubsec; i++ ) {
            subsec[ i ]->scaleFixedSupply( mrkdmd/totalFixedSupply, per ); 
        }
        totalFixedSupply = mrkdmd;
    }
    
    // debuging check
    // sjs TEMP -- this check generally spits out a few inocuous warnings.
    // If simultunaeities are resolved then this should only happen a couple times per iteration.
    if ( debugChecking && world->getCalibrationSetting()) {
         if ( fabs(fixedShareSavedVal - totalFixedSupply/mrkdmd) > 1e-5 && fixedShareSavedVal != 0 ) {
            cerr << "Fixed share changed from " << fixedShareSavedVal << " to ";
            cerr << totalFixedSupply/mrkdmd << endl;
         }
    }
    
    // Adjust shares for any fixed output
    if (totalFixedSupply > 0) {
         if (totalFixedSupply > mrkdmd) {            
            variableSharesNew = 0; // should be no variable shares in this case
        }
        else {
            assert( mrkdmd != 0); // check for 0 so that variableSharesNew does not blow up
            variableSharesNew = 1 - (totalFixedSupply/mrkdmd);
        }
        
        if (variableShares == 0) {
            shareRatio = 0; // in case all subsectors are fixed output, unlikely
        }
        else {
            shareRatio = variableSharesNew/variableShares;
        }
        
        // now that parameters are set, adjust shares for all sub-sectors
        for ( i=0; i<nosubsec; i++ ) {
            // shareRatio = 0 is okay, sets all non-fixed shares to 0
            subsec[ i ]->adjShares( mrkdmd, shareRatio, totalFixedSupply, per ); 
        }
    }
}

/*! \brief Set supply sector output
*
* This routine takes the market demand and propagates that through the supply sub-sectors
where it is shared out (and subsequently passed to the technology level within each sub-sector
to be shared out).

Routine also calls adjustForFixedSupply which adjusts shares, if necessary, for any fixed output sub-sectors.

* \author Sonny Kim
\param regionName region name
\param per Model period
*/
void sector::supply( const string& regionName, const int per) {
    Marketplace* marketplace = scenario->getMarketplace();
    
    double mrkdmd;
    
    carbonTaxPaid[ per ] = 0; // initialize carbon taxes paid
    
    mrkdmd = marketplace->getDemand( name, regionName, per ); // demand for the good produced by this sector
    
    if ( mrkdmd < 0 ) {
        cerr << "ERROR: Demand value < 0 for good " << name << " in region " << regionName << endl;
    }
    
    // Adjust shares for fixed supply
   if ( anyFixedCapacity ) {
      adjustForFixedSupply( mrkdmd, regionName, per);
   }
   
    // This is where subsector and technology outputs are set
    for (int i=0;i<nosubsec;i++) {
        // set subsector output from sector demand
        subsec[ i ]->setoutput( regionName, name, mrkdmd, per ); // CHANGED JPL
        // for reporting only
        carbonTaxPaid[ per ] += subsec[ i ]->getTotalCarbonTaxPaid( per );
    }
    
    if ( debugChecking ) {
        // If the model is working correctly this should never give an error
        // An error here means that the supply summed up from the supply sectors 
        // is not equal to the demand that was passed in 
        double mrksupply = getOutput( per );
        
         bool watchSector = (name == "electricity" && regionName == "USAxxx");
         if ( watchSector ) {
            cout << "sector:supply elec mktdmd: " << mrkdmd;
            cout << " supply: " << mrksupply << endl;
         }

        // if demand identically = 1 then must be in initial iteration so is not an error
        if ( per > 0 && fabs(mrksupply - mrkdmd) > 0.01 && mrkdmd != 1.0000 ) {
            mrksupply = mrksupply * 1.000000001;
            cout << regionName << " Market "<<  name<< " demand and derived supply are not equal by: ";
            cout << fabs(mrksupply - mrkdmd) << ": ";
            cout << "S: " << mrksupply << "  D: " << mrkdmd << endl;
            
            if ( 1 == 2 ) { // detailed debugging output
               for (int i=0;i<nosubsec;i++) {
                  cout << subsec[ i ]->getName( ) << " Share:";
                  cout << subsec[ i ]->getShare( per ) << " Output:";
                  cout << subsec[ i ]->getOutput( per ) << endl;
               }
            }
        }
    }
}


/*! \brief returns the number of sub-sectors in this sector.
*
* In practice this is not used, the nosubsec variable is used instead since that is needed so often.
*
* \author Sonny Kim
* \return number of subsectors
*/
int sector::getNumberOfSubSec(void) const
{
    return nosubsec;
}

/*! \brief Sum subsector outputs.
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void sector::sumOutput( int per )
{
    output[ per ] = 0;
    for ( int i=0; i<nosubsec; i++ ) {
        double temp = subsec[ i ]->getOutput( per );
        output[ per ] += temp;
        
        // error check
        if ( temp != temp ) {
           cerr << "output for sector "<< i <<" is not valid, with value " << temp << endl;
        }
    }
}

/*! \brief returns sector output.
*
* Returns the total amount of the sector. 
* 
* Routine now incorporates sumOutput so that output is automatically correct.
*
* \author Sonny Kim
* \param per Model period
* \todo make year 1975 regular model year so that logic below can be removed
* \return total output
*/
double sector::getOutput( int per ) 
{

   // this is needed because output for 1975 is hard coded at the sector level for some sectors,
   // in which case do not want to sum.
   if ( per > 0 || output[ per ] == 0 ) {
      sumOutput( per );
   }
    return output[ per ]; 
}


/*! \brief Calculate GHG emissions for each sector from subsectors.
*
* Calculates emissions for subsectors and technologies, then updates emissions maps for emissions by gas and emissions by fuel & gas.
*
* Note that at present (10/03), emissions only occur at technology level.
*
* \author Sonny Kim
* \param per Model period
*/
void sector::emission( int per )
{
    summary[ per ].clearemiss(); // clear emissions map
    summary[ per ].clearemfuelmap(); // clear emissions fuel map
    for (int i=0;i<nosubsec;i++) {
        subsec[ i ]->emission(per,name);
        summary[ per ].updateemiss(subsec[ i ]->getemission( per )); // by gas
        summary[ per ].updateemfuelmap(subsec[ i ]->getemfuelmap( per )); // by fuel and gas
    }
}

/*! \brief Calculate indirect GHG emissions for each sector from subsectors.
*
* \author Sonny Kim
* \param per Model period
* \param emcoef_ind Vector of indirect emissions objects. 
*/
void sector::indemission( const int per, const vector<Emcoef_ind>& emcoef_ind )
{
    summary[ per ].clearemindmap(); // clear emissions map
    for (int i=0;i<nosubsec;i++) {
        subsec[ i ]->indemission( per, emcoef_ind );
        summary[ per ].updateemindmap(subsec[ i ]->getemindmap( per ));
    }
}

/*! \brief Sums subsector primary and final energy consumption.
*
* Routine sums all input energy consumption and puts that into the input variable.
*
* \author Sonny Kim, Josh Lurz
* \param per Model period
*/
void sector::sumInput( int per )
{
    input[ per ] = 0;
    for (int i=0;i<nosubsec;i++) {
        input[ per ] += subsec[ i ]->getInput( per );
    }
   double tempval = input[ per ];
   tempval = tempval * 1; 
}

/*! \brief Returns sectoral energy consumption.
*
* Routine sums all input energy consumption and puts that into the input variable.
* Sector input is now summed every time this function is called.
*
* \author Sonny Kim
* \param per Model period
* \return total input
*/
double sector::getInput( int per )
{
    sumInput( per);
    return input[ per ];
}

//! Write sector output to database.
void sector::outputfile( const string& regname)
{
    // function protocol
    void fileoutput3(string var1name,string var2name,string var3name,
        string var4name,string var5name,string uname,vector<double> dout);
    
    // function arguments are variable name, double array, db name, table name
    // the function writes all years
    // total sector output
    fileoutput3(regname,name," "," ","production","EJ",output);
    // total sector eneryg input
    fileoutput3( regname,name," "," ","consumption","EJ",input);
    // sector price
    fileoutput3( regname,name," "," ","price","$/GJ",sectorprice);
    // sector carbon taxes paid
    fileoutput3( regname,name," "," ","C tax paid","Mil90$",carbonTaxPaid);
}

//! Write out subsector results from demand sector.
void sector::MCoutput_subsec( const string& regname )	
{	// do for all subsectors in the sector
    for (int i=0;i<nosubsec;i++) {
        // output or demand for each technology
        subsec[ i ]->MCoutputB(regname,name);
        subsec[ i ]->MCoutputC(regname,name);
    }
}

//! Write MiniCAM style sector output to database.
void sector::MCoutput(const string& regname ) {
    const Modeltime* modeltime = scenario->getModeltime();
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);
    int m;
    
    // total sector output
    dboutput4(regname,"Secondary Energy Prod","by Sector",name,"EJ",output);
    dboutput4(regname,"Secondary Energy Prod",name,"zTotal","EJ",output);
    
    int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    string str; // temporary string
    
    // sector fuel consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        dboutput4(regname,"Fuel Consumption",name,fmap->first,"EJ",temp);
    }
    
    // sector emissions for all greenhouse gases
    map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        str = "Sec: "; // sector heading
        str+= name; // sector name
        dboutput4(regname,"Emissions",str,gmap->first,"MTC",temp);
    }
    // CO2 emissions by sector
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    dboutput4(regname,"CO2 Emiss","by Sector",name,"MTC",temp);
    dboutput4(regname,"CO2 Emiss",name,"zTotal","MTC",temp);
    
    // CO2 indirect emissions by sector
    for (m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emindmap_second("CO2");
    }
    dboutput4(regname,"CO2 Emiss(ind)",name,"zTotal","MTC",temp);
    
    // sector price
    dboutput4(regname,"Price",name,"zSectorAvg","$/GJ",sectorprice);
    // for electricity sector only
    if (name == "electricity") {
        for (m=0;m<maxper;m++) {
            temp[m] = sectorprice[m] * 2.212 * 0.36;
        }
        dboutput4(regname,"Price","electricity C/kWh","zSectorAvg","90C/kWh",temp);
    }

    // sector price
    dboutput4(regname,"Price","by Sector",name,"$/GJ",sectorprice);
    // sector carbon taxes paid
    dboutput4(regname,"General","carbonTaxPaid",name,"$",carbonTaxPaid);
    // do for all subsectors in the sector
    for (int i=0;i<nosubsec;i++) {
        // output or demand for each technology
        subsec[ i ]->MCoutputA(regname,name);
        subsec[ i ]->MCoutputC(regname,name);
    }
}

//! Write subsector output to database.
void sector::subsec_outfile( const string& regname )
{
    // do for all subsectors in the sector
    for (int i=0;i<nosubsec;i++) {
        // output or demand for each technology
        subsec[ i ]->outputfile(regname,name);
    }
}

/*! \brief Sets output of sector, used for demand sectors
*
* For demand sectors, the output of the sector, which is the total service demand, is set directly, instead of summing up from subsectors.
* 
* \author Sonny Kim
* \param dmd Total service demand
* \param per Model period
*/
void sector::set_ser_dmd(double dmd, int per)
{
    output[ per ] = dmd;
}

/*! \brief Set name of region that contains this sector
*
* Extremely useful for debugging!
*
* \author Steve Smith
* \param regionNameIn Name of region
*/
void sector::setRegionName( const string& regionNameIn ) {
    regionName = regionNameIn;
}

/*! \brief Returns total carbon tax paid by sector.
*
* \author Sonny Kim
* \param per Model period
* \warning Input value is not accurate unless subsector::setoutput has been called first
* \return total carbon taxes paid
*/
double sector::getTotalCarbonTaxPaid( int per ) const
{
    return carbonTaxPaid[ per ];
}

/*! \brief Return fuel consumption map for this sector
*
* \author Sonny Kim
* \param per Model period
* \todo Input change name of this and other methods here to proper capitilization
* \return fuel consumption map
*/
map<string, double> sector::getfuelcons( int per ) const
{
    return summary[ per ].getfuelcons();
}

//!  Get the second fuel consumption map in summary object.
/*! \brief Return fuel consumption for the specifed fuel
*
* \author Sonny Kim
* \param per Model period
* \param fuelName name of fuel
* \return fuel consumption
*/
double sector::getConsByFuel( const int per, const std::string& fuelName ) const
{
    return summary[ per ].get_fmap_second( fuelName );
}

/*! \brief Clear fuel consumption map for this sector
*
* \author Sonny Kim
* \param per Model period
*/
void sector::clearfuelcons( int per )
{
    summary[ per ].clearfuelcons();
}

/*! \brief Return the ghg emissions map for this sector
*
* \author Sonny Kim
* \param per Model period
* \return GHG emissions map
*/
map<string, double> sector::getemission( int per ) const
{
    return summary[ per ].getemission();
}

/*! \brief Return ghg emissions map in summary object
*
* This map is used to calculate the emissions coefficient for this sector (and fuel?) in region
*
* \author Sonny Kim
* \param per Model period
* \return GHG emissions map
*/
map<string, double> sector::getemfuelmap( int per ) const
{
    return summary[ per ].getemfuelmap();
}

/*! \brief update summaries for reporting
*
*  Updates summary information for the sector and all subsectors.
*
* \author Sonny Kim
* \param per Model period
* \return GHG emissions map
*/
void sector::updateSummary( const int per )
{
    int i = 0;
    // clears sector fuel consumption map
    summary[ per ].clearfuelcons();
    
    for ( i=0; i<nosubsec; i++ ) {
        // call update summary for subsector
        subsec[ i ]->updateSummary( per );
        // sum subsector fuel consumption for sector fuel consumption
        summary[ per ].updatefuelcons(subsec[ i ]->getfuelcons( per )); 
    }
    // set input to total fuel consumed by sector
    // input in sector is used for reporting purposes only
    input[ per ] = summary[ per ].get_fmap_second("zTotal");
}

/*! \brief A function to add the sectors fuel dependency information to an existing graph.
*
* This function prints the sectors fuel dependencies to an existing graph.
*
* \author Josh Lurz
* \param outStream An output stream to write to which was previously created.
* \param period The period to print graphs for.
*/
void sector::addToDependencyGraph( ostream& outStream, const int period ) {
   Configuration* conf = Configuration::getInstance();
   string sectorName;
   string fuelName;
   map<string, double> sectorsUsed;  
   typedef map<string,double>:: const_iterator CI;
   CI fuelIter;
   
   // Make sure the outputstream is open.
   assert( outStream );
   // Get the supply sector name.
   sectorName = getName();
   util::replaceSpaces( sectorName );
   
   sectorsUsed = getfuelcons( period );
   
   // Now loop through the fuel map.
   for( fuelIter = sectorsUsed.begin(); fuelIter != sectorsUsed.end(); fuelIter++ ) {
      fuelName = fuelIter->first;
      if( fuelName != "zTotal" ) {
         util::replaceSpaces( fuelName );
         // outStream << "\t" << fuelName << " -> " << sectorName << " [label=\"" << fuelIter->second << "\"];" << endl;
         outStream << "\t" << fuelName << " -> " << sectorName;
         outStream << " [style=\"";
         
         if( fuelIter->second < 1.0 ) {
            outStream << "dotted";
         }
         else if ( fuelIter->second < 5.0 ) {
            outStream << "dashed";
         }
         else if ( fuelIter->second < 10.0 ) {
            outStream << "";
         }
         else {
            outStream << "bold";
         }
         
         outStream << "\"";
         
         if( conf->getBool( "PrintValuesOnGraphs" ) ) {
            outStream << ",label=\"";
            outStream << setiosflags( ios::fixed | ios::showpoint ) << setprecision( 2 );
            outStream << fuelIter->second;
            outStream << "\"";
         }
         outStream << "];" << endl;
      }
   }
}

/*! \brief A function to add the name of a sector the current sector has a simul with. 
*
* This function adds the name of the sector to the simulList vector, if the name
* does not already exist within the vector. This vector is 
* then used to sort the sectors by fuel dependencies so that calculations are always 
* consistent. 
*
* \author Josh Lurz
* \param sectorName The name of the sector the current sector has a simul with. 
*/
void sector::addSimul( const string sectorName ) {
    if( std::find( simulList.begin(), simulList.end(), sectorName ) == simulList.end() ) {
        simulList.push_back( sectorName );
    }
}

/*! \brief This function sets up the sector for sorting. 
*
* This function uses the recursive function getInputDependencies to 
* find the full list of dependencies for the sector, including 
* transative dependencies. It then sorts that list of dependencies
* for rapid searching.
*
* \author Josh Lurz
* \param parentRegion A pointer to the parent region.
*/
void sector::setupForSort( const Region* parentRegion ) {
    
    // Setup the internal dependencies vector.
    dependsList = getInputDependencies( parentRegion );
    
    // Now sort the list.
    sort( dependsList.begin(), dependsList.end() );
}

/*! \brief This gets the full list of input dependencies including transative dependencies. 
*
* This function recursively determines the input dependencies for the sector. To do this
* correctly, it must also recursively find all the input dependencies for its direct inputs.
* This can result in a long list of dependencies. Dependencies already accounted for by simuls
* are not included in this list. 
*
* \author Josh Lurz
* \param parentRegion A pointer to the parent region.
* \return The full list of input dependencies including transative ones. 
*/
vector<string> sector::getInputDependencies( const Region* parentRegion ) const {
    // Setup the vector we will return.
    vector<string> depVector;

    // Setup an input vector.
    map<string,double> tempMap = getfuelcons( 0 );
    
    for( map<string, double>::const_iterator fuelIter = tempMap.begin(); fuelIter != tempMap.end(); fuelIter++ ) {
        string depSectorName = fuelIter->first;
        
        // Check for zTotal, which is not a sector name and simuls which are not dependencies. 
        if( depSectorName != "zTotal" && ( find( simulList.begin(), simulList.end(), depSectorName ) == simulList.end() ) ) {
            // First add the dependency.
            depVector.push_back( depSectorName );
            
            // Now get the sector's dependencies.
            vector<string> tempDepVector = parentRegion->getSectorDependencies( depSectorName );
            
            // Add the dependencies if they are unique.
            for( vector<string>::const_iterator tempVecIter = tempDepVector.begin(); tempVecIter != tempDepVector.end(); tempVecIter++ ) {
                // If the sector is not already in the dep vector, add it. 
                if( find( depVector.begin(), depVector.end(), *tempVecIter ) == depVector.end() ){
                    depVector.push_back( *tempVecIter );
                }
            }
        }
    } // End input list loop
    // Return the list of dependencies. 
    return depVector;
}

/*! This function returns a copy of the list of dependencies of the sector
*
* This function returns a vector of strings created during setupForSort.
* It lists the names of all inputs the sector uses. These inputs are also sectors. 
*
* \author Josh Lurz
* \return A vector of sector names which are inputs the sector uses. 
*/
const vector<string> sector::getDependsList() const {
    return dependsList;
}



