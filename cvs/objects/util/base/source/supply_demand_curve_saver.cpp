#include <cassert>
#include <vector>
#include <iostream>
#include "containers/include/imodel_feedback_calc.h"
#include "containers/include/scenario.h"
#include "containers/include/world.h"
#include "solution/util/include/solution_info_set.h"
#include "solution/util/include/solution_info.h"
#include "solution/util/include/solvable_solution_info_filter.h"
#include "solution/util/include/solution_info_param_parser.h"
#include "util/base/include/auto_file.h"
#include "util/base/include/definitions.h"
#include "util/base/include/supply_demand_curve.h"
#include "util/base/include/supply_demand_curve_saver.h"
#include "util/base/include/xml_helper.h"
#include "util/base/include/model_time.h"
#include "marketplace/include/market.h"
#include "marketplace/include/marketplace.h"

using namespace std;
using namespace xercesc;


#define DEFAULT_NUM_POINTS 10

SupplyDemandCurveSaver::SupplyDemandCurveSaver() : mNumPoints( DEFAULT_NUM_POINTS )
{
  
}

// First open uses "out" mode to overwrite; subsequent calls append
std::ios_base::openmode SupplyDemandCurveSaver::mOpenMode = std::ios_base::out;

SupplyDemandCurveSaver::~SupplyDemandCurveSaver() {
}

const string& SupplyDemandCurveSaver::getXMLNameStatic() {
  // This is the string you will use to refer to this object
  // in input files.
  const static string XML_NAME = "supply-demand-curve";
  return XML_NAME;
}

const string& SupplyDemandCurveSaver::getName() const {
    return mName;
}

/* Example XML:
<scenario>
   <supply-demand-curve-saver name="USAbiomass">
      <num-points>10</num-points>
   </supply-demand-curve-saver>
</scenario>
 */

bool SupplyDemandCurveSaver::XMLParse( const DOMNode* aNode ) {
    /*! \pre Make sure we were passed a valid node. */
    assert( aNode );
    
    // get the name attribute.
    mName = XMLHelper<string>::getAttr( aNode, XMLHelper<void>::name() );

    // get all child nodes.
    DOMNodeList* nodeList = aNode->getChildNodes();
    
    // loop through the child nodes.
    for ( unsigned int i = 0; i < nodeList->getLength(); i++ ){
        DOMNode* curr = nodeList->item( i );
        string nodeName = XMLHelper<string>::safeTranscode( curr->getNodeName() );
        
        if ( nodeName == XMLHelper<void>::text() ) {
            continue;
        }
        else if ( nodeName == "num-points" ) {
            mNumPoints = XMLHelper<int>::getValue( curr );
        }
        else {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Unknown element " << nodeName << " encountered while parsing " << getXMLNameStatic() << endl;
        }
    }
    
    return true;
}

void SupplyDemandCurveSaver::toInputXML( ostream& aOut, Tabs* aTabs ) const {
    // XMLWriteOpeningTag( getXMLNameStatic(), aOut, aTabs );

    aOut << "<" << getXMLNameStatic() << " name=\"" << mName << "\">" << endl;

    XMLWriteElement( mNumPoints, "num-points", aOut, aTabs );
    
    XMLWriteClosingTag( getXMLNameStatic(), aOut, aTabs );
}


/*! \brief Find and print supply-demand curves for designated market.
*
* This function creates a SupplyDemandCurve for the designated market by calculating
* the supply and demand at a series of prices, and to print the resulting curve.
*
* \author Rich Plevin (based on SolutionInfoSet::findAndPrintSD)
* \param aWorld The world to use to calculate new points.
* \param aMarketplace The marketplace to use to calculate new points.
* \param aPeriod Period for which to print supply-demand curves.
* \param aOut Output stream to print the curves to.
*/
void SupplyDemandCurveSaver::printSD( ostream& aOut, Scenario* aScenario, const int aPeriod, bool printHeader ) {
  World* world = aScenario->getWorld();
  Marketplace* marketplace = scenario->getMarketplace();
  SolutionInfoSet solnInfoSet = SolutionInfoSet( marketplace );
  SolutionInfoParamParser solnParams;
  solnInfoSet.init( aPeriod, 0.001, 0.001, &solnParams );
  vector<SolutionInfo> solvable = solnInfoSet.getSolvableSet();
  
  if ( solvable.size() == 0 )	// occurs in year 1975
    return;

  int market_index = getMarketIndex(mName, solvable);

  if ( market_index < 0 ) {
    aOut << "# Market for " << mName << " was not found." << endl;
    
  } else {
    SupplyDemandCurve sdCurve( market_index, mName );
    
    sdCurve.calculatePoints( mNumPoints, solnInfoSet, world, marketplace, aPeriod );
    sdCurve.print2( aOut, aPeriod, printHeader );
  }
}

/*! \brief Find the given marketName in the given vector of solvable markets and return it's index, or -1 if not found.
 */
int SupplyDemandCurveSaver::getMarketIndex(const string& marketName, vector<SolutionInfo> &aSolvable ) {
  for ( int i = 0; i < aSolvable.size(); ++i ) {
    if ( aSolvable[ i ].getName() == marketName )
      return i;
  }
  return -1;
}

void SupplyDemandCurveSaver::calcFeedbacksBeforePeriod( Scenario* aScenario,
							const IClimateModel* aClimateModel,
							const int aPeriod ) 
{
    // do nothing
}

void SupplyDemandCurveSaver::calcFeedbacksAfterPeriod( Scenario* aScenario,
						       const IClimateModel* aClimateModel,
						       const int aPeriod )
{
  if ( aPeriod == 0 )	// Nothing to do for initial period
    return;
  
  const Configuration* conf = Configuration::getInstance();
  std::string fileName = conf->getFile( "supplyDemandCurves", "supplyDemandCurves.csv");
  
  AutoOutputFile outFile(fileName, mOpenMode );
    
  // First time through (before resetting open mode to append) write header, too.
  printSD( *outFile, scenario, aPeriod, mOpenMode == std::ios_base::out);

  mOpenMode = std::ios_base::app;   // after first call, append
}
