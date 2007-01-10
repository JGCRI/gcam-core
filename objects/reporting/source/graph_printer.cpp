/*
* This software, which is provided in confidence, was prepared by employees of
* Pacific Northwest National Laboratory operated by Battelle Memorial Institute.
* Battelle has certain unperfected rights in the software which should not be
* copied or otherwise disseminated outside your organization without the express
* written authorization from Battelle. All rights to the software are reserved
* by Battelle. Battelle makes no warranty, express or implied, and assumes no
* liability or responsibility for the use of this software.
*/

/*! 
* \file graph_printer.cpp
* \ingroup Objects
* \brief The GraphPrinter class source file.
*
* \author Josh Lurz
*/

#include "util/base/include/definitions.h"

#include <iomanip>
#include <string>
#include <iostream>

#include "reporting/include/graph_printer.h"
#include "util/base/include/util.h"
#include "containers/include/region.h"
#include "sectors/include/sector.h"
#include "sectors/include/demand_sector.h"
#include "technologies/include/technology.h"
#include "resources/include/resource.h"
#include "util/base/include/configuration.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"

extern Scenario* scenario;

using namespace std;

/*! \brief Default Constructor
* \param aRegionNameToPrint The region for which to print graphs.
*/
GraphPrinter::GraphPrinter( const string& aRegionToPrint, ostream& aFile ):
mFile( aFile ),
mRegionToPrint( aRegionToPrint ),
mCorrectRegion( false )
{
}

/*!
* \brief Begin visiting a region with the graph printer.
* \details Opens the graph and prints the header.
* \param aRegion Region to visit.
* \param aPeriod Period for which to visit.
*/
void GraphPrinter::startVisitRegion( const Region* aRegion, const int aPeriod ){
    // Check if this is the region to print.
    if( aRegion->getName() == mRegionToPrint ){
        mCorrectRegion = true;
        // Print the graph header.
        mFile << "digraph " << util::replaceSpaces( aRegion->getName() ) << " {" << endl;
    }
    else {
        // Don't print this region.
        mCorrectRegion = false;
    }
}

/*!
* \brief End visiting a region with the graph printer.
* \details Closes the graph.
* \param aRegion Region to visit.
* \param aPeriod Period for which to visit.
*/
void GraphPrinter::endVisitRegion( const Region* aRegion, const int aPeriod ){
    if( mCorrectRegion ){
        // Now close the graph.
        mFile << "}" << endl << endl;
    }
}

/*! \brief Add the resource to a dependency graph.
* \details Outputs a node for the resource with a label for the resource name
*          and style information.
* \author Josh Lurz, Steve Smith
* \param aResource Resource to output.
* \param aPeriod Period for which to output.
*/
void GraphPrinter::startVisitResource( const AResource* aResource, const int aPeriod ){
    if( !mCorrectRegion ){
        return;
    }

    // Output a node with the resource label and styling.
    mFile << "\t" << util::replaceSpaces( aResource->getName() ) << "[label=\"" << aResource->getName() 
            << "\", shape=box, style=filled, color=indianred1 ];" << endl;
}

/*! \brief Add the sector to a dependency graph.
* \details Outputs a node for the sector with a label containing the name. No
*          style information is added.
* \author Josh Lurz
* \param aSector Sector to output.
* \param aPeriod Period for which to output.
*/
void GraphPrinter::startVisitSector( const Sector* aSector, const int aPeriod ){
    if( !mCorrectRegion ){
        return;
    }

    // Store the name of the current sector without spaces, this is the name of
    // the node in the graph.
    mCurrSectorName = util::replaceSpaces( aSector->getName() );

    // Write out a node with a label.
    mFile << "\t" << mCurrSectorName << "[label=\"" << aSector->getName() << "\"];" << endl;
}

/*! \brief Visits the demand sector.
* \details This function adds the Sector specific coloring and style to the
*          dependency graph.
* \param aDemandSector Demand sector for which to write output.
* \param aPeriod Period to output.
* \author Josh Lurz
*/
void GraphPrinter::startVisitDemandSector( const DemandSector* aDemandSector, const int aPeriod ){
    if( !mCorrectRegion ){
        return;
    }
    // output sector coloring here.
   mFile << "\t" << util::replaceSpaces( aDemandSector->getName() )
           << " [style=filled, color=steelblue1 ];" << endl;
}

/*! \brief Add the technology to the graph.
* \details Adds the technology's fuel as an edge in the graph. Depending on the
*          configuration options, the path can be labeled with the price of the
*          input or the quantity used. There are three configuration variables
*          which affect the way the graph is printed.
*          <li>PrintPrices Use prices as the weights and labels for the edges
*              instead of quantities.</li>
*          <li>ShowNullPaths Show paths with weights below DISPLAY_THRESHOLD.</li>
*          <li>PrintValuesOnGraphs Activates printing labels on the edges
               with the weights, either prices or quantities of the input used.</li>
* \param aTechnology Technology for which to write output.
* \param aPeriod Period to output.
*/
void GraphPrinter::startVisitTechnology( const Technology* aTechnology, const int aPeriod ){
    if( !mCorrectRegion ){
        return;
    }
    
    // Do not show links with values below this.
    const double DISPLAY_THRESHOLD = 1E-5;
    
    // Number of digits to print of the value on the graph.
    const unsigned int DISPLAY_PRECISION = 2;

    // Values at which to switch the type of line used to display the link.
    const double DOTTED_LEVEL = 1.0;
    const double DASHED_LEVEL = 5.0;
    const double LINE_LEVEL = 10.0;

    // Set whether to print prices or quantities on the graph. Initialize the
    // value of the line to a price or quantity.
    double graphValue = 0;
    const static bool printPrices = Configuration::getInstance()->getBool( "PrintPrices", false );
    if( printPrices ){
        graphValue = scenario->getMarketplace()->getPrice( aTechnology->getFuelName(), mRegionToPrint,
                                                           aPeriod, false );
        // Technologies with fake fuels will have a price equal to
        // NO_MARKET_PRICE at this point. Reset the price to 0.
        if( graphValue == Marketplace::NO_MARKET_PRICE ){
            graphValue = 0;
        }
    } 
    else {
        graphValue = aTechnology->getInput( aPeriod );
    }

    // Add the edge to the graph with a weight determined by the value.
    const static bool showNullPaths = Configuration::getInstance()->getBool( "ShowNullPaths", false );
    if( showNullPaths || graphValue >  DISPLAY_THRESHOLD ) {
        mFile << "\t" << util::replaceSpaces( aTechnology->getFuelName() ) << " -> " << mCurrSectorName;
        mFile << " [style=\"";
        if( graphValue < DOTTED_LEVEL ) {
            mFile << "dotted";
        }
        else if ( graphValue < DASHED_LEVEL ) {
            mFile << "dashed";
        }
        else if ( graphValue < LINE_LEVEL ) {
            mFile << "";
        }
        else {
            mFile << "bold";
        }

        mFile << "\"";
    
        // Add a label to the graph optionally showing the weight, either the
        // price of the quantity.
        const static bool printValues = Configuration::getInstance()->getBool( "PrintValuesOnGraphs", false );
        if( printValues ) {
            mFile << ",label=\"";
            mFile << setiosflags( ios::fixed | ios::showpoint ) << setprecision( DISPLAY_PRECISION );
            mFile << graphValue;
            mFile << "\"";
        }
        mFile << "];" << endl;
    }
}
