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
 * \file complex_carbon_dot_printer.cpp
 * \ingroup Objects
 * \brief The ComplexCarbonDotPrinter class source file.
 *
 * \author Jim Naslund
 */

#include "util/base/include/definitions.h"

#include "reporting/include/complex_carbon_dot_printer.h"
#include "containers/include/region.h"
#include "util/base/include/util.h"
#include "ccarbon_model/include/carbon_box.h"
#include "ccarbon_model/include/npp.h"
#include "ccarbon_model/include/acarbon_flow.h"
#include "util/base/include/model_time.h"
#include "containers/include/scenario.h"

extern Scenario* scenario; // for modeltime

using namespace std;

/*! 
 * \brief Default Constructor
 * \param aRegionToPrint region to print.
 * \param aFile file to print to.
 */
ComplexCarbonDotPrinter::ComplexCarbonDotPrinter( const string& aRegionToPrint,
                                            ostream& aFile ):
mFile( aFile ),
mCorrectRegion( false ),
mRegionToPrint( aRegionToPrint )
{
}

/*!
 * \brief Begin visiting a region with the graph printer.
 * \details Opens the graph and prints the header.
 * \param aRegion Region to visit.
 * \param aPeriod Period for which to visit.
 */
void ComplexCarbonDotPrinter::startVisitRegion( const Region* aRegion, const int aPeriod ){
    // Check if this is the region to print.
    if(aRegion->getName() == mRegionToPrint ){
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
void ComplexCarbonDotPrinter::endVisitRegion( const Region* aRegion, const int aPeriod ){
    if( mCorrectRegion ){
        // Now close the graph.
        mFile << "}" << endl << endl;
    }
}

void ComplexCarbonDotPrinter::startVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                           const int aPeriod ){
    mFile << "digraph " << util::replaceSpaces( "watever" ) << " {" << endl;    
}
void ComplexCarbonDotPrinter::endVisitCarbonCalc( const ICarbonCalc* aCarbonCalc,
                         const int aPeriod ){
    mFile << "}" << endl << endl;
}

void ComplexCarbonDotPrinter::startVisitCarbonBox( const CarbonBox* aCarbonBox,
                                                   const int aPeriod ){
    const Modeltime* modeltime = scenario->getModeltime();
    mCurrentBox = aCarbonBox->getName();
    mFile << aCarbonBox->getName()
          << "[label=\"" << aCarbonBox->getName() << "\\n"
          << aCarbonBox->mStock->getStock( modeltime->getper_to_yr( aPeriod ) )
          << "\"];" << endl;
}

void ComplexCarbonDotPrinter::startVisitCarbonFlow( const ACarbonFlow* aCarbonFlow,
                           const int aPeriod ){
    mFile << mCurrentBox << "->" << aCarbonFlow->getTargetName()
          << "[label=\"" << aCarbonFlow->mFraction << "\"];" << endl;
}
